"""Implements parts of the specification related to tokenization (also called "lexing"; see spec. sections numbered 3 and 4) of CSS text.

_All_ input is preserved as tokens are vended, including whitespace and comments; this is done to support more general parsing applications than what a strictly paint-by-numbers implementation would get us, where comments would be absent from the tokenized text. Filtering of code points mandated by the specification, is made effectively reversible (see the `source` attribute on `Token`) so that input source text can be recovered exactly.

Certain decisions in the code were motivated by the limits of the [MyPy] type checker which was taken into use to gain confidence in and for verification of the code base -- since MyPy is limited in its introspection of arbitrary code constructs, we had to rewrite some of the code for MyPy to be able to check it. For instance, MyPy does not "recognize" `dataclass`-decorated classes unless on the source level the decorator is written out using the `dataclass` identifer _verbatim_ (or `typing.dataclass_transform` is used, which we thought was too much a distraction for the reader). Without said constraint, one could use `partial` to alias the `dataclass` decorator, e.g. `tokenclass = dataclass(...)`, to create a derivative for annotating of `Token` sub-classes, but with MyPy that is unable to understand the intent, we had to find a way to have `@dataclass\n...` in the code verbatim. This and similar constraints are what has largely motivated compiling `tokenizing.py` from the corresponding template.
"""

from .preprocessing import filter_code_points, FilteredCodePoint
from ..utils import CP, BufferedPeekingReader, is_surrogate_code_point_ordinal, IteratorReader, join, parser_error, PeekingUnreadingReader

from abc import ABC
from collections.abc import Callable, Iterable, Iterator
from dataclasses import dataclass
from decimal import Decimal
from enum import auto, StrEnum
from functools import partial
from numbers import Number
from itertools import chain

from typing import cast, ClassVar, TypeVar

@dataclass(frozen=True, kw_only=True, slots=True)
class Token(ABC):
    """A class of objects that group codepoint sequences as part of tokenization.

    Tokenization emits a stream of these objects where each token encapsulates a sequence of code points, after a pattern, grouping together a word or a number or a bracket, a comment or a quoted string, etc. The end result of tokenization, a stream of tokens, is the principal type of input to the parser proper.
    """
    source: str # The original text in the code point stream that the token is formed from

def tokenize(input: PeekingUnreadingReader[FilteredCodePoint], *, parser_error: Callable[..., None] = parser_error, unicode_ranges_allowed: bool = False) -> Iterator[Token]:
    """Generate a sequence of tokens from a sequence of [filtered] code points (characters, i.e. in a CSS file).

    Deviation from the spec.: no input normalization is done -- we don't want to "predict" type of input from a potentially unbound set of types and we want to allow caller to be able to use their own types of input (as long as it follows expected protocol(s), i.e. the interface).

    Tokenization, as this is also called, may or may not generally be an explicit stage during parsing. Because the specification refers to it as one, it is featured in this implemention explicitly as well, taking care of turning sequences of code points into tokens, for the parser proper to consume and turn into productions assembled into a so-called parse tree (aka parse product).

    Implements http://drafts.csswg.org/css-syntax/#css-tokenize.
    """
    def current_cp() -> FilteredCodePoint:
        """See http://drafts.csswg.org/css-syntax/#current-input-code-point."""
        return consumed[-1]
    def next(n: int) -> str:
        """See http://drafts.csswg.org/css-syntax/#next-input-code-point."""
        return join(input.peek(n))
    consumed: list[FilteredCodePoint] = [] # Tokens that have been consumed; trimmed from the head, as needed
    def consume(n: int) -> None:
        """Consume the next code point from the stream.

        Consuming removes a [filtered] code point from the stream. If no code points are available for consumption (the stream is "exhausted"), an empty string signifying the so-called EOF ("end of file", see https://drafts.csswg.org/css-syntax/#eof-code-point) value, is consumed instead.
        """
        nonlocal consumed # required for the `+=` to work for mutable non-locals like lists (despite the fact that the equivalent `extend` does _not_ require the statement)
        consumed += input.read(n) or [ FilteredCodePoint('', source='') ]
    def reconsume(*cps: FilteredCodePoint) -> None:
        """See http://drafts.csswg.org/css-syntax/#reconsume-the-current-input-code-point."""
        assert cps and list(cps) == consumed[-len(cps):]
        input.unread(cps)
        del consumed[-len(cps):]
    T = TypeVar('T', bound=Token)
    def sourced(cls: type[T]) -> Callable[..., T]:
        """Return a callable that constructs a token of specified class (`cls`) with its `source` attribute initialized automatically.

        :param cls: Class of token to create
        :returns: A callable that creates and returns a new token of type `cls`, with the required `source` attribute initialized automatically
        """
        return lambda *args, **kwargs: cls(*args, **kwargs, source=join(cp.source for cp in consumed))
    def consume_comment_token() -> CommentToken: # Not in the spec. -- this one is our own addition, to preserve comments
        """Tokenize the leading `/* ... */` sequence of code points in the stream and return corresponding token."""
        assert next(2) == '/*'
        consume(2)
        while next(1):
            if next(2) == '*/':
                consume(2)
                return sourced(CommentToken)(value=join(consumed[2:-2]))
            consume(1)
        else:
            parser_error()
            return sourced(CommentToken)(value=join(consumed[2:]))
    def is_valid_escape(cps: str | None = None) -> bool:
        """See http://drafts.csswg.org/css-syntax/#starts-with-a-valid-escape."""
        if not cps:
            cps = current_cp() + next(1)
        return cps[0:1] == '\\' and not is_newline(cps[1:2])
    def consume_escaped_code_point() -> CP:
        """See http://drafts.csswg.org/css-syntax/#consume-escaped-code-point."""
        assert is_valid_escape()
        consume(1)
        match consumed[-1]:
            case cp if is_hex_digit(cp):
                mark = len(consumed)
                while is_hex_digit(next(1)) and len(consumed) - mark < 5:
                    consume(1)
                if is_whitespace(next(1)):
                    consume(1)
                num = int(join(consumed[mark-1:]), 16)
                return '\ufffd' if (num == 0 or is_for_surrogate(num) or num > 0x10ffff) else chr(num)
            case '':
                parser_error()
                return '\ufffd'
            case _ as cp:
                return cp
    def consume_ident_like_token() ->  FunctionToken | IdentToken | URLToken | BadURLToken:
        """See http://drafts.csswg.org/css-syntax/#consume-ident-like-token."""
        string = consume_ident_sequence()
        if next(1) == '(':
            consume(1)
            if string.lower() == 'url':
                while all(is_whitespace(cp) for cp in next(2)):
                    consume(1)
                # Had to clarify the condition determining whether to return a function or url roken, with one of the spec. authors (Anne van Kesteren), as the wording in the spec. was confusing to me; see http://matrix.to/#/!AGetWbsMpFPdSgUrbs:matrix.org/$hbuveKYzzKp2_yYADhEGuN3WsVSuNccfcC9TjzlNt8s and/or http://github.com/w3c/csswg-drafts/issues/10120
                if not ((cps := next(2))[0:1] in ('"', '\'') or (is_whitespace(cps[0:1]) and cps[1:2] in ('"', '\''))):
                    return consume_url_token()
            return sourced(FunctionToken)(value=string)
        else:
            return sourced(IdentToken)(value=string)
    def consume_ident_sequence() -> str:
        """See http://drafts.csswg.org/css-syntax#consume-name."""
        result = ''
        while True:
            consume(1)
            match consumed[-1]:
                case cp if is_ident_code_point(cp):
                    result += cp
                case _ if is_valid_escape():
                    result += consume_escaped_code_point()
                case _:
                    reconsume(current_cp())
                    return result
    def consume_number() -> tuple[ Number, NumberTokenType, NumberTokenSign | None ]:
        """See http://drafts.csswg.org/css-syntax#consume-number."""
        type = NumberTokenType.integer
        number, exponent = '', ''
        sign: NumberTokenSign | None = None
        if (cp := next(1)) in ('+', '-'):
            consume(1)
            number += (sign := NumberTokenSign(cp))
        while is_digit(cp := next(1)):
            consume(1)
            number += cp
        if (cps := next(2))[0:1] == '.' and is_digit(cps[1:2]):
            consume(1)
            number += '.'
            while is_digit(cp := next(1)):
                consume(1)
                number += cp
            type = NumberTokenType.number
        if (cps := next(3))[0:1] in ('E', 'e') and ((cps[1:2] in ('-', '+') and is_digit(cps[2:3])) or is_digit(cps[1:2])):
            consume(1)
            if (cp := next(1)) in ('+', '-'):
                consume(1)
                exponent += cp
            while is_digit(cp := next(1)):
                consume(1)
                exponent += cp
            type = NumberTokenType.number
        # A straight-forward (if functional) way of compiling a number from the above components would be `(int if type == 'integer' else float)(join(number)) * 10**(int(join(exponent)) if exponent else 0)` but IEEE-754 floating point arithmetics gets in the way where e.g. `12 * 10**-1` is `1.2000000000000002` which isn't "precise" enough for at least doing _validation_ of the parser (i.e. unit tests).
        # An alternative, which doesn't work for all values of `number`, is `eval((lambda x: x.lstrip('+-0') or x)(join(number)) + ('e' + join(exponent) if exponent else ''))`.
        return (int if type == 'integer' else float)(Decimal(number) * Decimal(10) ** Decimal(exponent or '0')), type, sign # `Decimal` covers our requirements.
    def consume_numeric_token() -> DimensionToken | NumberToken | PercentageToken:
        """See http://drafts.csswg.org/css-syntax#consume-numeric-token."""
        value, type, sign = consume_number()
        if starts_ident_sequence(next(3)):
            return sourced(DimensionToken)(value=value, sign=sign, type=type, unit=consume_ident_sequence())
        elif next(1) == '%':
            consume(1)
            return sourced(PercentageToken)(value=value, sign=sign)
        else:
            return sourced(NumberToken)(value=value, sign=sign, type=type)
    def consume_remnants_of_bad_url() -> None:
        """See http://drafts.csswg.org/css-syntax#consume-remnants-of-bad-url."""
        while True:
            consume(1)
            match consumed[-1]:
                case ')' | '':
                    return
                case _ if is_valid_escape():
                    consume_escaped_code_point()
    def consume_url_token() -> URLToken | BadURLToken:
        """See http://drafts.csswg.org/css-syntax#consume-url-token."""
        while is_whitespace(next(1)):
            consume(1)
        value = ''
        while True:
            consume(1)
            match cast(str, consumed[-1]):
                case ')':
                    break
                case '':
                    parser_error()
                    break
                case cp if is_whitespace(cp):
                    while is_whitespace(cp := next(1)):
                        consume(1)
                    if cp in (')', ''):
                        consume(1)
                        if cp == '':
                            parser_error()
                        break
                    else:
                        consume_remnants_of_bad_url()
                        return sourced(BadURLToken)()
                case cp if cp in ('"', '\'', '(') or is_non_printable_code_point(cp):
                    parser_error()
                    consume_remnants_of_bad_url()
                    return sourced(BadURLToken)()
                case '\\':
                    if is_valid_escape():
                        value += consume_escaped_code_point()
                    else:
                        parser_error()
                        consume_remnants_of_bad_url()
                        return sourced(BadURLToken)()
                case _ as cp:
                    value += cp
        return sourced(URLToken)(value=value)
    def starts_ident_sequence(cps: str | None = None) -> bool:
        """See http://drafts.csswg.org/css-syntax#would-start-an-identifier."""
        if not cps:
            cps = current_cp() + next(2)
        match cps[0]:
            case '-':
                return (is_ident_start_code_point(cp := cps[1:2]) or cp == '-') or is_valid_escape(cps[1:3])
            case cp if is_ident_start_code_point(cp):
                return True
            case '\\':
                return is_valid_escape(cps[0:2])
            case _:
                return False
    def starts_number(cps: str | None = None) -> bool:
        """See http://drafts.csswg.org/css-syntax#starts-with-a-number."""
        if not cps:
            cps = current_cp() + next(2)
        match cps[0]:
            case '+' | '-':
                return is_digit(cp := cps[1:2]) or (cp == '.' and is_digit(cps[2:3]))
            case '.':
                return is_digit(cps[1:2])
            case cp if is_digit(cp):
                return True
            case _:
                return False
    def starts_unicode_range(cps: str | None = None) -> bool:
        """See http://drafts.csswg.org/css-syntax#starts-a-unicode-range."""
        if not cps:
            cps = current_cp() + next(2)
        return cps[0:1] in ('U', 'u') and cps[1:2] == '+' and ((cp := cps[2:3]) == '?' or is_hex_digit(cp))
    def consume_token() -> Token | None:
        """See http://drafts.csswg.org/css-syntax#consume-token."""
        assert not consumed
        consume(1)
        match consumed[-1]:
            case cp if is_whitespace(cp):
                while is_whitespace(next(1)):
                    consume(1)
                return sourced(WhitespaceToken)(value=join(consumed))
            case '"' | '\'':
                return consume_string_token()
            case '#' as cp:
                if is_ident_code_point(next(1)) or is_valid_escape(next(2)):
                    return sourced(HashToken)(**(dict(type=HashTokenType.id) if starts_ident_sequence(next(3)) else {}), value=consume_ident_sequence())
                else:
                    return sourced(DelimToken)(value=cp)
            case '(':
                return sourced(OpenParenToken)()
            case ')':
                return sourced(CloseParenToken)()
            case '+' as cp:
                if starts_number():
                    reconsume(current_cp())
                    return consume_numeric_token()
                else:
                    return sourced(DelimToken)(value=cp)
            case ',':
                return sourced(CommaToken)()
            case '-' as cp:
                if starts_number():
                    reconsume(current_cp())
                    return consume_numeric_token()
                elif next(2) == '->': # The spec. doesn't say much about "CDC"/"CDO" but there is http://codereview.chromium.org/825063004 (which, somewhat ironically, links the spec.)
                    consume(2)
                    return sourced(CDCToken)()
                elif starts_ident_sequence():
                    reconsume(current_cp())
                    return consume_ident_like_token()
                else:
                    return sourced(DelimToken)(value=cp)
            case '.' as cp:
                if starts_number():
                    reconsume(current_cp())
                    return consume_numeric_token()
                else:
                    return sourced(DelimToken)(value=cp)
            case ':':
                return sourced(ColonToken)()
            case ';':
                return sourced(SemicolonToken)()
            case '<' as cp:
                if next(3) == '!--':
                    consume(3)
                    return sourced(CDOToken)()
                else:
                    return sourced(DelimToken)(value=cp)
            case '@' as cp:
                if starts_ident_sequence(next(3)):
                    return sourced(AtKeywordToken)(value=consume_ident_sequence())
                else:
                    return sourced(DelimToken)(value=cp)
            case '[':
                return sourced(OpenBracketToken)()
            case '\\' as cp:
                if is_valid_escape():
                    reconsume(current_cp())
                    return consume_ident_like_token()
                else:
                    parser_error()
                    return sourced(DelimToken)(value=cp)
            case ']':
                return sourced(CloseBracketToken)()
            case '{':
                return sourced(OpenBraceToken)()
            case '}':
                return sourced(CloseBraceToken)()
            case cp if is_digit(cp):
                reconsume(current_cp())
                return consume_numeric_token()
            case 'U' | 'u' as cp:
                reconsume(current_cp())
                return consume_unicode_range_token() if unicode_ranges_allowed and starts_unicode_range(next(3)) else consume_ident_like_token()
            case cp if is_ident_start_code_point(cp):
                reconsume(current_cp())
                return consume_ident_like_token()
            case cp if cp == '/' and next(1) == '*':
                reconsume(current_cp())
                return consume_comment_token()
            case '':
                return None
            case _ as cp:
                return sourced(DelimToken)(value=cp)
    def consume_string_token(ending_cp: CP | None = None) -> StringToken | BadStringToken:
        """See http://drafts.csswg.org/css-syntax#consume-string-token."""
        if not ending_cp:
            ending_cp = consumed[-1]
        value = ''
        while True:
            consume(1)
            match cast(str, consumed[-1]):
                case cp if cp == ending_cp:
                    break
                case '':
                    parser_error()
                    break
                case cp if is_newline(cp):
                    parser_error()
                    reconsume(current_cp())
                    return sourced(BadStringToken)()
                case '\\':
                    if not (cp := next(1)):
                        pass
                    elif is_newline(cp):
                        consume(1)
                    else:
                        value += consume_escaped_code_point()
                case _ as cp:
                    value += cp
        return sourced(StringToken)(value=value)
    def consume_unicode_range_token() -> UnicodeRangeToken:
        """See http://drafts.csswg.org/css-syntax#consume-unicode-range-token."""
        assert starts_unicode_range(next(3))
        consume(2)
        mark = len(consumed)
        while is_hex_digit(next(1)) and not (len(consumed) - mark > 6):
            consume(1)
        while (len(consumed) - mark < 6) and next(1) == '?':
            consume(1)
        first_segment = consumed[mark:]
        if '?' in first_segment:
            range_start = int(join('0' if cp == '?' else cp for cp in first_segment), 16)
            range_end = int(join('F' if cp == '?' else cp for cp in first_segment), 16)
        else:
            range_start = int(join(first_segment), 16)
            if (cps := next(2)) and cps[0] == '-' and (cps[1:] and is_hex_digit(cps[1])):
                consume(1)
                mark = len(consumed)
                while is_hex_digit(next(1)) and not (len(consumed) - mark > 6):
                    consume(1)
                range_end = int(join(consumed[mark:]), 16)
            else:
                range_end = range_start
        return sourced(UnicodeRangeToken)(start=range_start, end=range_end)
    while token := consume_token():
        yield token
        consumed.clear()

def normalize_input(input: str | Callable[[], CP] | Iterable[str]) -> PeekingUnreadingReader[FilteredCodePoint]:
    """Normalize input to the tokenization procedure.

    This is a convenience procedure which offers the caller of e.g. `tokenize` to not have to speculate on how to construct input to the latter while allowing `tokenize` to be strict about its input type (so that its code can be more easily inspected and verified).

    :param input: An input object to "normalize"; if a [regular Python] string, the value is interpreted as a sequence of _unfiltered_ (the spec. mandates "filtering" of original input; see also `./preprocessor.py`) code points and is normalized into an object that filters and vends these code points on-demand; if a callable object, the value is intepreted as a `next` function that takes no parameters and when called returns the next _unfiltered_ code point in a stream of code points, presumed for tokenization; otherwise, the input is assumed to be iterable and yield _unfiltered_ code points, same as in the former case
    :return: An object representing the passed `input`, of type fitting for passing as the `input` parameter to the `tokenize` procedure
    """
    match input:
        case str():
            input = partial(next, iter(input), '')
        case Callable(): # type: ignore # False positive with at least MyPy, see also http://github.com/python/typeshed/issues/11766
            pass
        case Iterable():
            # Assume the iterable vends sequences of code points (e.g. `str` values); whether each sequence is of length 1 (i.e. is a single code point) or not, normalization will treat `input` as if vending sequences of multiple code points (even if a particular sequence consists of only one)
            input = partial(next, chain.from_iterable(input), '')
        case _:
            raise TypeError
    # Assume `input` is an iterable of code point sequences and that code points are not filtered (per the filtering procedure defined by the spec.).
    return BufferedPeekingReader(IteratorReader(filter_code_points(input)))

@macro # The `macro` is a decorator recognized by the template processor to replace the entire decorated procedure definition with the "unparsed" result of calling the procedure; the decorator is not valid outside of template processing context (as is evident from the fact there's no corresponding definition of `template`, it's a purely syntactical construct)
def token_types():
    """Create and return a sequence of syntactical constructs to insert into current module.

    This procedure is called during processing of the module file assumed to be a template (having the `.template.py` at the end of the name), into the corresponding Python module proper.

    :returns: A sequence of abstract syntax tree nodes (`Node` from the built-in `ast` module) to insert in place of the procedure definition
    """
    import ast
    from functools import partial
    from itertools import chain, groupby
    from typing import Callable, Iterable
    def aggregate(prefix_attrs_pairs, fn: Callable, *, key: Callable) -> Iterable[tuple]:
        """Aggregate pairs of (prefix, attrs) using specified function.

        See http://en.wikipedia.org/wiki/Aggregate_function.

        E.g. for the tuple `(('Foo', (('bar', int),)) ('Foo', ('baz', str)))` for `prefix_attrs_pairs` and `sum` for `fn` the tuple is sorted (involving `key`) and Python's own `groupby` is called before `sum` sums up the attribute specifications (second item of each pair) which all results in the tuple `('Foo', (('bar', int), ('baz', str)))`.
        """
        return ((prefix, fn((fields for _, fields in group), start=())) for prefix, group in groupby(sorted(prefix_attrs_pairs, key=key), key=key))
    aggregate_attributes = partial(aggregate, fn=sum, key=lambda pair: pair[0])
    return [
        *(ast.ClassDef(name, bases=[ ast.Name('StrEnum') ], keywords=[], body=[ *(ast.Assign([ ast.Name(name) ], ast.Constant(values[name] if isinstance(values, dict) else name)) for name in values) ], decorator_list=[]) for name, values in (('HashTokenType', ('id', 'unrestricted')), ('NumberTokenSign', dict(plus='+', minus='-')), ('NumberTokenType', ('integer', 'number')))),
        *(ast.ClassDef(prefix + 'Token', bases=[ ast.Name('Token') ], keywords=[], body=[ *((ast.AnnAssign(ast.Name(name), type, (rest[0] if rest else None), simple=1) if type else ast.Assign([ ast.Name(name) ], (rest[0] if rest else None))) for name, type, *rest in sorted(attributes, key=len)) ] or [ ast.Pass() ], decorator_list=[ ast.Call(ast.Name('dataclass'), args=[], keywords=[ ast.keyword('frozen', ast.Constant(True)), ast.keyword('kw_only', ast.Constant(True)), ast.keyword('slots', ast.Constant(True)) ]) ]) for prefix, attributes in aggregate_attributes(chain(
        ((prefix, ()) for prefix in ('AtKeyword', 'BadString', 'BadURL', 'CDC', 'CDO', 'Colon', 'Comma', 'Delim', 'Dimension', 'Function', 'Hash', 'Ident', 'OpenBrace', 'OpenBracket', 'OpenParen', 'Number', 'Percentage', 'CloseBrace', 'CloseBracket', 'CloseParen', 'Semicolon', 'String', 'UnicodeRange', 'URL', 'Whitespace')),
        ((prefix, (('value', ast.Name('str')),)) for prefix in (('AtKeyword', 'Function', 'Hash', 'Ident', 'String', 'URL') + ('Whitespace',))), # In this impl. "Whitespace" tokens also have value
        (('Hash', (('type', ast.Name('HashTokenType'), ast.Attribute(ast.Name('HashTokenType'), 'unrestricted')),)),),
        (('Delim', (('value', ast.Name('str')),)),),
        ((prefix, (('value', ast.BinOp(ast.Name('int'), ast.BitOr(), ast.Name('float'))), ('sign', ast.BinOp(ast.Name('NumberTokenSign'), ast.BitOr(), ast.Constant(None)), ast.Constant(None)))) for prefix in ('Number', 'Percentage', 'Dimension')),
        ((prefix, (('type', ast.Name('NumberTokenType'), ast.Attribute(ast.Name('NumberTokenType'), 'integer')),)) for prefix in ('Number', 'Dimension')),
        ((prefix, (('unit', ast.Name('str')),)) for prefix in ('Dimension',)),
        ((prefix, (('start', ast.Name('int')), ('end', ast.Name('int')))) for prefix in ('UnicodeRange',)),
        # TODO: Turn the following set of `mirror_type` definitions into something less repetitive
        ((prefix, (('mirror_type', ast.Subscript(ast.Name('ClassVar'), ast.Subscript(ast.Name('type'), ast.Constant('OpenBraceToken')))),)) for prefix in ('CloseBrace',)),
        ((prefix, (('mirror_type', ast.Subscript(ast.Name('ClassVar'), ast.Subscript(ast.Name('type'), ast.Constant('OpenBracketToken')))),)) for prefix in ('CloseBracket',)),
        ((prefix, (('mirror_type', ast.Subscript(ast.Name('ClassVar'), ast.Subscript(ast.Name('type'), ast.Constant('OpenParenToken')))),)) for prefix in ('CloseParen',)),
        ((prefix, (('mirror_type', ast.Subscript(ast.Name('ClassVar'), ast.Subscript(ast.Name('type'), ast.Constant('CloseBraceToken')))),)) for prefix in ('OpenBrace',)),
        ((prefix, (('mirror_type', ast.Subscript(ast.Name('ClassVar'), ast.Subscript(ast.Name('type'), ast.Constant('CloseBracketToken')))),)) for prefix in ('OpenBracket',)),
        ((prefix, (('mirror_type', ast.Subscript(ast.Name('ClassVar'), ast.Subscript(ast.Name('type'), ast.Constant('CloseParenToken')))),)) for prefix in ('OpenParen',)),
        ))) ]

class CommentToken(WhitespaceToken):
    """Class of tokens for capturing comments ("/* ... */"), beyond the spec.

    Since the spec. instructs to consume and _discard_ comments during tokenization (http://drafts.csswg.org/css-syntax/#consume-token), if we're to guarantee all input can be "unparsed", comments must be preserved somehow. We choose to preserve these with tokens, much like the rest of the input, and consider it prudent to have a dedicated class for these. A compliant parser only deals with white-space, since that's what the spec. instructs it to, and this class being a subclass of `WhitespaceToken` allows such parser to be oblivious to comments in the token stream since to it they appear as "true" white-space. With this class, a parser, should it choose to stray from the spec., would be able to distinguish comments from [other kind(s) of] white-space.

    There's otherwise nothing noteworthy about comment tokens beyond what the super-class already allows -- there's `source` and there's `value` for holding the text between "/*" and "*/".
    """
    pass

# TODO: Create the corresponding effect with template processing instead
CloseBraceToken.mirror_type = OpenBraceToken
CloseBracketToken.mirror_type = OpenBracketToken
CloseParenToken.mirror_type = OpenParenToken
OpenBraceToken.mirror_type = CloseBraceToken
OpenBracketToken.mirror_type = CloseBracketToken
OpenParenToken.mirror_type = CloseParenToken

def is_digit(cp: CP) -> bool:
    """See http://drafts.csswg.org/css-syntax/#digit."""
    return ('0' <= cp <= '9')

is_for_surrogate = is_surrogate_code_point_ordinal # Alias, for convenience [of following the spec.]

def is_hex_digit(cp: CP) -> bool:
    """See http://drafts.csswg.org/css-syntax/#hex-digit."""
    return is_digit(cp) or ('A' <= cp <= 'F') or ('a' <= cp <= 'f')

def is_ident_code_point(cp: CP) -> bool:
    """See http://drafts.csswg.org/css-syntax/#ident-code-point."""
    return is_ident_start_code_point(cp) or is_digit(cp) or cp == '-'

def is_ident_start_code_point(cp: CP) -> bool:
    """See http://drafts.csswg.org/css-syntax/#ident-start-code-point."""
    return ('A' <= cp <= 'Z') or ('a' <= cp <= 'z') or is_non_ascii_ident_code_point(cp) or cp == '_'

def is_newline(cp: CP) -> bool:
    """See http://drafts.csswg.org/css-syntax/#newline."""
    return cp == '\n'

def is_non_ascii_ident_code_point(cp: CP) -> bool:
    """See http://drafts.csswg.org/css-syntax/#non-ascii-ident-code-point."""
    return cp == '\u00b7' or '\u00c0' <= cp <= '\u00d6' or '\u00d8' <= cp <= '\u00f6' or '\u00f8' <= cp <= '\u037d' or '\u037f' <= cp <= '\u1fff' or cp in ('\u200c', '\u200d', '\u203f', '\u2040') or '\u2070' <= cp <= '\u218f' or '\u2c00' <= cp <= '\u2fef' or '\u3001' <= cp <= '\ud7ff' or '\uf900' <= cp <= '\ufdcf' or '\ufdf0' <= cp <= '\ufffd' or cp >= '\U00010000'

def is_non_printable_code_point(cp: CP) -> bool:
    """See http://drafts.csswg.org/css-syntax/#non-printable-code-point."""
    return '\u0000' <= cp <= '\u0008' or cp == '\u000b' or '\u000e' <= cp <= '\u001f' or cp == '\u007f'

def is_whitespace(cp: CP) -> bool:
    """See http://drafts.csswg.org/css-syntax/#whitespace."""
    return is_newline(cp) or cp in ('\t', ' ')
