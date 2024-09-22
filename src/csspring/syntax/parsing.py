"""Implements parts of the [Syntax] specification related to parsing (see section 5) of CSS text.

NOTE: Parsing of specifically CSS selector expressions -- the "prelude" part in qualified rule syntax, as it is referred to by the specification -- is _not_ covered by the specification alone. The specification only instructs to effectively _tokenize_ the part of the stream spanning a qualified rule's selector list expression (as it is called by the ["Selectors Level 4"](http://drafts.csswg.org/selectors-4) specification), which falls short of parsing it. Rest assured, however, that the `csspring` package, when initialized, augments `QualifiedRule` with the `selector_list` attribute which returns the rule's prelude part _parsed_ as a selector list (using the `csspring.selectors` module).

NOTE: We are aligned with a _draft_ of the specification. Drafts change during their development which makes correlation and verification of compliance effectively impossible -- at any time the URL of a draft may offer content that has changed from the time this module was implemented and no longer features elements referenced here. We had little choice in this matter, however -- it was either implement the module in accordance with the relatively stable technical recommendation (TR) version published by W3C, which contained ambiguities that would have made a _broken_ parser (as much was [arguably alluded to by by one of the co-editors of the specification](http://github.com/w3c/csswg-drafts/issues/10119#issuecomment-2016850313)). Ideally, we would of course prefer to track a specific version of the specification, as opposed to a draft that is edited in place, but alas.

# Deviations

* The specification mentions "set the stylesheet rules to the result" (5.4.3), but for preservation of input in this parser, a `Contents` object is instead referenced directly by `StyleSheet` object(s), while the `rules` property is read-only and thus cannot be set; it is association of a `Contents` object with a `StyleSheet` object, done through appending of elements to the latter in its capacity as a list, that has the effect of associating said "rules" with the stylesheet
* For preservation of input, "discarding" of a token is equivalent to consuming it, in fact there's no `discard_token` procedure or even an alias of one -- `consume_token` is used instead
* EOF (end-of-file) token isn't featured in the parse tree, and `None` is used to identify the condition
"""

from . import tokenizing
from .tokenizing import Token, tokenize
from .tokenizing import AtKeywordToken, BadStringToken, BadURLToken, CDOToken, CDCToken, ColonToken, CommaToken, CommentToken, DelimToken, DimensionToken, FunctionToken, HashToken, IdentToken, OpenBraceToken, OpenBracketToken, OpenParenToken, NumberToken, CloseBraceToken, CloseBracketToken, CloseParenToken, PercentageToken, SemicolonToken, StringToken, URLToken, WhitespaceToken
from ..utils import Appender, is_custom_property_name_string, parser_error, ParseError

import builtins
from itertools import chain

from collections.abc import Iterable, Iterator, MutableSequence, Sequence
from typing import cast, Protocol, runtime_checkable, TypeAlias, Union

class InvalidRuleError(ParseError):
    """See http://drafts.csswg.org/css-syntax/#invalid-rule-error."""
    pass

# A [parse] _product_ is an object returned by the parser and its constituents. As evident from the type definition, the type references itself -- products make up other products, ultimately making up what is generally called a _parse tree_. A _railroad diagram_, for example, consists of [grammar] _productions_, which correspond to products when the parser actually consumes input
Product = Sequence[Union['Product', Token]] # The [immutable] product type is offered as a convenient default super-type of parse product, in case the parser is ever re-factored to use type of product which is immutable after construction, e.g. one using `tuple` as the base class

MutableProduct = list[Union['MutableProduct', Token]] # Mutable products are necessarily used by the parser itself as the spec. pretty much mandates use of an "append" procedure during parsing

class Function(MutableProduct):
    """Class of parse products that represent parsed CSS function calls.

    Example: in `foo { bar: baz(); }`, `baz()` is parsed to a `Function` object with `'baz'` for value of its `name` attribute, and `()` parsed as a component value referenced by its `value` attribute.

    See also http://drafts.csswg.org/css-syntax/#function.
    """
    @property
    def name(self) -> str:
        return self[0] # type: ignore
    @property
    def value(self) -> Sequence['ComponentValue']:
        return self[1] # type: ignore

class SimpleBlock(MutableProduct):
    """Class of parse products that represent sequences of tokens surrounded by pair of parentheses/brackets/braces.

    Example: in `foo { color: rgb(100, 0, 0); }` the `(100, 0, 0)` is parsed as a `SimpleBlock` object embedding the sequence of tokens starting with the opening parenthesis, through the number and comma tokens, and ending with the closing parenthesis.

    See also http://drafts.csswg.org/css-syntax/#simple-block.
    """
    @property
    def token(self) -> Token:
        return self[0] # type: ignore
    @property
    def value(self) -> Sequence['ComponentValue']:
        return self[1] # type: ignore

ComponentValue = Token | Function | SimpleBlock # See http://drafts.csswg.org/css-syntax/#component-value; TODO: Switch from `Token` to a [more constrained] "preserved token" type as per the spec.

class Declaration(MutableProduct):
    """Class of parse products that represent a CSS declaration.

    Example: in `foo { color: red; }` the `color: red` is parsed as a `Declaration`, with `'color'` for its `name` attribute and a list (consisting of the identifier token with the value `"red"`) for its `value` attribute.

    See also http://drafts.csswg.org/css-syntax/#declaration.
    """
    original_text: str | None
    @property
    def name(self) -> str:
        return cast(IdentToken, self[0]).value
    @property
    def value(self) -> Sequence[ComponentValue]:
        return cast(Sequence[ComponentValue], self[4])
    @property
    def important(self) -> bool:
        return cast(bool, self[6])

class Rule(MutableProduct):
    """An abstract class of parse products that represent a CSS rule.

    See http://drafts.csswg.org/css-syntax/#css-rule.
    """
    pass

class AtRule(Rule):
    """Class of parse products that represent a so-called "at-rule" (identifiable in CSS text as rule definition statements with leading `@`).

    See http://drafts.csswg.org/css-syntax/#at-rule.
    """
    @property
    def name(self) -> str:
        return cast(AtKeywordToken, self[0]).value
    @property
    def prelude(self) -> Sequence[ComponentValue]:
        """Sequence of tokens featured by the "head" part of the rule and ending with the opening brace.

        May be fed to a CSS selector list parser for additional structure.
        """
        return cast(Sequence[ComponentValue], self[1])
    @property
    def block(self) -> 'Block | None':
        """The block contains declarations and child rules."""
        return self[2] if isinstance(self[2], Block) else None

class QualifiedRule(Rule):
    """A class of parse products that represent the commonly known CSS "style" rule.

    Keep in mind that in accordance to the ruling specification, "prelude" part of a CSS rule remains _normally_ (during parsing per the spec. at least) effectively unparsed in the resulting product.

    See http://drafts.csswg.org/css-syntax/#qualified-rule.
    """
    @property
    def prelude(self) -> Sequence[ComponentValue]:
        return cast(Sequence[ComponentValue], self[0])
    @property
    def block(self) -> 'Block':
        return cast(Block, self[1])
    @property
    def declarations(self) -> Sequence[Declaration]:
        """The declarations nested _directly_ in the rule.

        The specification mandates declarations be "split" into multiple groups if e.g. interrupted by [nested] rules, i.e. in parsed `foo { a: 1; bar { }; b: 1 }`, there are _two_ lists of declarations -- the first containing `a: 1`, and the second containing `b: 1`, due to the presence of the nested [qualified] rule splitting the body of `foo`.

        This also serves as a short-hand for accessing the declarations on the contents of the block of the rule.
        """
        return self.block.contents.declarations
    @property
    def rules(self) -> Sequence[Rule]:
        """The "child" rules of the rule.

        This also serves as a short-hand for accessing the rules on the contents of the block of the rule.
        """
        return self.block.contents.rules

class Contents(MutableProduct):
    """A class of parse products that represent contents of a block.

    A block normally represents also its pair of opening and closing parentheses/brackets/braces, while its contents only represent everything else in the block (meaning that which is contained in between the aforementioned pair of parentheses/brackets/braces).

    This class isn't specified by the ruling specification, but is a convenience offered by the parser.
    """
    @property
    def declarations(self) -> Sequence[Declaration]:
        """Declarations that are _directly_ nested (i.e. 'children') as part of these contents."""
        return tuple(item for item in self if isinstance(item, Declaration))
    @property
    def rules(self) -> Sequence[Rule]:
        """Rules that are _directly_ nested (i.e. 'children') as part of these contents."""
        return tuple(item for item in self if isinstance(item, Rule))

class Block(MutableProduct):
    """A class of parse products where the first and last element are a pair of [corresponding] brackets.

    E.g. `{ display: none; & > foo { --bar: 1 } }` is called a block and the parser produces a `Block` parsing it.

    Note that a "block" as modeled by this class is not defined by the specification beyond what the latter refers to with "simple block" and its sub-types "{}-block", "[]-block" and "()-block". This class does _not_ model any of the mentioned types, but is a related type which should be considered _neither_ a superset nor a subset of these. This class models blocks which are the "body" of a qualified rule, although it arguably does share _some_ traits with the types defined by the spec. -- a `Block` starts and ends with a bracket, for example -- unlike `SimpleBlock` and its defined sub-types, a `Block` may contain rules and declarations, while a `SimpleBlock` may only contain component values.
    """
    @property
    def token(self) -> Token:
        return self[0] # type: ignore
    @property
    def contents(self) -> Contents:
        return self[1] # type: ignore

class StyleSheet(MutableProduct):
    """A class of parse products that represent an entire parsed stylesheet, e.g. what is usually written end-to-end in a CSS file."""
    Location: TypeAlias = str
    location: Location | None = None # See http://drafts.csswg.org/cssom-1/#concept-css-style-sheet-location
    @property
    def contents(self) -> Contents:
        """The top-level elements of the style sheet.

        The elements may include qualified (aka "block") rules, at-rules (e.g. '@namespace...'), and tokens not grouped into compound products, e.g. whitespace.
        """
        return cast(Contents, self[0])
    @property
    def rules(self) -> Sequence[Rule]:
        """The rules (_only_ those directly nested, i.e. "top-level") of the style sheet."""
        return self.contents.rules
    def __init__(self, *args, location: Location | None = None, **kwargs):
        """Initialize the style sheet.

        The stylesheet starts empty, only storing specified location."""
        super().__init__(*args, **kwargs)
        self.location = location

class TokenStream:
    """A class of [token streams][spec], objects which a parser normally uses for token consumption.

    The stream, a so-called FIFO, is in turn fed by an iterator of tokens.

    See http://drafts.csswg.org/css-syntax/#css-token-stream.
    """
    _index: int = 0
    _tokens: list[Token | None]
    _marked_indexes: list[int]
    def __init__(self, source: Iterator[Token]):
        """Initialize the stream.

        :param source: An iterator that is used as the source of tokens this stream will vend.
        """
        self._source = source
        self._tokens = []
        self._marked_indexes = []
    def next_token(self) -> Token | None:
        """See http://drafts.csswg.org/css-syntax/#token-stream-next-token."""
        if self._index >= len(self._tokens):
            self._tokens.append(next(self._source, None))
        return self._tokens[self._index]
    def empty(self) -> bool:
        """See http://drafts.csswg.org/css-syntax/#token-stream-empty."""
        return not self.next_token()
    def consume_token(self) -> Token | None:
        """See http://drafts.csswg.org/css-syntax/#token-stream-consume-atoken."""
        token = self.next_token()
        self._index += 1
        return token
    def mark(self) -> None:
        """See http://drafts.csswg.org/css-syntax/#token-stream-mark."""
        self._marked_indexes.append(self._index)
    def restore_mark(self) -> None:
        """See http://drafts.csswg.org/css-syntax/#token-stream-restore-a-mark."""
        self._index = self._marked_indexes.pop()
    def discard_mark(self) -> None:
        """See http://drafts.csswg.org/css-syntax/#token-stream-discard-a-mark."""
        self._marked_indexes.pop()

@runtime_checkable
class Input(Protocol):
    """The input interface that the parser expects.

    Modeled after the `TokenStream` class which implements http://drafts.csswg.org/css-syntax/#css-token-stream.
    """
    def next_token(self) -> Token | None: raise NotImplementedError
    def empty(self) -> bool: raise NotImplementedError
    def consume_token(self) -> Token | None: raise NotImplementedError
    def mark(self) -> None: raise NotImplementedError
    def restore_mark(self) -> None: raise NotImplementedError
    def discard_mark(self) -> None: raise NotImplementedError

def is_valid(_: Rule | Declaration) -> bool:
    """Check if a rule/declaration is "valid".

    The specification does not define _what_ constitutes a "valid" rule or declaration, only that validation is done. In absense of a definition, we offer this for mere convenience. Currently every rule is considered to be "valid". It's entirely possible that the notion of "validity" is simply outside of the scope of the specification (which defines the _syntactical_ nature of CSS, after all).
    """
    return True

def consume_token(input: Input, *, to: Appender[Token]) -> Token:
    token = input.consume_token()
    assert token, 'End-of-stream condition' # This function _cannot_ be used to consume the [conceptual] end-of-stream "token" represented with `None`, i.e. it assumes e.g. `empty` has been used to handle such condition earlier; we do this to guarantee that a parse tree may ultimately consist of only _tokens_ (`Token` objects) -- never `None` or something that is not a `Token`
    to.append(token)
    return token

def discard_whitespace(input: Input, *, to: Appender[list[WhitespaceToken]]) -> list[WhitespaceToken]:
    """Discard consecutive white-space tokens ahead.

    This procedure follows http://drafts.csswg.org/css-syntax#token-stream-discard-whitespace except that because `CommentToken` is a subclass of `WhitespaceToken`, both kinds of tokens are considered white-space by this procedure. This relationship between the token classes is intentional as the specification doesn't outright deny it and because it instructs discarding of comments during tokenization, something that does not align well with our explicit design of preserving _all_ parsed text for "unparsing" (reproduction of original). To allow the latter comments have to be preserved and be visible to the parser, but not disturb the parsing in the sense that the latter must still be as faithful to the specification as possible. All these requirements have resulted in the decision to simply consider comments to be white-space in the context of parsing.
    """
    consumed: list[WhitespaceToken] = []
    while isinstance(input.next_token(), WhitespaceToken):
        consume_token(input, to=consumed) # type: ignore # Safe [to pass `consumed`] because the token to be consumed is a `WhitespaceToken`, as asserted by the loop condition
    to.append(consumed)
    return consumed

def consume_at_rule(input: Input, *, nested: bool = False, to: Appender[AtRule]) -> AtRule | None:
    """Implements http://drafts.csswg.org/css-syntax/#consume-at-rule."""
    rule = AtRule()
    assert isinstance(input.next_token(), AtKeywordToken)
    consume_token(input, to=rule)
    rule.append([]) # Initialize the rule's prelude
    while not input.empty():
        match input.next_token():
            case SemicolonToken():
                consume_token(input, to=rule)
                break
            case CloseBraceToken():
                if nested:
                    break
                consume_token(input, to=cast(Appender[ComponentValue], rule.prelude))
            case OpenBraceToken():
                consume_block(input, to=rule) # the child rules are elements of the rule that follow the `prelude`
                break
            case _:
                consume_component_value(input, to=cast(Appender[ComponentValue], rule.prelude))
    if is_valid(rule):
        to.append(rule)
        return rule
    else:
        return None

def consume_block(input: Input, *, to: Appender[Block]) -> Block:
    """Implements http://drafts.csswg.org/css-syntax/#consume-block."""
    block = Block()
    assert isinstance(input.next_token(), OpenBraceToken)
    consume_token(input, to=block)
    consume_block_contents(input, to=block)
    assert isinstance(input.next_token(), CloseBraceToken)
    consume_token(input, to=block)
    to.append(block)
    return block

def consume_block_contents(input: Input, *, to: Appender[Contents]) -> Contents:
    """Implements http://drafts.csswg.org/css-syntax/#consume-block-contents."""
    contents = Contents()
    while not input.empty():
        match input.next_token():
            case WhitespaceToken() | SemicolonToken():
                consume_token(input, to=contents)
            case CloseBraceToken():
                break
            case AtKeywordToken():
                consume_at_rule(input, nested=True, to=contents)
            case _:
                input.mark()
                if consume_declaration(input, nested=True, to=contents):
                    input.discard_mark()
                else:
                    input.restore_mark()
                    consume_qualified_rule(input, nested=True, stop_token=SemicolonToken, to=contents)
    to.append(contents)
    return contents

def consume_component_value(input: Input, *, to: Appender[ComponentValue]) -> ComponentValue:
    """Implements http://drafts.csswg.org/css-syntax/#consume-component-value."""
    match input.next_token():
        case OpenBraceToken() | OpenBracketToken() | OpenParenToken():
            return consume_simple_block(input, to=to)
        case FunctionToken():
            return consume_function(input, to=to)
        case _:
            return consume_token(input, to=to)

def consume_declaration(input: Input, *, nested: bool = False, to: Appender[Declaration]) -> Declaration | None:
    """Implements http://drafts.csswg.org/css-syntax/#consume-declaration."""
    decl = Declaration()
    if isinstance(input.next_token(), IdentToken):
        token = cast(IdentToken, consume_token(input, to=decl))
        assert decl.name == token.value
    else:
        consume_remnants_of_bad_declaration(input, nested=nested, to=decl)
        return None
    discard_whitespace(input, to=decl) # type: ignore # TODO: Prove that the type checker warnings _may_ be ignored here
    if isinstance(input.next_token(), ColonToken):
        consume_token(input, to=decl)
    else:
        consume_remnants_of_bad_declaration(input, nested=nested, to=decl)
        return None
    discard_whitespace(input, to=decl) # type: ignore # TODO: Prove that the type checker warnings _may_ be ignored here
    consume_list_of_component_values(input, nested=nested, stop_token=SemicolonToken, to=decl) # type: ignore
    match tuple(((index, item) for index, item in enumerate(decl.value) if not isinstance(item, WhitespaceToken)))[-2:]: # Match the last two non-whitespace tokens in the declaration's value... (§ 5.5.6 step 6)
        case ((i, DelimToken(value='!')), (j, IdentToken() as second)) if second.value.lower() == 'important': # ...Are the tokens a "!" followed by "important"?
            pass # The indices of the [matched] tokens (`i` and `j`) are set
        case _:
            i = len(decl.value)
            j = i - 1
    decl.append(cast(MutableProduct, pre_match_ws := list(reversed(match_whitespace(reversed(decl.value[:i])))))) # Find longest sequence of white-space preceding the match and append it to the list; if there was no match, white-space at the end of the list is found instead
    decl.append(cast(MutableProduct, decl.value[i:j+1])) # Append the match to the declation
    decl.append(cast(MutableProduct, post_match_ws := match_whitespace(decl.value[j+1:]))) # Find longest sequence of white-space succeeding the match and append it to the declaration; if there was no match this should only append an empty list (because all white-space would have been added as the list preceding the list that signifies the empty match)
    assert isinstance(decl.value, MutableSequence)
    del decl.value[i-len(pre_match_ws):j+1+len(post_match_ws)] # Remove the portion of the value that corresponds to the consecutive white-space preceding possibly matched "!" followed by "important" (or empty list if no match), the match itself (including any white-space between "!" and "important") and consecutive white-space following the match.
    if is_custom_property_name_string(decl.name):
        decl.original_text = source(decl.value)
    elif (block := builtins.next((item for item in decl.value if (isinstance(item, SimpleBlock) and isinstance(item.token, OpenBraceToken))), None)) and any((item for item in decl.value if (item is not block and not isinstance(item, WhitespaceToken)))):
            return None
    elif decl.name.lower() == 'unicode-range':
        decl.value[:] = consume_value_of_unicode_range_descriptor(source(decl.value))
    else:
        pass # Probably a "do nothing" but the spec. doesn't outright say so; see http://github.com/w3c/csswg-drafts/issues/10353
    if is_valid(decl):
        to.append(decl)
        return decl
    else:
        return None

def consume_function(input: Input, *, to: Appender[Function]) -> Function:
    """Implements http://drafts.csswg.org/css-syntax/#consume-function."""
    assert isinstance(input.next_token(), FunctionToken)
    function = Function()
    consume_token(input, to=function)
    function.append([])
    while not input.empty():
        match input.next_token():
            case CloseParenToken():
                consume_token(input, to=function)
                break
            case _:
                consume_component_value(input, to=cast(Appender[ComponentValue], function.value))
    to.append(function)
    return function

def consume_list_of_component_values(input: Input, *, stop_token: type[Token | None] = type(None), nested: bool = False, to: Appender[list[ComponentValue]]) -> list[ComponentValue]:
    """Implements http://drafts.csswg.org/css-syntax/#consume-list-of-components."""
    values: list[ComponentValue] = []
    while not input.empty():
        match input.next_token():
            case stop_token(): # type: ignore # Rationale: `stop_token` is guaranteed to be a class -- either `Token` or one of its sub-classes, or the `NoneType` class; the case statement, as written, will thus work correctly (http://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-class_pattern); it's immediately unclear to me _why_, given the rationale, would MyPy flag this as an error
                break
            case CloseBraceToken():
                if nested:
                    break
                parser_error()
                consume_token(input, to=values)
            case _:
                consume_component_value(input, to=values)
    to.append(values)
    return values

def consume_simple_block(input: Input, *, to: Appender[SimpleBlock]) -> SimpleBlock:
    """Implements http://drafts.csswg.org/css-syntax/#consume-simple-block."""
    assert isinstance(token := input.next_token(), (OpenBraceToken, OpenBracketToken, OpenParenToken))
    ending_token = token.mirror_type
    block = SimpleBlock()
    consume_token(input, to=block)
    block.append([])
    while not input.empty():
        match input.next_token():
            case ending_token(): # type: ignore # `ending_token` is a `type` so per [Python class pattern matching] the `case` statement does not constitute a real typing issue
                consume_token(input, to=block)
                break
            case _:
                consume_component_value(input, to=cast(Appender[ComponentValue], block.value))
    to.append(block)
    return block

def consume_qualified_rule(input: Input, *, stop_token: type[Token | None] = type(None), nested: bool = False, to: Appender[QualifiedRule]) -> QualifiedRule | InvalidRuleError | None:
    """Implements http://drafts.csswg.org/css-syntax/#consume-qualified-rule."""
    rule = QualifiedRule()
    rule.append([]) # Initialize the rule's prelude
    while not input.empty():
        match input.next_token():
            case stop_token(): # type: ignore # `stop_token` is either a `type` (of `Token` or a subclass of the latter) or `NoneType`, all of which are allowed with [Python class pattern matching]
                break
            case CloseBraceToken():
                parser_error()
                if nested:
                   return None
                consume_token(input, to=cast(Appender[ComponentValue], rule.prelude))
            case OpenBraceToken():
                match tuple((item for item in rule.prelude if not isinstance(item, WhitespaceToken)))[:2]:
                    case (IdentToken() as first, ColonToken()) if first.value.startswith('--'):
                        if nested:
                            consume_remnants_of_bad_declaration(input, nested=True, to=rule)
                        else:
                            consume_block(input, to=rule)
                        return None
                    case _:
                        consume_block(input, to=rule)
                        # DEBUG: How does the following comment affect the parser? Add a "why" to the comment
                        # We do not implement any special handling of consumed block's contents as specifed by 5.5.3 with "...Otherwise, consume a block..."
                if is_valid(rule):
                    to.append(rule)
                    return rule
                else:
                    return InvalidRuleError()
            case _:
                consume_component_value(input, to=cast(Appender[ComponentValue], rule.prelude))
    parser_error()
    return None

def consume_remnants_of_bad_declaration(input: Input, *, nested: bool, to: Appender[Token | ComponentValue]) -> None:
    """Implements http://drafts.csswg.org/css-syntax/#consume-the-remnants-of-a-bad-declaration."""
    while not input.empty():
        match input.next_token():
            case SemicolonToken():
                consume_token(input, to=to)
                break
            case CloseBraceToken():
                if nested:
                    return
                consume_token(input, to=to)
            case _:
                consume_component_value(input, to=to)

def consume_stylesheet_contents(input: Input, *, to: Appender[Contents]) -> Contents:
    """Implements http://drafts.csswg.org/css-syntax/#consume-stylesheet-contents."""
    contents = Contents()
    while not input.empty():
        match input.next_token():
            case WhitespaceToken():
                consume_token(input, to=contents)
            case CDCToken() | CDOToken():
                consume_token(input, to=contents)
            case AtKeywordToken():
                consume_at_rule(input, to=contents)
            case _:
                consume_qualified_rule(input, to=contents)
    to.append(contents)
    return contents

def consume_value_of_unicode_range_descriptor(string: str) -> Sequence[ComponentValue]:
    """Implements http://drafts.csswg.org/css-syntax/#consume-unicode-range-value."""
    return consume_list_of_component_values(TokenStream(tokenize(tokenizing.normalize_input(string), unicode_ranges_allowed=True)), to=[])

def normalize_input(input: Input | Iterable[Token] | Iterable[str]) -> Input:
    """Wrap input (if needed) into a "canonical" kind of parser input object which the latter can use directly.

    Implements http://drafts.csswg.org/css-syntax/#normalize-into-a-token-stream, with some following caveats.

    Both "list" and "string" in the specification are intepreted to effectively permit `Iterable` (as opposed to using `list` and `str`, respectively) when no random value access is actually required for the object. This allows us to permit more kinds of useful objects for input, e.g. files.

    :param input: An object to use as backing the canonical form of input suitable to pass to the parser (e.g. `parse_stylesheet`); if already suitable, normalization is a "no-op"; otherwise if the object is iterable, normalization depends on whether the object is determined to vend tokens or code point sequences; if the former, a token stream is constructed with the token vending iterable as source, otherwise the iterable is assumed to vend code point sequences and a token stream is constructed through filtering the iterable as per the specification, then tokenizing the result and using said result as stream source; although not type-hinted, this procedure _does_ de-facto support every type of `input` passable to `tokenizing.normalize_input`, including a callable (see `tokenizing.normalize_input` for details)
    """
    if isinstance(input, Input):
        return input
    try:
        input = iter(cast(Iterable, input))
    except TypeError:
        assert not isinstance(input, Iterable) # Communicate to the type checker that `input` is not iterable
    else:
        try:
            item = next(input) # Determine the type of item(s) `input` vends (we assume it vends items of the same type -- if `item` is of type A then all subsequently vended items will also be of type A; an assumption to the contrary would make normalization of such `input` infeasible)
        except StopIteration:
            pass # No items (of any kind) were vended, so we can safely assume "empty" input
        else:
            input = chain((item,), input) # "Restore" the otherwise forward-only iterator by creating an equivalent with the recently vended item at the front
            if isinstance(item, Token):
                return TokenStream(input)
    return TokenStream(tokenize(tokenizing.normalize_input(input)))

def parse_stylesheet(input, *, location: StyleSheet.Location | None = None, **kwargs) -> StyleSheet:
    """Parse a stylesheet.

    Attempts to implement http://drafts.csswg.org/css-syntax/#parse-stylesheet with the following exception(s):

    :param input: see `normalize_input`
    :param location: see `StyleSheet.location`
    :returns: the concrete syntax parse tree (CST) of `input` parsed as a stylesheet
    """
    input = normalize_input(input)
    stylesheet = StyleSheet(location=location)
    consume_stylesheet_contents(input, to=stylesheet)
    return stylesheet

def source(element: Product | Token) -> str:
    """Return the source text of a parser product.

    Allows verbatim reproduction of originally parsed text.

    E.g. if "body { }" was parsed into `[ IdentToken(...), OpenBraceToken(...), ... ]`, then calling `source` on the latter list will return the former string.
    """
    return element.source if hasattr(element, 'source') else ''.join(source(item) for item in element)

def tokens(product: Iterable | Token) -> Iterator[Token]:
    """
    Yield every token contained in a parser production.

    This includes tokens contained transitively, i.e. as part of productions contained in the production, and so on.
    """
    match product:
        case Iterable():
            for item in product:
                yield from tokens(item)
        case Token():
            yield product
        case _:
            raise TypeError

def match_whitespace(tokens: Iterable[ComponentValue]) -> Sequence[WhitespaceToken]:
    """Find consecutive white-space in a stream.

    :param tokens: The stream of component values (which may include tokens, by nature of the `ComponentValue` definition) to look for a sequence of white-space tokens
    :returns: The [longest] sequence of consecutive white-space tokens at the head of the stream
    """
    matched: list[WhitespaceToken] = []
    for token in tokens:
        if not isinstance(token, WhitespaceToken):
            break
        matched.append(token)
    return matched
