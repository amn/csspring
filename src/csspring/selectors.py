"""Parsing of CSS selectors per the [CSS Selectors Level 4](http://drafts.csswg.org/selectors-4/) specification (within the scope of this module simply called "the specification" or "Selectors").

The parsing method chosen with this module is a so-called recursive descent parser (RDP). An RDP allows a straightforward implementation when the grammar is defined with a BNF-like syntax, which happens to be the case with the specification (see http://drafts.csswg.org/selectors-4/#grammar). Being an RDP, the parser is composed of callables (procedures or [other] callable objects), each such callable designed to consume and parse (or reject) input in the stream, typically according to a grammar production rule.

In context of parsing implemented with this module, "accepting" input means to consume a number of tokens (including zero, depending on the parser) from the stream [of tokens destined for the parser], and return a corresponding _parse product_ containing these arranged in meaningful fashion. "Parsing" input refers to much the same thing although it more stresses the actual procedure leading to "acceptance" of input and returning of the parse product. Conversely, a parser "rejecting" input means that the parser will return nothing signifying e.g. a syntax error in the input (mismatch between what was expected in the input vs. what was encountered in its place).

NOTE: Although it's being referred to as parsing [of] a "selector" (singular), the specification itself refers to the latter with "selector list", a [comma-separated] _list_ of selectors. For simplicity, however, _and_ because a lot of the rest of CSS lingo out there seems to have resigned to calling the rule's [parsed] "prelude" (the syntactical term for it) a "selector" (singular), we opted to call the export `parse_selector` and not e.g. `parse_selector_list`.
"""

from .syntax.parsing import Input as TokenStream, Product, tokens # Conveniently reusing some of the helpful constructs offered by the [CSS] syntax-level parsing module
from .syntax.tokenizing import Token, BadStringToken, BadURLToken, CloseBraceToken, CloseBracketToken, CloseParenToken, ColonToken, DelimToken, FunctionToken, HashToken, IdentToken, OpenBraceToken, OpenBracketToken, OpenParenToken, StringToken

from .syntax.grammar import any_value
from .values import Production, AlternativesProduction, CommaSeparatedRepetitionProduction, ConcatenationProduction, NonEmptyProduction, OptionalProduction, ReferenceProduction, RepetitionProduction, TokenProduction

from functools import singledispatch
from typing import cast

@singledispatch
def parse(production: Production, input: TokenStream) -> Product | Token | None:
    """The generic parse procedure written to parse input in accordance with the Selectors grammar.

    Per the applied `singledispatch` decorator, this procedure is only called for productions for which no more applicable overload variant of `parse` is defined (those are annotated with `parse.register` decorator).

    :param production: A grammar element (see the `Grammar` class for an example of the set of elements constituting the Selectors grammar)
    :param input: Input for parsing according to `production`
    :returns: The result of parsing a portion of input consumed per `production`
    """
    match production:
        case _ if production == any_value: return parse_any_value(input)
        case _: raise ValueError(f"No suitable `parse` method for {production}")

@parse.register
def _(production: AlternativesProduction, input: TokenStream) -> Product | Token | None:
    """Variant of `parse` for productions of the `|` combinator variety (see https://drafts.csswg.org/css-values-4/#component-combinators)."""
    input.mark()
    for element in production.elements:
        result = parse(element, input)
        if result is not None:
            input.discard_mark()
            return result
    input.restore_mark()
    return None

def parse_any_value(input: TokenStream) -> Product | None:
    """Variant of `parse` for specifically the `any_value` production.

    `any_value` is a "non-compound" production of an opaque type defined by the Values and Units specification in terms which do not easily allow for implementing the production through one of the combinator or multiplier production types. For this reason, the singular value cannot be handled by `singledispatch` decorator (which only works for _types_) unless we make `any_value` to be of some distinct sub-class of `Production`, something we chose against since this concerns one singular _unique_ production. Because the production isn't distinguished by _type_ but by value, dispatching of this procedure is done in the fall-back `parse` procedure variant.
    """
    result: list[Token] = []
    count = { type: 0 for type in { OpenBraceToken, OpenBracketToken, OpenParenToken } }
    while True:
        match token := input.consume_token():
            case BadStringToken() | BadURLToken():
                break
            case OpenParenToken() | OpenBracketToken() | OpenBraceToken():
                count[type(token)] += 1
            case CloseParenToken() | CloseBracketToken() | CloseBraceToken():
                if count[token.mirror_type] <= 0:
                    break
                count[token.mirror_type] -= 1
            case None:
                break
        result.append(token)
    if result:
        return result
    else:
        return None

@parse.register
def _(production: ConcatenationProduction, input: TokenStream) -> Product | None:
    """Variant of `parse` for productions of the ` ` combinator variety (see "juxtaposing components" at https://drafts.csswg.org/css-values-4/#component-combinators)."""
    result: list[Product | Token] = []
    input.mark()
    for element in production.elements:
        if (value := parse(element, input)) is None:
            input.restore_mark()
            return None
        result.append(value)
    input.discard_mark()
    return result

@parse.register
def _(production: NonEmptyProduction, input: TokenStream) -> Product | None:
    """Variant of `parse` for productions of the `!` multiplier variety (see https://drafts.csswg.org/css-values-4/#mult-req)."""
    result = cast(Product, parse(production.element, input)) # The element of a non-empty production is concatenation, and the `parse` overload for `ConcatenationProduction` never returns a `Token`, only `Product | None`
    if result and any(tokens(result)):
        return result
    else:
        return None

@parse.register
def _(production: ReferenceProduction, input: TokenStream) -> Product | Token | None:
    """Variant of `parse` for production _references_.

    A production reference is featured in grammar rules.
    """
    return parse(production.element, input) # Parsing a production reference naturally implies parsing of the referenced production

@parse.register
def _(production: RepetitionProduction, input: TokenStream) -> Product | None:
    """Variant of `parse` for productions of the `*` multiplier variety and its related sub-types (see http://drafts.csswg.org/css-values-4/#mult-zero-plus)."""
    result: list[Product | Token] = []
    input.mark()
    while True:
        if result and production.separator:
            input.mark()
            separator = parse(production.separator, input)
            if separator is None:
                input.restore_mark()
                break
        value = parse(production.element, input)
        if value is None:
            if result and production.separator:
                input.restore_mark()
            break
        if result and production.separator:
            assert separator is not None
            result.append(separator)
            input.discard_mark()
        result.append(value)
        if len(result) == production.max:
            break
    if len(result) >= production.min:
        input.discard_mark()
        return result
    else:
        input.restore_mark()
        return None

@parse.register
def _(production: TokenProduction, input: TokenStream) -> Token | None:
    """Variant of `parse` for token productions.

    A token production can be identified in the grammar at https://drafts.csswg.org/selectors-4/#grammar with the `<...-token>` text.
    """
    input.mark()
    if isinstance(token := input.consume_token(), production.type) and all((getattr(token, name) == value) for name, value in production.attributes.items()):
        input.discard_mark()
        return token
    input.restore_mark()
    return None

def parse_selector_list(input: TokenStream) -> Product | None:
    """Parse input according to the `selector_list` production in the Selectors grammar (see `grammar` below).

    Parsing of selector lists is the _reason d'etre_ for this module and this is the [convenience] procedure that exposes the feature.
    """
    return cast(Product | None, parse(grammar.selector_list, input))

class Grammar:
    """The grammar defining the language of selector list expressions.

    Normally a grammar would be defined as a set of rules (for deriving productions), where each rule would feature a component to the left side of the `->` operator (the "rewriting" operator) and a component to the right side of the operator. Owing to relative simplicity of the Selectors grammar -- where the left-hand side component is always a production name _reference_ (an identifying factor of context free grammars), we leverage Python's meta-programming facilities and use class attribute assignment statements to define the rules instead, where the assigned value is the right side of the rule, an arbitrary production (which may be an opaque value). Each attribute of the grammar is assigned the corresponding name automatically, owing to the `__set_name__` dunder method of the common production (super)class (where appropriate).

    Implements http://drafts.csswg.org/selectors-4/#grammar.
    """
    ns_prefix = ConcatenationProduction(OptionalProduction(AlternativesProduction(TokenProduction(IdentToken), TokenProduction(DelimToken, value='*'))), TokenProduction(DelimToken, value='|'))
    wq_name = ConcatenationProduction(OptionalProduction(ReferenceProduction(ns_prefix)), TokenProduction(IdentToken))
    type_selector = AlternativesProduction(ReferenceProduction(wq_name), ConcatenationProduction(OptionalProduction(ReferenceProduction(ns_prefix)), TokenProduction(DelimToken, value='*')))
    id_selector = TokenProduction(HashToken)
    class_selector = ConcatenationProduction(TokenProduction(DelimToken, value='.'), TokenProduction(IdentToken))
    attr_matcher = ConcatenationProduction(OptionalProduction(AlternativesProduction(*(TokenProduction(DelimToken, value=value) for value in ('~', '|', '^', '$', '*')))), TokenProduction(DelimToken, value='='))
    attr_modifier = AlternativesProduction(*(TokenProduction(DelimToken, value=value) for value in ('i', 's')))
    attribute_selector = AlternativesProduction(ConcatenationProduction(TokenProduction(OpenBracketToken), ReferenceProduction(wq_name), TokenProduction(CloseBracketToken)), ConcatenationProduction(TokenProduction(OpenBracketToken), ReferenceProduction(wq_name), ReferenceProduction(attr_matcher), AlternativesProduction(TokenProduction(StringToken), TokenProduction(IdentToken)), OptionalProduction(ReferenceProduction(attr_modifier)), TokenProduction(CloseBracketToken)))
    legacy_pseudo_element_selector = ConcatenationProduction(TokenProduction(ColonToken), AlternativesProduction(*(TokenProduction(IdentToken, value=value) for value in ('before', 'after', 'first-line', 'first-letter'))))
    pseudo_class_selector = AlternativesProduction(ConcatenationProduction(TokenProduction(ColonToken), TokenProduction(IdentToken)), ConcatenationProduction(TokenProduction(ColonToken), TokenProduction(FunctionToken), ReferenceProduction(any_value), TokenProduction(CloseParenToken)))
    pseudo_element_selector = AlternativesProduction(ConcatenationProduction(TokenProduction(ColonToken), ReferenceProduction(pseudo_class_selector)), ReferenceProduction(legacy_pseudo_element_selector))
    pseudo_compound_selector = ConcatenationProduction(ReferenceProduction(pseudo_element_selector), RepetitionProduction(ReferenceProduction(pseudo_class_selector)))
    subclass_selector = AlternativesProduction(ReferenceProduction(id_selector), ReferenceProduction(class_selector), ReferenceProduction(attribute_selector), ReferenceProduction(pseudo_class_selector))
    compound_selector = NonEmptyProduction(ConcatenationProduction(OptionalProduction(ReferenceProduction(type_selector)), RepetitionProduction(ReferenceProduction(subclass_selector))))
    complex_selector_unit = NonEmptyProduction(ConcatenationProduction(OptionalProduction(ReferenceProduction(compound_selector)), RepetitionProduction(ReferenceProduction(pseudo_compound_selector))))
    combinator = AlternativesProduction(*(TokenProduction(DelimToken, value=value) for value in ('>', '+', '~')), ConcatenationProduction(*(TokenProduction(DelimToken, value=value) for value in ('|', '|'))))
    complex_selector = ConcatenationProduction(ReferenceProduction(complex_selector_unit), RepetitionProduction(ConcatenationProduction(OptionalProduction(ReferenceProduction(combinator)), ReferenceProduction(complex_selector_unit))))
    complex_selector_list = CommaSeparatedRepetitionProduction(ReferenceProduction(complex_selector))
    selector_list = ReferenceProduction(complex_selector_list)
    complex_real_selector = ConcatenationProduction(ReferenceProduction(compound_selector), RepetitionProduction(ConcatenationProduction(OptionalProduction(ReferenceProduction(combinator)), ReferenceProduction(compound_selector))))
    complex_real_selector_list = CommaSeparatedRepetitionProduction(ReferenceProduction(complex_real_selector))
    compound_selector_list = CommaSeparatedRepetitionProduction(ReferenceProduction(compound_selector))
    simple_selector = AlternativesProduction(ReferenceProduction(type_selector), ReferenceProduction(subclass_selector))
    simple_selector_list = CommaSeparatedRepetitionProduction(ReferenceProduction(simple_selector))
    relative_selector = ConcatenationProduction(OptionalProduction(ReferenceProduction(combinator)), ReferenceProduction(complex_selector))
    relative_selector_list = CommaSeparatedRepetitionProduction(ReferenceProduction(relative_selector))
    relative_real_selector = ConcatenationProduction(OptionalProduction(ReferenceProduction(combinator)), ReferenceProduction(complex_real_selector))
    relative_real_selector_list = CommaSeparatedRepetitionProduction(ReferenceProduction(relative_real_selector))

grammar = Grammar()
