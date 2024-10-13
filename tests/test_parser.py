"""Testing of the `syntax.parsing` module.

Some of the input text used for testing was copied from [the `tests.js` script of Tab Atkins' `parse-css` project](http://github.com/tabatkins/parse-css/blob/c7859c41887409c533beed36956b7976ea0144b4/tests.js), since the project is ostensibly for purposes of testing the "CSS Syntax" specification (of which Atkins is a co-author).
"""

from pytest import mark

# NOTE: This module uses `eval` to e.g. load reference parse products from [Python] files, which may reference names in the global scope that are otherwise not referenced in the module explicitly; for this reason, do not assume objects imported below are "unused" for not being explicitly referenced in the module's [Python] cod, and ensure that whatever referenced by source code passed to `eval` in this module, _is_ available by name in the global scope of this module

from csspring.syntax.tokenizing import HashTokenType, NumberTokenSign, NumberTokenType
from csspring.syntax.tokenizing import AtKeywordToken, BadStringToken, BadURLToken, CDOToken, CDCToken, ColonToken, CommaToken, CommentToken, DelimToken, DimensionToken, FunctionToken, HashToken, IdentToken, OpenBraceToken, OpenBracketToken, OpenParenToken, NumberToken, CloseBraceToken, CloseBracketToken, CloseParenToken, PercentageToken, SemicolonToken, StringToken, UnicodeRangeToken, URLToken, WhitespaceToken
from csspring.syntax.parsing import normalize_input, parse_stylesheet, Product, source, AtRule, Block, Contents, Declaration, QualifiedRule, StyleSheet

import os

def reference_product_file_path(path: str) -> str:
    """Get the path of a file storing the reference parse product corresponding to given [CSS] file path."""
    return os.path.splitext(path)[0] + '-product.py'

def parse_file_at(path: str) -> StyleSheet:
    """Parse the file at the specified path, as a stylesheet."""
    return parse_stylesheet(normalize_input(open(path, newline='')))

css_file_name_stems = ( # Name stems of CSS files that are subject to parsing tests, including files which may not be "valid" CSS [stylesheets]
    '1c8a182edf42bae99ae3bfb5303eb27f',
    '3afb9fd7cfc7976a0b527b6bf9176616',
    '59732d15cadc11bc6c6ef8c90d6e6158',
    '387011ca369f92f1feb8b82b6ec3dcc5',
    '3fd474a35e676b55f574c5d53cecca63',
    '79e63aeac5fa573a619818b54dfc88e9',
    'b577a36c565e38e4736cb970d79a6fa8',
    'fd97336ed4b81a5ec56417dee0c3ce3f',
    '34cb4a2de40eaef671522833c0cecd76',
    '3a37076ff4a6c03109bf9217e674be5d',
    '497125b3346b5d2639e9bb65099fb3a4',
    '8d8f0e264a5aa70c88b4cc7fadc4425a',
    'df3f93751ffc2b35dfeff0b1e550d593',
    '9934acf97e88e8ca7e89fc73e5faffcd',
    )

erroneous_css_file_name_stems = ( # Name stems of CSS files for which parsing is expected to produce error(s)
    '9934acf97e88e8ca7e89fc73e5faffcd',
    )

valid_css_file_name_stems = tuple(set(css_file_name_stems) - set(erroneous_css_file_name_stems)) # Name stems of CSS files for which parsing is expected to _not_ produce error(s) (and by implication consume input in its entirely, i.e. "full match")

def css_file_path(stem: str) -> str:
    """Get the path of a test subject [CSS] file by given file name stem.

    :param stem: the "stem" of a [CSS] file's name -- the part of the name sans the ending part that starts with the right-most `.`
    """
    return os.path.join(os.path.split(__file__)[0], stem + '.css')

# As per `pytest` convention, procedures with names that start with `test_` are test elements (or their templates, see `mark.parametrize`) collected automatically by `pytest`, use `assert` to represent their respective test, and return nothing

@mark.parametrize('path', tuple(css_file_path(stem) for stem in valid_css_file_name_stems)) # We currently do not test recovery of original text for parsing of _broken_ CSS files; their parse product certainly may not be reversed to the contents of the parsed file; tests of such broken files may certainly be implemented, however, they need only define corresponding reference products
def test_file_parsing_product_source_equals_file_contents(path: str) -> None:
    """Test whether the text recovered from the product of parsing the file at given path, is identical to the contents of the file.

    Recovery of text refers to an "unparsing" procedure that is expected to return text corresponding to a parse product, an [_inverse_](http://en.wikipedia.org/wiki/Inverse_function) of some parse function.

    :param path: Path of the file to parse [as a [CSS] stylesheet]
    """
    assert source(parse_file_at(path)) == open(path, newline='').read()

@mark.parametrize('path', tuple(css_file_path(stem) for stem in css_file_name_stems))
def test_file_parsing_product_equals_reference_product(path: str) -> None:
    """Test whether the product of parsing the file at given path equals the corresponding reference parse product.

    :param path: See `test_file_parsing_product_source_equals_file_contents`
    """
    assert parse_file_at(path) == eval(open(reference_product_file_path(path)).read())

text_reference_product_parametrized = mark.parametrize(('text', 'reference_product'), (
    ('foo {\n\tbar: baz;\n}', [ [ [ [ IdentToken(value='foo', source='foo'), WhitespaceToken(value=' ', source=' ') ], [ OpenBraceToken(source='{'), [ WhitespaceToken(value='\n\t', source='\n\t'), [ IdentToken(value='bar', source='bar'), [], ColonToken(source=':'), [ WhitespaceToken(value=' ', source=' ') ], [ IdentToken(value='baz', source='baz') ], [], [], [] ], SemicolonToken(source=';'), WhitespaceToken(value='\n', source='\n') ], CloseBraceToken(source='}') ] ] ] ]),
    ('foo { bar: rgb(255, 0, 127); }', [ [ [ [ IdentToken(value='foo', source='foo'), WhitespaceToken(value=' ', source=' ') ], [ OpenBraceToken(source='{'), [ WhitespaceToken(value=' ', source=' '), [ IdentToken(value='bar', source='bar'), [], ColonToken(source=':'), [ WhitespaceToken(value=' ', source=' ') ], [ [ FunctionToken(value='rgb', source='rgb('), [ NumberToken(sign=None, type=NumberTokenType.integer, value=255, source='255'), CommaToken(source=','), WhitespaceToken(value=' ', source=' '), NumberToken(sign=None, type=NumberTokenType.integer, value=0, source='0'), CommaToken(source=','), WhitespaceToken(value=' ', source=' '), NumberToken(sign=None, type=NumberTokenType.integer, value=127, source='127') ], CloseParenToken(source=')') ] ], [], [], [] ], SemicolonToken(source=';'), WhitespaceToken(value=' ', source=' ') ], CloseBraceToken(source='}') ] ] ] ]),
    ('#foo {}', [ [ [ [ HashToken(type=HashTokenType.id, value='foo', source='#foo'), WhitespaceToken(value=' ', source=' ') ], [ OpenBraceToken(source='{'), [], CloseBraceToken(source='}') ] ] ] ]),
    ('@media{ }', [ [ [ AtKeywordToken(value='media', source='@media'), [], [ OpenBraceToken(source='{'), [ WhitespaceToken(value=' ', source=' ') ], CloseBraceToken(source='}') ] ] ] ]),
    ('.foo {color: red; @media { foo: bar } color: green }', [ [ [ [ DelimToken(value='.', source='.'), IdentToken(value='foo', source='foo'), WhitespaceToken(value=' ', source=' ') ], [ OpenBraceToken(source='{'), [ [ IdentToken(value='color', source='color'), [], ColonToken(source=':'), [ WhitespaceToken(value=' ', source=' ') ], [ IdentToken(value='red', source='red') ], [], [], [] ], SemicolonToken(source=';'), WhitespaceToken(value=' ', source=' '), [ AtKeywordToken(value='media', source='@media'), [ WhitespaceToken(value=' ', source=' ') ], [ OpenBraceToken(source='{'), [ WhitespaceToken(value=' ', source=' '), [ IdentToken(value='foo', source='foo'), [], ColonToken(source=':'), [ WhitespaceToken(value=' ', source=' ') ], [ IdentToken(value='bar', source='bar') ], [ WhitespaceToken(value=' ', source=' ') ], [], [] ] ], CloseBraceToken(source='}') ] ], WhitespaceToken(value=' ', source=' '), [ IdentToken(value='color', source='color'), [], ColonToken(source=':'), [ WhitespaceToken(value=' ', source=' ') ], [ IdentToken(value='green', source='green') ], [ WhitespaceToken(value=' ', source=' ') ], [], [] ] ], CloseBraceToken(source='}') ] ] ] ]),
    ('foo{div:hover; color:red{};}', [ [ [ [ IdentToken(value='foo', source='foo') ], [ OpenBraceToken(source='{'), [ [ IdentToken(value='div', source='div'), [], ColonToken(source=':'), [], [ IdentToken(value='hover', source='hover') ], [], [], [] ], SemicolonToken(source=';'), WhitespaceToken(value=' ', source=' '), [ [ IdentToken(value='color', source='color'), ColonToken(source=':'), IdentToken(value='red', source='red') ], [ OpenBraceToken(source='{'), [], CloseBraceToken(source='}') ] ], SemicolonToken(source=';') ], CloseBraceToken(source='}') ] ] ] ]),
    ('@foo;;foo {}', [ [ [ AtKeywordToken(source='@foo', value='foo'), [], SemicolonToken(source=';') ], [ [ SemicolonToken(source=';'), IdentToken(source='foo', value='foo'), WhitespaceToken(source=' ', value=' ') ], [ OpenBraceToken(source='{'), [], CloseBraceToken(source='}') ] ] ] ]),
    ('foo{@foo;;foo {}}', [ [ [ [ IdentToken(source='foo', value='foo') ], [ OpenBraceToken(source='{'), [ [ AtKeywordToken(source='@foo', value='foo'), [], SemicolonToken(source=';') ], SemicolonToken(source=';'), [ [ IdentToken(source='foo', value='foo'), WhitespaceToken(source=' ', value=' ') ], [ OpenBraceToken(source='{'), [], CloseBraceToken(source='}') ] ] ], CloseBraceToken(source='}') ] ] ] ]),
    ('foo { --div:hover{}}', [ [ [ [ IdentToken(source='foo', value='foo'), WhitespaceToken(source=' ', value=' ') ], [ OpenBraceToken(source='{'), [ WhitespaceToken(source=' ', value=' '), [ IdentToken(source='--div', value='--div'), [], ColonToken(source=':'), [], [ IdentToken(source='hover', value='hover'), [ OpenBraceToken(source='{'), [], CloseBraceToken(source='}') ] ], [], [], [] ] ], CloseBraceToken(source='}') ] ] ] ]),
    )) # This test function parametrization mark defines a set of pairs where the first element of the pair is some [CSS] stylesheet source code (text) and the second element is the corresponding product that would be obtained by parsing the former

@mark.parametrize('text', tuple(text for text, _ in text_reference_product_parametrized.mark.args[1]))
def test_parsing_product_source_equals_parsed_text(text: str) -> None:
    """Test whether the text recovered from the product of parsing given text, equals the parsed text.

    For definition of "text recovery" see `test_file_parsing_product_source_equals_file_contents`.

    :param text: The text to parse [as a [CSS] stylesheet]
    """
    assert source(parse_stylesheet(normalize_input(text))) == text

@text_reference_product_parametrized
def test_parsing_product_equals_reference_product(text: str, reference_product: Product) -> None:
    """Test whether the product of parsing of given text equals given reference product.

    With the equality operator the test effectively compares two [concrete syntax trees](http://en.wikipedia.org/wiki/Parse_tree) _exactly_ -- _all_ of the attributes contained in the parse product are used in the comparison. Thus, changes in the general structure of parse products will potentially invalidate some, possibly all of the reference products, requiring their re-provision.

    :param text: See `test_parsing_product_source_equals_parsed_text`
    :param reference_product: A parse product to use as reference (i.e. the "right side") in the comparison
    """
    assert parse_stylesheet(normalize_input(text)) == reference_product

@text_reference_product_parametrized
def test_parsing_product_source_equals_reference_product_source(text: str, reference_product: Product) -> None:
    """Test whether text recovered from the product of parsing given text, equals the text recovered from given "reference" product.

    This is a variant of the `test_parsing_product_source_equals_parsed_text` procedure that compares against text recovered from an _explicitly specified_ product as reference.

    :param text: See `test_parsing_product_source_equals_parsed_text`
    :param reference_product: See `test_parsing_product_equals_reference_product`
    """
    assert source(parse_stylesheet(normalize_input(text))) == source(reference_product)

@mark.parametrize('text', (
    'foo {\n\tbar: baz;\n}',
    'foo { bar: rgb(255, 0, 127); }',
    '#foo {}',
    '@media{ }',
    ))
def test_parsing_product_matches_reference_product(text: str) -> None:
    """Test whether a product of parsing given text matches corresponding reference pattern.

    The reference pattern is provided in the body of the procedure, since the `match` statement used for comparison requires the pattern in [Python] source code.

    :param text: See `test_parsing_product_source_equals_parsed_text`
    """
    match parse_stylesheet(normalize_input(text)):
        case StyleSheet(contents=Contents([ QualifiedRule(prelude=[ IdentToken(value='foo'), WhitespaceToken(value=' ') ], block=Block(token=OpenBraceToken(), contents=Contents([ WhitespaceToken(value='\n\t'), Declaration(name='bar', value=[ IdentToken(value='baz') ]), SemicolonToken(), WhitespaceToken() ]))) ])) if text == 'foo {\n\tbar: baz;\n}':
            pass
        case StyleSheet(contents=Contents([ QualifiedRule(prelude=[ IdentToken(value='foo'), WhitespaceToken(value=' ') ], block=Block(token=OpenBraceToken(), contents=Contents([ WhitespaceToken(value=' '), Declaration(name='bar', value=[ [ FunctionToken(value='rgb'), [ NumberToken(value=255), CommaToken(), WhitespaceToken(), NumberToken(value=0), CommaToken(), WhitespaceToken(), NumberToken(value=127) ], CloseParenToken() ] ]), SemicolonToken(), WhitespaceToken() ]))) ])) if text == 'foo { bar: rgb(255, 0, 127); }':
            pass
        case StyleSheet(contents=Contents([ QualifiedRule(prelude=[ HashToken(value='foo'), WhitespaceToken(value=' ') ], block=Block(token=OpenBraceToken(), contents=Contents([]))) ])) if text == '#foo {}':
            pass
        case StyleSheet(contents=Contents([ AtRule(name='media', prelude=[], block=Block(token=OpenBraceToken(), contents=Contents([ WhitespaceToken(value=' ') ]))) ])) if text == '@media{ }':
            pass
        case _ as product:
            raise AssertionError('No reference')
