"""A macro processing module for Python code.

Macro processing refers here to eager rewriting/replacement/substitution of Python code constructs decorated with the "syntactic" (no definition available normally, when the containing module is imported) decorator `macro`. The purpose of such processing is to implement the equivalent to what is usually called "pre-processing" for e.g. C/C++ language(s). As `macro`-decorated procedures (only decorating of procedures is currently effectively supported for `macro`) are encountered during processing of Python code, the entire procedure is removed and "unparsed" equivalent of the series of AST statements it returned, are inserted in its place instead.

This implements powerful and "semantically-aware" code pre-processing mechanism, for situations demanding it. Our immediate need with this was to allow type checkers like MyPy to be able to analyze as much of the project's Python code as possible, which these are normally unable to do in cases of so-called dynamically created types (and consequently object(s) of such types). And so instead of living with effectively uncheckable dynamic types created with the `type` built-in -- for e.g. `Token` subclasses -- we employ _pre-processing_ of Python code into Python code which lends to type-checking, a benefit we deemed to ba a "must-have" for the project.
"""

import ast
from collections.abc import Mapping, Sequence
import os
import sys
from typing import Any, Callable, cast, Iterable, TypeAlias

Pos: TypeAlias = tuple[int, int] # A [2-D] "position" (aka vector) type, for dealing with source code locations

def is_template_rewrite_decorator(decorator: ast.AST) -> bool:
    """Identify the `macro` decorator.
    :param decorator: An abstract syntax tree (AST) node representing a decorator in some [parsed] Python code
    :returns: `True` if the node represents the `macro` decorator, our marker for rewriting the entire decorated object, `False` otherwise
    """
    match decorator:
        case ast.Name(macro.__name__):
            return True
        case _:
            return False

def macro(callable: Callable[[], Iterable[ast.AST]]):
    """A `macro` decorator stub.

    The `macro` decorator isn't used beyond just identifying constructs in the code that it decorates -- but compiling of decorated constructs as part of dynamically constructed modules, something we depend on for actually executing the "macro" (the procedure `macro` decorates), demands that `macro` is defined in context of executing the module (see the `exec` call in `process`).
    :param callable: A callable to decorate with this decorator, as per convention; although all callables are permitted, decoration of object(s) other than procedures is undefined
    :returns: The decorated object; as is, currently `macro` is an identity function and the result value is immaterial to this module, since the result of decoration isn't actually executed
    """
    return callable

def source_span(lines: Sequence[str], prev: Pos, cur: Pos) -> Iterable[str]:
    """Get chunk(s) of text between two [line-and-column] positions

    E.g. `source_span('foo\nbar\nbaz'.splitlines(keepends=True), (1, 1), (3, 1))` will yield `'oo\n'`, `'bar\n'` and `'b'` (in that order).

    :param lines: Lines of source code to use for getting a span in
    :param prev: The "starting" position of the span, a 2-tuple with the (1-based) line number and column offset for first and second items, respectively
    :param cur: The "ending" position of the span, also a 2-tuple of the same profile as `prev`
    :returns: An iterable of chunks of text contained exactly between the two positions
    """
    yield lines[prev[0] - 1].encode()[prev[1]:(cur[1] if cur[0] == prev[0] else None)].decode()
    if cur[0] != prev[0]:
        for i in range(prev[0] + 1, cur[0]):
            yield lines[i - 1]
        yield lines[cur[0] - 1].encode()[:cur[1]].decode()

def process(source: str) -> Iterable[str]:
    """Find and replace macros in Python source code, vending rewritten copy.

    :param source: Body of Python source code (e.g. contents of Python module file)
    :returns: An iterable of chunks of source code generally equivalent to `source` but with occurrences of `macro`-decorated constructs "expanded" (replaced with "unparsed" result of calling the decorated construct)
    """
    lines = getattr(ast, '_splitlines_no_ff')(source) # TODO: Find a stable way to split source into lines in the manner compatible with `ast.parse`
    prev_node = ast.stmt(end_lineno=1, end_col_offset=0)
    assert prev_node.end_lineno is not None
    assert prev_node.end_col_offset is not None
    for node in ast.parse(source).body:
        is_macro_node = any(is_template_rewrite_decorator(decorator) for decorator in getattr(node, 'decorator_list', []))
        if is_macro_node:
            assert hasattr(node, 'decorator_list')
            macros: Mapping[str, Any] = dict()
            exec(compile(ast.Module(body=[ node ], type_ignores=[]), __file__, mode='exec'), globals(), macros)
            macro_node = node
            decorator_node = node.decorator_list[0]
            node = ast.stmt(lineno=decorator_node.lineno, col_offset=decorator_node.col_offset - 1)
        yield from source_span(lines, (prev_node.end_lineno, prev_node.end_col_offset), (node.lineno, node.col_offset))
        if is_macro_node:
            prev_item = None
            yield f"# The following construct(s) were inserted automatically through expansion of the {repr(cast(ast.FunctionDef | ast.ClassDef, macro_node).name)} macro\n\n"
            for item in next(iter(macros.values()))():
                if prev_item:
                    yield from os.linesep * 2
                ast.fix_missing_locations(item)
                yield ast.unparse(item)
                prev_item = item
            yield f"\n\n# End of macro expansion result"
        else:
            assert node.end_lineno is not None
            assert node.end_col_offset is not None
            yield from source_span(lines, (node.lineno, node.col_offset), (node.end_lineno, node.end_col_offset))
        prev_node = macro_node if is_macro_node else node

if __name__ == '__main__':
    source = (sys.stdin if len(sys.argv) < 2 or sys.argv[1] == '-' else open(sys.argv[1], newline='')).read()
    sys.stdout.writelines(process(source))
