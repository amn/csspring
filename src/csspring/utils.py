"""Set of constructs to aid the rest of the package, of both the package-specific and the general kind that would otherwise warrant third-party dependencies."""

import builtins
from functools import partial
from itertools import groupby
import re
from sys import stderr
from traceback import print_stack

from abc import abstractmethod
from collections.abc import Iterable, Iterator, Sequence
from typing import Any, Protocol, runtime_checkable, TypeAlias, TypeVar

T = TypeVar('T')
T_co = TypeVar('T_co', covariant=True)
T_contra = TypeVar('T_contra', contravariant=True)

def intersperse(*items: T, separator: T) -> Iterable[T]:
    """Yield items with a separator yielded between each item.

    E.g. `intersperse(("foo", "bar", "baz"), "-")` will yield "foo", "-", "bar", "-", then "baz".

    Owing to a mere design choice, this procedure demands, by the type checker, that the separator be of a type co-variant with the type of items in the sequence, with the latter assumed to be homogenous (items are all of the same type).

    :param items: A sequence (items are assumed to be of the same type or share a super-type)
    :param separator: A value to yield between yielding each item in the sequence
    """
    it = iter(items)
    yield next(it)
    for item in it:
        yield separator
        yield item

@runtime_checkable
class Reader(Protocol[T_co]):
    """An interface to readable/readers, to assist type checking for the most part."""
    def read(self, size: int = -1, /) -> Sequence[T_co]:
        """See Python's own `IOBase.read` in the `io` module."""
        raise NotImplementedError

@runtime_checkable
class Unreader(Protocol[T_contra]):
    def unread(self, items: Iterable[T_contra]) -> None:
        """Undo a "reading" operation on a stream.

        The "unreading" refers to the operation where previously read item(s) inserted back into the stream in the same order they were read, so that the next `read` operation after "unreading" them returns them (in the same order they were read previously).

        :param items: the sequence of items to re-insert back at the head (readable with the next `read` operation); the items may or may not match what was read earlier, so technically something else than what was `read` may legitimately be inserted
        """
        raise NotImplementedError

@runtime_checkable
class Peeker(Protocol[T_co]):
    def peek(self, size: int, /) -> Sequence[T_co]:
        """Peek into the stream (without consuming/changing it).

        Defined more concretely, this allows peeking into a portion of the stream, as defined by `size` which specifies the size of the portion (i.e. number of items it contains).

        A [class of] text streams for use with so-called "tokenization" of input, for example, would implement this protocol to allow peeking at a number of code points available for immediate reading from the stream.

        :param size: maximum number of items to return
        :returns: a sequence of items; details would depend on concrete implementation, e.g. it could be the items that are available for reading from the stream (with the next `read` operation); the length of the sequence may be smaller than `size` if end of stream was encountered
        """
        raise NotImplementedError

@runtime_checkable
class PeekingUnreadingReader(Peeker[T], Reader[T], Unreader[T], Protocol[T]):
    """A protocol that defines readers that also support peeking ahead (at what will be consumed next) and undoing result of reading operations (inserting the read value back into the front of the stream)."""
    pass # The protocol is simply a union of its super-types.

class BufferedPeekingReader(PeekingUnreadingReader[T]):
    """Class of stream consumers (readers) that conform to the `PeekingUnreadingReader` protocol through employing a buffer.

    All streams that conform to the `Unreader` protocol, dubbed "reconsumable streams", are in principle [trivially] peekable through one additional method -- if something was consumed and can be reconsumed, it was already peeked at, essentially. Not all peekable streams are reconsumable, however -- a stream may provide peeking but not be able to _insert_ data back at the front of the stream for re-consuming.
    """
    _source: Reader[T]
    _buffer: list[T]
    def __init__(self, source: Reader[T]):
        self._source = source
        self._buffer = []
    def peek(self, size: int, /) -> Sequence[T]:
        assert size >= 0 # Reading with negative sizes is defined by `read` defined by Python, but not implemented yet for this class (that would call for a more elaborate implementation, while the reader is never used with negative values)
        r = size - len(self._buffer)
        if r > 0:
            self._buffer += [*self._source.read(r)]
        return self._buffer[:size]
    def read(self, size: int = -1, /) -> Sequence[T]:
        elements = self.peek(size)
        del self._buffer[:size]
        return elements
    def unread(self, items) -> None:
        self._buffer[:0] = items

def public_attrs(obj: object) -> Iterable[str]:
    """Return all of the object's public attributes.

    In this context, a public attribute is any whose name doesn't start with an underscore (`_`).
    """
    return (name for name in dir(obj) if not name.startswith('_'))

def dash_form(name: str) -> str:
    """Transform a [camel-case-like] string into a snake-case-like variant but one with periods and underscores replaced with the dash.

    This is useful for turning Python names into names featured in URLs etc, where different convention applies."""
    return re.sub(r'[_.]', '-', re.sub(r'(?<=[^A-Z])([A-Z])', r'_\1', name).lower())

def join(iterable:Iterable[str]) -> str:
    """Join a sequence into a string."""
    return ''.join(iterable)

def qualified_type_name(cls: type) -> str:
    """A stable (no reliance on "dunder" property) means to obtain the qualified name for a type."""
    return cls.__qualname__

def setattrs(obj: object, **kwargs) -> None:
    """Set multiple attributes on an object."""
    for attr in kwargs.items():
        setattr(obj, *attr)

CP: TypeAlias = str # [Unicode] code points are strings of length 1; note how this is distinct from but identically named type to the one in the `preprocessor` module; since the two are _mostly_ co-variant on account of both being co-variant with `str` (one being subclass and another an alias of `str`), this approach was deemed acceptable

class IteratorReader(Reader[T]):
    """Class of readers (objects featuring the eponymous `read` method) which "feed off" an iterator.

    In other words, constructing an object of this class given an `Iterator` as `source` allows to effectively utilize the `Iterator` through a `Reader` interface, which is essentially about the ability to read _multiple_ items vended by the iterator with a single call (to `read`).
    """
    _source: Iterator[T]
    def __init__(self, source: Iterator[T]):
        self._source = source
    def read(self, /, size=-1) -> Sequence[T]:
        result: list[T] = []
        while len(result) != size:
            try:
                result.append(next(self._source))
            except StopIteration:
                break
        return result

class ParseError(RuntimeError):
    """A [catch-all] class of errors that occur during or otherwise related to parsing."""
    pass

@runtime_checkable
class Appender(Protocol[T_contra]):
    """An abstract type that defines appending of elements to, presumably, a collection of elements.

    This is an interface for type checking purposes that allows working with e.g. lists "contra-variantly" if only write-only operations are needed on the list -- safely allowing `list[Y]` where `list[X]` parameter is expected, where `Y` is a supertype of `X`. See http://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science).
    """
    @abstractmethod
    def append(self, x: T_contra) -> None:
        raise NotImplementedError

def parser_error() -> None:
    """A "default" parser error handler procedure.

    The spec. does not really define any particular procedure for dealing with errors during parsing, despite allusion to the contrary. Neither do parsing procedures define passing of any error context explicitly, so we leverage Python's capable stack trace APIs to give the user sufficient context.

    Keep in mind that a tokenizer is _also_ a parser, in the general sense and in this specific context, and may thus also utilize this procedure.

    See also http://drafts.csswg.org/css-syntax/#parse-error for general definition of handling of parsing errors.
    """
    print_stack() # A convenience, retained even in "release" mode (in absense of a better way to communicate and handle parser errors)
    stderr.write('\nParsing encountered an error.\n')
    if __debug__: # When debugging parsing errors in an "interactive" terminal, load the debugger and suspend execution, for convenience
        from sys import stdin
        if stdin.isatty():
            breakpoint()

def is_custom_property_name_string(s: str) -> bool:
    """See http://www.w3.org/TR/css-typed-om-1/#custom-property-name-string."""
    return s.startswith('--')

def is_surrogate_code_point_ordinal(o: int) -> bool:
    """See `is_surrogate_code_point`."""
    return (0xd800 <= o <= 0xdbff) or (0xdc00 <= o <= 0xdfff)

def is_surrogate_code_point(cp: CP) -> bool:
    """Determine if a code point is a so-called surrogate code point.

    For definition of said "surrogate", see http://infra.spec.whatwg.org/#surrogate.
    """
    return is_surrogate_code_point_ordinal(ord(cp))
