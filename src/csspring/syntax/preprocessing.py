"""Preprocessing of input to tokenization of CSS text, per http://drafts.csswg.org/css-syntax/#input-preprocessing."""

from ..utils import CP, is_surrogate_code_point

from collections.abc import Callable, Iterator

class FilteredCodePoint(CP):
    """Class of code point derivatives that reference the original (unfiltered) code point sequence.

    E.g. filtered '\r\n' can be represented by a `CP` object with the `source` attribute being `\r\n` and the value (`CP` is a string) being the filtered product, in this case `\n`.

    Said representation facilitates recovery of original text from a sequence of filtered code points, and by indirection, from token(s).
    """
    source: CP
    def __new__(cls, *args, source: CP, **kwargs):
        """Most native primitive types, including `str`, cannot be effectively extended in traditional manner -- by invoking `super` in the subclass constructor -- `__new__` must be overriden instead to yield the object of appropriate type because the super-class does not feature a constructor and objects of the class are constructed with e.g. `str.__new__`.
        """
        obj = super().__new__(cls, *args, **kwargs)
        assert len(obj) <= 1 # Code points are always single-character strings or an empty string (signifying the end-of-stream condition)
        obj.source = source
        return obj

def filter_code_points(next: Callable[[], CP]) -> Iterator[FilteredCodePoint]:
    """See http://drafts.csswg.org/css-syntax/#css-filter-code-points."""
    cp = next()
    while cp:
        match cp:
            case '\r' if (cp := next()) == '\n':
                yield FilteredCodePoint('\n', source='\r\n')
            case '\r':
                yield FilteredCodePoint('\n', source='\r')
                continue
            case '\f':
                yield FilteredCodePoint('\n', source=cp)
            case _ if cp == '\0' or is_surrogate_code_point(cp):
                yield FilteredCodePoint('\uFFFD', source=cp)
            case _:
                yield FilteredCodePoint(cp, source=cp)
        cp = next()
