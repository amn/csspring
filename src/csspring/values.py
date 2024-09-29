"""Implement the ["CSS Values and Units Module Level 4"](http://drafts.csswg.org/css-values-4) specification.

Only parts currently in use by the rest of the `csspring` pcakge, are implemented.
"""
from .syntax.tokenizing import Token, CommaToken, CommentToken, WhitespaceToken
from .syntax.tokenizing import ColonToken

import builtins
from collections.abc import Iterable, Mapping
from functools import singledispatchmethod
import re
from typing import cast

class Production:
	"""An [abstract] class of CSS grammar elements.

	A production is a broadly accepted term for objects that express a subset of valid linguistic permutations for a given formal language.

	In this case the formal language is CSS, with tokens (see `Token`) for symbols.

	This class is effectively a "tag" type (given how it imposes no effective protocol), it serves to help distinguish productions from other kinds of objects, for at least type checking purposes. The feature that allows productions to automatically acquire a name, owing to Python's meta-programming features, is only added to this specific class for sheer convenience of it.

	Note that a production is not the same as a [parse] product -- the former is an element of some language grammar, while the latter is a result of parsing a sequence of tokens in accordance with a grammar production, in effect expressing one single element from the set of all permutations a production would permit, corresponding to consumed part of the sequence.
	"""
	name: str
	def __set_name__(self, _, name):
		assert not hasattr(self, "name") or self.name == name # Given the usage context, we don't want to support re-setting the name, not with a different value at least
		self.name = name

# The fundamental grammar productions are defined below, see also http://drafts.csswg.org/css-values-4/#value-defs

class ReferenceProduction(Production):
	"""Class of productions expressing a _reference_ to some production.

	In the canonical notation, references use _names_ to identify the production they point to, but since we do not employ a grammar parser (e.g. one that parses BNF) but use the equivalent of an already parsed grammar, we permit ourselves the convenience of references pointing to productions directly, without using names. This is not to say production names aren't used, they are, just for different purposes (e.g. for serializing the parsed grammar back to the kind of notation defined by the "Values and Units" specification.
	"""
	element: Production
	def __init__(self, element: Production):
		self.element = element

class AlternativesProduction(Production):
	"""Class of productions expressing exactly one production from from an _ordered_ set of alternatives.

	Note that the ordered set type is expressed with `Iterable`, as Python doesn't provide a true ordered set type (with _ordered_ insertion) and while `Iterable` will allow duplicate items, something that a set normally must not, it was deemed an acceptable compromise to retain simplicity and avoid importing exotic third-party type(s) that implement ordered sets.

	Implements the `|` combinator as defined at http://drafts.csswg.org/css-values-4/#component-combinators.
	"""
	elements: Iterable[Production] # The alternative(s)
	def __init__(self, *elements: Production):
		self.elements = elements

class ConcatenationProduction(Production):
	"""Class of productions equivalent to a concatenation (ordered sequence) of productions.

	Implements "juxtaposing components" as defined at http://drafts.csswg.org/css-values-4/#component-combinators.

	The name of the class borrows the term "concatenation" from the more general parsing lingo.
	"""
	elements: Iterable[Production]
	def __init__(self, *elements: Production):
		"""
		:param elements: A sequence of productions expressing the concatenation
		"""
		self.elements = elements

class NonEmptyProduction(Production):
	"""Class of productions that behave much like `ConcatenationProduction` but only permit a _non-empty_ concatenation.

	Implements the `[...]!` notation as defined at http://drafts.csswg.org/css-values-4/#mult-req, see the "An exclamation point (!) after a group..." bullet point.
	"""
	element: ConcatenationProduction
	def __init__(self, element: ConcatenationProduction):
		"""
		:param element: A [`ConcatenationiProduction`] production that will express the non-empty concatenation
		"""
		self.element = element

class RepetitionProduction(Production):
	"""Class of productions that express repetition of an element (with optionally lower and upper bounds on the number of repetitions).

	Implements the `*` notation as defined at http://drafts.csswg.org/css-values-4/#mult-zero-plus.
	"""
	element: Production
	min: int
	max: int | None
	def __init__(self, element: Production, min: int = 0, max: int | None = None):
		"""
		:param element: The production expressing the repeating part of this production
		:param min: The minimum amount of times the parser must accept input, i.e. the minimum number of repetitions of token sequences accepted by the parser
		:param max: The maximum amount of times the parser will be called, i.e. the maximum number of repetitions that may be consumed in the input; the value of `None` implies no maximum (i.e. no upper bound on repetition)
		"""
		assert min >= 0
		assert max is None or max > 0
		assert max is None or min <= max
		self.min = min
		self.max = max
		self.element = element

class OptionalProduction(RepetitionProduction):
	"""Class of productions equivalent to `RepetitionProduction` with no lower bound and accepting no repetition of the element, meaning the element is expressed at most once.

	Implements the `?` notation as defined at http://drafts.csswg.org/css-values-4/#mult-opt.
	"""
	def __init__(self, element: Production):
		super().__init__(element, 0, 1)

class TokenProduction(Production):
	"""Class of productions that express a token, optionally one with a matching set of attributes.
	"""
	type: type[Token]
	attributes: Mapping
	def __init__(self, type: builtins.type[Token], **attributes):
		"""
		:param type: The type of token this production will express
		:param attributes: Mapping of presumably token attribute values by name, to use for expressing the set of attributes on the token this production will express
		"""
		self.type = type
		self.attributes = attributes

whitespace = RepetitionProduction(TokenProduction(WhitespaceToken), min=1) # The white-space production; presence of white-space expressed with this production, is _mandatory_ (`min=1`); the definition was "hoisted" here because a) it depends on `RepetitionProduction` and `TokenProduction` definitions, which must thus precede it, and b) because the `CommaSeparatedRepetitionParser` definition that follows, depends on it, in turn

class CommaSeparatedRepetitionProduction(Production):
	"""Class of productions that express a non-empty comma-separated repetition (CSR) of a production element.

	Unlike `RepetitionProduction` which permits arbitrary number of the production element, this class does not currently implement arbitrary repetition bounds. The delimiting part (a comma optionally surrounded by white-space) is mandatory, which implies at least one repetition (two expressions of the element). Disregarding the delimiting behaviour, productions of this class thus behave like those of `RepetitionProduction` with `2` for `min` and `None` for `max` property values.

	Implements the `#` notation as defined at http://drafts.csswg.org/css-values-4/#mult-comma.
	"""
	delimiter = ConcatenationProduction(OptionalProduction(AlternativesProduction(whitespace, TokenProduction(CommentToken))), TokenProduction(CommaToken), OptionalProduction(AlternativesProduction(whitespace, TokenProduction(CommentToken)))) # The production expressing the delimiter to use with the repetition, a comma with [optional] white-space around it
	element: Production
	def __init__(self, element: Production):
		"""
		:param element: A production to use for expressing the repeating part in this production
		"""
		self.element = element

class Formatter:
	"""Class of objects that offer procedures for serializing productions into streams of text formatted per the [value definition syntax](http://drafts.csswg.org/css-values-4/#value-defs)."""
	grouping_strings = ('[ ', ' ]') # The kind of grouping symbol to use when a production expression must be surrounded with a pair of brace-like grouping symbols, in its serialized form
	def grouping_mode(self, production: Production):
		"""Determine whether a given production shall require an explicit pair of grouping symbols when featured as an _operand_ (e.g. in binary/unary operation context).
		:returns: `True` if the expression of `production` serialized with this formatter, should feature explicit grouping symbols wrapping it, `False` otherwise
		"""
		match production:
			case AlternativesProduction() | CommaSeparatedRepetitionProduction() | ConcatenationProduction() | NonEmptyProduction() | RepetitionProduction(): return not hasattr(production, "name")
			case ReferenceProduction() | TokenProduction(): return False
			case _:
				raise ValueError
	def combined(self, productions: Iterable, combinator: str) -> Iterable[str]:
		it = (self.format(production) for production in productions)
		yield from next(it)
		for item in it:
			yield combinator
			yield from item
	@singledispatchmethod
	def format(self, production: Production) -> Iterable[str]:
		raise TypeError(f"No suitable `format` method for {production}")
	@format.register
	def _(self, production: AlternativesProduction) -> Iterable[str]:
		return self.combined(production.elements, ' | ')
	@format.register
	def _(self, production: ConcatenationProduction) -> Iterable[str]:
		return self.combined(production.elements, ' ')
	@format.register
	def _(self, production: NonEmptyProduction) -> Iterable[str]:
		yield from self.operand(production.element)
		yield '!'
	@format.register
	def _(self, production: ReferenceProduction) -> Iterable[str]:
		yield '<' + self.name(production.element) + '>'
	@format.register
	def _(self, production: RepetitionProduction) -> Iterable[str]:
		yield from self.operand(production.element)
		yield self.multiplier(production)
	@format.register
	def _(self, production: TokenProduction) -> Iterable[str]:
		if production.attributes:
			if 'value' not in production.attributes or len(production.attributes) > 1:
				raise NotImplementedError # the "Values and Units" specification doesn't feature token productions with matching of attributes other than `value`
			yield repr(production.attributes['value'])
		else:
			if hasattr(production.type, 'value'):
				yield '<' + re.sub(r'(^)?[A-Z]', lambda m: (('-' if m[1] is None else '') + m[0].lower()), production.type.__name__) + '>' # type: ignore # MyPy 1.11 complains with "error: Unsupported operand types for + ("str" and "bytes")  [operator]", but the error appears to be a false positive: http://github.com/python/mypy/issues/12961 # TODO: Revisit the issue following MyPy updates
			else:
				if issubclass(production.type, ColonToken):
					value = ','
				else:
					raise TypeError
				yield repr(value)
	@format.register
	def _(self, production: CommaSeparatedRepetitionProduction) -> Iterable[str]:
		yield from self.operand(production.element)
		yield '#'
	def multiplier(self, production: RepetitionProduction) -> str:
		match (production.min, production.max):
			case (0, 1):
				return '?'
			case (0, None):
				return '*'
			case (1, None):
				return '+'
			case _:
				return '{' + (str(production.min) if production.min == production.max else str(production.min) + ',' + (str(production.max) if production.max else '')) + '}'
	def name(self, production: Production) -> str:
		"""Get the name of a production.

		:raises AttributeError: if the production does not have a name
		"""
		return production.name.replace('_', '-')
	def operand(self, production) -> Iterable[str]:
		group_start, group_end = self.grouping_strings if self.grouping_mode(production) else ('', '')
		yield group_start
		yield from self.format(production)
		yield group_end
