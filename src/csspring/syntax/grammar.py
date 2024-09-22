"""A utility module that implements grammar-related elements of the Syntax specification.
"""

from ..values import Production

# See https://drafts.csswg.org/css-syntax/#any-value
any_value = Production()
any_value.name = 'any_value'
