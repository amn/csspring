"""Parsing of CSS text aligned with [CSS] specification(s)."""

def _enable_selector_parsing():
    """Augment parsing procedures to enable parsing of CSS selectors.

    This adds on-demand parsing of the prelude part of every qualified rule (see the `prelude` attribute on `QualifiedRule`), when the `selector_list` property is accessed on the latter. Because parsing is only done when the property added with this procedure, is accessed, `csspring.syntax` remains compliant with its respective specification.
    """
    from .syntax.parsing import normalize_input, QualifiedRule
    from .selectors import parse_selector_list
    def qualified_rule_selector_list(self):
        return parse_selector_list(normalize_input(self.prelude))
    setattr(QualifiedRule, "selector_list", property(qualified_rule_selector_list))

_enable_selector_parsing()
