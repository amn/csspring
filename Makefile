INSTALL_DATA = install -m 644
PYTHON = python
SHELL = bash
.SHELLFLAGS += -o pipefail
VPATH += $(dir $(lastword $(MAKEFILE_LIST)))

$(addprefix csspring/,$(addprefix syntax/,$(addsuffix .py,tokenizing))): %.py: expand-macros.py expand/%.py
	$(PYTHON) $< < $(lastword $^) | $(INSTALL_DATA) -D /dev/stdin $@

.DELETE_ON_ERROR:
