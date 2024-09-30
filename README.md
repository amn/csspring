Offered here is `csspring`, a software library for parsing of [CSS](http://www.w3.org/TR/CSS) text, implemented as a Python package.

## Installation

Installation of the library follows [Python package installation conventions](http://packaging.python.org/en/latest/tutorials/installing-packages).

Releases of the library are made available for immediate installation at PyPi, the project page is at http://pypi.org/project/csspring.

Installation can thus be done with e.g. [`pip`](http://packaging.python.org/en/latest/key_projects/#pip) over Internet:

```shell
pip install csspring
```

Releases are also published with the project's [canonical] Github repository, at http://github.com/amn/csspring/releases. These releases contain the same files that get uploaded to PyPi.

The installation package files are built with [the conventional method](http://packaging.python.org/en/latest/tutorials/packaging-projects) (with the repository being current working directory):

 ```shell
python -m build
```

It should go without saying that whether you choose to install the package with `pip install csspring` or specify one of the files downloaded from the "Releases" Github page or built yourself (`csspring-....tar.gz` or `csspring-....whl`), to `pip install` — the result is equivalent, as the building process described above is used for releasing the package for distribution and installation.

## Usage

### Examples

The code snippet below demonstrates obtaining of a _parse tree_ (in the `stylesheet` variable) by parsing the file `example.css`:

```python
from csspring.syntax.parsing import parse_stylesheet
stylesheet = parse_stylesheet(open('example.css', newline='')) # The `newline=''` argument prevents default re-writing of newline sequences in input — per the CSS Syntax spec., parsing does filtering of newline sequences so no rewriting by `open` is necessary or desirable
```

## Documentation

Proficient usage of the library is expected first and foremost reading the documentation supplied with the source code in the form of [_docstrings_](http://docs.python.org/3.11/glossary.html#term-docstring) which annotate the package and its elements:

```python
import csspring
help(csspring) # Will list packages and modules contained by `csspring`, which one may further invoke `help` on, as is convention
```

Some of the documentation is naturally deferred to the [Syntax](http://drafts.csswg.org/css-syntax) and [Selectors](http://drafts.csswg.org/selectors-4) specifications that the library implements.

Requirements to Python (version, platform etc) are expressed with the provided `pyproject.toml` file (as per [PEP 621](http://peps.python.org/pep-0621/), originally).

## Compliance

The `csspring.syntax` package was written to implement the Editor's Draft edition of the ["CSS Syntax Module Level 3"](http://drafts.csswg.org/css-syntax) specification, with the latter serving as reference during development. This was done to reduce the amount of effort required to implement a parser — the parser can "blindly" follow the steps outlined in the specification and defer to the latter what regards ambiguities and even design choices (for better and for worse). The specification does de-facto double as an abstract CSS parser, after all.

The Editor's Draft edition was chosen specifically instead of [the "Technical Report" version](http://www.w3.org/TR/css-syntax-3) because initial attempts at following the latter uncovered some ambiguities that we could not resolve. [^1]

> [!NOTE]
> Staying true to the specification, the `csspring.syntax` package does _not_ itself implement parsing of [_selectors_](http://drafts.csswg.org/selectors). Parsing of CSS text that includes parsing of selectors [in qualified rules], is enabled by the top-level `csspring` package augmenting construct(s) in the `csspring.syntax` package, which happens automatically during importing of any module. Said augmentation is done in a manner that doesn't break compliance for `csspring.syntax` yet enables parsing of selectors in CSS text all the same. Parsing of selectors is done on demand — a rule's `prelude` value is parsed when accessing the `selector_list` property on `QualifiedRule` objects.

The `csspring.selectors` module was written to implement the Editor's Draft edition of the ["Selectors Level 4"](http://drafts.csswg.org/selectors-4) specification. The module offers parsing of CSS selectors specifically. The Editor's Draft edition was chosen over [the "Technical Report" version](http://www.w3.org/TR/selectors-4) for consistency with the `csspring.syntax` package following an Editor's Draft edition.

## Deviations

### Preservation of input

Parsing offered by the library _preserves all input text_ it is fed, character for character (down to the original unfiltered input text). This allows recovery of white-space and comments in CSS text, as-is, a property of the parser that was neither defined nor facilitated by the CSS syntax specification. Such preservation of input was designed into this implementation to facilitate a broader range of parsing applications, where e.g. transforming stylesheets must be done without discarding of comments or inadverted change of white-space in transformed output. The parser thus includes the so-called identity transformation parsing — where input is parsed into a product from which the original text stream may be recovered _exactly_, without any loss.

## Disclaimer

Parsing is offered only in the form of Python modules — no "command-line" program(s), e.g. such that can be invoked from the shell to parse CSS file(s) and write parse trees (in some format fit for the purpose), are included. This was a deliberate choice to contain the scope of the project, since adding a command-line parsing tool arguably implies solving a number of problems which have little to do with parsing proper. For instance, the tool would need to decide on the serialization format for the parse trees it writes. In any case, such a tool would likely benefit from a development project of its own, and so was considered out of scope here. Rest assured this library should be well able to support such tool and that one may be written in the future, to complement the library and make its function more inter-operable and accessible.

## Frequently Asked Questions

### Why?

We wanted a "transparent" CSS parser — one that could be used in different configurations without it imposing limitations that would strictly speaking go beyond parsing. Put differently, we wanted a parser that does not assume any particular application — a software _library_ in the classical sense of the term, or a true _API_ if you will.

For instance, the popular [Less](http://lesscss.org) software seems to rather effortlessly parse CSS [3] text, but it invariably re-arranges white-space in the output, without giving the user any control over the latter. Less is not _transparent_ like that — there is no way to use it with recovery of the originally parsed text from the parse tree — parsing with Less is a one-way street for at least _some_ applications (specifically those that "transform" CSS but need to preserve all of the original input as-is).

In comparison, this library was written to preserve _all_ input, _as-is_. This became one of the requirements defining the library, contributing to its _reason d'etre_.

### Why Python?

As touched upon in [the disclaimer above](#disclaimer), the parser was written "from the bottom up" - if it ever adopts a top layer exposing its features with a "command line" tool, said layer will invariably have to tap into the rest of it, the library, and so in the very least a library is offered. Without a command-line tool (implying switches and other facilities commonly associated with command-line tools) the utility of the parser is tightly bound to the capabilities of e.g. the programming language it was written in, since the language effectively functions as the interface to the library (you can hardly use a library offered in the form of a C code without a C compiler and/or a dynamic linker). A parser is seldom used in isolation, after all — its output, the parse tree, is normally fed to another component in a larger application. Python is ubiquitous and attractive on a number of metrics relevant to us. The collective amount of Python code is growing steadily, which drives adoption, both becoming factors for choosing to offer CSS parsing written in specifically Python.

Another factor for choosing Python was the fact we couldn't find any _sufficiently capable_ CSS parsing libraries written specifically as [reusable] Python module(s). While there _are_ a few CSS parsing libraries available, none declared compliance with or de-facto support CSS 3 (including features like nested rules etc). In comparison, this library was written in close alignment with CSS 3 standard specification(s) (see [the compliance declaration](#compliance)).

### What's with the name?

Ignoring the "css" part, "spring" in the name refers to my starting the project in [early] spring of [2024]. A Python package needs a name, _some_ name, from the get-go, and the name stuck. I pronounce it as *cs-spring*.

## References

[^1]: http://github.com/w3c/csswg-drafts/issues/10119#issuecomment-2016156566
