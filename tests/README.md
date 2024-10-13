This directory defines a [`pytest`](http://www.pytest.org) application for testing `csspring`, for purposes of quality assurance.

The application is configured with the `pyproject.toml` file distributed with the `csspring` package.

You can, however, invoke it with this directory as current working directory, with:

```shell
pytest
```

The `csspring` package must be available to `pytest` for importing.

# Frequently Asked Questions

## Why do [most of] the CSS test files use CR+LF (aka DOS/Windows) for newline sequences?

It's just a historical artefact from the earlier days of the test suite when there was more rigorous testing of specifically implementation of [_preprocessing_](http://drafts.csswg.org/css-syntax/#input-preprocessing). Preprocessing is sensitive to newline coding and so the files have been left as-is to maintain the test suite. Looking ahead, however, we could imagine maintaining each test file, or at least _some_ of these, in two variants each where one exclusively uses CR+LF and the other exclusively LF. That would give more test coverage since neither is prohibited by the applicable specification(s).
