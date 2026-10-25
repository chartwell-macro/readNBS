# Contributing to readNBS

Thank you for helping improve `readNBS`. The package is a source-faithful
client for selected National Bureau of Statistics of China (NBS) series and
release tables. Contributions are most useful when they make a source,
identity, schema, or user workflow more explicit without weakening the
package's fail-closed guarantees.

## Before reporting a problem or opening a pull request

For a bug, email [chartwellmacro@gmail.com](mailto:chartwellmacro@gmail.com).
For a proposed API, coverage expansion, or change to a retained source, email a proposal describing the user need, the exact NBS source, and the compatibility
implications before implementing it. Please do not paste credentials,
private data, or unredacted local paths into an issue.

A useful report includes:

* the `readNBS` version, R version, operating system, and locale/time zone;
* the function, stable series or dataset ID, filters, and date range involved;
* the complete error or warning and a small reproducible example; and
* whether the failure occurs with the bundled fixtures/offline path or only
  when contacting the public NBS portal.

## Development principles

* Stable semantic IDs, exact UUIDs, published values, reporting-period
  geometry, schema boundaries, and row-level provenance are part of the
  package contract. Do not silently rename, rebase, interpolate, or splice
  incompatible official series.
* Ordinary package use and tests must not depend on live portal availability.
  Use the injected transport and the existing small fixtures for deterministic
  tests. Live requests belong in explicit maintenance workflows only.
* Preserve the per-client cache, bounded request settings, and the package's
  explicit network and archive-loading behaviour.
* Roxygen comments are the source of truth for reference documentation.
  Update examples and task-oriented vignettes when a public contract changes.
* Retained source captures and normalized archives are reviewable artifacts.
  Do not edit generated archive rows by hand; use the documented maintenance
  scripts, retain the official source URL and hash, and add a focused test for
  each changed schema or coverage boundary.

## Checks before requesting review

From the package root, run the focused tests for the affected area and then
the full fixture-based test suite. Also run `R CMD check --as-cran` on a built
source tarball when the change affects package metadata, documentation,
examples, vignettes, or runtime code. Regenerate roxygen documentation and
render the README/vignettes when their sources change. The repository's CI
checks are authoritative for the supported R versions and operating systems.

Keep commits small enough to review and explain any intentional change to
public output, source provenance, performance, or network behaviour in the
pull request. Changes that alter a downstream contract should include the
before/after schema and an explicit migration discussion.

## Pull requests

Open a pull request against `main` at
<https://github.com/chartwell-macro/readNBS/pulls>. Describe the motivation, the
source or fixture evidence, checks run, and any limitations or follow-up
maintenance required. A maintainer may request a narrower change if a pull
request combines unrelated documentation, source, and archive updates.
