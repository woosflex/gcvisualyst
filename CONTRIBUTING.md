# Contributing to gcvisualyst

Thanks for considering a contribution. gcvisualyst is an R package for
compositional genomics analysis (GC content, GC skew, CpG islands) with an
interactive Shiny app. We aim for clean, correct, well-tested R code that can
graduate to Bioconductor.

## Code of Conduct

All participants must follow the [Code of Conduct](CODE_OF_CONDUCT.md).

## Developer Certificate of Origin (DCO)

By contributing you agree to the [Developer Certificate of Origin](https://developercertificate.org/)
(v1.1) — that you have the right to submit the code under the project's license
(Artistic 2.0). Sign off each commit with `git commit -s`. Commits without a
sign-off may be rejected.

## How to Contribute

1. **Fork** the repository and clone your fork.
2. **Create a topic branch** (`fix/...`, `feat/...`, `docs/...`).
3. Make a small, focused change — one PR = one logical change.
4. Add **tests** for your change and ensure the suite stays green:
   ```r
   # from the package root
   devtools::test()
   ```
5. **Commit** with a clear message (see conventions) and push to your fork.
6. Open the PR; reference the issue it closes (`Closes: #12`).

## Requirements

- **License:** The package is **Artistic 2.0**. Do not add files under other
  license terms without discussion.
- **Style:** Follow standard tidyverse-style R, roxygen2 documentation for every
  exported function (so man/ + NAMESPACE regenerate cleanly).
- **Tests:** New/modified behavior must be covered in `tests/testthat/`.
- **Cross-check:** run
  ```bash
  R CMD build .
  R CMD check --no-manual <tarball>
  ```
  and aim for **Status: OK**, zero ERROR/WARNING/NOTE.
- **Shiny app:** if you touch `inst/shiny/app.R`, keep it working from
  `run_app()` and using only exported package functions.

## Commit Conventions

- Title: concise, under 50 chars, imperative ("Fix X", not "Fixed X").
- Description: wrap at 72 chars; state the **why** and reference the issue.
- Group related changes into one commit.

## Reporting Issues

Include: package version, R version, OS, a minimal reproducible example, and
expected vs actual behavior. Security issues: use the private channel in
[SECURITY.md](SECURITY.md) — do not open a public issue.
