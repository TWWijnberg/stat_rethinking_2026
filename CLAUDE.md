# CLAUDE.md

Project facts only. How to work is governed by the `bayes-workflow` skill — its
`CONTEXT.md` carries the language rules and the vocabulary, and its `SKILL.md` carries the
sign-off protocol. Do not duplicate those here; they would drift.

## What this is

Homework for Richard McElreath's Statistical Rethinking 2026 course.

## Where analyses live

`homework/contributions/<assignment>/` — one folder per assignment, e.g.
`homework/contributions/A08/`. This is the answer to the `bayes-workflow` skill's question
about where the analysis folder goes; do not ask.

Assignment briefs are PDFs in `homework/`, named `homework_A08.pdf`.

## Environment

**Python is the default backend.** The stack is installed and verified — bambi, pymc,
arviz, marginaleffects, networkx, pgmpy. Check it, do not reinstall:

```
python -c "import bambi, arviz, pymc, marginaleffects, networkx; print('ok')"
```

**`renv` is not working in this project.** `renv/library` does not exist and
`renv::install()` reports success while installing nothing. R packages are in the user
library. Do not trust `renv::status()`.

For R work: never attach `rethinking` alongside `brms` — see the skill's
`backends/brms.md`.
