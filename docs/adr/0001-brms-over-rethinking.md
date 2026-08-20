# brms, not rethinking, is this project's fitting backend

**Status:** accepted, 2026-08-19 · **Context:** Statistical Rethinking 2026 coursework

## Decision

Statistical models in this project are fitted with `brms` on the `cmdstanr` backend, not
with `rethinking::ulam` or `rethinking::quap`, despite this being a `rethinking` course.

## Why

`brms` is actively maintained and is the tool the wider applied Bayesian community uses;
`rethinking` is a teaching package tied to one book and maintained accordingly. The point
of the course is to learn Bayesian workflow, and that skill transfers further when it is
practised on the library the learner will keep using afterwards.

It is also what the `bayes-workflow` skill is built around (its ADR 0001): `brms` covers
prior predictive (`sample_prior = "only"`), posterior predictive (`pp_check()`), marginal
effects (`marginaleffects`) and comparison (`loo_compare()`) directly, so the workflow's
steps map onto one-liners rather than bespoke code.

## Considered and rejected

**`rethinking::ulam`** — pedagogically clearer, since `alist()` puts the likelihood and
every prior on the page where `bf(y ~ x)` leaves them implicit behind defaults. It is also
what the lectures hand you. Rejected on maintenance grounds; the transparency loss is
recovered by writing priors explicitly with `set_prior()` and reading `brms::stancode()`.

**`rethinking::quap`** — superseded by `algorithm = "laplace"` below.

## Consequences

**`library(rethinking)` must never be attached.** With it attached, every `brms` fit fails
at `invalid names for slots of class "stanfit": model_`. Reach its datasets without
attaching it:

```r
e <- new.env(); data("foxes", package = "rethinking", envir = e); d <- e$foxes
```

**Lecture code needs translating.** `ulam()` models from the course must be rewritten as
`brm()` formulas plus explicit `set_prior()` calls. Priors that `ulam` states outright
have to be supplied deliberately, since brms would otherwise fill them from defaults.

**Fitting algorithm follows the stage of the work.** `algorithm = "laplace"` while
prototyping or iterating rapidly — it is the `quap` analogue and, on the A07 model,
matched NUTS to four decimal places in 1.2s against 29.7s. Switch to the default
`algorithm = "sampling"` once a statistical model is settled, and report only from NUTS
draws. Variational algorithms (`meanfield`, `fullrank`) are not used at any stage: on the
same trivial two-parameter model `meanfield` returned a slope an order of magnitude off
with a posterior sd 80% too wide. `pathfinder` is broken under brms 2.23.0 + cmdstanr 0.9.0.

**Compiled models are cached** via `brm(file = ...)`, so the ~30s compile is paid once.
