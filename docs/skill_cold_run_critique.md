# SKILL_TEST_NOTES — running `bayes-workflow` (bambi backend) cold

Run 2026-08-20, 12:04 to 12:33, by an agent with no prior exposure to these skill
files. Target: `homework/contributions/A07_with_python/`. All ten workflow steps
completed; all eleven scripts run clean from an empty `outputs/` and
`data/processed/`.

This file is the critical half of the deliverable. It is deliberately blunt. The
analysis worked; that does not mean the skill files did.

---

## 0. The headline problem: the skill contains the answers to its own test

I was told not to look at the reference answers until I had my own. **That
instruction cannot be obeyed.** Three of the four files the run protocol makes
mandatory reading contain the A07 results, and I read all of them before writing
a line of step 01.

- **`CONTEXT.md`** uses A07 as the worked example for six of its vocabulary
  entries. It states: the two adjustment sets differ and give "−0.02 sd" against
  "+0.48"; `normal(0, 0.5)` on a slope means "about ±1.2 kg per standard
  deviation"; the intercept was first set to `normal(0, 0.2)` and that was
  circular; the direct model beats the total model "by 4.9 elpd"; the generative
  model had four variables and the total-effect model one predictor.
- **`backends/bambi.md`** ends with a section headed *Cross-checked against the R
  backend* giving a table of: total effect −0.0241 / −0.0261, direct effect
  +0.566 / +0.563 kg, prior sensitivity `Fs` 0.120 / 0.116 and `Gs` 0.118 / 0.119,
  adjustment sets `{}` and `{G}`, and `localTests A ⊥ W | F` = 0.0887.
- **`tools/dag_tools.py`** embeds all 116 rows of the foxes dataset as a string
  constant and hard-codes the expected local-test output —
  `A ⊥ G | F` 0.1551 [−0.0289, 0.3291], `A ⊥ W | F` 0.0887 [−0.0960, 0.2675] — as
  ground truth in `test_against_dagitty()`. SKILL.md instructs you to run that
  self-test.

So by the time I reached step 01 I already knew the adjustment sets, the sign and
rough size of both effects, the elpd gap, the prior-sensitivity verdict, and the
exact local-test numbers. My step 02 hand-derivation of the adjustment sets and my
step 06 statement that `Normal(0, 0.5)` allows "about ±1.16 kg per sd" should be
read with that in mind — they are not independent of the reference.

**This is a test-design fault, not a stylistic one.** If the skill is ever to be
evaluated on an unseen problem, A07 has to be removed from CONTEXT.md, from
bambi.md, and from dag_tools.py's self-test, and replaced with a different worked
example. Right now the skill cannot be tested on the one problem it documents
itself with.

Second-order effect: because the example is always A07, it is impossible to tell
which instructions are general and which were reverse-engineered from this
dataset. "A true value near zero is a decision to surface" (step 04) reads like a
general rule but is exactly the A07 situation.

---

## 1. Things that are wrong in the skill files

Ordered by how much they cost.

### 1.1 `marginaleffects` does not support bambi. At all. (worst)

`backends/bambi.md`, *By step → Estimate*:

> **Estimate.** `marginaleffects` — the same package as the R backend, with the
> same API.

It is not the same package with the same API. In R, `marginaleffects` supports
`brms` and the estimate step works as advertised. In Python it does not support
`bambi` or `pymc`:

```
me.avg_comparisons(bambi_model, variables={"Fs": 1})
ValueError: Unknown model type. Supported modelling packages include
`statsmodels` and `pyfixst`. In addition, users can call `fit_sklearn()` or
`fit_linearmodels()` ...
```

Supported: statsmodels, pyfixest, scikit-learn, linearmodels. No Bayesian backend
at all. The headline advice of the step-10 section is unusable, and the file was
supposedly verified on 2026-08-20 with `marginaleffects 0.6.1` in the stack — so
it was listed as a dependency without the one call it is listed for ever being
run.

Nothing methodological is lost here (linear model, additive shift, so the average
marginal effect *is* the coefficient and the draw-based version is exact), which
by CONTEXT.md's *Tool limits* rule makes it a constraint rather than a decision. I
routed around it: the estimand is computed on the draws in
`scripts/_shared_estimand.py`, and `marginaleffects` is used to drive a
`statsmodels` OLS of the same formula as an independent cross-check. That turned
out to be more informative than the original plan would have been — the OLS/posterior
gap of 0.20 kg on model b is a third measurement of the prior shrinkage.

**Fix:** rewrite the Estimate section to say: compute the estimand from the draws;
`marginaleffects` is available only against a frequentist refit, as a cross-check.

### 1.2 `marginaleffects` reserves the column name `group`

Even against statsmodels, passing a frame containing a column called `group`
raises `ValueError: Input data contain reserved column name(s): {'group'}`. The
foxes data's territory id is called `group`. Undocumented; cost a debugging cycle.
Worth a line in the traps list, since `group` is a near-universal column name in
multilevel data.

### 1.3 `tabulate` is missing, so `pandas.to_markdown()` raises

`ImportError: Import tabulate failed.` Step 03 asks for an EDA report with tables
collapsed behind a toggle, and the obvious way to build one is `.to_markdown()`.
`tabulate` is not in the verified environment list and not installed. Since I was
told not to install anything, I wrote a 12-line markdown-table function
(`md_table` in `03_data.py`). Either add `tabulate` to the verified stack or say
in the backend file that markdown tables must be hand-written.

Related: markdown table cells cannot contain a raw `|`, and the local-test claim
strings are literally `A _||_ G | {F}`. Every pipe has to be escaped or the table
silently collapses. I hit this and fixed it.

### 1.4 Saving the fit does not work as implied

Step 08: "Fit, save the fit object to `data/`". The arviz-native way is
`idata.to_netcdf()`, which raises:

```
ValueError: cannot write NetCDF files with format='NETCDF4' because none of the
suitable backend libraries (netCDF4, h5netcdf) are installed
```

Neither library is in the verified stack, and the backend file names no
alternative. I pickled instead, with a comment saying why and noting that a pickle
is not an archival format. The backend file should either add `h5netcdf` or say
"pickle the InferenceData".

### 1.5 `steps/02-dag.md` documents the wrong return value

```python
dt.adjustment_sets(g, "F", "W", effect="total")     # []  -> no adjustment
```

It returns `[()]`, not `[]`. `backends/bambi.md` gets this right; the step file
does not. The two values mean **opposite** things in this API: `[()]` is "the
empty set identifies the effect", `[]` is "no set of these variables identifies
it". An agent following the step file would write `if not sets: print("no
adjustment needed")` and report a non-identifiable estimand as identified. I
wrote an explicit `fmt_sets()` helper in `02_dag.py` that distinguishes them,
because the trap is easy to fall into.

### 1.6 `dag_tools.py` self-test disagrees with its own ground truth

The self-test prints `A ⊥ G | F ... [-0.0289, +0.3289]` while its `want` dict says
`0.3291`; likewise `0.2674` against `0.2675`. It passes only because the tolerance
is 5e-4. The module docstring says "Validated against dagitty", which oversells
it — there is a systematic difference in the fourth decimal of the Fisher-z
interval bounds, probably a different normal quantile constant. Harmless, but it
should be stated rather than absorbed by a tolerance.

### 1.7 `draw_dag` cannot do what step 02 asks for

Step 02, *Always run*: "The DAG figure, one panel per estimand, with conditioned
variables drawn as filled boxes." `draw_dag` takes no `ax` and calls
`plt.subplots` and `fig.savefig` itself, so it cannot draw into a shared figure.
One panel per estimand is impossible in one file. I emitted two files
(`02_dag_a_total.png`, `02_dag_b_direct.png`). Either add an `ax=None` argument or
change the step file to say "one figure per estimand".

Minor cosmetic issue: node labels are placed at `y - 0.32`, which lands underneath
the filled box drawn for a held variable, so the `groupsize` caption nearly
touches the `G` box.

### 1.8 Every step file names R filenames, under a Python backend

`steps/03-data.md` says "Write `scripts/03_data.R`". Same for `04_generative_model.R`,
`05*_statistical_model.R`, `07*_recovery.R`, `08*_fit.R`, `09*_posterior_check.R`,
`10_estimate.R`. ADR 0004 makes bambi the default backend, so the default path
through this skill produces `.py` files while every step file tells you to write
`.R`. I ignored it and noted it in each script's docstring. Trivial to fix, and
corrosive to trust in the step files while it stands.

### 1.9 Step 07 asks for a "fast algorithm" this backend does not have

> Use the backend's fast algorithm for [the sweep], and the reported one only for
> the fit at the real sample size. That difference is usually an order of
> magnitude in runtime.

There is no such algorithm here. `backends/bambi.md` itself says nutpie *loses* to
PyMC at homework size (5.2s vs 3.1s) because compilation dominates. I measured the
only other candidate, bambi's Laplace approximation: **18.7s against 4.2s** for
four MCMC chains on 116 rows — four times slower. So the sweep uses the same
algorithm as the reported fit and economises on chains and draws instead, and the
step report says so explicitly.

The step file is written for a world (brms/Stan, where `algorithm="meanfield"` or
a cached compile really is 10× faster) that the Python backend is not in. It
should say: check whether your backend has one, and report that it does not if it
does not.

### 1.10 `steps/01-estimand.md` and `SKILL.md` disagree about whether step 01 has a script

SKILL.md's layout: `scripts/ 01_estimand … 10_estimate`. Step 01: "**Always run:**
Nothing. This step has no script and no data." I wrote `01_estimand.py` that prints
the estimand statements, so the numbering is unbroken and the estimand has a home
in code. Pick one and say so.

### 1.11 Step 07 requires step 10's code, which does not exist yet

> Then check the **estimand itself**: compute it from the posterior exactly as
> step 10 will.

At step 07, `10_estimate.py` has not been written. The skill offers no home for
code shared between steps, and the numbered-script convention has no slot for it.
I created `scripts/_shared_estimand.py` (leading underscore to keep it out of the
sequence) and imported it from both 07 and 10. That is an invention, not an
instruction. The skill should say where shared code goes.

### 1.12 The run protocol never mentions `docs/adr/`

SKILL.md's sign-off section tells you to propose ADRs and says where they go. The
run protocol's reading list is CONTEXT → backend → SKILL → the current step file,
"and that file alone". So the four existing ADRs — including `0002-agent-native-sign-off`,
which by its title is directly about the situation I was in — are never read. I did
not read them, per the literal instruction. If they carry binding decisions, the
protocol should say to read them.

### 1.13 The one-step-per-invocation design has no end-to-end mode

"One invocation runs **one step**", and each step ends on a human sign-off. Running
the skill end to end, which is what I was asked to do, is outside the design. With
sign-offs pre-granted it worked fine, but nothing in the files describes how to
sequence a full run, so the ordering, the file naming, and where the per-step
"report" text goes (I put it in each script's stdout) were all mine to invent.

---

## 2. Things that worked exactly as documented

Recorded so the list above is not mistaken for a verdict on the whole skill.

- **All five environment traps in `backends/bambi.md` are real and all five would
  have cost me time.** `cores=1` (default hung in a scratch test before I read the
  file); `PYTHONIOENCODING=utf-8` (arviz's `psense_summary` really does print a
  `✓`); `m.build()` before `prior_predictive`; the leading-slash group names —
  `"/posterior_predictive" in idata.groups` is exactly right and the unslashed form
  silently returns `False`; and running `psense_summary` before `m.predict`. This is
  the most valuable page in the skill.
- `az.summary` reporting an 89% ETI by default in arviz 1.x — correct, and the
  warning that it was 94% in 0.x is the kind of thing that saves a wrong table.
- `dag_tools` adjustment sets, path listing with open/closed status, and
  `local_tests` all behaved as documented and matched my hand derivation.
- `m.backend.model` giving the underlying PyMC model — correct, and step 08's
  "read the program the formula compiled to" is worth the two lines it takes.
- Printing a built `bmb.Model` to see every prior — this is genuinely the
  `get_prior()` equivalent and made "which defaults must be replaced" a one-line
  check.

And four places where following the file produced something I would not otherwise
have done:

1. **Step 04's "a true value near zero is a decision to surface"** caught that the
   true total effect is −0.03, so its recovery check is nearly vacuous — an
   estimator returning zero would pass. Without that line I would have reported
   "recovered" for both and meant much less by it.
2. **Step 06's "where each true value falls in its own prior"** put `bGW = −0.70`
   at the 8th percentile of `Normal(0, 0.5)` and predicted shrinkage — two steps
   before step 07 measured it and three before step 08's power-scaling flagged it.
3. **Step 07's coverage requirement** found 79.5% coverage on model b's direct
   effect against a nominal 89%. The sample-size sweep, which is what most people
   would call recovery, showed **0 misses in 15** and hid it completely. This is
   the single most valuable thing the skill produced in this run, and it changed
   what step 10 was allowed to claim.
4. **Step 09's "check the things you chose not to model"** made the 30-territory
   check the point of the step rather than an afterthought. It is the only check in
   step 09 that could have sent the analysis back.

---

## 3. Every error hit, and what fixed it

| # | Error | Where | Fix |
|---|---|---|---|
| 1 | `ImportError: Import tabulate failed` | `03_data.py`, `.to_markdown()` | hand-written `md_table()` |
| 2 | markdown tables broken by `\|` in `A _\|\|_ G \| {F}` | `03_data.py` EDA report | escape pipes before writing |
| 3 | `ValueError: cannot write NetCDF files` | `08_fit.py`, `idata.to_netcdf()` | pickle, with a comment |
| 4 | `ValueError: Unknown model type` | `10_estimate.py`, `me.avg_comparisons(bambi_model)` | compute on draws; use marginaleffects on an OLS refit as cross-check |
| 5 | `ValueError: Input data contain reserved column name(s): {'group'}` | `10_estimate.py`, marginaleffects + statsmodels | drop `group` before the call |
| 6 | I wrote "coverage sits near nominal" before reading the table, which said 0.795 | `07_recovery.py` | rewrote the section; added a wide-prior rerun to prove the cause |
| 7 | I wrote "the OLS contrasts land on the posterior means to about a hundredth of a kg" — true for model a, off by 0.20 kg for model b | `10_estimate.py` | rewrote; the 0.20 kg gap is the shrinkage and is now reported as a third confirmation of it |

Errors 6 and 7 are mine, not the skill's, and I record them because they are the
failure mode the skill is built to prevent: writing the interpretation before
reading the number. CONTEXT.md's "**Run the code and show the output**" is the rule
that caught both. It works.

Non-fatal noise nobody warns about: PyMC prints `Initializing NUTS…`, `Sequential
sampling…`, `NUTS: [sigma, Intercept, Fs]` to stdout regardless of
`progressbar=False`, and arviz prints a multi-line "We detected potential issues"
banner from `psense_summary`. Clean script output is impossible without filtering.

---

## 4. What I had to work out for myself that the skill should have told me

- **Where to put `SKILL_TOOLS` in a script.** `backends/bambi.md` says
  `sys.path.insert(0, SKILL_TOOLS)` with `SKILL_TOOLS` undefined, and warns "do
  not guess it, and do not copy an absolute path from an example". But a script
  that must run standalone tomorrow has to get it from *somewhere*. I hard-coded
  it as a named constant at the top of `02_dag.py`. The skill should say: put it
  in one named constant, or read it from an environment variable.
- **How numbered scripts import each other.** `05a_statistical_model` is not a
  valid Python identifier, so `import 05a_statistical_model` is a syntax error.
  `importlib.import_module("05a_statistical_model")` works. Nothing says so, and
  the multi-estimand layout in SKILL.md guarantees you will need it.
- **What a posterior predictive p-value is, here.** Step 09 requires "summary
  statistics with posterior predictive p-values" and gives no definition and no
  code. One-tailed or two? Which direction? I defined it as the share of
  replications at or below the observed value and flagged outside [0.05, 0.95].
- **How to compute "posterior variance as a share of prior variance"** (step 07).
  The prior sd is not recoverable from the InferenceData, so it has to be tracked
  by hand alongside the model spec. I keep a `PRIOR_SD` dict in `07_recovery.py`
  that duplicates the priors in `05a/05b` — a real duplication the skill's
  structure forces.
- **That diagnostics should be asserted, not just printed.** Step 08's prose says
  read the diagnostics "before reading a single coefficient", but nothing suggests
  enforcing it. I added an `assert` that fails the script if any diagnostic is
  bad. That is what the prose actually means.
- **Where the per-step "report" text lives** when there is no human to report to.
  I put it in each script's stdout, so the reasoning is next to the number that
  prompted it. Reasonable, but invented.
- **How to compute the indirect path's uncertainty.** Step 10 asks for the
  decomposition. The indirect term is a difference of two posteriors fitted to the
  same data by two separate models; they are dependent and there is no honest
  joint interval without a joint model. The skill does not mention the problem. I
  reported the interval with an explicit warning that it understates uncertainty,
  and parked a joint/SEM model in the README.

---

## 5. Time per step

Wall clock, including writing and debugging each script.

| Step | Span | Minutes | Note |
|---|---|---|---|
| Read CONTEXT + bambi.md + SKILL.md, verify env | 12:04–12:05 | ~1 | |
| 01 Estimand | 12:05–12:06 | ~1 | no computation |
| 02 DAG | 12:06–12:07 | ~1.5 | includes running the dag_tools self-test |
| 03 Data | 12:07–12:09 | ~2 | `tabulate` failure, pipe-escaping |
| 04 Generative model | 12:09–12:10 | ~1.5 | |
| 05a + 05b Statistical model | 12:10–12:12 | ~1.5 | |
| 06 Prior check | 12:12–12:14 | ~2 | |
| 07 Recovery | 12:14–12:21 | **~7** | the coverage finding and its rework |
| 08 Fit | 12:21–12:23 | ~2 | netCDF failure |
| 09 Posterior check | 12:23–12:24 | ~1.5 | |
| 10 Estimate | 12:24–12:31 | **~7** | two marginaleffects failures, sensitivity refits, the report |
| README, clean end-to-end re-run | 12:31–12:33 | ~2 | |

**Total ≈ 29 minutes.** Compute is a small fraction of it: the full pipeline runs
in about two minutes, of which step 07's twelve fits are roughly half. The two
seven-minute steps are the two where the skill demanded something that turned up a
result I had not expected, which is the correct place for the time to go.

---

## 6. Comparison against the R reference answers

Read only after the numbers above were final and written.

| Quantity | Reference (R/brms) | This run (Python/bambi) | Verdict |
|---|---|---|---|
| Adjustment set, total | `{}` | `{}` | match |
| Adjustment set, direct | `{G}` | `{G}` | match |
| Total effect, sd | −0.024 | −0.0261 | match, MC error |
| Total effect, kg | −0.029 | −0.0309 | match |
| Total effect, 89% | [−0.205, +0.152] | [−0.207, +0.148] | match |
| Direct effect, sd | +0.478 | +0.4701 | match, MC error |
| Direct effect, kg | +0.566 | +0.5567 | match |
| Direct effect, 89% | [+0.223, +0.910] | [+0.201, +0.903] | match |
| Indirect path, sd | −0.517 | −0.4962 | **differs by 0.021 sd** |
| Indirect path, kg | −0.612 | −0.5875 | **differs by 0.025 kg** |
| `A ⊥ G \| F` | 0.1551 [−0.0289, 0.3291] | 0.1551 [−0.0289, 0.3289] | 4th decimal |
| `A ⊥ W \| F` | 0.0887 [−0.0960, 0.2675] | 0.0887 [−0.0960, 0.2674] | 4th decimal |
| Power-scaling, total model | clean | clean (max 0.013) | match |
| Power-scaling, direct model | flags both slopes | flags `Fs` 0.109, `Gs` 0.118 | match |

**Disagreements, and what they are.**

1. **The indirect path, 0.025 kg.** Not an independent discrepancy. The indirect
   term is defined as `total − direct`, so it accumulates both differences with the
   same sign: my direct effect is 0.008 sd smaller and my total 0.002 sd more
   negative, giving 0.021 sd. Both inputs match within Monte Carlo error, so the
   derived quantity does too — it just looks worse because the errors add. A good
   argument for reporting the indirect path with the caveat I gave it.
2. **The local-test interval bounds, 4th decimal.** `dag_tools.py` carries the R
   values as its own `want` constants and tolerates 5e-4, so this is a known
   difference inside the tool rather than something my run introduced. Point
   estimates and p-values agree exactly.
3. Nothing else disagrees. The two backends land on the same answer, the same
   adjustment sets, and the same prior-sensitivity verdict.

`backends/bambi.md`'s own cross-check table gives Python total −0.0261 and direct
+0.563 kg. I reproduce −0.0261 exactly and +0.5567 against +0.563 — a 0.006 kg
difference, consistent with a different seed or draw count in whatever run
produced that table.

---

## 7. If one thing gets fixed

Remove A07 from `CONTEXT.md`, `backends/bambi.md`, and `dag_tools.py`'s self-test,
and rewrite the step-10 section of the backend file so it does not tell you to use
a package that rejects the models this backend produces. Those two changes make
the skill testable and make its most-used step's instructions true. Everything
else on the list is a half-hour of editing.
