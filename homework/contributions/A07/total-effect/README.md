# A07 total effect — food on fox weight

## Estimand
The expected change in a fox's body weight, in kg, caused by raising its territory's
`avgfood` by one standard deviation (0.198 index units), averaged over the 116 foxes in
the sample — with group size left free to respond to food as it naturally does.
— **causal**

## Backend
brms

## Status
| Step | Name | State |
|------|------|-------|
| 01 | Estimand | complete |
| 02 | Generative model | complete |
| 03 | Data | complete |
| 04 | Statistical model | complete |
| 05 | Prior check | complete |
| 06 | Recovery | complete |
| 07 | Fit | complete |
| 08 | Posterior check | complete |
| 09 | Estimate | complete |

## Decisions

### Step 01
**M1. Estimand as a quantity** — the statement above, in kg per 1 sd of `avgfood`, averaged
over the 116 foxes, group size free to respond. The averaging unit is the fox, not the
territory: the homework asks about "the weight of foxes inside it", and a fox is the unit
that has a weight. Rejected "the effect of food on weight" as a topic rather than a
quantity. — signed off 2026-08-19

**M2. Scale: standardise both F and W; report the estimate back in kg** — standardising
makes the priors interpretable without arithmetic (β is sd-per-sd, so |β| > 1 is visibly
absurd), makes the intercept the average fox rather than a fox in a territory with zero
food, and removes a −0.967 intercept/slope posterior correlation. `avgfood` is a bare index
with no natural unit and a 4.24 sd observed range, so raw units are uninterpretable.
Rejected: raw units; rejected: standardising F only. — signed off 2026-08-19

**M3. Two analyses, not one** — the total and direct effects are separate estimands with
different adjustment sets (∅ vs {G}), so they run as separate analyses and are never
ranked against each other. No WAIC/PSIS comparison between them. — signed off 2026-08-19

**M4. Backend: brms on cmdstanr** — chosen on maintenance grounds: brms is actively
maintained and is what the wider applied community uses, where `rethinking` is a teaching
package tied to one book. Rejected `rethinking::ulam`, which is pedagogically clearer but
less transferable; the transparency loss is recovered by writing priors explicitly with
`set_prior()` and reading `brms::stancode()`. — signed off 2026-08-19

**M5. Record M4 as an ADR** — yes. Written to `docs/adr/0001-brms-over-rethinking.md`.
Extended on sign-off with a fitting-algorithm policy: `algorithm = "laplace"` while
prototyping or iterating rapidly, `algorithm = "sampling"` once a statistical model is
settled, and results reported only from NUTS draws. — signed off 2026-08-19

## Environment constraint
`library(rethinking)` must never be attached — it breaks every brms fit with
`invalid names for slots of class "stanfit": model_`. Reach its datasets without attaching:
`e <- new.env(); data("foxes", package = "rethinking", envir = e)`.

## Declarations
Standardising by the sample sd makes any prior stated in sd-units mildly dependent on this
dataset — a weak form of **empirical prior**. Declared rather than silent. Second-order
here, but a candidate for sensitivity analysis if a prior turns out to matter.

## Parked improvements
Left out of the generative model deliberately. Each would be adopted only if the condition
beside it is met, and compared against the simple version on merit.

| Complication | Left out because | Adopt if |
|---|---|---|
| Group structure — 30 territories, predictors shared, rows per group = G | the statistical models never treat G as an outcome, so G's realism buys nothing | posterior widths in step 06 turn out not to resemble those on the real data |
| Integer group size, clamped to [2, 8] | rounding is a non-linearity that breaks the closed-form truth for a cosmetic gain | a later model puts G on the left-hand side |
| Group-level varying intercept on W | measured, and there is nothing there: ANOVA on residuals after F and G gives F = 0.80, p = 0.75, ICC ≈ 0 | step 08 shows residual structure by group |
| Unobserved confounder on F–W | the homework's DAG asserts it away, and it would leave both estimands unidentified | sensitivity analysis is wanted |

`scripts/06_recovery.R` — fits the step 04 model with step 05 priors to simulated data;
checks parameters and the estimand against step 02's true values; sweeps n from 116 to
50 000. Emits `06_recovery.png`.

`scripts/07_fit.R` — NUTS fit of `Ws ~ Fs` to the 116 real foxes; diagnostics; emits
`07_trace.png` and `data/07_fit.rds`.

`scripts/08_posterior_check.R`, `scripts/09_estimate.R` — posterior checks and the
estimand, computed on draws. Emit `08_posterior_check.png`, `09_estimate.png`,
`09_results_report.html`.

## Result
| Quantity | kg per 1 sd of avgfood | 89% interval | P(>0) |
|---|---|---|---|
| **Total effect** | **-0.029** | [-0.205, +0.152] | 40% |
| Direct effect | +0.566 | [+0.223, +0.910] | 100% |
| Indirect path F->G->W | -0.612 | [-0.915, -0.312] | 0% |
| Group size, per 1 sd of G | -0.679 | [-1.015, -0.346] | 0% |

`total = direct + indirect`. Food makes each fox heavier and attracts enough extra foxes
to cancel the gain. Report: `outputs/09_results_report.html`.

### Caveats
Assumes the homework's DAG, untested. Averages over the 116 foxes in this sample, not
British urban foxes generally. Explains 0.1% of the variation in weight (10% with group
size) — an average causal effect, not a predictive model. The total effect's interval
spans zero: the honest claim is *small*, not *negative*.

## Revisions
*none*

## Files
`scripts/01_estimand.R` — draws the DAG, marks the adjustment set for each estimand, and
lists every path from F to W. Emits `01_estimand_dag.png`. No model is fitted.
`scripts/02_generative_model.R` — encodes the fox system as both a `dagitty` DAG and a
`simulate_foxes()` function; holds the true parameter values; computes the true total and
direct effects by intervening on the simulation; compares simulated marginals and
correlations against the real data.
`outputs/02_generative_check.png` — simulated vs real marginals and F–W scatter.
`scripts/03_data.R` — loads `data(foxes)` unmodified, describes it (univariate stats, and
bivariate stats for the four relationships the DAG asserts), builds the standardised
analysis frame, and tests the frame against simulated data of the same size.
`outputs/03_data_check.png` — real vs simulated marginals and pairwise scatters.
`outputs/03_eda_hist.png`, `outputs/03_eda_scatter.png`, `outputs/03_eda_report.html`
— the descriptive report (published artifact).
`outputs/03_analysis_frame.rds` — the analysis frame plus the scaling constants step 09
needs to convert back to kg.
`scripts/04_statistical_model.R` — `bf(Ws ~ Fs)`, gaussian/identity, no fitting.
`scripts/05_prior_check.R` — vague vs weakly informative priors, prior predictive draws
read against a fox weighing 0–15 kg; emits `05_prior_check.png` and `05_priors.rds`.
