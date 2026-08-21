# A07 direct effect — food on fox weight, holding group size fixed

## Estimand
The expected change in a fox's body weight, in kg, caused by raising its territory's
`avgfood` by one standard deviation (0.198 index units), **holding group size fixed at its
observed value**, averaged over the 116 foxes in the sample.
— **causal**

The single clause separating this from the total effect is "held fixed" versus "left free
to respond". That clause is the whole difference between the two analyses.

## Backend
brms

## Status
| Step | Name | State |
|------|------|-------|
| 01 | Estimand | complete |
| 02 | Generative model | complete (inherited from `../total-effect/`) |
| 03 | Data | complete (inherited from `../total-effect/`) |
| 04 | Statistical model | complete |
| 05 | Prior check | complete |
| 06 | Recovery | complete |
| 07 | Fit | complete |
| 08 | Posterior check | complete |
| 09 | Estimate | complete |

There is one fox system and one dataset, so steps 02 and 03 are shared rather than
duplicated. This analysis forks at step 04, where the adjustment set becomes {G}.

## Decisions

### Step 01
**M3b. Estimand as a quantity** — the statement above. Adjustment set {G}, confirmed with
`dagitty::adjustmentSets(effect = "direct")`. G's only parent is F, so G is not a collider
and conditioning on it is safe: it closes `F → G → W` and leaves `F → W` standing.
— signed off 2026-08-19

M1's averaging unit, M2's scale decision, and M4's backend carry over unchanged from
`../total-effect/README.md`.

## Constraint inherited from this ruling
Step 02's generative model must carry **separate** coefficients on `F → W` and `G → W`, so
that both the total and the direct effect have a known closed-form truth value for the
step 06 recovery check.

`scripts/06_recovery.R` — the same, for `Ws ~ Fs + Gs`, where Fs and Gs are correlated
at 0.85 in simulation and 0.90 in the real data. Emits `06_recovery.png`.

`scripts/07_fit.R` — NUTS fit of `Ws ~ Fs + Gs`; diagnostics; emits `07_trace.png`
and `data/07_fit.rds`.

`scripts/08_posterior_check.R` — posterior checks. Step 09 is computed in
`../total-effect/scripts/09_estimate.R`, which covers both estimands so the contrast
between them can be taken on shared draws.

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
`scripts/04_statistical_model.R` — `bf(Ws ~ Fs + Gs)`, gaussian/identity, no fitting.
`scripts/05_prior_check.R` — the same priors re-checked, since a second slope adds variance.
Inherited: `../total-effect/scripts/02_generative_model.R`, `03_data.R`, `05_priors.rds`. Its `simulate_foxes(hold_G = TRUE)`
arm is this analysis's estimand, and `true_pars$bFW = 0.60` is its recovery target.
