# A07 — Food and fox weight

Statistical Rethinking 2026, homework A07. `data(foxes)`: 116 foxes in 30 urban groups.

Assumed DAG:

```
      A
      ↓
      F ──→ G
      ↓     ↓
      └──→  W
```

`F` = avgfood, `G` = groupsize, `A` = area, `W` = weight.

Two estimands, so two analyses. They are reported side by side and never ranked, because
they answer different questions.

| Analysis | Estimand | Adjustment set |
|---|---|---|
| [`total-effect/`](total-effect/README.md) | total causal effect of F on W | ∅ |
| [`direct-effect/`](direct-effect/README.md) | direct causal effect of F on W | {G} |

Adjustment sets confirmed with `dagitty::adjustmentSets()`. There are no backdoor paths
from F to W — A's only outgoing edge is `A → F`, a dead end with respect to W — so the
total effect needs no adjustment at all. G is a descendant of F, disqualified from the
total-effect adjustment set by the backdoor criterion's first condition, and required in
the direct-effect one.
