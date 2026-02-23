# =============================================================================
# 03_dag_analysis.R - DAG Specification and Causal Analysis
# =============================================================================
# Purpose: Define causal model, identify adjustment sets, specify priors
# =============================================================================

source(here::here("homework", "contributions", "A06", "scripts", "00_setup.R"))

# --- DAG Definition ----------------------------------------------------------
dag <- dagitty("dag {
  A -> F
  F -> G
  F -> W
  G -> W
}")

# Set coordinates to match homework diagram layout
coordinates(dag) <- list(
  x = c(F = 1, A = 2, G = 3, W = 2),
  y = c(F = 2, A = 1, G = 2, W = 3)
)

# Plot the DAG
plot(dag)

# --- Adjustment Sets ---------------------------------------------------------
# Estimate the causal effect of F on W P(W | do(F))
# without using dagitty, my reasoning is as follows:
# There are two paths from F to W. F->W and F->G->W. The first path is a direct causal path, while the second path is a pipe. Because we want to estimate the total causal effect of F on W, we do not have to block the pipe path by adjusting for G.
# A is not relevant for estimating the causal effect of F on W.
adjustmentSets(dag, exposure = "F", outcome = "W")