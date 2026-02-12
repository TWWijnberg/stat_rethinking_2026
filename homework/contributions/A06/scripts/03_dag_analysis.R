# =============================================================================
# 03_dag_analysis.R - DAG Specification and Causal Analysis
# =============================================================================
# Purpose: Define causal model, identify adjustment sets, specify priors
# =============================================================================

source(here::here("homework", "contributions", "A06", "scripts", "00_setup.R"))

# --- DAG Definition ----------------------------------------------------------
dag <- dagitty("dag {
  Z -> X
  Z -> Y
  A -> Z
  A -> Y
  X -> Y
}")

# Set coordinates to match homework diagram layout
coordinates(dag) <- list(
  x = c(X = 3, Y = 4, Z = 2, A = 1),
  y = c(X = 1, Y = 2, Z = 1, A = 2)
)

# Plot the DAG
plot(dag)

# --- Adjustment Sets ---------------------------------------------------------
# Estimate the causal effect of X on Y P(Y | do(X))
# without using dagitty, my reasoning is as follows:
# The key goal is to remove the indirect effect from Z to X to Y, which influences the causal effect of X on Y. 
# this means we need a model that includes Z. Finally, we can break the fork by stratifying on Z
# A is not relevan for estimating the causal effect of X on Y.
adjustmentSets(dag, exposure = "X", outcome = "Y")

# this model will also produce an estimate for Z. What is the interpretation of this parameter?
# it is certainly not the causal effect of Z on Y, because A will need to be included for this model.
# my suspicion is that the parameter reflects the causal effect of Z on Y AND the direct effect of A on Y through Z.
# This is because this is the part of the variation on Y that can be detected through Z.