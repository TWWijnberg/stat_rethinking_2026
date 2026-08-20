# A07 step 02 — Generative model
#
# The process assumed to have produced data(foxes). Consumes parameters, emits fake data.
# Never fitted. Shared with ../../direct-effect/, which inherits this script.
#
# NOTE: rethinking must not be attached — it breaks every brms fit.
# See docs/adr/0001-brms-over-rethinking.md.

library(dagitty)
library(here)

set.seed(2026)
# GEN_FIG, not OUT: this script is source()d by later steps, and a bare OUT
# would clobber theirs.
GEN_FIG <- here("homework", "contributions", "A07", "total-effect", "outputs")


# ---- The DAG -----------------------------------------------------------------
# One notation for the generative model; simulate_foxes() below is the other. The two
# must agree, or step 06 fails in ways that look inexplicable.

fox_dag <- dagitty("dag{ A -> F  F -> G  F -> W  G -> W }")

# Each edge is a claim:
#   A -> F   a bigger territory holds more food
#   F -> G   more food supports a larger group (foxes settle where the food is)
#   F -> W   more food in the territory makes a fox heavier
#   G -> W   more foxes sharing that territory makes each one lighter
# Deliberately absent:
#   A -> W   area reaches weight only through the food it holds
#   A -> G   area reaches group size only through food
#   G -> F   a larger group does not create food

cat("total  effect of F on W, adjustment set: "); print(adjustmentSets(fox_dag, "F", "W", effect = "total"))
cat("direct effect of F on W, adjustment set: "); print(adjustmentSets(fox_dag, "F", "W", effect = "direct"))


# ---- True parameter values ---------------------------------------------------
# On the standardised scale (M2 of step 01), so every coefficient reads as sd-per-sd.

true_pars <- list(
  bAF = 0.88,   # area -> food.        observed cor(A, F) = 0.883
  bFG = 0.90,   # food -> group size.  observed cor(F, G) = 0.901
  bFW = 0.60,   # food -> weight, direct: more food in the territory
  bGW = -0.70   # group size -> weight:   more mouths sharing it
)
# bFW and bGW are calibrated against the real correlation structure: solving
#   cor(F,W) = bFW + bFG*bGW = -0.025   and   cor(G,W) = bGW + bFG*bFW = -0.161
# gives 0.63 and -0.73, rounded here. This is a DECLARED use of the analysis dataset to
# set the generative model's true values. It keeps the recovery check in the regime the
# statistical model will actually operate in. It does not touch the priors.

# Residual sds derived from the coefficients so each variable has marginal sd 1 — derived
# rather than chosen, so they cannot drift out of step with the coefficients.
resid_sds <- local({
  p <- true_pars
  list(sF = sqrt(1 - p$bAF^2),
       sG = sqrt(1 - p$bFG^2),
       sW = sqrt(1 - (p$bFW^2 + p$bGW^2 + 2 * p$bFW * p$bGW * p$bFG)))
})


# ---- The simulation ----------------------------------------------------------
# Deliberately the simplest thing that carries the causal structure: four continuous
# variables, linear dependencies, Gaussian noise. See "Parked improvements" in the README
# for the realism that was left out and what would make it worth adding.

simulate_foxes <- function(n = 116, pars = true_pars, sds = resid_sds,
                           dF = 0, hold_G = FALSE, seed = NULL) {
  # dF     : shift applied to every territory's food, in sd units (the intervention)
  # hold_G : TRUE pins group size at its dF = 0 value (direct effect);
  #          FALSE lets it respond to food (total effect)
  if (!is.null(seed)) set.seed(seed)

  A  <- rnorm(n, 0, 1)
  F0 <- pars$bAF * A + rnorm(n, 0, sds$sF)     # food before any intervention
  F  <- F0 + dF

  eG <- rnorm(n, 0, sds$sG)
  G  <- pars$bFG * (if (hold_G) F0 else F) + eG

  W  <- pars$bFW * F + pars$bGW * G + rnorm(n, 0, sds$sW)

  data.frame(A = A, F = F, G = G, W = W)
}


# ---- The two estimands, as truth values for step 06 --------------------------
# Closed form: read off the parameters. Exact, because every dependency is linear.
#   total  = the direct path plus the path through group size
#   direct = the direct path alone

truth <- c(total  = true_pars$bFW + true_pars$bFG * true_pars$bGW,
           direct = true_pars$bFW)

# Consistency check: get the same numbers by actually running the intervention through
# simulate_foxes(). If these disagree, the algebra above and the code below have drifted
# apart — which is the failure step 06 would otherwise surface as unexplained.
truth_mc <- local({
  n <- 4e6
  c(total  = mean(simulate_foxes(n, dF = 1, seed = 99)$W - simulate_foxes(n, seed = 99)$W),
    direct = mean(simulate_foxes(n, dF = 1, hold_G = TRUE, seed = 99)$W -
                    simulate_foxes(n, seed = 99)$W))
})
stopifnot(all(abs(truth - truth_mc) < 0.005))

cat("\ntrue estimands (sd of W per 1 sd of F):\n")
print(round(rbind(closed_form = truth, monte_carlo = truth_mc), 4))

e <- new.env(); data("foxes", package = "rethinking", envir = e); real <- e$foxes
cat("\nin the estimand's units (kg per 1 sd of avgfood):\n")
print(round(truth * sd(real$weight), 4))


# ---- Run it and look at what comes out ---------------------------------------

sim <- simulate_foxes(116)
big <- simulate_foxes(1e5)
zr  <- data.frame(A = scale(real$area), F = scale(real$avgfood),
                  G = scale(real$groupsize), W = scale(real$weight))

cat("\ncorrelations (A-F, A-G, F-G, A-W, F-W, G-W):\n")
print(round(rbind(simulated = cor(big)[upper.tri(cor(big))],
                  real      = cor(zr)[upper.tri(cor(zr))]), 3))

dir.create(GEN_FIG, showWarnings = FALSE, recursive = TRUE)
png(file.path(GEN_FIG, "02_generative_check.png"), width = 1150, height = 760, res = 110)
op <- par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
for (v in c("A", "F", "G", "W")) {
  plot(density(big[[v]]), main = paste("marginal:", v), xlab = v, lwd = 2, col = "grey30",
       ylim = c(0, max(density(zr[[v]])$y) * 1.1))
  lines(density(zr[[v]]), lwd = 2, col = "firebrick")
  legend("topright", c("simulated", "real"), col = c("grey30", "firebrick"),
         lwd = 2, bty = "n", cex = 0.75)
}
plot(big$F[1:1500], big$W[1:1500], pch = 16, col = "#00000020",
     main = "F vs W (simulated)", xlab = "F", ylab = "W")
abline(lm(W ~ F, big), lwd = 2, col = "firebrick")
plot(zr$F, zr$W, pch = 16, col = "#00000060", main = "F vs W (real)", xlab = "F", ylab = "W")
abline(lm(W ~ F, zr), lwd = 2, col = "firebrick")
par(op); invisible(dev.off())
cat("\nwrote", file.path(GEN_FIG, "02_generative_check.png"), "\n")
