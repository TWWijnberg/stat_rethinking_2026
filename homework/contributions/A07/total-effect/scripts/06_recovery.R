# A07 step 06 — Recovery
#
# Fit the step 04 statistical model, with the step 05 priors, to data from the step 02
# generative model where the truth is known. Check the coefficients come back, then check
# the ESTIMAND comes back — a model can recover its coefficients and still get the
# estimand wrong.
#
# Also sweeps sample size: the estimate should converge on the truth, and the priors
# should lose their grip, as n grows.
#
# NOTE: rethinking must not be attached. See docs/adr/0001-brms-over-rethinking.md.

library(brms)
library(here)

options(brms.backend = "cmdstanr", mc.cores = parallel::detectCores())
source(here("homework", "contributions", "A07", "total-effect",
            "scripts", "02_generative_model.R"))

MD  <- here(".stan_models")
OUT  <- here("homework", "contributions", "A07", "total-effect", "outputs")   # figures, reports
DATA <- here("homework", "contributions", "A07", "total-effect", "data")      # rds, fits

priors <- readRDS(file.path(DATA, "05_priors.rds"))
W_SD   <- readRDS(file.path(DATA, "03_analysis_frame.rds"))$scales$W["sd"]

TRUE_TOTAL  <- unname(truth["total"])    # -0.030 sd of W per sd of F
TRUE_DIRECT <- unname(truth["direct"])   # +0.600


# ---- The estimand, computed from a posterior exactly as step 09 will ---------
# The total effect is the average contrast in W between do(F) and do(F + 1 sd). For this
# model that is the Fs coefficient itself, but it is computed here through the same
# prediction path step 09 uses, so both are tested together.

estimand_draws <- function(fit, d) {
  lo <- posterior_epred(fit, newdata = transform(d, Fs = Fs))
  hi <- posterior_epred(fit, newdata = transform(d, Fs = Fs + 1))
  rowMeans(hi - lo)                       # one value per posterior draw
}

# algorithm follows ADR 0001: NUTS for the fit we report, Laplace for the sweep,
# which is a diagnostic. At these sample sizes the posterior is Gaussian and the two
# agree to several decimals; the sweep is ~30x cheaper this way.
fit_at <- function(n, seed = 7, algorithm = "sampling") {
  it <- if (algorithm == "sampling") 2000 else 1000
  ch <- if (algorithm == "sampling") 4 else 2
  set.seed(seed)
  s <- simulate_foxes(n)
  d <- data.frame(Ws = s$W, Fs = s$F, Gs = s$G)
  fit <- brm(bf(Ws ~ Fs), data = d, family = gaussian(), prior = priors,
             chains = ch, iter = it, algorithm = algorithm, refresh = 0,
             silent = 2, seed = 6, stan_model_args = list(dir = MD),
             file = file.path(DATA, paste0("06_recovery_n", format(n, scientific = FALSE), "_", algorithm)))
  list(fit = fit, d = d, est = estimand_draws(fit, d))
}


# ---- Recovery at the real sample size ----------------------------------------

cat("\n\n================ STEP 06 ================\n")
r116 <- fit_at(116)
dr <- as_draws_df(r116$fit)

within <- function(x, lo, hi) if (x >= lo && x <= hi) "yes" else "NO"
q <- function(v, p) unname(quantile(v, p))

par_tab <- data.frame(
  parameter = c("b_Fs (= total effect)", "sigma"),
  true      = sprintf("%+.3f", c(TRUE_TOTAL, resid_sds$sW)),
  posterior = sprintf("%+.3f", c(mean(dr$b_Fs), mean(dr$sigma))),
  ci89      = c(sprintf("[%+.3f, %+.3f]", q(dr$b_Fs, .055), q(dr$b_Fs, .945)),
                sprintf("[%+.3f, %+.3f]", q(dr$sigma, .055), q(dr$sigma, .945))),
  recovered = c(within(TRUE_TOTAL, q(dr$b_Fs, .055), q(dr$b_Fs, .945)),
                within(resid_sds$sW, q(dr$sigma, .055), q(dr$sigma, .945))))
cat("\nparameters, n = 116:\n"); print(par_tab, row.names = FALSE)

e <- r116$est
cat(sprintf("\nESTIMAND (total effect), n = 116:\n  true %+.4f | posterior mean %+.4f | 89%% [%+.4f, %+.4f] | recovered: %s\n",
            TRUE_TOTAL, mean(e), q(e, .055), q(e, .945),
            within(TRUE_TOTAL, q(e, .055), q(e, .945))))
cat(sprintf("  in kg per sd of food: true %+.3f | posterior %+.3f\n",
            TRUE_TOTAL * W_SD, mean(e) * W_SD))


# ---- Sample-size sweep -------------------------------------------------------
# Does the estimate converge on the truth, and do the priors stop mattering?

ns <- c(116, 500, 2000, 10000, 50000, 100000)
sweep <- lapply(ns, fit_at, algorithm = "laplace")
names(sweep) <- ns

# prior sd on the slope, for the "how much of this is prior?" column
PRIOR_SD <- 0.5

sw <- do.call(rbind, lapply(seq_along(ns), function(i) {
  e <- sweep[[i]]$est
  data.frame(n = ns[i],
             estimate = sprintf("%+.4f", mean(e)),
             ci89 = sprintf("[%+.4f, %+.4f]", q(e, .055), q(e, .945)),
             post_sd = sprintf("%.4f", sd(e)),
             err = sprintf("%+.4f", mean(e) - TRUE_TOTAL),
             covers = within(TRUE_TOTAL, q(e, .055), q(e, .945)),
             prior_share = sprintf("%.0f%%", 100 * sd(e)^2 / PRIOR_SD^2))
}))
cat("\nsample-size sweep — true total effect", sprintf("%+.4f", TRUE_TOTAL), "\n")
cat("(prior_share = posterior variance as a fraction of prior variance;\n",
    " small means the data, not the prior, is setting the answer)\n\n", sep = "")
print(sw, row.names = FALSE)



# ---- Is a missed interval at large n a problem? ------------------------------
# As n grows the interval shrinks, so ordinary sampling noise can push it off the truth
# even though the estimator is unbiased. An 89% interval is SUPPOSED to miss 11% of the
# time, at every n. Repeating the fit over many simulated datasets shows whether the
# misses are at that rate. lm is used here: at n = 50 000 the priors contribute nothing
# (posterior variance is under 0.01% of prior variance), so it matches brms and lets the
# check run 100 times cheaply.

NREP <- 100; NCAL <- 50000
cal <- replicate(NREP, {
  s <- simulate_foxes(NCAL); m <- lm(W ~ F, s)
  ci <- confint(m, "F", level = 0.89)
  c(est = unname(coef(m)["F"]), lo = ci[1], hi = ci[2])
})
cal_cover <- mean(cal["lo", ] <= TRUE_TOTAL & cal["hi", ] >= TRUE_TOTAL)
cat(sprintf("
calibration at n = %d over %d simulated datasets:
", NCAL, NREP))
cat(sprintf("  mean estimate %+.4f (true %+.4f)  ->  bias %+.4f
",
            mean(cal["est", ]), TRUE_TOTAL, mean(cal["est", ]) - TRUE_TOTAL))
cat(sprintf("  %.0f%% of 89%% intervals contain the truth (89%% expected)
", 100 * cal_cover))

# ---- Figure ------------------------------------------------------------------
INK <- "#17211F"; ACC <- "#A8501F"; STR <- "#2C6455"
png(file.path(OUT, "06_recovery.png"), width = 1500, height = 430, res = 118)
op <- par(mfrow = c(1, 4), mar = c(4.6, 4.6, 3.2, 1), col.axis = INK, col.lab = INK,
          fg = "#9AA6A0", font.main = 1, cex.main = 1.15)

# 1: posterior at n = 116 against the truth
plot(density(e), main = "estimand at n = 116", xlab = "total effect (sd of W per sd of F)",
     lwd = 2.5, col = STR)
abline(v = TRUE_TOTAL, lwd = 2.5, col = ACC)
legend("topright", c("posterior", "true value"), col = c(STR, ACC), lwd = 2.5,
       bty = "n", cex = 0.85)

# 2: convergence
m  <- sapply(sweep, function(s) mean(s$est))
lo <- sapply(sweep, function(s) q(s$est, .055))
hi <- sapply(sweep, function(s) q(s$est, .945))
plot(seq_along(ns), m, ylim = range(c(lo, hi)), xaxt = "n", pch = 16, col = STR, cex = 1.3,
     xlab = "sample size", ylab = "total effect", main = "convergence on the truth")
axis(1, at = seq_along(ns), labels = format(ns, scientific = FALSE, big.mark = ","),
     cex.axis = 0.78)
arrows(seq_along(ns), lo, seq_along(ns), hi, angle = 90, code = 3, length = 0.05,
       col = STR, lwd = 2)
abline(h = TRUE_TOTAL, lwd = 2, col = ACC)

# 3: priors losing their grip
plot(seq_along(ns), sapply(sweep, function(s) sd(s$est)), type = "b", pch = 16,
     col = STR, lwd = 2, cex = 1.2, xaxt = "n", log = "y",
     xlab = "sample size", ylab = "posterior sd (log scale)",
     main = "priors losing their grip")
axis(1, at = seq_along(ns), labels = format(ns, scientific = FALSE, big.mark = ","),
     cex.axis = 0.78)
abline(h = PRIOR_SD, lty = 2, col = ACC, lwd = 2)
text(1, PRIOR_SD, "prior sd = 0.5", pos = 3, cex = 0.8, col = ACC)

# 4: calibration — where our sweep's estimate sits among 100 replicates
plot(density(cal["est", ]), main = "n = 50 000 across 100 datasets",
     xlab = "total effect", lwd = 2.5, col = STR)
abline(v = TRUE_TOTAL, lwd = 2.5, col = ACC)
ours <- mean(sweep[[which(ns == NCAL)]]$est)
abline(v = ours, lwd = 2, lty = 2, col = INK)
legend("topleft", c("true value", "our dataset", sprintf("%.0f%% coverage", 100 * cal_cover)),
       col = c(ACC, INK, NA), lwd = c(2.5, 2, NA), lty = c(1, 2, NA), bty = "n", cex = 0.8)
par(op); invisible(dev.off())
cat("\nwrote", file.path(OUT, "06_recovery.png"), "\n")
