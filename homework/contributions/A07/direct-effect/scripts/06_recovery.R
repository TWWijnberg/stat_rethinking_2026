# A07 step 06 — Recovery (direct effect)
#
# The harder case: Fs and Gs are correlated at ~0.90, so the model is asked to separate
# two things the data barely separates. Steps 02, 03 and 05 inherited.
#
# NOTE: rethinking must not be attached. See docs/adr/0001-brms-over-rethinking.md.

library(brms)
library(here)

options(brms.backend = "cmdstanr", mc.cores = parallel::detectCores())
source(here("homework", "contributions", "A07", "total-effect",
            "scripts", "02_generative_model.R"))

MD  <- here(".stan_models")
TE  <- here("homework", "contributions", "A07", "total-effect", "data")
OUT  <- here("homework", "contributions", "A07", "direct-effect", "outputs")   # figures, reports
DATA <- here("homework", "contributions", "A07", "direct-effect", "data")      # rds, fits

priors <- readRDS(file.path(TE, "05_priors.rds"))
W_SD   <- readRDS(file.path(TE, "03_analysis_frame.rds"))$scales$W["sd"]
TRUE_DIRECT <- unname(truth["direct"])            # +0.600
TRUE_BGW    <- true_pars$bGW                      # -0.700

# The estimand: raise F by 1 sd, hold G where it is. Computed through the same prediction
# path step 09 uses.
estimand_draws <- function(fit, d) {
  rowMeans(posterior_epred(fit, newdata = transform(d, Fs = Fs + 1)) -
           posterior_epred(fit, newdata = d))
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
  fit <- brm(bf(Ws ~ Fs + Gs), data = d, family = gaussian(), prior = priors,
             chains = ch, iter = it, algorithm = algorithm, refresh = 0,
             silent = 2, seed = 6, stan_model_args = list(dir = MD),
             file = file.path(DATA, paste0("06_recovery_n", format(n, scientific = FALSE), "_", algorithm)))
  list(fit = fit, d = d, est = estimand_draws(fit, d), cor = cor(d$Fs, d$Gs))
}

q <- function(v, p) unname(quantile(v, p))
within <- function(x, lo, hi) if (x >= lo && x <= hi) "yes" else "NO"

cat("\n\n================ STEP 06 (direct) ================\n")
r116 <- fit_at(116)
dr <- as_draws_df(r116$fit)
cat(sprintf("cor(Fs, Gs) in the simulated data: %.3f  (real data: 0.901)\n", r116$cor))

tab <- data.frame(
  parameter = c("b_Fs (= direct effect)", "b_Gs", "sigma"),
  true      = sprintf("%+.3f", c(TRUE_DIRECT, TRUE_BGW, resid_sds$sW)),
  posterior = sprintf("%+.3f", c(mean(dr$b_Fs), mean(dr$b_Gs), mean(dr$sigma))),
  ci89 = c(sprintf("[%+.3f, %+.3f]", q(dr$b_Fs, .055), q(dr$b_Fs, .945)),
           sprintf("[%+.3f, %+.3f]", q(dr$b_Gs, .055), q(dr$b_Gs, .945)),
           sprintf("[%+.3f, %+.3f]", q(dr$sigma, .055), q(dr$sigma, .945))),
  recovered = c(within(TRUE_DIRECT, q(dr$b_Fs, .055), q(dr$b_Fs, .945)),
                within(TRUE_BGW,    q(dr$b_Gs, .055), q(dr$b_Gs, .945)),
                within(resid_sds$sW, q(dr$sigma, .055), q(dr$sigma, .945))))
cat("\nparameters, n = 116:\n"); print(tab, row.names = FALSE)
cat(sprintf("\nposterior cor(b_Fs, b_Gs): %+.3f  <- the collinearity, showing up in the posterior\n",
            cor(dr$b_Fs, dr$b_Gs)))

e <- r116$est
cat(sprintf("\nESTIMAND (direct effect), n = 116:\n  true %+.4f | posterior %+.4f | 89%% [%+.4f, %+.4f] | recovered: %s\n",
            TRUE_DIRECT, mean(e), q(e, .055), q(e, .945),
            within(TRUE_DIRECT, q(e, .055), q(e, .945))))
cat(sprintf("  in kg per sd of food: true %+.3f | posterior %+.3f\n",
            TRUE_DIRECT * W_SD, mean(e) * W_SD))

ns <- c(116, 500, 2000, 10000, 50000, 100000)
sweep <- lapply(ns, fit_at, algorithm = "laplace")
sw <- do.call(rbind, lapply(seq_along(ns), function(i) {
  e <- sweep[[i]]$est
  data.frame(n = ns[i], estimate = sprintf("%+.4f", mean(e)),
             ci89 = sprintf("[%+.4f, %+.4f]", q(e, .055), q(e, .945)),
             post_sd = sprintf("%.4f", sd(e)),
             err = sprintf("%+.4f", mean(e) - TRUE_DIRECT),
             covers = within(TRUE_DIRECT, q(e, .055), q(e, .945)))
}))
cat("\nsample-size sweep — true direct effect", sprintf("%+.4f", TRUE_DIRECT), "\n\n")
print(sw, row.names = FALSE)

# ---- Is a missed interval at large n a problem? ------------------------------
# As n grows the interval shrinks, so ordinary sampling noise can push it off the truth
# even though the estimator is unbiased. An 89% interval is SUPPOSED to miss 11% of the
# time, at every n. Repeating over many simulated datasets shows whether the misses are at
# that rate. lm is used here: at n = 50 000 the priors contribute nothing, so it matches
# brms and lets the check run 100 times cheaply.

NREP <- 100; NCAL <- 50000
cal <- replicate(NREP, {
  s <- simulate_foxes(NCAL); m <- lm(W ~ F + G, s)
  ci <- confint(m, "F", level = 0.89)
  c(est = unname(coef(m)["F"]), lo = ci[1], hi = ci[2])
})
cal_cover <- mean(cal["lo", ] <= TRUE_DIRECT & cal["hi", ] >= TRUE_DIRECT)
cat(sprintf("
calibration at n = %d over %d simulated datasets:
", NCAL, NREP))
cat(sprintf("  mean estimate %+.4f (true %+.4f)  ->  bias %+.4f
",
            mean(cal["est", ]), TRUE_DIRECT, mean(cal["est", ]) - TRUE_DIRECT))
cat(sprintf("  %.0f%% of 89%% intervals contain the truth (89%% expected)
", 100 * cal_cover))

INK <- "#17211F"; ACC <- "#A8501F"; STR <- "#2C6455"
png(file.path(OUT, "06_recovery.png"), width = 1500, height = 430, res = 118)
op <- par(mfrow = c(1, 4), mar = c(4.6, 4.6, 3.2, 1), col.axis = INK, col.lab = INK,
          fg = "#9AA6A0", font.main = 1, cex.main = 1.15)
plot(density(e), main = "estimand at n = 116", xlab = "direct effect (sd per sd)",
     lwd = 2.5, col = STR)
abline(v = TRUE_DIRECT, lwd = 2.5, col = ACC)
legend("topleft", c("posterior", "true"), col = c(STR, ACC), lwd = 2.5, bty = "n", cex = 0.85)

m <- sapply(sweep, function(s) mean(s$est))
lo <- sapply(sweep, function(s) q(s$est, .055)); hi <- sapply(sweep, function(s) q(s$est, .945))
plot(seq_along(ns), m, ylim = range(c(lo, hi)), xaxt = "n", pch = 16, col = STR, cex = 1.3,
     xlab = "sample size", ylab = "direct effect", main = "convergence on the truth")
axis(1, at = seq_along(ns), labels = format(ns, scientific = FALSE, big.mark = ","),
     cex.axis = 0.78)
arrows(seq_along(ns), lo, seq_along(ns), hi, angle = 90, code = 3, length = 0.05,
       col = STR, lwd = 2)
abline(h = TRUE_DIRECT, lwd = 2, col = ACC)

plot(dr$b_Fs, dr$b_Gs, pch = 16, col = "#2C645518", xlab = "b_Fs", ylab = "b_Gs",
     main = "collinearity in the posterior")
points(TRUE_DIRECT, TRUE_BGW, pch = 4, lwd = 3, col = ACC, cex = 1.6)
legend("topright", sprintf("r = %+.2f", cor(dr$b_Fs, dr$b_Gs)), bty = "n",
       cex = 0.9, text.col = STR, text.font = 2)

# 4: calibration — where our sweep's estimate sits among 100 replicates
plot(density(cal["est", ]), main = "n = 50 000 across 100 datasets",
     xlab = "direct effect", lwd = 2.5, col = STR)
abline(v = TRUE_DIRECT, lwd = 2.5, col = ACC)
ours <- mean(sweep[[which(ns == NCAL)]]$est)
abline(v = ours, lwd = 2, lty = 2, col = INK)
legend("topleft", c("true value", "our dataset", sprintf("%.0f%% coverage", 100 * cal_cover)),
       col = c(ACC, INK, NA), lwd = c(2.5, 2, NA), lty = c(1, 2, NA), bty = "n", cex = 0.8)
par(op); invisible(dev.off())
cat("\nwrote", file.path(OUT, "06_recovery.png"), "\n")
