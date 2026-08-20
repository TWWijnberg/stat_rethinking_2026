# A07 step 05 — Prior check
#
# Give every parameter a prior, then look at the data those priors imply — on the scale
# the outcome is measured in (kg), read against what a fox can physically weigh.
#
# NOTE: rethinking must not be attached. See docs/adr/0001-brms-over-rethinking.md.

library(brms)
library(here)

options(brms.backend = "cmdstanr", mc.cores = parallel::detectCores())
MD  <- here(".stan_models")
OUT  <- here("homework", "contributions", "A07", "total-effect", "outputs")   # figures, reports
DATA <- here("homework", "contributions", "A07", "total-effect", "data")      # rds, fits

spec   <- readRDS(file.path(DATA, "04_model_spec.rds"))
frame  <- readRDS(file.path(DATA, "03_analysis_frame.rds"))
dat    <- spec$dat
W_MEAN <- frame$scales$W["mean"]; W_SD <- frame$scales$W["sd"]

to_kg <- function(z) z * W_SD + W_MEAN

# Physical bounds for an adult red fox in Britain. Nothing below zero exists; the heaviest
# credible animal is well under 15 kg. These are the bounds the check is read against —
# NOT the observed range of this dataset.
LO <- 0; HI <- 15


# ---- Candidate 1: vague but proper -------------------------------------------
# brms's own default on the slope is FLAT, and brms refuses to sample from it:
#   "Sampling from priors is not possible as some parameters have no proper priors."
# That is the CONTEXT.md rule enforced by the tool. So the naive candidate here is the
# nearest proper thing — a very wide normal on the slope, with brms's defaults left in
# place for the intercept and sigma. Fitted only to see what it implies, so the revision
# has evidence behind it.

priors_vague <- c(set_prior("normal(0, 10)", class = "b"),
                  set_prior("student_t(3, -0.1, 2.5)", class = "Intercept"),
                  set_prior("student_t(3, 0, 2.5)", class = "sigma"))

fit_default <- brm(spec$formula, data = dat, family = spec$family, prior = priors_vague,
                   sample_prior = "only", chains = 4, iter = 2000, refresh = 0,
                   silent = 2, seed = 5, stan_model_args = list(dir = MD),
                   file = file.path(DATA, "05_prior_vague"))


# ---- Candidate 2: weakly informative -----------------------------------------
#
#   Intercept ~ normal(0, 1)     WEAKLY INFORMATIVE. In kg this says the average fox
#                                weighs somewhere in [2.2, 6.9] — defensible from fox
#                                biology alone. The centring at 0 comes from standardising
#                                (declared at step 01 M2); the width asserts nothing
#                                further. REVISED from normal(0, 0.2), which claimed the
#                                mean was known to within half a kilo and was justified
#                                only by the standardisation itself.
#   b         ~ normal(0, 0.5)   WEAKLY INFORMATIVE. On the standardised scale b is
#                                sd-of-weight per sd-of-predictor, so |b| > 1 asserts that
#                                one predictor moves weight further than weight varies.
#                                sd 0.5 puts 95% of the mass inside +/-1 without ruling
#                                out a strong effect.
#   sigma     ~ exponential(1)   WEAKLY INFORMATIVE. The outcome has sd 1 by construction,
#                                so residual sd above ~1 would mean the predictors make
#                                the fit worse than the mean.

priors <- c(set_prior("normal(0, 1)",    class = "Intercept"),
            set_prior("normal(0, 0.5)",  class = "b"),
            set_prior("exponential(1)",  class = "sigma"))

fit_weak <- brm(spec$formula, data = dat, family = spec$family, prior = priors,
                sample_prior = "only", chains = 4, iter = 2000, refresh = 0,
                silent = 2, seed = 5, stan_model_args = list(dir = MD),
                file = file.path(DATA, "05_prior_weak"))


# ---- Read the implied datasets against physical plausibility -----------------

report <- function(fit, label) {
  yrep <- to_kg(posterior_predict(fit, draws = 2000))
  b    <- as_draws_df(fit)$b_Fs * W_SD          # kg per 1 sd of avgfood
  c(label = label,
    below_0  = sprintf("%.1f%%", 100 * mean(yrep < LO)),
    above_15 = sprintf("%.1f%%", 100 * mean(yrep > HI)),
    med_wt   = sprintf("%.1f kg", median(yrep)),
    wt_95    = sprintf("[%.0f, %.0f]", quantile(yrep, .025), quantile(yrep, .975)),
    slope_95 = sprintf("[%+.1f, %+.1f] kg", quantile(b, .025), quantile(b, .975)))
}

cat("\n================ STEP 05 ================\n")
cat("prior predictive weight, read against a fox being 0-15 kg:\n\n")
tab <- rbind(report(fit_default, "vague: b ~ normal(0,10)"),
             report(fit_weak,    "weakly informative"))
print(as.data.frame(tab), row.names = FALSE)


# ---- Coverage: do the priors rule the real data out? -------------------------
# NOT a match test. Tuning priors until the prior predictive resembles the data would use
# the data twice and turn every prior into an empirical one. This is the one-sided
# version: the observed data must lie inside what the priors consider possible. Priors
# that make the real data essentially impossible are prior-data conflict, and that is a
# reason to revise.

y_obs  <- to_kg(dat$Ws)
yrep_w <- to_kg(posterior_predict(fit_weak, draws = 4000))

stat_pct <- function(yrep, obs, f) {
  draws <- apply(yrep, 1, f)
  100 * mean(draws < f(obs))          # percentile of the observed value
}
stats <- list(mean = mean, sd = sd, min = min, max = max)

cat("
where each observed statistic falls in the prior predictive distribution
")
cat("(50% = dead centre; under 1% or over 99% would be prior-data conflict)

")
cov_tab <- data.frame(
  statistic = names(stats),
  observed  = sprintf("%.2f kg", sapply(stats, function(f) f(y_obs))),
  percentile = sprintf("%.0f%%", sapply(stats, function(f) stat_pct(yrep_w, y_obs, f))))
print(cov_tab, row.names = FALSE)


# ---- Figure ------------------------------------------------------------------

INK <- "#17211F"; ACC <- "#A8501F"; STR <- "#2C6455"
Fgrid <- seq(min(dat$Fs), max(dat$Fs), length.out = 50)

png(file.path(OUT, "05_prior_check.png"), width = 1250, height = 700, res = 118)
# coverage gets the full-height right-hand column: it is the summary check
layout(matrix(c(1, 2, 5,
                3, 4, 5), nrow = 2, byrow = TRUE), widths = c(1, 1, 1.15))
op <- par(mar = c(4.4, 4.4, 3.2, 1), col.axis = INK, col.lab = INK,
          fg = "#9AA6A0", font.main = 1, cex.main = 1.15)

dens_panel <- function(fit, main, col) {
  y <- to_kg(as.numeric(posterior_predict(fit, draws = 800)))
  plot(density(y), main = main, xlab = "implied fox weight (kg)", lwd = 2.5, col = col,
       xlim = range(c(-30, 40, quantile(y, c(.005, .995)))))
  abline(v = c(LO, HI), lty = 2, col = "#B0BAB4")
  text(LO, par("usr")[4] * 0.9, " 0 kg", pos = 4, cex = 0.8, col = "#7A857F")
  text(HI, par("usr")[4] * 0.9, " 15 kg", pos = 4, cex = 0.8, col = "#7A857F")
  legend("topleft", sprintf("%.0f%% outside", 100 * mean(y < LO | y > HI)),
         bty = "n", cex = 0.95, text.col = col, text.font = 2)
}

lines_panel <- function(fit, main, col) {
  d <- as_draws_df(fit)[1:60, ]
  plot(NA, xlim = range(Fgrid), ylim = c(-15, 25), xlab = "avgfood (sd)",
       ylab = "weight (kg)", main = main)
  rect(par("usr")[1], LO, par("usr")[2], HI, col = "#2C645512", border = NA)
  for (i in seq_len(nrow(d)))
    lines(Fgrid, to_kg(d$b_Intercept[i] + d$b_Fs[i] * Fgrid), col = paste0(col, "55"), lwd = 1.4)
  abline(h = c(LO, HI), lty = 2, col = "#B0BAB4")
}

dens_panel(fit_default, "prior predictive — vague", ACC)          # 1
dens_panel(fit_weak,    "prior predictive — weakly informative", STR)  # 2
lines_panel(fit_default, "implied relationships — vague", ACC)     # 3
lines_panel(fit_weak,    "implied relationships — weakly informative", STR)  # 4

# 5 — coverage: does the prior predictive contain the real data?
par(mar = c(6.2, 4.4, 3.2, 1))   # extra bottom room for the percentile caption
yk <- to_kg(as.numeric(posterior_predict(fit_weak, draws = 800)))
plot(density(yk), main = "coverage of the real data", xlab = "weight (kg)",
     lwd = 2.5, col = STR, xlim = c(-6, 16))
polygon(density(y_obs), col = "#A8501F33", border = ACC, lwd = 2)
legend("topleft", c("prior predictive", "observed weight"), col = c(STR, ACC),
       lwd = 2, bty = "n", cex = 0.85)
mtext(sprintf("observed percentile — mean %.0f%%  sd %.0f%%  min %.0f%%  max %.0f%%",
              stat_pct(yrep_w, y_obs, mean), stat_pct(yrep_w, y_obs, sd),
              stat_pct(yrep_w, y_obs, min), stat_pct(yrep_w, y_obs, max)),
      side = 1, line = 4.6, cex = 0.72, col = "#7A857F")

par(op); invisible(dev.off())
cat("\nwrote", file.path(OUT, "05_prior_check.png"), "\n")

saveRDS(priors, file.path(DATA, "05_priors.rds"))
