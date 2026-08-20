# A07 step 05 — Prior check (direct effect)
#
# Same priors as ../../total-effect/, checked again because this model carries a second
# slope: two normal(0, 0.5) coefficients add variance, so plausibility does not carry over
# automatically.
#
# NOTE: rethinking must not be attached. See docs/adr/0001-brms-over-rethinking.md.

library(brms)
library(here)

options(brms.backend = "cmdstanr", mc.cores = parallel::detectCores())
MD  <- here(".stan_models")
TE  <- here("homework", "contributions", "A07", "total-effect", "data")
OUT  <- here("homework", "contributions", "A07", "direct-effect", "outputs")   # figures, reports
DATA <- here("homework", "contributions", "A07", "direct-effect", "data")      # rds, fits

spec   <- readRDS(file.path(DATA, "04_model_spec.rds"))
frame  <- readRDS(file.path(TE, "03_analysis_frame.rds"))
priors <- readRDS(file.path(TE, "05_priors.rds"))
dat    <- spec$dat
W_MEAN <- frame$scales$W["mean"]; W_SD <- frame$scales$W["sd"]
to_kg  <- function(z) z * W_SD + W_MEAN
LO <- 0; HI <- 15

fit_weak <- brm(spec$formula, data = dat, family = spec$family, prior = priors,
                sample_prior = "only", chains = 4, iter = 2000, refresh = 0,
                silent = 2, seed = 5, stan_model_args = list(dir = MD),
                file = file.path(DATA, "05_prior_weak"))

yrep <- to_kg(posterior_predict(fit_weak, draws = 2000))
dr   <- as_draws_df(fit_weak)

cat("\n================ STEP 05 (direct) ================\n")
cat(sprintf("implied weight: median %.1f kg, 95%% [%.0f, %.0f]\n",
            median(yrep), quantile(yrep, .025), quantile(yrep, .975)))
cat(sprintf("outside 0-15 kg: %.1f%%  (total-effect model: 1.6%%)\n",
            100 * mean(yrep < LO | yrep > HI)))
cat(sprintf("implied slope on food:       95%% [%+.1f, %+.1f] kg per sd\n",
            quantile(dr$b_Fs * W_SD, .025), quantile(dr$b_Fs * W_SD, .975)))
cat(sprintf("implied slope on group size: 95%% [%+.1f, %+.1f] kg per sd\n",
            quantile(dr$b_Gs * W_SD, .025), quantile(dr$b_Gs * W_SD, .975)))

INK <- "#17211F"; STR <- "#2C6455"
png(file.path(OUT, "05_prior_check.png"), width = 1250, height = 400, res = 118)
op <- par(mfrow = c(1, 3), mar = c(4.4, 4.4, 3.2, 1), col.axis = INK, col.lab = INK,
          fg = "#9AA6A0", font.main = 1, cex.main = 1.15)
y <- to_kg(as.numeric(posterior_predict(fit_weak, draws = 800)))
plot(density(y), main = "prior predictive weight", xlab = "kg", lwd = 2.5, col = STR,
     xlim = c(-20, 30))
abline(v = c(LO, HI), lty = 2, col = "#B0BAB4")
legend("topleft", sprintf("%.0f%% outside 0-15 kg", 100 * mean(y < LO | y > HI)),
       bty = "n", cex = 0.95, text.col = STR, text.font = 2)
plot(density(dr$b_Fs * W_SD), main = "prior on food slope", xlab = "kg per sd",
     lwd = 2.5, col = STR); abline(v = 0, lty = 3, col = "#B0BAB4")
plot(density(dr$b_Gs * W_SD), main = "prior on group-size slope", xlab = "kg per sd",
     lwd = 2.5, col = STR); abline(v = 0, lty = 3, col = "#B0BAB4")
par(op); invisible(dev.off())
cat("\nwrote", file.path(OUT, "05_prior_check.png"), "\n")
