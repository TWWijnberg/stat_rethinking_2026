# A07 step 08 — Posterior check (total-effect)
#
# Does data simulated from the fitted model look like the data we have? Checked on the
# features the ESTIMAND depends on, not the marginal alone: a model can match the overall
# shape of weight beautifully and still be wrong about weight-against-food, which is the
# only comparison the estimand asks for.
#
# NOTE: rethinking must not be attached. See docs/adr/0001-brms-over-rethinking.md.

library(brms)
library(bayesplot)
library(here)

options(brms.backend = "cmdstanr", mc.cores = parallel::detectCores())
TE   <- here("homework", "contributions", "A07", "total-effect", "data")
OUT  <- here("homework", "contributions", "A07", "total-effect", "outputs")
DATA <- here("homework", "contributions", "A07", "total-effect", "data")

fit    <- readRDS(file.path(DATA, "07_fit.rds"))
frame  <- readRDS(file.path(TE, "03_analysis_frame.rds"))
dat    <- frame$dat
W_MEAN <- frame$scales$W["mean"]; W_SD <- frame$scales$W["sd"]
to_kg  <- function(z) z * W_SD + W_MEAN

NDRAWS <- 500
yrep <- posterior_predict(fit, ndraws = NDRAWS)
y    <- dat$Ws

cat("\n\n================ STEP 08 — total-effect ================\n")

# ---- 1. Overall shape and tails ---------------------------------------------
# The likelihood is gaussian; if weight were skewed or heavy-tailed this is where it shows.

pval <- function(f) {
  d <- apply(yrep, 1, f)
  min(mean(d >= f(y)), mean(d <= f(y))) * 2      # two-sided posterior predictive p
}
shape <- data.frame(
  statistic = c("mean", "sd", "min", "max", "skewness", "kurtosis"),
  observed  = round(c(mean(y), sd(y), min(y), max(y),
                      mean((y - mean(y))^3) / sd(y)^3,
                      mean((y - mean(y))^4) / sd(y)^4), 3),
  pp_p      = round(c(pval(mean), pval(sd), pval(min), pval(max),
                      pval(function(v) mean((v - mean(v))^3) / sd(v)^3),
                      pval(function(v) mean((v - mean(v))^4) / sd(v)^4)), 3))
shape$flag <- ifelse(shape$pp_p < 0.05, "MISFIT", "ok")
cat("\nposterior predictive p-values (low = the model cannot reproduce it):\n")
print(shape, row.names = FALSE)

# ---- 2. Where the estimand lives: weight against FOOD ------------------------
# The estimand is a contrast along Fs. If the model got this relationship wrong, every
# other check could pass and the estimate would still be meaningless.

fbin <- cut(dat$Fs, quantile(dat$Fs, seq(0, 1, 0.25)), include.lowest = TRUE,
            labels = c("Q1 low food", "Q2", "Q3", "Q4 high food"))
bin_tab <- do.call(rbind, lapply(levels(fbin), function(b) {
  i <- fbin == b
  rep_means <- rowMeans(yrep[, i, drop = FALSE])
  data.frame(bin = b, n = sum(i),
             observed = sprintf("%.2f kg", to_kg(mean(y[i]))),
             predicted = sprintf("%.2f kg", to_kg(mean(rep_means))),
             pp89 = sprintf("[%.2f, %.2f]", to_kg(quantile(rep_means, .055)),
                            to_kg(quantile(rep_means, .945))),
             inside = ifelse(mean(y[i]) >= quantile(rep_means, .055) &
                             mean(y[i]) <= quantile(rep_means, .945), "yes", "NO"))
}))
cat("\nmean weight by food quartile — where the estimand lives:\n")
print(bin_tab, row.names = FALSE)

# ---- 3. Group structure, flagged as a discrepancy at step 03 -----------------
# Step 03 accepted that the simulation ignores the 30 territories. If that mattered, the
# model would misfit systematically within groups.

grp_obs <- tapply(y, dat$group, mean)
grp_rep <- sapply(sort(unique(dat$group)), function(g)
  rowMeans(yrep[, dat$group == g, drop = FALSE]))
inside <- mean(grp_obs >= apply(grp_rep, 2, quantile, .055) &
               grp_obs <= apply(grp_rep, 2, quantile, .945))
cat(sprintf("\ngroup means inside their 89%% predictive interval: %.0f%% of 30 territories (89%% expected)\n",
            100 * inside))

# ---- Figure ------------------------------------------------------------------
INK <- "#17211F"; ACC <- "#A8501F"; STR <- "#2C6455"
png(file.path(OUT, "08_posterior_check.png"), width = 1400, height = 400, res = 118)
op <- par(mfrow = c(1, 4), mar = c(4.6, 4.4, 3.2, 1), col.axis = INK, col.lab = INK,
          fg = "#9AA6A0", font.main = 1, cex.main = 1.1)

# 1: density overlay
plot(density(to_kg(yrep[1, ])), col = "#2C645530", lwd = 1, main = "overall shape",
     xlab = "weight (kg)", ylim = c(0, 0.42), xlim = c(0, 9))
for (i in 2:120) lines(density(to_kg(yrep[i, ])), col = "#2C645520", lwd = 1)
lines(density(to_kg(y)), col = ACC, lwd = 3)
legend("topright", c("replicates", "observed"), col = c(STR, ACC), lwd = c(1, 3),
       bty = "n", cex = 0.8)

# 2: tails — on the kg scale, like every other panel
rep_max <- to_kg(apply(yrep, 1, max))
plot(density(rep_max), main = "heaviest fox in the sample", xlab = "weight (kg)",
     lwd = 2.5, col = STR, xlim = range(c(rep_max, to_kg(max(y)))))
abline(v = to_kg(max(y)), lwd = 2.5, col = ACC)
legend("topright", c("replicates", "observed"), col = c(STR, ACC), lwd = 2.5,
       bty = "n", cex = 0.8)

# 3: the estimand's own relationship — fitted mean with an 89% band
fgrid <- seq(min(dat$Fs), max(dat$Fs), length.out = 60)
ep <- posterior_epred(fit, newdata = data.frame(Fs = fgrid))
lo <- to_kg(apply(ep, 2, quantile, .055)); hi <- to_kg(apply(ep, 2, quantile, .945))
plot(dat$Fs, to_kg(y), type = "n", xlab = "avgfood (sd)", ylab = "weight (kg)",
     main = "weight vs food", ylim = range(to_kg(y)))
polygon(c(fgrid, rev(fgrid)), c(lo, rev(hi)), col = "#2C645530", border = NA)
points(dat$Fs, to_kg(y), pch = 16, col = "#A8501FAA")
lines(fgrid, to_kg(colMeans(ep)), lwd = 3, col = STR)
legend("topright", c("observed", "fitted mean, 89%"), col = c(ACC, STR),
       pch = c(16, NA), lwd = c(NA, 3), bty = "n", cex = 0.8)

# 4: group means
plot(seq_along(grp_obs), to_kg(grp_obs), pch = 16, col = ACC, cex = 1.1,
     xlab = "territory", ylab = "mean weight (kg)", main = "by territory",
     ylim = to_kg(range(c(apply(grp_rep, 2, quantile, .055),
                          apply(grp_rep, 2, quantile, .945)))))
arrows(seq_along(grp_obs), to_kg(apply(grp_rep, 2, quantile, .055)),
       seq_along(grp_obs), to_kg(apply(grp_rep, 2, quantile, .945)),
       angle = 90, code = 3, length = 0.02, col = "#2C645580", lwd = 1.5)
points(seq_along(grp_obs), to_kg(grp_obs), pch = 16, col = ACC, cex = 1.1)
par(op); invisible(dev.off())
cat("\nwrote", file.path(OUT, "08_posterior_check.png"), "\n")
