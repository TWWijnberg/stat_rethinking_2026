# A07 step 09 — Estimate
#
# Turn posterior draws into the quantity step 01 asked for. Everything is computed ON the
# draws, so every number carries its own uncertainty: a point estimate assembled from
# posterior means and given an interval afterwards is a different quantity.
#
# Covers BOTH analyses, because the interesting result is the contrast between them and
# they are reported side by side, never ranked.
#
# NOTE: rethinking must not be attached. See docs/adr/0001-brms-over-rethinking.md.

library(brms)
library(here)

options(brms.backend = "cmdstanr", mc.cores = parallel::detectCores())
TE   <- here("homework", "contributions", "A07", "total-effect", "data")
DE   <- here("homework", "contributions", "A07", "direct-effect", "data")
OUT  <- here("homework", "contributions", "A07", "total-effect", "outputs")

frame  <- readRDS(file.path(TE, "03_analysis_frame.rds"))
dat    <- frame$dat
W_SD   <- unname(frame$scales$W["sd"]); F_SD <- unname(frame$scales$F["sd"])
fit_t  <- readRDS(file.path(TE, "07_fit.rds"))
fit_d  <- readRDS(file.path(DE, "07_fit.rds"))

# ---- The estimand as a function of the draws --------------------------------
# Causal, so: predict with the exposure set to each value on the SAME draws, difference
# them, and average over the 116 observed foxes. The adjustment set is held at observed
# values — empty for the total effect, {Gs} for the direct effect, which is exactly what
# makes one "group size free to respond" and the other "group size held fixed".

contrast_draws <- function(fit, d, shift = 1) {
  rowMeans(posterior_epred(fit, newdata = transform(d, Fs = Fs + shift)) -
           posterior_epred(fit, newdata = d))
}

total  <- contrast_draws(fit_t, dat)
direct <- contrast_draws(fit_d, dat)

# Derived quantity the homework's explanation needs: the indirect path, F -> G -> W.
# total = direct + indirect, so the indirect effect is their difference — computed on
# draws rather than by subtracting two summaries.
d_dr   <- as_draws_df(fit_d)
slope_FG <- unname(coef(lm(Gs ~ Fs, dat))["Fs"])
indirect <- slope_FG * d_dr$b_Gs
grpsize  <- d_dr$b_Gs

# ---- Report ------------------------------------------------------------------
# In kg, the unit step 01 named. The conversion is linear, so it can be applied to draws
# or to summaries identically; applied to draws here so the pattern is right if a later
# analysis needs a non-linear one.

q <- function(v, p) unname(quantile(v, p))
row <- function(v, label, unit = "kg per 1 sd of avgfood") {
  k <- v * W_SD
  data.frame(quantity = label,
             sd_scale = sprintf("%+.3f", mean(v)),
             kg = sprintf("%+.3f", mean(k)),
             ci89_kg = sprintf("[%+.3f, %+.3f]", q(k, .055), q(k, .945)),
             p_positive = sprintf("%.0f%%", 100 * mean(v > 0)))
}

cat("\n\n================ STEP 09 ================\n")
cat("estimand units: kg of fox weight per 1 sd of avgfood (1 sd =",
    round(F_SD, 3), "index units)\n\n")
res <- rbind(row(total,    "TOTAL effect of food (group size free)"),
             row(direct,   "DIRECT effect of food (group size held)"),
             row(indirect, "  indirect path F -> G -> W"),
             row(grpsize,  "effect of group size (per 1 sd of G)"))
print(res, row.names = FALSE)

cat(sprintf("\ndecomposition check on draws: direct + indirect = %+.4f, total = %+.4f\n",
            mean(direct + indirect), mean(total)))

# A quantity the homework's question invites: how much food to move a fox by 1 kg?
cat(sprintf("\nfood needed for +1 kg per fox, if group size were held: %.1f sd of avgfood\n",
            1 / (mean(direct) * W_SD)))

# ---- Branches ----------------------------------------------------------------
# Sensitivity: step 05 M6 recorded NO empirical priors after the intercept was revised
# from normal(0, 0.2) to normal(0, 1), so there is nothing whose influence needs probing.
# Run anyway on the one prior that was ever in question, because it is cheap and the
# earlier version is what a reader following McElreath's convention would have used.

pr_tight <- c(set_prior("normal(0, 0.2)", class = "Intercept"),
              set_prior("normal(0, 0.5)", class = "b"),
              set_prior("exponential(1)", class = "sigma"))
sens <- brm(bf(Ws ~ Fs), data = dat, family = gaussian(), prior = pr_tight,
            chains = 4, iter = 2000, seed = 7, refresh = 0, silent = 2,
            stan_model_args = list(dir = here(".stan_models")),
            file = file.path(TE, "09_sensitivity_tight_intercept"))
s <- contrast_draws(sens, dat) * W_SD
cat(sprintf("\nSENSITIVITY — intercept prior normal(0,1) vs normal(0,0.2):\n"))
cat(sprintf("  total effect %+.4f kg -> %+.4f kg   (shift %.4f kg, %.0f%% of one posterior sd)\n",
            mean(total * W_SD), mean(s), abs(mean(s) - mean(total * W_SD)),
            100 * abs(mean(s) - mean(total * W_SD)) / sd(total * W_SD)))

saveRDS(list(total = total, direct = direct, indirect = indirect, grpsize = grpsize,
             W_SD = W_SD, F_SD = F_SD), file.path(TE, "09_estimand_draws.rds"))

# ---- Figure ------------------------------------------------------------------
INK <- "#17211F"; ACC <- "#A8501F"; STR <- "#2C6455"; PUR <- "#5B4B8A"
png(file.path(OUT, "09_estimate.png"), width = 1250, height = 430, res = 118)
op <- par(mfrow = c(1, 3), mar = c(4.6, 4.4, 3.2, 1), col.axis = INK, col.lab = INK,
          fg = "#9AA6A0", font.main = 1, cex.main = 1.15)

dk <- lapply(list(direct, indirect, total), function(v) density(v * W_SD))
plot(NA, xlim = c(-1.1, 1.1), ylim = c(0, max(sapply(dk, function(d) max(d$y)))),
     xlab = "kg per 1 sd of avgfood", ylab = "Density", main = "the two paths, and their sum")
polygon(dk[[1]], col = "#A8501F25", border = ACC, lwd = 2.5)
polygon(dk[[2]], col = "#5B4B8A25", border = PUR, lwd = 2.5)
polygon(dk[[3]], col = "#2C645540", border = STR, lwd = 3)
abline(v = 0, lty = 3, col = "#7A857F")
legend("topleft", c("direct  F -> W", "indirect  F -> G -> W", "total"),
       col = c(ACC, PUR, STR), lwd = c(2.5, 2.5, 3), bty = "n", cex = 0.8)

plot(density(total * W_SD), lwd = 3, col = STR, main = "the homework's answer",
     xlab = "kg per 1 sd of avgfood")
polygon(density(total * W_SD), col = "#2C645530", border = NA)
abline(v = 0, lty = 3, col = "#7A857F")
abline(v = q(total * W_SD, c(.055, .945)), lty = 2, col = STR)
mtext(sprintf("%+.3f kg  89%% [%+.3f, %+.3f]", mean(total) * W_SD,
              q(total * W_SD, .055), q(total * W_SD, .945)),
      side = 3, line = -1.4, cex = 0.72, col = INK)

fg <- seq(-2, 2, length.out = 40)
ep_t <- posterior_epred(fit_t, newdata = data.frame(Fs = fg))
ep_d <- posterior_epred(fit_d, newdata = data.frame(Fs = fg, Gs = 0))
kg <- function(m) m * W_SD + frame$scales$W["mean"]
plot(NA, xlim = range(fg), ylim = c(3.6, 5.6), xlab = "avgfood (sd)",
     ylab = "expected weight (kg)", main = "what a fox weighs")
polygon(c(fg, rev(fg)), c(kg(apply(ep_d, 2, quantile, .055)),
                          rev(kg(apply(ep_d, 2, quantile, .945)))),
        col = "#A8501F22", border = NA)
polygon(c(fg, rev(fg)), c(kg(apply(ep_t, 2, quantile, .055)),
                          rev(kg(apply(ep_t, 2, quantile, .945)))),
        col = "#2C645522", border = NA)
lines(fg, kg(colMeans(ep_d)), lwd = 3, col = ACC)
lines(fg, kg(colMeans(ep_t)), lwd = 3, col = STR)
legend("topleft", c("group size held fixed", "group size free to respond"),
       col = c(ACC, STR), lwd = 3, bty = "n", cex = 0.8)
par(op); invisible(dev.off())
cat("\nwrote", file.path(OUT, "09_estimate.png"), "\n")
