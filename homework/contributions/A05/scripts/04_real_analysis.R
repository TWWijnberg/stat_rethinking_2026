# Homework A05 - Real Data Analysis
# Statistical Rethinking 2026
# Purpose: Estimate total causal effect of sex on weight in children

source(here::here("homework", "contributions", "A05", "scripts", "02_dag_analysis.R"))

# ============================================================================
# Setup output directory
# ============================================================================

output_dir <- here::here("homework", "contributions", "A05", "outputs", "figures")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ============================================================================
# STEP 1: Load and Prepare Data
# ============================================================================

data(Howell1)

# Filter to children (< 13 years)
d_real <- Howell1[Howell1$age < 13, ]

# Convert age from years to months (to match synthetic data scale)
d_real$age <- d_real$age * 12

# Prepare data
d_real <- prepare_data(d_real, source = "Howell1")

cat("=== Howell1 Data Summary (Children < 13 years) ===\n")
cat(sprintf("N = %d observations\n", nrow(d_real)))
cat(sprintf("Age range: %.0f - %.0f months (%.1f - %.1f years)\n",
            min(d_real$age), max(d_real$age),
            min(d_real$age)/12, max(d_real$age)/12))
cat(sprintf("Sex: %d Female, %d Male\n",
            sum(d_real$sex == "Female"), sum(d_real$sex == "Male")))

# Visualize real data
p1 <- ggplot(d_real, aes(x = age, y = weight, color = sex)) +
  geom_point(alpha = 0.6) +
  labs(title = "Howell1 Data: Age vs Weight (Children)") +
  theme_minimal()
ggsave(file.path(output_dir, "06_real_data.png"), p1, width = 8, height = 6)
cat("Saved: 06_real_data.png\n")

# Sex vs Weight distribution
p_sex_weight <- ggplot(d_real, aes(x = sex, y = weight, fill = sex)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.2, alpha = 0.8) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1) +
  labs(title = "Weight Distribution by Sex (Children < 13)",
       x = "Sex", y = "Weight (kg)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(file.path(output_dir, "06b_sex_weight_distribution.png"), p_sex_weight, width = 6, height = 6)
cat("Saved: 06b_sex_weight_distribution.png\n")

# Height vs Weight colored by sex
p_height_weight <- ggplot(d_real, aes(x = height, y = weight, color = sex)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Height vs Weight by Sex (Children < 13)",
       x = "Height (cm)", y = "Weight (kg)") +
  theme_minimal()
ggsave(file.path(output_dir, "06c_height_weight_by_sex.png"), p_height_weight, width = 8, height = 6)
cat("Saved: 06c_height_weight_by_sex.png\n")

# ============================================================================
# STEP 2: Fit Model
# ============================================================================

# Prepare data for ulam
# Note: sex is a factor with integer levels (1=Female, 2=Male)
d_fit <- list(
  weight = d_real$weight,
  height = d_real$height,
  height_bar = rep(mean(d_real$height), times = length(d_real$height)),
  age = d_real$age,
  age_bar = rep(mean(d_real$age), times = length(d_real$age)),
  sex = as.integer(d_real$sex)
)

sex_on_weight_direct <- alist(
  weight ~ dnorm(mu, sigma),
  mu <- a[sex] + b_height[sex] * (height - height_bar) + b_age[sex] * (age - age_bar),
  a[sex] ~ dnorm(0, 10),          # Sex-specific intercepts
  b_height[sex] ~ dnorm(0.3, 1),  # kg per cm of height
  b_age[sex] ~ dnorm(0, 0.5),     # kg per month of age
  sigma ~ dunif(0, 15)            # Residual SD
)

fit_direct_real <- ulam(sex_on_weight_direct,
  data = d_fit,
  chains = 4,
  cores = 4,
  iter = 2000,
  sample = TRUE
)


# ============================================================================
# STEP 3: MCMC Diagnostics
# ============================================================================

cat("\n=== MCMC Diagnostics ===\n")
print(precis(fit_direct_real, depth = 2))

png(file.path(output_dir, "07_real_traceplot.png"), width = 800, height = 600)
traceplot(fit_direct_real)
dev.off()
cat("Saved: 07_real_traceplot.png\n")

# ============================================================================
# STEP 4: Posterior Predictive Check
# ============================================================================

# Generate predictions for a range of heights by sex
post <- extract.samples(fit_direct_real)
height_seq <- seq(min(d_real$height), max(d_real$height), length.out = 50)
height_bar <- mean(d_real$height)

# Predictions for each sex
pred_female <- sapply(height_seq, function(h) {
  post$a[, 1] + post$b_height[, 1] * (h - height_bar)
})
pred_male <- sapply(height_seq, function(h) {
  post$a[, 2] + post$b_height[, 2] * (h - height_bar)
})

# Create prediction data frame
pred_df <- rbind(
  data.frame(
    height = height_seq,
    weight_mean = colMeans(pred_female),
    weight_lower = apply(pred_female, 2, quantile, 0.055),
    weight_upper = apply(pred_female, 2, quantile, 0.945),
    sex = "Female"
  ),
  data.frame(
    height = height_seq,
    weight_mean = colMeans(pred_male),
    weight_lower = apply(pred_male, 2, quantile, 0.055),
    weight_upper = apply(pred_male, 2, quantile, 0.945),
    sex = "Male"
  )
)

p2 <- ggplot() +
  geom_ribbon(data = pred_df, aes(x = height, ymin = weight_lower, ymax = weight_upper, fill = sex), alpha = 0.3) +
  geom_line(data = pred_df, aes(x = height, y = weight_mean, color = sex), linewidth = 1) +
  geom_point(data = d_real, aes(x = height, y = weight, color = sex), alpha = 0.5) +
  labs(title = "Posterior Predictive: Weight ~ Height by Sex (Howell1 Children)",
       x = "Height (cm)", y = "Weight (kg)") +
  theme_minimal()
ggsave(file.path(output_dir, "08_real_posterior_predictive.png"), p2, width = 8, height = 6)
cat("Saved: 08_real_posterior_predictive.png\n")

# ============================================================================
# STEP 5: Estimate Direct Causal Effect of Sex on Weight
# ============================================================================

# Direct effect = a[Male] - a[Female] (difference in intercepts at same height and age)
direct_effect <- post$a[, 2] - post$a[, 1]

# Difference in height slopes
slope_diff_height <- post$b_height[, 2] - post$b_height[, 1]

# Difference in age slopes
slope_diff_age <- post$b_age[, 2] - post$b_age[, 1]

cat("\n")
cat("============================================================\n")
cat("  CAUSAL INFERENCE: Direct Effect of Sex on Weight\n")
cat("  (Conditioning on Height and Age)\n")
cat("============================================================\n")
cat("\n")
cat("Question: How much heavier are male children than female\n")
cat("          children of the SAME HEIGHT and AGE?\n")
cat("\n")
cat("--- Direct Effect (intercept difference at mean height & age) ---\n")
cat(sprintf("Posterior mean: %.2f kg\n", mean(direct_effect)))
cat(sprintf("Posterior SD:   %.2f kg\n", sd(direct_effect)))
cat(sprintf("89%% CI:        [%.2f, %.2f] kg\n",
            quantile(direct_effect, 0.055),
            quantile(direct_effect, 0.945)))
cat("\n")
cat("--- Height Slope Difference (Male - Female) ---\n")
cat(sprintf("Posterior mean: %.3f kg/cm\n", mean(slope_diff_height)))
cat(sprintf("89%% CI:        [%.3f, %.3f] kg/cm\n",
            quantile(slope_diff_height, 0.055),
            quantile(slope_diff_height, 0.945)))
cat("\n")
cat("--- Age Slope Difference (Male - Female) ---\n")
cat(sprintf("Posterior mean: %.4f kg/month\n", mean(slope_diff_age)))
cat(sprintf("89%% CI:        [%.4f, %.4f] kg/month\n",
            quantile(slope_diff_age, 0.055),
            quantile(slope_diff_age, 0.945)))
cat("\n")
cat("Interpretation:\n")
cat(sprintf("  At the same height and age, male children weigh approximately %.1f kg\n",
            mean(direct_effect)))
cat("  more than female children. This is the DIRECT effect of sex,\n")
cat("  blocking the indirect path through height (S -> H -> W).\n")
cat("  Conditioning on age also closes the collider path S -> H <- A -> W.\n")
cat("\n")

# Visualize posterior
png(file.path(output_dir, "09_real_direct_effect.png"), width = 800, height = 600)
hist(direct_effect, breaks = 30, col = "skyblue", border = "white",
     main = "Posterior: Direct Effect of Sex on Weight (Howell1)",
     xlab = "Effect (kg, Male - Female at same height & age)")
abline(v = mean(direct_effect), col = "blue", lwd = 2)
abline(v = quantile(direct_effect, c(0.055, 0.945)), col = "blue", lwd = 1, lty = 2)
dev.off()
cat("Saved: 09_real_direct_effect.png\n")

# ============================================================================
# STEP 6: Estimate Total Effect Using Model + Observed Differences
# ============================================================================

# Get observed differences between sexes
mean_height_male <- mean(d_real$height[d_real$sex == "Male"])
mean_height_female <- mean(d_real$height[d_real$sex == "Female"])
height_diff <- mean_height_male - mean_height_female

# Age should be independent of sex, but check for any sample imbalance
mean_age_male <- mean(d_real$age[d_real$sex == "Male"])
mean_age_female <- mean(d_real$age[d_real$sex == "Female"])
age_diff <- mean_age_male - mean_age_female
age_bar <- mean(d_real$age)

cat("\n")
cat("============================================================\n")
cat("  DECOMPOSING: Total Effect of Sex on Weight\n")
cat("============================================================\n")
cat("\n")
cat("Observed differences (Male - Female):\n")
cat(sprintf("  Height: %.2f cm\n", height_diff))
cat(sprintf("  Age:    %.1f months (should be ~0 if sex independent of age)\n", age_diff))
cat("\n")

# Expected weight for average male vs average female
# Evaluate at sex-specific mean heights, but overall mean age
# (Age is not on causal path from sex, so we hold it constant)
# E[weight|male] = a[2] + b_height[2] * (h_male - h_bar) + b_age[2] * 0
# E[weight|female] = a[1] + b_height[1] * (h_female - h_bar) + b_age[1] * 0
expected_weight_male <- post$a[, 2] + post$b_height[, 2] * (mean_height_male - height_bar)
expected_weight_female <- post$a[, 1] + post$b_height[, 1] * (mean_height_female - height_bar)

total_effect <- expected_weight_male - expected_weight_female

# Indirect effect (via height) using sex-specific slopes
# This ensures: total = direct + indirect (exact decomposition)
# indirect = b_height[male] * (h_male - h_bar) - b_height[female] * (h_female - h_bar)
indirect_effect_height <- post$b_height[, 2] * (mean_height_male - height_bar) -
                          post$b_height[, 1] * (mean_height_female - height_bar)

cat("--- Total Effect (comparing average male to average female) ---\n")
cat(sprintf("Posterior mean: %.2f kg\n", mean(total_effect)))
cat(sprintf("Posterior SD:   %.2f kg\n", sd(total_effect)))
cat(sprintf("89%% CI:        [%.2f, %.2f] kg\n",
            quantile(total_effect, 0.055),
            quantile(total_effect, 0.945)))
cat("\n")

cat("--- Decomposition ---\n")
cat(sprintf("Direct effect (S -> W):           %.2f kg  [%.2f, %.2f]\n",
            mean(direct_effect),
            quantile(direct_effect, 0.055),
            quantile(direct_effect, 0.945)))
cat(sprintf("Indirect effect (S -> H -> W):    %.2f kg  [%.2f, %.2f]\n",
            mean(indirect_effect_height),
            quantile(indirect_effect_height, 0.055),
            quantile(indirect_effect_height, 0.945)))
cat(sprintf("Sum (Direct + Indirect):          %.2f kg\n",
            mean(direct_effect) + mean(indirect_effect_height)))
cat(sprintf("Total effect (from prediction):   %.2f kg\n", mean(total_effect)))
cat("\n")
cat("Note: Age held constant at overall mean (not on causal path S -> W).\n")
cat("      Conditioning on age closes collider path S -> H <- A -> W.\n")

# Visualize decomposition
png(file.path(output_dir, "10_effect_decomposition.png"), width = 800, height = 600)
par(mfrow = c(1, 3))
hist(direct_effect, breaks = 30, col = "coral", border = "white",
     main = "Direct Effect\n(S -> W)", xlab = "kg")
abline(v = mean(direct_effect), col = "darkred", lwd = 2)

hist(indirect_effect_height, breaks = 30, col = "steelblue", border = "white",
     main = "Indirect Effect\n(S -> H -> W)", xlab = "kg")
abline(v = mean(indirect_effect_height), col = "darkblue", lwd = 2)

hist(total_effect, breaks = 30, col = "purple", border = "white",
     main = "Total Effect", xlab = "kg")
abline(v = mean(total_effect), col = "purple4", lwd = 2)
par(mfrow = c(1, 1))
dev.off()
cat("Saved: 10_effect_decomposition.png\n")

# ============================================================================
# STEP 6b: Simple Model for Total Effect (for comparison)
# ============================================================================

# Fit simple model: weight ~ sex only (no mediators)
# This directly estimates total effect without blocking any paths
cat("\n")
cat("============================================================\n")
cat("  SIMPLE MODEL: Total Effect (weight ~ sex)\n")
cat("============================================================\n")
cat("\n")

sex_on_weight_total <- alist(
  weight ~ dnorm(mu, sigma),
  mu <- a[sex],
  a[sex] ~ dnorm(15, 10),
  sigma ~ dunif(0, 15)
)

fit_total_simple <- ulam(sex_on_weight_total,
  data = d_fit,
  chains = 4,
  cores = 4,
  iter = 2000,
  sample = TRUE
)

cat("--- Simple Model Diagnostics ---\n")
print(precis(fit_total_simple, depth = 2))
cat("\n")

# Extract total effect from simple model
post_simple <- extract.samples(fit_total_simple)
total_effect_simple <- post_simple$a[, 2] - post_simple$a[, 1]

cat("--- Total Effect from Simple Model ---\n")
cat(sprintf("Posterior mean: %.2f kg\n", mean(total_effect_simple)))
cat(sprintf("Posterior SD:   %.2f kg\n", sd(total_effect_simple)))
cat(sprintf("89%% CI:        [%.2f, %.2f] kg\n",
            quantile(total_effect_simple, 0.055),
            quantile(total_effect_simple, 0.945)))
cat("\n")

cat("--- Comparison: Simple vs Decomposed ---\n")
cat(sprintf("Simple model (weight ~ sex):           %.2f kg  [%.2f, %.2f]\n",
            mean(total_effect_simple),
            quantile(total_effect_simple, 0.055),
            quantile(total_effect_simple, 0.945)))
cat(sprintf("Complex model (Direct + Indirect):     %.2f kg  [%.2f, %.2f]\n",
            mean(total_effect),
            quantile(total_effect, 0.055),
            quantile(total_effect, 0.945)))
cat(sprintf("Difference:                            %.2f kg\n",
            mean(total_effect_simple) - mean(total_effect)))
cat("\n")
cat("Note: If these match closely, the decomposition is working correctly.\n")
cat("      The simple model is the 'gold standard' for total effect.\n")

# Visualize comparison
png(file.path(output_dir, "10b_total_effect_comparison.png"), width = 800, height = 400)
par(mfrow = c(1, 2))
hist(total_effect_simple, breaks = 30, col = "forestgreen", border = "white",
     main = "Simple Model\n(weight ~ sex)", xlab = "Total Effect (kg)",
     xlim = range(c(total_effect_simple, total_effect)))
abline(v = mean(total_effect_simple), col = "darkgreen", lwd = 2)

hist(total_effect, breaks = 30, col = "purple", border = "white",
     main = "Complex Model\n(Direct + Indirect)", xlab = "Total Effect (kg)",
     xlim = range(c(total_effect_simple, total_effect)))
abline(v = mean(total_effect), col = "purple4", lwd = 2)
par(mfrow = c(1, 1))
dev.off()
cat("Saved: 10b_total_effect_comparison.png\n")

# ============================================================================
# STEP 7: Direct Effect Across Height Range (at mean age)
# ============================================================================

# Since we have sex-specific slopes, the direct effect varies with height:
# direct_effect(h) = (a[2] - a[1]) + (b_height[2] - b_height[1]) * (h - height_bar)
# Age terms cancel out when comparing at same age

# Calculate direct effect at each height in the observed range
direct_effect_by_height <- sapply(height_seq, function(h) {
  (post$a[, 2] - post$a[, 1]) + (post$b_height[, 2] - post$b_height[, 1]) * (h - height_bar)
})

# Create data frame for plotting
direct_effect_df <- data.frame(
  height = height_seq,
  mean = colMeans(direct_effect_by_height),
  lower = apply(direct_effect_by_height, 2, quantile, 0.055),
  upper = apply(direct_effect_by_height, 2, quantile, 0.945),
  lower_50 = apply(direct_effect_by_height, 2, quantile, 0.25),
  upper_50 = apply(direct_effect_by_height, 2, quantile, 0.75)
)

p_direct_by_height <- ggplot(direct_effect_df, aes(x = height)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "steelblue", alpha = 0.5) +
  geom_line(aes(y = mean), color = "steelblue", linewidth = 1.2) +
  geom_rug(data = d_real, aes(x = height), sides = "b", alpha = 0.3) +
  labs(
    title = "Direct Effect of Sex on Weight by Height (at mean age)",
    subtitle = "Male - Female weight difference at each height (89% and 50% CI)",
    x = "Height (cm)",
    y = "Direct Effect (kg)"
  ) +
  theme_minimal()

ggsave(file.path(output_dir, "11_direct_effect_by_height.png"), p_direct_by_height, width = 8, height = 6)
cat("Saved: 11_direct_effect_by_height.png\n")

cat("\n--- Direct Effect by Height ---\n")
cat(sprintf("At min height (%.0f cm): %.2f kg [%.2f, %.2f]\n",
            min(height_seq),
            direct_effect_df$mean[1],
            direct_effect_df$lower[1],
            direct_effect_df$upper[1]))
cat(sprintf("At mean height (%.0f cm): %.2f kg [%.2f, %.2f]\n",
            height_bar,
            mean(direct_effect),
            quantile(direct_effect, 0.055),
            quantile(direct_effect, 0.945)))
cat(sprintf("At max height (%.0f cm): %.2f kg [%.2f, %.2f]\n",
            max(height_seq),
            direct_effect_df$mean[nrow(direct_effect_df)],
            direct_effect_df$lower[nrow(direct_effect_df)],
            direct_effect_df$upper[nrow(direct_effect_df)]))

cat("\nReal data analysis complete.\n")
