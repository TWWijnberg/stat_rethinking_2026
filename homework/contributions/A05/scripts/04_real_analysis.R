# Homework A05 - Real Data Analysis
# Statistical Rethinking 2026
# Purpose: Estimate total causal effect of sex on weight in children

source(here::here("homework", "contributions", "A05", "scripts", "02_dag_analysis.R"))

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
print(p1)

# ============================================================================
# STEP 2: Fit Model
# ============================================================================

d_fit <- list(
  weight = d_real$weight,
  age = d_real$age,
  sex_id = d_real$sex_id
)

# Fit the same model as synthetic analysis
fit_real <- ulam(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a[sex_id] + b_age * age,
    vector[2]:a ~ dnorm(5, 10),
    b_age ~ dunif(0, 0.3),
    sigma ~ dunif(0, 15)
  ),
  data = d_fit,
  chains = 4,
  cores = 4,
  iter = 2000
)

# ============================================================================
# STEP 3: MCMC Diagnostics
# ============================================================================

cat("\n=== MCMC Diagnostics ===\n")
print(precis(fit_real, depth = 2))

traceplot(fit_real)

# ============================================================================
# STEP 4: Posterior Predictive Check
# ============================================================================

p2 <- plot_predictions(d_real, fit_real, outcome = "weight",
                       title = "Posterior Predictive: Howell1 Children")
print(p2)

# ============================================================================
# STEP 5: Estimate Total Causal Effect of Sex
# ============================================================================

post <- extract.samples(fit_real)

# Total effect = a[Male] - a[Female]
total_effect <- post$a[, 2] - post$a[, 1]

cat("\n")
cat("============================================================\n")
cat("  CAUSAL INFERENCE: Total Effect of Sex on Weight\n")
cat("============================================================\n")
cat("\n")
cat("Question: How much heavier are male children than female\n")
cat("          children of the same age?\n")
cat("\n")
cat(sprintf("Posterior mean: %.2f kg\n", mean(total_effect)))
cat(sprintf("Posterior SD:   %.2f kg\n", sd(total_effect)))
cat(sprintf("89%% CI:        [%.2f, %.2f] kg\n",
            quantile(total_effect, 0.055),
            quantile(total_effect, 0.945)))
cat("\n")
cat("Interpretation:\n")
cat(sprintf("  Male children weigh approximately %.1f kg more than\n",
            mean(total_effect)))
cat("  female children of the same age. This total effect includes\n")
cat("  both the direct effect of sex and the indirect effect\n")
cat("  through height (boys are taller, taller children weigh more).\n")
cat("\n")

# Visualize posterior
hist(total_effect, breaks = 30, col = "skyblue", border = "white",
     main = "Posterior: Total Effect of Sex on Weight (Howell1)",
     xlab = "Effect (kg, Male - Female)")
abline(v = mean(total_effect), col = "blue", lwd = 2)
abline(v = quantile(total_effect, c(0.055, 0.945)), col = "blue", lwd = 1, lty = 2)

# ============================================================================
# STEP 6: Compare to Effect Holding Height Constant (Optional)
# ============================================================================

# If we wanted the DIRECT effect only, we would condition on height
# This is NOT what the question asks, but useful for comparison

cat("\n=== Comparison: What if we conditioned on height? ===\n")
cat("(This would give DIRECT effect only, blocking indirect path)\n")
cat("Not computed here - see weight_model_direct in 02_dag_analysis.R\n")

cat("\nReal data analysis complete.\n")
