# Homework A05 - Synthetic Data Analysis
# Statistical Rethinking 2026
# Purpose: Validate model on synthetic data before applying to real data

source(here::here("homework", "contributions", "A05", "scripts", "02_dag_analysis.R"))

# ============================================================================
# STEP 1: Generate Synthetic Data
# ============================================================================

set.seed(42)
d_syn <- sim_children(n = 200, params = true_params)
d_syn <- prepare_data(d_syn, source = "synthetic")

cat("=== Synthetic Data Summary ===\n")
cat(sprintf("N = %d observations\n", nrow(d_syn)))
cat(sprintf("Age range: %.0f - %.0f months\n", min(d_syn$age), max(d_syn$age)))
cat(sprintf("Sex: %d Female, %d Male\n",
            sum(d_syn$sex == "Female"), sum(d_syn$sex == "Male")))

# Visualize synthetic data
p1 <- ggplot(d_syn, aes(x = age, y = weight, color = sex)) +
  geom_point(alpha = 0.6) +
  labs(title = "Synthetic Data: Age vs Weight") +
  theme_minimal()
print(p1)

# ============================================================================
# STEP 2: Fit Model
# ============================================================================

# Prepare data for ulam (only include required columns)
d_fit <- list(
  weight = d_syn$weight,
  age = d_syn$age,
  sex_id = d_syn$sex_id
)

# Fit the model - KEEP THIS EXPLICIT so you can see what's being fit
fit_syn <- ulam(
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
# STEP 3: Check MCMC Diagnostics
# ============================================================================

cat("\n=== MCMC Diagnostics ===\n")
print(precis(fit_syn, depth = 2))

# Check trace plots
traceplot(fit_syn)

# ============================================================================
# STEP 4: Parameter Recovery
# ============================================================================

# Did we recover the true parameters?
# Build true values vector matching parameter names
true_for_plot <- c(
  `a[1]` = true_params$a_weight[1] +
           true_params$b_weight_height * true_params$a_height[1],
  `a[2]` = true_params$a_weight[2] +
           true_params$b_weight_height * true_params$a_height[2],
  b_age = true_params$b_weight_age +
          true_params$b_height_age * true_params$b_weight_height,
  sigma = true_params$sigma_weight
)

# NOTE: Because we're NOT conditioning on height, the intercepts absorb
# the height effect. The "true" values for this model are the marginal effects.

cat("\n=== Parameter Recovery ===\n")
cat("Note: Model estimates marginal effects (not conditional on height)\n")
cat("Expected a[1] (Female): ", round(true_for_plot["a[1]"], 2), "\n")
cat("Expected a[2] (Male): ", round(true_for_plot["a[2]"], 2), "\n")
cat("Expected b_age (total): ", round(true_for_plot["b_age"], 3), "\n")

# Caterpillar plot with true values
p2 <- plot_caterpillar(fit_syn, true_values = true_for_plot,
                       title = "Parameter Recovery: Synthetic Data")
print(p2)

# ============================================================================
# STEP 5: Posterior Predictive Check
# ============================================================================

p3 <- plot_predictions(d_syn, fit_syn, outcome = "weight",
                       title = "Posterior Predictive: Synthetic Data")
print(p3)

# ============================================================================
# STEP 6: Estimate Total Effect of Sex
# ============================================================================

post <- extract.samples(fit_syn)

# Total effect = a[Male] - a[Female]
total_effect <- post$a[, 2] - post$a[, 1]

cat("\n=== Total Effect of Sex on Weight (Synthetic Data) ===\n")
cat(sprintf("Posterior mean: %.2f kg\n", mean(total_effect)))
cat(sprintf("89%% CI: [%.2f, %.2f] kg\n",
            quantile(total_effect, 0.055),
            quantile(total_effect, 0.945)))
cat(sprintf("True value: %.2f kg\n", true_params$total_sex_effect))

# Check if true value is within 89% CI
in_ci <- true_params$total_sex_effect >= quantile(total_effect, 0.055) &
         true_params$total_sex_effect <= quantile(total_effect, 0.945)
cat(sprintf("True value in 89%% CI: %s\n", ifelse(in_ci, "YES", "NO")))

# Visualize posterior
hist(total_effect, breaks = 30, col = "skyblue", border = "white",
     main = "Posterior: Total Effect of Sex on Weight",
     xlab = "Effect (kg, Male - Female)")
abline(v = true_params$total_sex_effect, col = "red", lwd = 2, lty = 2)
abline(v = mean(total_effect), col = "blue", lwd = 2)
legend("topright", c("True value", "Posterior mean"),
       col = c("red", "blue"), lty = c(2, 1), lwd = 2)

cat("\nSynthetic analysis complete. Model validated.\n")
