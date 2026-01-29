# Homework A04 - Post-processing
# Statistical Rethinking 2026
#
# Step 4: Extract and visualize causal estimates

# ---- Setup ----
source(here::here("homework", "contributions", "A04", "scripts", "03_fit_real_data.R"))

# ---- Extract Posterior Estimates ----
# Compute posterior mean and credible intervals
age_seq <- seq(0, 19, length.out = 50)
mu <- link(fitted_model, data = data.frame(age = age_seq))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI, prob = 0.89)

# ---- Visualize Fitted Model ----
plot(height ~ age, data = children,
     xlab = "Age (years)", ylab = "Height (cm)",
     main = "Fitted Model on Children Data",
     xlim = c(0, 20), ylim = c(0, 200))
lines(age_seq, mu_mean)
shade(mu_PI, age_seq)

# ---- Parameter Estimates ----
plot(precis(fitted_model))

# ---- Causal Interpretation ----
# TODO: For A04, this section should include:
# 1. Causal effect estimate of age on height (controlling for sex)
# 2. Counterfactual predictions
# 3. Uncertainty quantification
# 4. Discussion of unmeasured confounding (nutrition)

cat("\nNote: Update this script to include Sex in the model for A04\n")
cat("Current model estimates association, not causal effect\n")
cat("Need to condition on Sex to block backdoor path\n")
