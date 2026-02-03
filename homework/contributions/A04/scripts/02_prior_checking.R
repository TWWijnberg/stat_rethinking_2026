# Homework A04 - Prior Checking
# Statistical Rethinking 2026
# https://www.youtube.com/watch?v=GIdwLrW2nNo
# Step 2: Prior predictive simulation, fit model on synthetic data,
# and inspect the fitted model

# ---- Setup ----
source(here::here("homework", "contributions", "A04", "scripts", "01_scientific_model.R"))

# ---- Define Prior Parameters ----
# Define all prior parameters in one place for easy tweaking
# Using index coding: a_height[sex_id] and a_weight[sex_id]
# Both sexes share the same prior distributions
# Aligned with true_params from script 01:
#   a_height = c(60, 65), b_height_age = 0.5, sigma_height = 6
#   a_weight = c(-10, -8), b_weight_height = 0.20, b_weight_age = 0.05, sigma_weight = 5
prior_params <- list(
  # Height model priors
  # Intercepts by sex: centered around ~60-65 cm (true values)
  a_height_mean = 62.5,     # Prior mean centered on true values (60, 65)
  a_height_sd = 10,         # Wide uncertainty: 95% between ~43-82 cm
  b_height_age_min = 0.3,   # Min growth rate (cm/month)
  b_height_age_max = 0.8,   # Max growth rate (cm/month, true = 0.5)
  sigma_height_max = 12,    # Max residual SD (true = 6)

  # Weight model priors
  # Intercepts by sex: base weight after accounting for height
  a_weight_min = -35,       # Min intercept (true values: -10, -8)
  a_weight_max = 5,         # Max intercept
  b_weight_height_min = 0.05,  # Min weight gain per cm (true = 0.20)
  b_weight_height_max = 0.4,   # Max weight gain per cm
  b_weight_age_min = 0,     # Min direct age effect (true = 0.05, holding height constant)
  b_weight_age_max = 0.2,   # Max direct age effect (kg/month)
  sigma_weight_max = 10     # Max residual SD (true = 5)
)

# ---- Define Statistical Models ----
# Model: Height and Weight as functions of Age and Sex
# Based on UK growth charts: children grow ~3-10 cm per year
# Birth height around 50 cm
# https://www.rcpch.ac.uk/resources/uk-who-growth-charts-2-18-years

# IMPORTANT: Define models once to enable caching
# Model formulas are only generated once when this script is sourced
# The if(!exists()) check prevents regeneration on subsequent runs

# Model for quap: uses indicator variable (simpler, faster for development)
# Uses sex_male where 0 = Female, 1 = Male
if (!exists("height_model_quap")) {
  height_model_quap <- eval(bquote(alist(
    height ~ dnorm(mu_height, sigma_height),
        mu_height <- a_height + b_height_age * age + b_height_sex * sex_male,
        # Baseline (Female) intercept
        a_height ~ dnorm(.(prior_params$a_height_mean), .(prior_params$a_height_sd)),
        b_height_age ~ dunif(.(prior_params$b_height_age_min),
                             .(prior_params$b_height_age_max)),
        # Male effect: difference from female baseline
        b_height_sex ~ dnorm(2.5, 5),  # Expected ~5cm difference
        sigma_height ~ dunif(0, .(prior_params$sigma_height_max)),
    # Weight model
    weight ~ dnorm(mu_weight, sigma_weight),
        mu_weight <- a_weight + b_weight_height * height +
                     b_weight_age * age + b_weight_sex * sex_male,
        a_weight ~ dunif(.(prior_params$a_weight_min),
                         .(prior_params$a_weight_max)),
        b_weight_height ~ dunif(.(prior_params$b_weight_height_min),
                                .(prior_params$b_weight_height_max)),
        b_weight_age ~ dunif(.(prior_params$b_weight_age_min),
                             .(prior_params$b_weight_age_max)),
        # Male effect: difference from female baseline
        b_weight_sex ~ dnorm(1, 3),  # Expected ~2kg difference
        sigma_weight ~ dunif(0, .(prior_params$sigma_weight_max))
  )))
}

# Model for ulam: uses index coding (Stan-optimized, for final results)
# Uses sex_id where 1 = Female, 2 = Male
if (!exists("height_model_ulam")) {
  height_model_ulam <- eval(bquote(alist(
    height ~ dnorm(mu_height, sigma_height),
        mu_height <- a_height[sex_id] + b_height_age * age,
        # Sex-specific intercepts: birth height by sex (vector of length 2)
        vector[2]:a_height ~ dnorm(.(prior_params$a_height_mean),
                                    .(prior_params$a_height_sd)),
        b_height_age ~ dunif(.(prior_params$b_height_age_min),
                             .(prior_params$b_height_age_max)),
        sigma_height ~ dunif(0, .(prior_params$sigma_height_max)),
    # Weight model
    weight ~ dnorm(mu_weight, sigma_weight),
        mu_weight <-
            a_weight[sex_id] +
            b_weight_height * height +
            b_weight_age * age,
        # Sex-specific intercepts: base weight by sex (vector of length 2)
        vector[2]:a_weight ~ dunif(.(prior_params$a_weight_min),
                                    .(prior_params$a_weight_max)),
        b_weight_height ~ dunif(.(prior_params$b_weight_height_min),
                                .(prior_params$b_weight_height_max)),
        b_weight_age ~ dunif(.(prior_params$b_weight_age_min),
                             .(prior_params$b_weight_age_max)),
        sigma_weight ~ dunif(0, .(prior_params$sigma_weight_max))
  )))
}

# ---- Prior Predictive Simulation ----
# Generate predictions from the prior to see if they make sense
n_sample <- 500
n_lines <- 50  # Number of prior regression lines to plot

# Sample ages and sexes
age_sample <- runif(n_sample, 0, 19 * 12)  # Age in months
sex_sample <- sample(c("Female", "Male"), n_sample, replace = TRUE)
sex_id_sample <- as.integer(factor(sex_sample, levels = c("Female", "Male")))

# Sample from height priors (using prior_params)
# For index coding: sample sex-specific intercepts
a_height_female_sample <- rnorm(n_sample, prior_params$a_height_mean, prior_params$a_height_sd)
a_height_male_sample <- rnorm(n_sample, prior_params$a_height_mean, prior_params$a_height_sd)
b_height_age_sample <- runif(n_sample, prior_params$b_height_age_min, prior_params$b_height_age_max)
sigma_height_sample <- runif(n_sample, 0, prior_params$sigma_height_max)

# Select appropriate intercept based on sex_id
a_height_sample <- ifelse(sex_id_sample == 1, a_height_female_sample, a_height_male_sample)

# Generate height predictions
mu_height_sample <- a_height_sample + b_height_age_sample * age_sample
height_sample <- rnorm(n_sample, mu_height_sample, sigma_height_sample)

# Sample from weight priors (using prior_params)
# For index coding: sample sex-specific intercepts
a_weight_female_sample <- runif(n_sample, prior_params$a_weight_min, prior_params$a_weight_max)
a_weight_male_sample <- runif(n_sample, prior_params$a_weight_min, prior_params$a_weight_max)
b_weight_height_sample <- runif(n_sample, prior_params$b_weight_height_min, prior_params$b_weight_height_max)
b_weight_age_sample <- runif(n_sample, prior_params$b_weight_age_min, prior_params$b_weight_age_max)
sigma_weight_sample <- runif(n_sample, 0, prior_params$sigma_weight_max)

# Select appropriate intercept based on sex_id
a_weight_sample <- ifelse(sex_id_sample == 1, a_weight_female_sample, a_weight_male_sample)

# Generate weight predictions
mu_weight_sample <- a_weight_sample +
    b_weight_height_sample * height_sample +
    b_weight_age_sample * age_sample
weight_sample <- rnorm(n_sample, mu_weight_sample, sigma_weight_sample)

# Create prior sample dataframe
prior_sample <- data.frame(
    age_months = age_sample,
    age_years = age_sample / 12,
    sex = sex_sample,
    height = height_sample,
    weight = weight_sample
)

# Visualize prior predictions
# Plot 1: Height vs Age
p1 <- ggplot(prior_sample, aes(x = age_years, y = height, color = sex)) +
    geom_point(alpha = 0.3) +
    xlim(0, 14) +
    ylim(0, 220) +
    labs(
x = "Age (years)",
y = "Height (cm)",
title = "Prior Predictions: Height vs Age") +
    theme_minimal()

# Plot 2: Weight vs Age
p2 <- ggplot(prior_sample, aes(x = age_years, y = weight, color = sex)) +
    geom_point(alpha = 0.3) +
    xlim(0, 14) +
    ylim(-50, 150) +
    labs(
x = "Age (years)",
y = "Weight (kg)",
title = "Prior Predictions: Weight vs Age") +
    theme_minimal()

# Plot 3: Weight vs Height
p3 <- ggplot(prior_sample, aes(x = height, y = weight, color = sex)) +
    geom_point(alpha = 0.3) +
    xlim(0, 13 * 12) +
    ylim(-50, 150) +
    labs(
x = "Height (cm)",
y = "Weight (kg)",
title = "Prior Predictions: Weight vs Height") +
    theme_minimal()

# Display plots
print(p1)
print(p2)
print(p3)

# ---- Prior Regression Lines ----
# Sample parameters to visualize prior regression lines (using prior_params)
# For index coding: sample separate intercepts for each sex
a_height_female_lines <- rnorm(n_lines, prior_params$a_height_mean, prior_params$a_height_sd)
a_height_male_lines <- rnorm(n_lines, prior_params$a_height_mean, prior_params$a_height_sd)
b_height_age_lines <- runif(n_lines, prior_params$b_height_age_min, prior_params$b_height_age_max)

a_weight_female_lines <- runif(n_lines, prior_params$a_weight_min, prior_params$a_weight_max)
a_weight_male_lines <- runif(n_lines, prior_params$a_weight_min, prior_params$a_weight_max)
b_weight_height_lines <- runif(n_lines, prior_params$b_weight_height_min, prior_params$b_weight_height_max)
b_weight_age_lines <- runif(n_lines, prior_params$b_weight_age_min, prior_params$b_weight_age_max)

# Create data for lines
age_seq <- seq(0, 13 * 12, length.out = 100)
height_seq <- seq(30, 160, length.out = 100)

# Plot 4: Prior lines for Age -> Height (both sexes)
height_lines_female <- sapply(1:n_lines, function(i) {
    a_height_female_lines[i] + b_height_age_lines[i] * age_seq
})
height_lines_male <- sapply(1:n_lines, function(i) {
    a_height_male_lines[i] + b_height_age_lines[i] * age_seq
})

# Convert to long format for plotting
height_lines_df <- data.frame(
    age_years = rep(age_seq / 12, n_lines * 2),
    height = c(as.vector(height_lines_female), as.vector(height_lines_male)),
    sex = rep(c(rep("Female", n_lines * 100), rep("Male", n_lines * 100))),
    line_id = c(rep(1:n_lines, each = 100), rep((n_lines + 1):(2 * n_lines), each = 100))
)

p4 <- ggplot(height_lines_df, aes(x = age_years, y = height, group = line_id, color = sex)) +
    geom_line(alpha = 0.2) +
    geom_point(data = d_sim, aes(x = age / 12, y = height, color = sex),
               inherit.aes = FALSE, size = 2, alpha = 0.8) +
    xlim(0, 14) +
    ylim(0, 220) +
    labs(
x = "Age (years)",
y = "Height (cm)",
title = "Prior Regression Lines: Age -> Height",
subtitle = "Points show synthetic data - do priors cover the data space?") +
    theme_minimal()

# Plot 5: Prior lines for Height -> Weight (holding age constant at mean)
mean_age <- 9.5 * 12  # midpoint of age range
weight_lines_female <- sapply(1:n_lines, function(i) {
    a_weight_female_lines[i] +
        b_weight_height_lines[i] * height_seq +
        b_weight_age_lines[i] * mean_age
})
weight_lines_male <- sapply(1:n_lines, function(i) {
    a_weight_male_lines[i] +
        b_weight_height_lines[i] * height_seq +
        b_weight_age_lines[i] * mean_age
})

weight_height_lines_df <- data.frame(
    height = rep(height_seq, n_lines * 2),
    weight = c(as.vector(weight_lines_female), as.vector(weight_lines_male)),
    sex = rep(c(rep("Female", n_lines * 100), rep("Male", n_lines * 100))),
    line_id = c(rep(1:n_lines, each = 100), rep((n_lines + 1):(2 * n_lines), each = 100))
)

p5 <- ggplot(weight_height_lines_df, aes(x = height, y = weight, group = line_id, color = sex)) +
    geom_line(alpha = 0.2) +
    geom_point(data = d_sim, aes(x = height, y = weight, color = sex),
               inherit.aes = FALSE, size = 2, alpha = 0.8) +
    xlim(50, 13 * 12) +
    ylim(-20, 120) +
    labs(
x = "Height (cm)",
y = "Weight (kg)",
title = "Prior Regression Lines: Height -> Weight",
subtitle = "Points show synthetic data - do priors cover the data space?") +
    theme_minimal()

# Plot 6: Prior lines for Age -> Weight (total effect through height)
weight_age_lines_female <- sapply(1:n_lines, function(i) {
    height_at_age <- a_height_female_lines[i] + b_height_age_lines[i] * age_seq
    a_weight_female_lines[i] +
        b_weight_height_lines[i] * height_at_age +
        b_weight_age_lines[i] * age_seq
})
weight_age_lines_male <- sapply(1:n_lines, function(i) {
    height_at_age <- a_height_male_lines[i] + b_height_age_lines[i] * age_seq
    a_weight_male_lines[i] +
        b_weight_height_lines[i] * height_at_age +
        b_weight_age_lines[i] * age_seq
})

weight_age_lines_df <- data.frame(
    age_years = rep(age_seq / 12, n_lines * 2),
    weight = c(as.vector(weight_age_lines_female), as.vector(weight_age_lines_male)),
    sex = rep(c(rep("Female", n_lines * 100), rep("Male", n_lines * 100))),
    line_id = c(rep(1:n_lines, each = 100), rep((n_lines + 1):(2 * n_lines), each = 100))
)

p6 <- ggplot(weight_age_lines_df, aes(x = age_years, y = weight, group = line_id, color = sex)) +
    geom_line(alpha = 0.2) +
    geom_point(data = d_sim, aes(x = age / 12, y = weight, color = sex),
               inherit.aes = FALSE, size = 2, alpha = 0.8) +
    xlim(0, 14) +
    ylim(-20, 120) +
    labs(
x = "Age (years)",
y = "Weight (kg)",
title = "Prior Regression Lines: Age -> Weight (total effect)",
subtitle = "Points show synthetic data - do priors cover the data space?") +
    theme_minimal()

# Display regression line plots
print(p4)
print(p5)
print(p6)

# ---- Fit Model on Synthetic Data ----
# Toggle between fast development (quap) and full posterior (ulam)
# NOTE: quap struggles with multivariate models where height is both
# an outcome and a predictor. Use ulam for reliable fitting.
DEV_MODE <- FALSE  # Set to FALSE for final run with ulam

# Prepare data for model fitting (only include necessary variables)
# This prevents warnings about unused character/factor variables
d_fit <- d_sim[, c("age", "sex_id", "height", "weight")]
d_fit$sex_male <- as.integer(d_sim$sex == "Male")

if (DEV_MODE) {
  cat("\n=== Fitting with quap (fast approximation) ===\n")
  # Provide explicit start values to avoid non-finite likelihoods
  # Use middle of prior ranges and reasonable values given the data
  start_values <- list(
    a_height = 62.5,           # Middle of prior
    b_height_age = 0.5,        # Middle of prior range
    b_height_sex = 2.5,        # Expected male-female difference
    sigma_height = 6,          # Middle of prior range
    a_weight = -15,            # Middle of prior range
    b_weight_height = 0.2,     # Middle of prior range
    b_weight_age = 0.1,        # Middle of prior range
    b_weight_sex = 1,          # Expected male-female difference
    sigma_weight = 5           # Middle of prior range
  )
  fitted_model <- quap(height_model_quap, data = d_fit, start = start_values)
} else {
  cat("\n=== Fitting with ulam (full MCMC) ===\n")
  fitted_model <- ulam(height_model_ulam, data = d_fit)
}

# Display parameter estimates
cat("\nParameter estimates:\n")
precis(fitted_model)

# ---- Parameter Recovery Check ----
# Extract posterior means and intervals
post_summary <- precis(fitted_model, prob = 0.89, depth = 2)  # depth=2 to show indexed parameters
param_names <- rownames(post_summary)

# Create dataframe for caterpillar plot
# True values differ based on parameterization (quap vs ulam)
if (DEV_MODE) {
  # quap uses indicator coding: baseline + sex effect
  # Parameters: a_height, b_height_age, b_height_sex, sigma_height,
  #             a_weight, b_weight_height, b_weight_age, b_weight_sex, sigma_weight
  true_values <- c(
    true_params$a_height[1],        # a_height (Female baseline)
    true_params$b_height_age,
    true_params$a_height[2] - true_params$a_height[1],  # b_height_sex (Male - Female)
    true_params$sigma_height,
    true_params$a_weight[1],        # a_weight (Female baseline)
    true_params$b_weight_height,
    true_params$b_weight_age,
    true_params$a_weight[2] - true_params$a_weight[1],  # b_weight_sex (Male - Female)
    true_params$sigma_weight
  )
} else {
  # ulam uses index coding: separate intercepts
  # Parameters: a_height[1], a_height[2], b_height_age, sigma_height,
  #             a_weight[1], a_weight[2], b_weight_height, b_weight_age, sigma_weight
  true_values <- c(
    true_params$a_height[1],        # a_height[1] = Female
    true_params$a_height[2],        # a_height[2] = Male
    true_params$b_height_age,
    true_params$sigma_height,
    true_params$a_weight[1],        # a_weight[1] = Female
    true_params$a_weight[2],        # a_weight[2] = Male
    true_params$b_weight_height,
    true_params$b_weight_age,
    true_params$sigma_weight
  )
}

recovery_df <- data.frame(
    parameter = param_names,
    estimate = post_summary$mean,
    lower = post_summary$`5.5%`,
    upper = post_summary$`94.5%`,
    true_value = true_values
)

# Check if true values are within credible intervals
recovery_df$recovered <- recovery_df$true_value >= recovery_df$lower &
                        recovery_df$true_value <= recovery_df$upper

# Print recovery summary
cat("\n=== Parameter Recovery Summary ===\n")
cat(sprintf("Parameters recovered (within 89%% CI): %d/%d\n",
            sum(recovery_df$recovered), nrow(recovery_df)))
print(recovery_df[, c("parameter", "estimate", "true_value", "recovered")])

# Caterpillar plot
p_caterpillar <- ggplot(recovery_df, aes(x = estimate, y = parameter)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
    geom_point(aes(x = true_value), color = "red", shape = 4, size = 4, stroke = 2) +
    labs(
x = "Parameter value",
y = "Parameter",
title = "Parameter Recovery: Estimates vs True Values",
subtitle = "Black points = posterior means with 89% CI, Red X = true values") +
    theme_minimal() +
    theme(axis.text.y = element_text(family = "mono"))

print(p_caterpillar)

# ---- Posterior Predictive Check ----
cat("\n=== Posterior Predictive Check ===\n")

# Generate predictions from the fitted model
n_pred <- 100
post_samples <- extract.samples(fitted_model, n = n_pred)

# Sample from the posterior predictive distribution
pred_data <- d_sim[sample(1:nrow(d_sim), 50), ]  # Use a subset for visualization

height_pred <- sapply(1:n_pred, function(i) {
    # Use index to select sex-specific intercept
    a_height_i <- post_samples$a_height[i, pred_data$sex_id]
    mu <- a_height_i + post_samples$b_height_age[i] * pred_data$age
    rnorm(nrow(pred_data), mu, post_samples$sigma_height[i])
})

weight_pred <- sapply(1:n_pred, function(i) {
    # Use index to select sex-specific intercept
    a_weight_i <- post_samples$a_weight[i, pred_data$sex_id]
    mu <- a_weight_i +
          post_samples$b_weight_height[i] * pred_data$height +
          post_samples$b_weight_age[i] * pred_data$age
    rnorm(nrow(pred_data), mu, post_samples$sigma_weight[i])
})

# Create prediction intervals
pred_df <- data.frame(
    age = pred_data$age,
    sex = pred_data$sex,
    height_obs = pred_data$height,
    weight_obs = pred_data$weight,
    height_mean = apply(height_pred, 1, mean),
    height_lower = apply(height_pred, 1, quantile, 0.055),
    height_upper = apply(height_pred, 1, quantile, 0.945),
    weight_mean = apply(weight_pred, 1, mean),
    weight_lower = apply(weight_pred, 1, quantile, 0.055),
    weight_upper = apply(weight_pred, 1, quantile, 0.945)
)

# Plot posterior predictions vs observations
p_pp_height <- ggplot(pred_df, aes(x = height_obs, y = height_mean)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    geom_point(aes(color = sex), alpha = 0.6) +
    geom_errorbar(aes(ymin = height_lower, ymax = height_upper, color = sex), alpha = 0.3) +
    labs(
x = "Observed height (cm)",
y = "Predicted height (cm)",
title = "Posterior Predictive Check: Height") +
    theme_minimal()

p_pp_weight <- ggplot(pred_df, aes(x = weight_obs, y = weight_mean)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    geom_point(aes(color = sex), alpha = 0.6) +
    geom_errorbar(aes(ymin = weight_lower, ymax = weight_upper, color = sex), alpha = 0.3) +
    labs(
x = "Observed weight (kg)",
y = "Predicted weight (kg)",
title = "Posterior Predictive Check: Weight") +
    theme_minimal()

print(p_pp_height)
print(p_pp_weight)

# ---- Residual Analysis ----
cat("\n=== Residual Analysis ===\n")

# Calculate residuals
post_mean <- coef(fitted_model)
# Use sex_id to index into sex-specific intercepts
d_sim$height_pred <- post_mean[paste0("a_height[", d_sim$sex_id, "]")] +
                     post_mean["b_height_age"] * d_sim$age
d_sim$weight_pred <- post_mean[paste0("a_weight[", d_sim$sex_id, "]")] +
                     post_mean["b_weight_height"] * d_sim$height +
                     post_mean["b_weight_age"] * d_sim$age

d_sim$height_resid <- d_sim$height - d_sim$height_pred
d_sim$weight_resid <- d_sim$weight - d_sim$weight_pred

# Plot residuals
p_resid_height <- ggplot(d_sim, aes(x = height_pred, y = height_resid)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(color = sex), alpha = 0.5) +
    labs(
x = "Predicted height (cm)",
y = "Residuals (cm)",
title = "Residual Plot: Height") +
    theme_minimal()

p_resid_weight <- ggplot(d_sim, aes(x = weight_pred, y = weight_resid)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(color = sex), alpha = 0.5) +
    labs(
x = "Predicted weight (kg)",
y = "Residuals (kg)",
title = "Residual Plot: Weight") +
    theme_minimal()

print(p_resid_height)
print(p_resid_weight)

# Calculate RMSE
rmse_height <- sqrt(mean(d_sim$height_resid^2))
rmse_weight <- sqrt(mean(d_sim$weight_resid^2))
cat(sprintf("\nRMSE - Height: %.2f cm (True sigma: %.2f)\n",
            rmse_height, true_params$sigma_height))
cat(sprintf("RMSE - Weight: %.2f kg (True sigma: %.2f)\n",
            rmse_weight, true_params$sigma_weight))

cat("\n=== Model Validation Complete ===\n")
cat("\nSuggested additional tests:\n")
cat("1. Simulation-based calibration (SBC) - Run model on many synthetic datasets\n")
cat("2. Cross-validation - Split data and check out-of-sample performance\n")
cat("3. Prior sensitivity analysis - Vary priors and check impact on estimates\n")
cat("4. Check for multicollinearity - Examine correlation between age and height\n")
cat("5. Posterior predictive p-values - Calculate test statistics on simulated data\n")
