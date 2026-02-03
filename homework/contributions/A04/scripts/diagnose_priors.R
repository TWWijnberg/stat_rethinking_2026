# Quick diagnostic: Are true parameters within prior ranges?
source(here::here("homework", "contributions", "A04", "scripts", "01_scientific_model.R"))

# Your current priors (from 02_prior_checking.R)
prior_params <- list(
  a_height_mean = 20, a_height_sd = 10,
  b_height_age_min = 0.5, b_height_age_max = 1,
  b_height_sex_mean = 5, b_height_sex_sd = 5,
  sigma_height_max = 10,

  a_weight_min = -20, a_weight_max = 0,
  b_weight_height_min = 0, b_weight_height_max = 0.3,  # PROBLEM!
  b_weight_age_min = 0, b_weight_age_max = 0.5,
  b_weight_sex_mean = 2, b_weight_sex_sd = 2,
  sigma_weight_max = 10
)

cat("=== Prior Coverage Check ===\n\n")

# Height model
cat("HEIGHT MODEL:\n")
cat(sprintf("a_height: TRUE=%.1f, PRIOR: N(%.1f, %.1f) [~%.1f to %.1f at ±2SD]\n",
            true_params$a_height,
            prior_params$a_height_mean, prior_params$a_height_sd,
            prior_params$a_height_mean - 2*prior_params$a_height_sd,
            prior_params$a_height_mean + 2*prior_params$a_height_sd))

cat(sprintf("b_height_age: TRUE=%.2f, PRIOR: U(%.2f, %.2f) %s\n",
            true_params$b_height_age,
            prior_params$b_height_age_min, prior_params$b_height_age_max,
            ifelse(true_params$b_height_age >= prior_params$b_height_age_min &
                   true_params$b_height_age <= prior_params$b_height_age_max,
                   "✓ OK", "✗ OUTSIDE")))

cat(sprintf("b_height_sex: TRUE=%.1f, PRIOR: N(%.1f, %.1f)\n",
            true_params$b_height_sex,
            prior_params$b_height_sex_mean, prior_params$b_height_sex_sd))

cat(sprintf("sigma_height: TRUE=%.1f, PRIOR: U(0, %.1f) %s\n\n",
            true_params$sigma_height,
            prior_params$sigma_height_max,
            ifelse(true_params$sigma_height <= prior_params$sigma_height_max,
                   "✓ OK", "✗ OUTSIDE")))

# Weight model
cat("WEIGHT MODEL:\n")
cat(sprintf("a_weight: TRUE=%.1f, PRIOR: U(%.1f, %.1f) %s\n",
            true_params$a_weight,
            prior_params$a_weight_min, prior_params$a_weight_max,
            ifelse(true_params$a_weight >= prior_params$a_weight_min &
                   true_params$a_weight <= prior_params$a_weight_max,
                   "✓ OK", "✗ OUTSIDE")))

cat(sprintf("b_weight_height: TRUE=%.2f, PRIOR: U(%.2f, %.2f) %s ← PROBLEM!\n",
            true_params$b_weight_height,
            prior_params$b_weight_height_min, prior_params$b_weight_height_max,
            ifelse(true_params$b_weight_height >= prior_params$b_weight_height_min &
                   true_params$b_weight_height <= prior_params$b_weight_height_max,
                   "✓ OK", "✗ OUTSIDE")))

cat(sprintf("b_weight_age: TRUE=%.2f, PRIOR: U(%.2f, %.2f) %s\n",
            true_params$b_weight_age,
            prior_params$b_weight_age_min, prior_params$b_weight_age_max,
            ifelse(true_params$b_weight_age >= prior_params$b_weight_age_min &
                   true_params$b_weight_age <= prior_params$b_weight_age_max,
                   "✓ OK", "✗ OUTSIDE")))

cat(sprintf("b_weight_sex: TRUE=%.1f, PRIOR: N(%.1f, %.1f)\n",
            true_params$b_weight_sex,
            prior_params$b_weight_sex_mean, prior_params$b_weight_sex_sd))

cat(sprintf("sigma_weight: TRUE=%.1f, PRIOR: U(0, %.1f) %s\n",
            true_params$sigma_weight,
            prior_params$sigma_weight_max,
            ifelse(true_params$sigma_weight <= prior_params$sigma_weight_max,
                   "✓ OK", "✗ OUTSIDE")))

cat("\n=== DIAGNOSIS ===\n")
cat("Your b_weight_height prior caps at 0.3, but true value is 0.33!\n")
cat("This forces quap to start with impossible parameter combinations.\n")
cat("\nFIX: Change b_weight_height_max to at least 0.5\n")
