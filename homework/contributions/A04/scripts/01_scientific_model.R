# Homework A04 - Scientific Model
# Statistical Rethinking 2026
#
# Step 1: Define the scientific/generative model with DAG,
# generate synthetic data, and inspect with visualizations

# ---- Setup ----
source(here::here("homework", "contributions", "A04", "scripts", "00_setup.R"))

# ---- Define DAG ----
# Create your DAG representing the causal structure
# Age -> Height (causal effect of interest)
# Sex -> Height (direct effect)
# Sex -> Age (e.g., different age distributions by sex in sample)
# Nutrition -> Height (unobserved confounder)
# Nutrition -> Age (unobserved confounder)

height_age_dag <- dagitty("dag {
  Age -> Height
  Sex -> Height
  Sex -> Age
  Nutrition -> Height
  Nutrition -> Age
  Age [exposure]
  Height [outcome]
  Sex [confounder]
  Nutrition [unobserved]
}")

# Plot the DAG
plot(height_age_dag)

# ---- Generate Synthetic Data ----
# TODO: Create synthetic age/height data for children
# Include sex as a confounder in the generative process
# This will help validate your statistical model before fitting real data

# Example structure for synthetic data generation:
# sim_height <- function(age, sex, beta_age, beta_sex, alpha, sd, n) {
#   # Generate height based on age and sex
#   # ...
# }

# For now, placeholder - you can build this based on your model
cat("Note: Add synthetic data generation for age/height model\n")

# ---- Visualize Synthetic Data ----
# Plot your synthetic data to verify it looks reasonable
# Separate by sex to see the confounding structure
