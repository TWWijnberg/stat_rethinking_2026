# Homework A05 - DAG Analysis and Model Selection
# Statistical Rethinking 2026

source(here::here("homework", "contributions", "A05", "scripts", "00_setup.R"))

# ============================================================================
# PART 1: Define the DAG
# ============================================================================

dag <- dagitty('dag {
  Sex -> Height
  Sex -> Weight
  Age -> Height
  Age -> Weight
  Height -> Weight
}')

coordinates(dag) <- list(
  x = c(Sex = 1, Age = 1, Height = 2, Weight = 3),
  y = c(Sex = 1, Age = 3, Height = 2, Weight = 2)
)

plot(dag)

# ============================================================================
# PART 2: Causal Analysis - What should we condition on?
# ============================================================================

# Scientific question: What is the TOTAL effect of Sex on Weight?
exposures(dag) <- "Sex"
outcomes(dag) <- "Weight"

# What adjustment sets give us the total effect?
cat("\n=== Adjustment Sets for Total Effect of Sex on Weight ===\n")
print(adjustmentSets(dag))
# Expected: { Age } - condition on Age only, NOT Height!
# Height is a MEDIATOR - conditioning on it blocks the indirect path

# What paths exist from Sex to Weight?
cat("\n=== Causal Paths from Sex to Weight ===\n")
print(paths(dag, from = "Sex", to = "Weight"))
# Direct: Sex -> Weight
# Indirect: Sex -> Height -> Weight

# What conditional independencies does this DAG imply?
cat("\n=== Implied Conditional Independencies ===\n")
print(impliedConditionalIndependencies(dag))

# ============================================================================
# PART 3: Model Selection
# ============================================================================

# For TOTAL effect: Weight ~ Sex + Age (no Height!)
# This captures both direct and indirect effects of Sex

# For DIRECT effect only: Weight ~ Sex + Age + Height
# This blocks the indirect path through Height

cat("\n=== Model Selection ===\n")
cat("To estimate TOTAL effect of Sex on Weight:\n")
cat("  - Condition on: Age\n")
cat("  - Do NOT condition on: Height (it's a mediator)\n")

# ============================================================================
# PART 4: True Parameters for Synthetic Data
# ============================================================================

true_params <- list(
  # Height model (needed to generate synthetic data)
  a_height = c(Female = 60, Male = 65),  # Intercept by sex (cm at age 0)
  b_height_age = 0.5,                     # cm per month
  sigma_height = 6,

  # Weight model
  a_weight = c(Female = -10, Male = -8), # Intercept by sex (kg when height=0)
  b_weight_height = 0.2,                  # kg per cm of height
  b_weight_age = 0.05,                    # kg per month (direct effect)
  sigma_weight = 5
)

# Calculate the TRUE total effect of Sex on Weight
# Total = Direct + Indirect
# Direct = a_weight[Male] - a_weight[Female] = -8 - (-10) = 2 kg
# Indirect = (a_height[Male] - a_height[Female]) * b_weight_height
#          = (65 - 60) * 0.2 = 1 kg
# Total = 2 + 1 = 3 kg
true_params$total_sex_effect <- 3

cat("\n=== True Total Effect of Sex on Weight ===\n")
cat(sprintf("Direct effect: %.1f kg\n",
            true_params$a_weight[2] - true_params$a_weight[1]))
cat(sprintf("Indirect effect (via height): %.1f kg\n",
            (true_params$a_height[2] - true_params$a_height[1]) * true_params$b_weight_height))
cat(sprintf("Total effect: %.1f kg\n", true_params$total_sex_effect))

# ============================================================================
# PART 5: Prior Parameters
# ============================================================================

prior_params <- list(
  a_mean = 5,           # Prior mean for sex-specific intercepts
  a_sd = 10,            # Prior SD for intercepts
  b_age_min = 0,        # Min effect of age on weight
  b_age_max = 0.3,      # Max effect of age on weight (kg/month)
  sigma_max = 15        # Max residual SD
)


cat("\nDAG analysis complete. Model ready for fitting.\n")
