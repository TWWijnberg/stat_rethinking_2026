# A05 Implementation Plan: Sex Effect on Weight in Children

## Goal
Estimate the **total causal effect of sex on weight** in children using a DAG-driven Bayesian workflow. Validate the approach on synthetic data before applying to real data.

## Design Philosophy
**Analysis code optimizes for transparency and reproducibility, not abstraction.**

- Keep analytical decisions visible (model formulas, priors, interpretations inline)
- Extract functions only when they genuinely reduce complexity
- Synthetic and real analyses have *different purposes* - don't force identical structure

---

## Folder Structure

```
homework/contributions/A05/
├── docs/
│   └── implementation_plan.md    # This file
├── scripts/
│   ├── 00_setup.R                # Libraries, source helpers
│   ├── 01_helpers.R              # Only 4 functions (not 12!)
│   ├── 02_dag_analysis.R         # DAG, causal reasoning, model selection
│   ├── 03_synthetic_analysis.R   # Validate model on synthetic data
│   └── 04_real_analysis.R        # Estimate effects on Howell1 data
└── outputs/
    ├── figures/
    └── results/
```

---

## Script Contents

### 00_setup.R (~15 lines)
```r
# Homework A05 - Setup
# Statistical Rethinking 2026

# ---- Libraries ----
library(rethinking)
library(here)
library(tidyverse)
library(cmdstanr)
library(dagitty)
library(patchwork)

# ---- Options ----
options(mc.cores = parallel::detectCores())

# ---- Source Helpers ----
source(here::here("homework", "contributions", "A05", "scripts", "01_helpers.R"))

cat("Setup complete.\n")
```

---

### 01_helpers.R (~80 lines) - ONLY 4 FUNCTIONS

**Why only 4?** Most "helper functions" in the original plan would hide analytical decisions. Keep model fitting, residual calculation, and effect summaries inline where you can see them.

#### Function 1: `prepare_data(data, source)`
**Why extract:** Genuinely handles format differences between Howell1 and synthetic data.
```r
prepare_data <- function(data, source = "unknown") {
  d <- data

  # Howell1 has 'male' column (0/1), synthetic has 'sex' factor
  if ("male" %in% names(d)) {
    d$sex <- factor(ifelse(d$male == 1, "Male", "Female"),
                    levels = c("Female", "Male"))
    d$male <- NULL
  }

  # Ensure sex is a factor with correct level order
  if (!is.factor(d$sex)) {
    d$sex <- factor(d$sex, levels = c("Female", "Male"))
  }

  # Create sex_id for index coding (Female=1, Male=2)
  d$sex_id <- as.integer(d$sex)

  d$source <- source
  return(d)
}
```

#### Function 2: `sim_children(n, params)`
**Why extract:** Core scientific model that defines the data-generating process. Worth testing independently.
```r
sim_children <- function(n = 200, params) {
  # Generate in causal order: Age, Sex -> Height -> Weight

  age <- runif(n, 0, 156)  # 0 to 13 years in months
  sex <- factor(sample(c("Female", "Male"), n, replace = TRUE),
                levels = c("Female", "Male"))
  sex_id <- as.integer(sex)

  # Height depends on Age and Sex
  height <- params$a_height[sex_id] +
            params$b_height_age * age +
            rnorm(n, 0, params$sigma_height)

  # Weight depends on Height, Age, and Sex
  weight <- params$a_weight[sex_id] +
            params$b_weight_height * height +
            params$b_weight_age * age +
            rnorm(n, 0, params$sigma_weight)

  data.frame(age = age, sex = sex, sex_id = sex_id,
             height = height, weight = weight)
}
```

#### Function 3: `plot_predictions(data, fit, outcome, title)`
**Why extract:** Complex enough to warrant abstraction, used for both height and weight outcomes.
```r
plot_predictions <- function(data, fit, outcome = "weight", title = NULL) {
  # Generate posterior predictions
  post_pred <- sim(fit, data = data)
  pred_mean <- apply(post_pred, 2, mean)
  pred_PI <- apply(post_pred, 2, PI, prob = 0.89)

  # Create plot dataframe
  plot_df <- data.frame(
    observed = data[[outcome]],
    predicted = pred_mean,
    lower = pred_PI[1, ],
    upper = pred_PI[2, ],
    sex = data$sex
  )

  ggplot(plot_df, aes(x = observed, y = predicted, color = sex)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.6) +
    geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.2, width = 0) +
    labs(x = paste("Observed", outcome),
         y = paste("Predicted", outcome),
         title = title %||% paste("Posterior Predictive Check:", outcome)) +
    theme_minimal() +
    coord_equal()
}
```

#### Function 4: `plot_caterpillar(fit, true_values = NULL, title = NULL)`
**Why extract:** Visualization logic is complex; scientific interpretation stays in calling code.
```r
plot_caterpillar <- function(fit, true_values = NULL, title = NULL) {
  post_summary <- precis(fit, depth = 2, prob = 0.89)

  plot_df <- data.frame(
    parameter = rownames(post_summary),
    estimate = post_summary$mean,
    lower = post_summary$`5.5%`,
    upper = post_summary$`94.5%`
  )

  p <- ggplot(plot_df, aes(x = estimate, y = parameter)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
    labs(x = "Parameter value", y = NULL,
         title = title %||% "Parameter Estimates (89% CI)") +
    theme_minimal()

  # Add true values if provided (for synthetic data validation)
  if (!is.null(true_values)) {
    true_df <- data.frame(
      parameter = names(true_values),
      true_value = unlist(true_values)
    )
    p <- p + geom_point(data = true_df, aes(x = true_value, y = parameter),
                        color = "red", shape = 4, size = 4, stroke = 2)
  }

  p
}
```

**Functions NOT extracted (keep inline):**
- Model fitting - use `ulam()` directly so you see the model
- Residual calculation - 3 lines of code, clearer inline
- Causal effect summary - just `cat()` statements, interpretation is the point

---

### 02_dag_analysis.R (~100 lines)

**Purpose:** Define DAG, run causal checks, determine what model to fit.

```r
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
  x = c(Sex = 0, Age = 2, Height = 1, Weight = 1),
  y = c(Sex = 0, Age = 0, Height = 1, Weight = 2)
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

# ============================================================================
# PART 6: Model Formula (for total effect)
# ============================================================================

# Model conditioning on Age only (not Height) to get TOTAL effect
weight_model <- alist(
  weight ~ dnorm(mu, sigma),
  mu <- a[sex_id] + b_age * age,
  vector[2]:a ~ dnorm(5, 10),     # Sex-specific intercepts
  b_age ~ dunif(0, 0.3),          # kg per month
  sigma ~ dunif(0, 15)
)

cat("\nDAG analysis complete. Model ready for fitting.\n")
```

---

### 03_synthetic_analysis.R (~120 lines)

**Purpose:** Validate the model recovers known parameters from synthetic data.

**Key difference from original plan:** Inline code for fitting, residuals, and interpretation. Functions only for data prep, simulation, and plotting.

```r
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
```

---

### 04_real_analysis.R (~100 lines)

**Purpose:** Apply validated model to Howell1 data and interpret causal effects.

**Key difference from synthetic:** Focus on interpretation and uncertainty, not validation.

```r
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
```

---

## Critical Implementation Details (Gotchas)

### 1. Index Coding Convention
- `sex_id = 1` = Female, `sex_id = 2` = Male
- Use `factor(sex, levels = c("Female", "Male"))` then `as.integer()`

### 2. Data List for ulam
```r
# ulam wants a list, not a data frame with extra columns
d_fit <- list(
  weight = d$weight,
  age = d$age,
  sex_id = d$sex_id
)
```

### 3. True Values for Parameter Recovery
The model estimates **marginal** effects (not conditioning on height). The "true" intercepts are:
```r
# True a[1] = a_weight[1] + b_weight_height * a_height[1]
# NOT just a_weight[1]
```

### 4. Age Units
- Howell1 age is in years
- Convert to months: `age <- age * 12`
- Keep units consistent with synthetic data

---

## Verification Steps

1. **DAG checks**: `adjustmentSets(dag)` recommends conditioning on Age only
2. **Synthetic validation**: True total effect (3 kg) falls within 89% CI
3. **MCMC diagnostics**: Rhat < 1.01, ESS > 400 for all parameters
4. **Posterior predictive**: Predicted vs observed shows reasonable fit
5. **Effect estimate**: Total effect should be positive (males heavier) with reasonable uncertainty

---

## Estimated Outcome

| Script | Lines | Purpose |
|--------|-------|---------|
| 00_setup.R | 15 | Libraries |
| 01_helpers.R | 80 | 4 functions only |
| 02_dag_analysis.R | 100 | DAG, causal reasoning, model |
| 03_synthetic_analysis.R | 120 | Validation (inline fitting) |
| 04_real_analysis.R | 100 | Estimation (inline fitting) |
| **Total** | **~415** | vs A04's ~982 lines |

**Key difference from original plan:** 80 lines of helpers instead of 220. Analytical decisions stay visible in the analysis scripts.

---

## Implementation Order

1. ~~Create folder structure~~ (done)
2. ~~Copy plan to docs/~~ (done)
3. Write `00_setup.R`
4. Write `01_helpers.R` (4 functions)
5. Write `02_dag_analysis.R`
6. Write `03_synthetic_analysis.R` - verify parameter recovery
7. Write `04_real_analysis.R` - estimate causal effect
8. Run end-to-end, save key figures
9. Commit with message describing the analysis
