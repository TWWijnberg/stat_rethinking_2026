# Stan/ulam Technical Reference

*Practical lessons for working with Stan through rethinking::ulam and quap*

---

## Index Coding vs Indicator Coding for Categorical Variables

**The Key Difference:**

When modeling categorical variables (like sex), you have two parameterization choices:

### Index Coding (required for ulam/Stan)

```r
# Define in model formula
height ~ dnorm(mu, sigma),
mu <- a[sex_id] + b_age * age,
vector[2]:a ~ dnorm(60, 10)

# Data preparation
d$sex_id <- as.integer(factor(d$sex, levels = c("Female", "Male")))
# sex_id = 1 for Female, sex_id = 2 for Male
```

**How it works:** Each category gets its own intercept. `a[1]` is the Female intercept, `a[2]` is the Male intercept.

**Advantages:**
- Required for Stan/ulam (Stan doesn't support indicator coding directly)
- More flexible for complex models
- Treats categories symmetrically
- Natural for hierarchical models

### Indicator Coding (works with quap)

```r
# Define in model formula
height ~ dnorm(mu, sigma),
mu <- a + b_sex * sex_male + b_age * age,
a ~ dnorm(60, 10),
b_sex ~ dnorm(0, 10)

# Data preparation
d$sex_male <- as.integer(d$sex == "Male")
# sex_male = 0 for Female, sex_male = 1 for Male
```

**How it works:** `a` is the baseline (Female) intercept. `b_sex` is the difference (Male - Female).

**Advantages:**
- Simpler for two-level factors
- Difference between categories is directly interpretable
- Familiar to those coming from classical regression

**Converting between parameterizations:**
- Index → Indicator: `baseline = a[1]`, `difference = a[2] - a[1]`
- Indicator → Index: `a[1] = baseline`, `a[2] = baseline + difference`

---

## When to Use quap vs ulam

### quap (Quadratic Approximation)

**Use when:**
- Model is simple (single outcome, linear relationships)
- You want fast iteration during development
- Posterior is approximately Gaussian (most parameters)
- You're doing prior predictive checks

**Fails when:**
- Multivariate models where one outcome predicts another (e.g., height → weight)
- Non-Gaussian posteriors (boundaries, skew)
- Complex hierarchical structures
- Large number of parameters

**Example failure case from A04:**
```r
# This struggles with quap because height is both outcome and predictor
height ~ dnorm(mu_height, sigma_height)
weight ~ dnorm(mu_weight, sigma_weight)
mu_weight <- a + b_height * height + b_age * age
```

The quadratic approximation can't handle the dependency structure well.

### ulam (Stan MCMC)

**Use when:**
- Multivariate models with dependencies between outcomes
- You need reliable parameter estimates
- Models with complex priors or constraints
- Final results for publication/reporting

**Trade-offs:**
- Slower (minutes vs seconds for quap)
- Requires proper Stan syntax (index coding)
- More diagnostics to check (R-hat, ESS, divergences)
- Models must be defined consistently for caching to work

---

## Development Pattern: DEV_MODE Toggle

**The Problem:** ulam is slow during development when you're iterating on model structure.

**The Solution:** Use a toggle to switch between quap (fast) and ulam (reliable).

```r
DEV_MODE <- TRUE  # Set to FALSE for final run

if (DEV_MODE) {
  cat("\n=== Using quap (fast approximation) ===\n")
  fitted_model <- quap(model_quap, data = d, start = start_values)
} else {
  cat("\n=== Using ulam (full MCMC) ===\n")
  fitted_model <- ulam(model_ulam, data = d)
}
```

**Important:** Keep both model formulas defined. quap uses indicator coding, ulam uses index coding.

**When to switch:**
- Development phase: Use quap if model structure allows
- Validation phase: Switch to ulam to verify parameter recovery
- Final analysis: Always use ulam for reporting results

---

## Stan Model Caching

**The Problem:** Stan compiles models to C++ before running. Re-compilation is slow.

**How caching works:** ulam caches compiled models based on the model formula. If the formula changes, it recompiles.

**Key pattern: Define models once**

```r
# Define model formulas wrapped in if(!exists()) to prevent regeneration
if (!exists("height_model_ulam")) {
  height_model_ulam <- eval(bquote(alist(
    height ~ dnorm(mu, sigma),
    mu <- a[sex_id] + b_age * age,
    vector[2]:a ~ dnorm(.(prior_params$a_mean), .(prior_params$a_sd)),
    b_age ~ dunif(.(prior_params$b_min), .(prior_params$b_max)),
    sigma ~ dunif(0, .(prior_params$sigma_max))
  )))
}
```

**Why this matters:**
- Define models in separate script (e.g., `02_prior_checking.R`)
- Source that script in subsequent scripts
- Model definition persists across scripts without regeneration
- Stan can reuse cached compilation

**What invalidates cache:**
- Changing model formula (even whitespace)
- Changing priors (use `bquote` to substitute values)
- Restarting R session

---

## Data Preparation for ulam

**Key principle:** Only include variables that ulam needs. Clean data prevents warnings.

### Common issues and fixes

**Problem: Character variables cause warnings**
```r
# Bad: d contains unused character/factor columns
fitted_model <- ulam(model, data = d)
# Warning: unknown variables...
```

**Solution: Create clean data frame with only needed variables**
```r
# Good: Only include variables in model formula
d_fit <- d[, c("age", "sex_id", "height", "weight")]
fitted_model <- ulam(model, data = d_fit)
```

**Problem: Factor levels not as expected**
```r
# Bad: Factor levels don't match sex_id
d$sex <- factor(d$sex)  # Creates arbitrary order
d$sex_id <- as.integer(d$sex)  # Sex_id might not be 1=F, 2=M
```

**Solution: Explicitly set factor levels**
```r
# Good: Control factor level order
d$sex <- factor(d$sex, levels = c("Female", "Male"))
d$sex_id <- as.integer(d$sex)  # Now sex_id is definitely 1=F, 2=M
```

---

## Prior Alignment with Parameters

**The Problem:** Priors that don't match the parameter scale cause issues.

**Example from A04: Weight model intercept**

The weight model is:
```r
weight <- a_weight[sex_id] + b_weight_height * height + b_weight_age * age
```

If height is ~80cm and b_weight_height is ~0.2, then `b_weight_height * height = 16kg`.

For a child weighing 15kg, `a_weight` needs to be negative (~-1kg) to balance the equation.

**Wrong prior:**
```r
a_weight ~ dnorm(0, 10)  # Expects positive intercepts
# Results in boundary issues, poor sampling
```

**Correct prior:**
```r
a_weight ~ dunif(-35, 5)  # Allows negative intercepts
# Matches the parameter space where the model operates
```

**General principle:** Understand what each parameter represents given the other parameters. Don't just use default Normal(0, 1) priors.

---

## Parameter Recovery Workflow

**Purpose:** Verify your model can find known parameters before fitting real data.

**Steps:**

1. **Define true parameters**
```r
true_params <- list(
  a_height = c(60, 65),
  b_height_age = 0.5,
  sigma_height = 6,
  # ... etc
)
```

2. **Generate synthetic data**
```r
sim_data <- function(n = 200, params = true_params) {
  # Follow DAG structure
  age <- runif(n, 0, 156)
  sex_id <- sample(1:2, n, replace = TRUE)
  height <- params$a_height[sex_id] + params$b_height_age * age +
            rnorm(n, 0, params$sigma_height)
  # ... etc
}
```

3. **Fit model to synthetic data**
```r
fitted <- ulam(model, data = sim_data())
```

4. **Check if true parameters are within credible intervals**
```r
post_summary <- precis(fitted, prob = 0.89, depth = 2)
# Compare post_summary with true_params
# True values should be within 89% CI
```

5. **Visualize with caterpillar plot**
```r
ggplot(recovery_df, aes(x = estimate, y = parameter)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper)) +
  geom_point(aes(x = true_value), color = "red", shape = 4)
```

**What to expect:**
- Well-specified model: True values within CI for ~89% of parameters
- If recovery fails: Check priors, model formula, or data generation

---

## Computing Total Causal Effects

**The concept:** Total effect = Direct effect + Indirect effects through mediators

**Example from A04: Age → Weight**

DAG structure:
```
Age → Height → Weight
Age → Weight (direct)
```

Total effect calculation:
```r
# Extract posterior samples
post <- extract.samples(fitted_model, n = 10000)

# Direct path: Age → Weight
direct_effect <- post$b_weight_age

# Indirect path: Age → Height → Weight
indirect_effect <- post$b_height_age * post$b_weight_height

# Total effect
total_effect <- direct_effect + indirect_effect

# Summarize with uncertainty
mean(total_effect)
quantile(total_effect, c(0.055, 0.945))
```

**Key insight:** By computing effects on posterior samples, uncertainty in all parameters propagates to the total effect estimate.

---

## Common Errors and Solutions

### "Non-finite likelihood" from quap

**Cause:** Starting values produce impossible predictions (e.g., negative sigma, out-of-range predictions).

**Solution:** Provide explicit start values in middle of prior range.
```r
start_values <- list(
  a = 60,           # Middle of prior
  b_age = 0.5,
  sigma = 6
)
fitted <- quap(model, data = d, start = start_values)
```

### "Initial value outside bounds" from ulam

**Cause:** Priors don't cover parameter space well.

**Solution:** Check prior predictive simulation, widen priors if needed.

### Stan warnings about divergent transitions

**Cause:** Posterior geometry is challenging (e.g., correlations, boundaries).

**Solutions:**
1. Reparameterize model
2. Use stronger priors to regularize
3. Check if model is identified (too many parameters for data)

### Cached model not updating

**Cause:** Model formula hasn't changed, so Stan uses cached version.

**Solution:** Either:
- Change formula slightly (add comment, rename variable)
- Restart R session to clear cache
- Delete Stan cache directory manually

---

*Document version: 1.0 | Created: 2026-02-03 | Capturing A04 technical lessons*
