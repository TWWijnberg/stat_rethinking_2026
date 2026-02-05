# Factor Variables in the Rethinking Package

This document describes the preferred approach for working with categorical (factor) variables when using the `rethinking` package and `ulam()` for Bayesian modeling.

## The Preferred Approach

**Use factors with positive integer levels (1, 2, 3, ...) and descriptive labels.**

### Creating Factors

```r
# Preferred: Integer levels with descriptive labels
sex <- factor(c(1, 2, 1, 2, 1),
              levels = c(1, 2),
              labels = c("Female", "Male"))

# The factor stores integers internally, but displays labels
as.integer(sex)  # Returns: 1 2 1 2 1
print(sex)       # Displays: Female Male Female Male Female
```

### Why This Approach?

1. **Direct use in model formulas**: The factor can be used directly as an index in `ulam()` models without creating a separate `_id` variable.

2. **Readable output**: Parameter names in model output use the descriptive labels, making interpretation easier.

3. **Consistent with rethinking conventions**: McElreath's examples in Statistical Rethinking use this pattern.

4. **Self-documenting**: The factor carries both the numeric values needed for indexing and the human-readable labels.

## Model Syntax

### Preferred (using factor directly)

```r
# Data preparation
d$sex <- factor(d$male + 1, levels = c(1, 2), labels = c("Female", "Male"))

# Data list for ulam
d_fit <- list(
  weight = d$weight,
  age = d$age,
  sex = as.integer(d$sex)  # Convert to integer for Stan
)

# Model formula
fit <- ulam(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a[sex] + b_age * age,
    a[sex] ~ dnorm(5, 10),      # Prior on sex-specific intercepts
    b_age ~ dunif(0, 0.3),
    sigma ~ dunif(0, 15)
  ),
  data = d_fit
)
```

### Avoid (creating separate index variable)

```r
# Less preferred: Creating a separate sex_id variable
d$sex_id <- as.integer(d$sex)

d_fit <- list(
  weight = d$weight,
  age = d$age,
  sex_id = d$sex_id
)

fit <- ulam(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a[sex_id] + b_age * age,
    vector[2]:a ~ dnorm(5, 10),  # Old syntax
    b_age ~ dunif(0, 0.3),
    sigma ~ dunif(0, 15)
  ),
  data = d_fit
)
```

## Key Differences in Prior Syntax

| Old Style | Preferred Style |
|-----------|-----------------|
| `vector[2]:a ~ dnorm(5, 10)` | `a[sex] ~ dnorm(5, 10)` |

The preferred syntax `a[sex] ~ dnorm(5, 10)`:
- Is more readable
- Automatically infers the number of levels from the data
- Produces clearer parameter names in output

## Converting Existing Data

### From binary (0/1) coding

```r
# Howell1 dataset has 'male' column (0=Female, 1=Male)
d$sex <- factor(d$male + 1, levels = c(1, 2), labels = c("Female", "Male"))
```

### From character strings

```r
# Convert character to proper factor
sex_int <- ifelse(d$sex == "Male", 2, 1)
d$sex <- factor(sex_int, levels = c(1, 2), labels = c("Female", "Male"))
```

## Practical Example

```r
library(rethinking)

# Load data
data(Howell1)
d <- Howell1[Howell1$age < 13, ]  # Children only

# Create factor with integer levels and labels
d$sex <- factor(d$male + 1, levels = c(1, 2), labels = c("Female", "Male"))

# Prepare data list
d_fit <- list(
  weight = d$weight,
  age = d$age * 12,              # Convert years to months
  sex = as.integer(d$sex)
)

# Fit model
fit <- ulam(
  alist(
    weight ~ dnorm(mu, sigma),
    mu <- a[sex] + b_age * age,
    a[sex] ~ dnorm(5, 10),
    b_age ~ dunif(0, 0.3),
    sigma ~ dunif(0, 15)
  ),
  data = d_fit,
  chains = 4
)

# View results - parameters will be named a[1] and a[2]
precis(fit, depth = 2)
```

## Summary

| Aspect | Recommendation |
|--------|----------------|
| Factor levels | Use positive integers (1, 2, 3, ...) |
| Factor labels | Use descriptive strings ("Female", "Male") |
| Model data | Pass `as.integer(factor_var)` to ulam |
| Prior syntax | Use `a[var_name] ~ prior()` not `vector[n]:a` |
| Separate ID variable | Avoid creating `_id` columns |

This approach keeps code clean, self-documenting, and consistent with rethinking conventions.
