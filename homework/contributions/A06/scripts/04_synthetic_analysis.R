# =============================================================================
# 04_synthetic_analysis.R - Synthetic Data Validation
# =============================================================================
# Purpose: Generate synthetic data, fit model, validate parameter recovery
# =============================================================================

source(here::here("homework", "contributions", "A06", "scripts", "00_setup.R"))

# --- Generate Synthetic Data -------------------------------------------------
n <- 1e4
A <- rnorm(n)
Z <- rnorm(n, mean = 0.5 * A)
X <- rnorm(n, mean = 0.5 * Z)
Y <- rnorm(n, mean = 0.5 * X + 0.5 * Z + 0.5 * A)

data <- data.frame(A, Z, X, Y)

plot(data)

# --- Fit Model ---------------------------------------------------------------

formula_quap <- alist(
  Y ~ dnorm(mu, sigma),
  mu <- a + bX * X + bZ * Z,
  a ~ dnorm(0, 1),
  bX ~ dnorm(0, 1),
  bZ ~ dnorm(0, 1),
  sigma ~ dexp(1)
)

fit_quap <- quap(formula_quap, data = data)

precis(fit_quap)


formula_quap_Z_on_Y <- alist(
  Y ~ dnorm(mu, sigma),
  mu <- a + bZ * Z + bA * A,
  a ~ dnorm(0, 1),
  bZ ~ dnorm(0, 1),
  bA ~ dnorm(0, 1),
  sigma ~ dexp(1)
)
fit_Z_on_Y_quap <- quap(formula_quap_Z_on_Y, data = data)
precis(fit_Z_on_Y_quap)

formula_quap_all <- alist(
  Y ~ dnorm(mu, sigma),
  mu <- a + bZ * Z + bA * A+ bX * X,
  a ~ dnorm(0, 1),
  bZ ~ dnorm(0, 1),
  bA ~ dnorm(0, 1),
  bX ~ dnorm(0, 1),
  sigma ~ dexp(1)
)
fit_all_quap <- quap(formula_quap_all, data = data)
precis(fit_all_quap)




# --- Causal Effect via Posterior Simulation ----------------------------------
# Estimate do(X): causal effect of 1-unit increase in X on Y
# True effect is 0.5

post <- extract.samples(fit_quap)

# Sample Z from data (marginalize over Z distribution)
n_sim <- 1e5
Zs <- sample(data$Z, size = n_sim, replace = TRUE)

# Simulate Y for X = 0 (baseline)
Y_X0 <- with(post,
  rnorm(n_sim, a + bX * 0 + bZ * Zs, sigma)
)

# Simulate Y for X = 1 (+1 unit increase)
Y_X1 <- with(post,
  rnorm(n_sim, a + bX * 1 + bZ * Zs, sigma)
)

# Contrast: causal effect of do(X = 1) vs do(X = 0)
X_contrast <- Y_X1 - Y_X0

# Plot the causal effect distribution
dens(X_contrast, lwd = 4, col = 2,
     xlab = "Causal effect of 1-unit increase in X on Y",
     main = "Posterior distribution of do(X)")
abline(v = 0.5, lty = 2, lwd = 2)  # true effect
legend("topright", legend = c("Posterior", "True effect (0.5)"),
       col = c(2, 1), lty = c(1, 2), lwd = c(4, 2))

cat("\n--- Causal Effect Estimate ---\n")
cat("True causal effect of X on Y: 0.5\n")
cat("Posterior mean:", mean(X_contrast), "\n")
cat("89% CI:", PI(X_contrast, prob = 0.89), "\n")



# study the collider effect of X on A
# make A independent of Z and Y
n <- 1e4
A <- rnorm(n)
Z <- rnorm(n)
X <- rnorm(n) + Z
Y <- rnorm(n) + X + Z + A

data <- data.frame(A, Z, X, Y)
plot(data)

formula_quap_no_collider <- alist(
  A ~ dnorm(mu, sigma),
  mu <- a + bX * X,
  a ~ dnorm(0, 1),
  bX ~ dnorm(0, 1),
  sigma ~ dexp(1)
)
no_collider <- quap(formula_quap_no_collider, data = data)

precis(no_collider)

formula_quap_collider <- alist(
  A ~ dnorm(mu, sigma),
  mu <- a + bX * X + bY * Y,
  a ~ dnorm(0, 1),
  bX ~ dnorm(0, 1),
  bY ~ dnorm(0, 1),
  sigma ~ dexp(1)
)
collider <- quap(formula_quap_collider, data = data)

# we've created a negative estimate now for X on A!
precis(collider)
