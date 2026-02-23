source(here::here("homework", "contributions", "A06", "scripts", "00_setup.R"))

# --- Generate Synthetic Data -------------------------------------------------
data(foxes)
plot(foxes)

# --- Fit Model ---------------------------------------------------------------
data <- list(
  A = foxes$area,
  Abar = mean(foxes$area),
  G = foxes$groupsize,
  Gbar = mean(foxes$groupsize),
  F = foxes$avgfood,
  Fbar = mean(foxes$avgfood),
  W = foxes$weight
)


formula_quap <- alist(
  W ~ dnorm(mu, sigma),
  mu <- a + bF * (F-Fbar),
  a ~ dnorm(0, 1),
  bF ~ dnorm(0, 1),
  sigma ~ dexp(1)
)

fit_quap <- quap(formula_quap, data = data)

precis(fit_quap)

# it appears that the total causal effect of food on weight is almost 0! this is counterintuitive, because we would expect that more food leads to more eating and weight gain. My suspicion is that a higher groupsize can be sustained with more food, which negates the direct positive effect of food on weight. Let's build a model to estimate the direct effect to test this out.

formula_quap_direct <- alist(
  W ~ dnorm(mu, sigma),
  mu <- a + bF * (F-Fbar) + bG * (G-Gbar),
  a ~ dnorm(0, 1),
  bF ~ dnorm(0, 1),
  bG ~ dnorm(0, 1),
  sigma ~ dexp(1)
)


fit_quap_direct <- quap(formula_quap_direct, data = data)

precis(fit_quap_direct)

# --- Causal Effect via Posterior Simulation ----------------------------------
# Estimate do(X): causal effect of 1-unit increase in X on Y
# True effect is 0.5

post <- extract.samples(fit_quap_direct)

# Sample Z from data (marginalize over Z distribution)
n_sim <- 1e5
Gs <- sample(data$G, size = n_sim, replace = TRUE)

# Simulate Y for X = 0 (baseline)
W_F0 <- with(post,
  rnorm(n_sim, a + bF * 0 + bG * Gs, sigma)
)

# Simulate Y for X = 1 (+1 unit increase)
W_F1 <- with(post,
  rnorm(n_sim, a + bF * 1 + bG * Gs, sigma)
)

# Contrast: causal effect of do(X = 1) vs do(X = 0)
X_contrast <- W_F1 - W_F0

# Plot the causal effect distribution
dens(X_contrast, lwd = 4, col = 2,
     xlab = "Direct causal effect of 1-unit increase in food on weight",
     main = "Posterior distribution of do(X)")
abline(v = 0.5, lty = 2, lwd = 2)  # true effect
legend("topright", legend = c("Posterior", "True effect (0.5)"),
       col = c(2, 1), lty = c(1, 2), lwd = c(4, 2))

cat("\n--- Causal Effect Estimate ---\n")
cat("True causal effect of X on Y: 0.5\n")
cat("Posterior mean:", mean(X_contrast), "\n")
cat("89% CI:", PI(X_contrast, prob = 0.89), "\n")


# take a look at the direct causal effect of area on weight
formula_quap <- alist(
  W ~ dnorm(mu, sigma),
  mu <- a + bA * (A-Abar),
  a ~ dnorm(0, 1),
  bA ~ dnorm(0, 1),
  sigma ~ dexp(1)
)

fit_quap <- quap(formula_quap, data = data)

precis(fit_quap)

# not much!   let's look at the effect of area through food.

# take a look at the direct causal effect of area on weight
formula_quap <- alist(
  W ~ dnorm(mu, sigma),
  mu <- a + bA * (A-Abar) + bG * (G-Gbar),
  a ~ dnorm(0, 1),
  bA ~ dnorm(0, 1),
  bG ~ dnorm(0, 1),
  sigma ~ dexp(1)
)

fit_quap <- quap(formula_quap, data = data)

precis(fit_quap)

# and the model with all variables
formula_quap <- alist(
  W ~ dnorm(mu, sigma),
  mu <- a + bA * (A-Abar) + bG * (G-Gbar) + bF * (F-Fbar),
  a ~ dnorm(0, 1),
  bA ~ dnorm(0, 1),
  bG ~ dnorm(0, 1),
  bF ~ dnorm(0, 1),
  sigma ~ dexp(1)
)

fit_quap <- quap(formula_quap, data = data)

precis(fit_quap)
