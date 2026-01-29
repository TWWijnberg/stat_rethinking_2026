# Homework A04 - Prior Checking
# Statistical Rethinking 2026
#
# Step 2: Prior predictive simulation, fit model on synthetic data,
# and inspect the fitted model

# ---- Setup ----
source(here::here("homework", "contributions", "A04", "scripts", "01_scientific_model.R"))

# ---- Define Statistical Model ----
# Model: Height as a function of Age
# Based on UK growth charts: children grow ~3-10 cm per year
# Birth height around 50 cm
# https://www.rcpch.ac.uk/resources/uk-who-growth-charts-2-18-years

height_model <- alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b_age * age,
    # you are born at around 50 cm
    a ~ dnorm(50, 20),
    # you grow 3 - 10 cm per year
    b_age ~ dunif(3, 10),
    # people vary by 3x sigma
    sigma ~ dunif(0, 10)
)

# ---- Prior Predictive Simulation ----
# Generate predictions from the prior to see if they make sense
n_sample <- 500
age_sample <- runif(n_sample, 0, 19)
a_prior_sample <- rnorm(n_sample, 50, 20)
b_age_prior_sample <- runif(n_sample, 3, 10)
sigma_prior_sample <- runif(n_sample, 0, 10)
height_prior_sample <- a_prior_sample + b_age_prior_sample * age_sample

prior_sample <- data.frame(age = age_sample, height = height_prior_sample)

# Visualize prior predictions
# Adjust priors until they look reasonable
ggplot(prior_sample, aes(x = age, y = height)) +
    geom_point() +
    xlim(0, 20) +
    ylim(0, 200) +
    labs(x = "Age", y = "Height", title = "Prior Predictive Simulation: Age vs Height")

# ---- Fit Model on Synthetic Data ----
# TODO: Once synthetic data is generated in script 01,
# fit the model here and check that you can recover the parameters
# This validates that your model can work before using real data

cat("Note: Add model fitting on synthetic data once available\n")
