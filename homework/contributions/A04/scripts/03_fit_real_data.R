# Homework A04 - Fit Real Data
# Statistical Rethinking 2026
# https://www.youtube.com/watch?v=GIdwLrW2nNo
# Step 3: Load real data, fit the model, and perform diagnostics

# ---- Setup ----
source(here::here("homework", "contributions", "A04", "scripts", "02_prior_checking.R"))

# ---- Load Data ----
data(Howell1)

# ---- Explore Full Dataset ----
ggplot(Howell1, aes(x = age, y = height)) +
    geom_point() +
    xlim(0, 90) +
    ylim(50, 200) +
    labs(x = "Age", y = "Height", title = "Age vs Height - Full Dataset")

# ---- Filter to Children ----
# Growth stops around 25 years old
# Model growth up to 18 years old
children <- Howell1 %>% filter(age <= 18)

ggplot(children, aes(x = age, y = height)) +
    geom_point() +
    xlim(0, 20) +
    ylim(0, 200) +
    labs(x = "Age", y = "Height", title = "Age vs Height - Children Only")

# ---- Fit Model ----
fitted_model <- quap(height_model, data = list(age = children$age, height = children$height))

# ---- Extract Prior and Posterior ----
prior <- extract.prior(fitted_model, n = 100)
post <- extract.samples(fitted_model)

# ---- Posterior Predictive Checks ----
# Method 1: Visual check - compare actual vs predicted
age_seq <- seq(0, 19, length.out = 50)
mu <- link(fitted_model, data = data.frame(age = age_seq))
mu_mean <- apply(mu, 2, mean)

posterior_pred <- sim(fitted_model, data = list(age = children$age))

plot(children$age, children$height,
     xlab = "Age", ylab = "Height",
     main = "Posterior Predictive Check")
for (i in 1:20) {
    points(children$age, posterior_pred[i, ], col = "gray", pch = 16)
}
lines(age_seq, mu_mean, col = "red", lwd = 2)

# Method 2: Check specific statistics
# Compare observed vs predicted distribution of heights
observed_mean <- mean(children$height)
predicted_means <- apply(posterior_pred, 1, mean)
hist(predicted_means, main = "Predicted vs Observed Mean Height")
abline(v = observed_mean, col = "red", lwd = 2)

# Method 3: Check residuals
residuals <- children$height - mu_mean
plot(children$age, residuals, main = "Residuals vs Age")
abline(h = 0, lty = 2)

# ---- Compare Prior and Posterior ----
ps <- par("bty")
par(bty = "n")
plot(precis(prior, 2), col.ci = "gray", bty = "n")
plot(precis(post, 2), add = TRUE, pch = 16)
par(bty = ps)

# Check posterior for intercept
hist(post$a)

# ---- Model Summary ----
# Note: The kids grow about 4.5 cm per year on average,
# lower than UK growth charts suggest.
# This might be due to malnutrition - data is from a
# Botswana community (August 1967 - May 1969)
summary(fitted_model)
plot(fitted_model)
