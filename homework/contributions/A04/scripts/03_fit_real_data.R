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
# Model growth up to 13 years old
# Load Howell1 dataset
children <- Howell1

# Filter for children younger than 13 years old
children <- children[children$age < 13, ]
children$sex <- factor(ifelse(children$male == 1, "Male", "Female"),
                levels = c("Female", "Male"))  # Female=1, Male=2
children$sex_id <- as.integer(children$sex)  # Create index variable for ulam
children$male <- NULL  # Remove the original male column

# Convert age from years to months
children$age <- children$age * 12

ggplot(children, aes(x = age, y = height, color = sex)) +
    geom_point() +
    xlim(0, 13*12) +
    ylim(0, 200) +
    labs(x = "Age", y = "Height", color = "Sex",
         title = "Age vs Height - Children Only")

# ---- Fit Model ----
fitted_model <- ulam(height_model, data = children)

# ---- Extract Prior and Posterior ----
prior <- extract.prior(fitted_model, n = 100)
post <- extract.samples(fitted_model)

# ---- Posterior Predictive Checks ----
# Method 1: Visual check - compare actual vs predicted
age_seq <- seq(0, 19, length.out = 50)
mu <- link(fitted_model, data = data.frame(age = age_seq))
mu_mean <- apply(mu, 2, mean)

posterior_pred <- sim(fitted_model, data = children)

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

# ---- Total Causal Effect of Age on Weight ----
# Calculate the total effect of age on weight using posterior samples
# Total effect = Direct effect + Indirect effect through height
# Direct: b_weight_age (Age -> Weight)
# Indirect: b_height_age * b_weight_height (Age -> Height -> Weight)

# Extract posterior samples (already done at line 46, but refresh for clarity)
post <- extract.samples(fitted_model, n = 10000)

# Calculate total effect for each posterior sample
# Effect is in kg per month of age
total_effect_age <- post$b_weight_age + (post$b_height_age * post$b_weight_height)

# Summary statistics
cat("\n=== Total Causal Effect of Age on Weight ===\n")
cat(sprintf("Mean total effect: %.3f kg/month (%.2f kg/year)\n",
            mean(total_effect_age), mean(total_effect_age) * 12))
cat(sprintf("89%% credible interval: [%.3f, %.3f] kg/month\n",
            quantile(total_effect_age, 0.055),
            quantile(total_effect_age, 0.945)))

# Break down into components
cat("\n--- Effect Decomposition ---\n")
cat(sprintf("Direct effect (b_weight_age): %.3f kg/month\n",
            mean(post$b_weight_age)))
cat(sprintf("Indirect effect (b_height_age × b_weight_height): %.3f kg/month\n",
            mean(post$b_height_age * post$b_weight_height)))
cat(sprintf("Proportion through height: %.1f%%\n",
            100 * mean(post$b_height_age * post$b_weight_height) / mean(total_effect_age)))

# Visualize the posterior distribution
hist(total_effect_age,
     breaks = 50,
     main = "Posterior Distribution: Total Effect of Age on Weight",
     xlab = "Effect (kg per month)",
     col = "skyblue",
     border = "white")
abline(v = mean(total_effect_age), col = "red", lwd = 2, lty = 2)
abline(v = quantile(total_effect_age, c(0.055, 0.945)),
       col = "red", lwd = 1, lty = 3)
