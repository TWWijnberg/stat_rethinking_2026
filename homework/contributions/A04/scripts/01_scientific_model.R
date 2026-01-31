# Homework A04 - Scientific Model
# Statistical Rethinking 2026
# https://www.youtube.com/watch?v=GIdwLrW2nNo
# Step 1: Define the scientific/generative model with DAG,
# generate synthetic data, and inspect with visualizations

# Using the Howell1 dataset, consider only the people younger than 13 years
# old. In this sample, estimate the causal effect of each month of growth on
# weight. Be sure to perform a prior predictive simulation to check and justify
# your priors. Also try to perform a validation of your model on synthetic data.


# ---- Setup ----
source(here::here("homework", "contributions", "A04", "scripts", "00_setup.R"))

# ---- Define DAG ----
# Create your DAG representing the causal structure for influencing 
# the weight of children

# Most intuitive variable to consider:
# Height -> Weight (taller children tend to weigh more)
# Age -> Weight (You get heavier as you get older (holding height constant))
# Sex -> Weight (boys are heavier)

# Age -> Height (older children tend to be taller)
# Sex -> Height (boys are taller)

# Exclude the less intuitive but plausible effects for simplicity:
# Sex -> Age (e.g., different age distributions by sex in sample)
# Nutrition -> Height (unobserved confounder)
# Nutrition -> Age (unobserved confounder)

weight_age_dag <- dagitty("dag {
  Height -> Weight
  Age -> Weight
  Sex -> Weight
  Age -> Height
  Sex -> Height
  }")

  coordinates(weight_age_dag) <- list(x = c(Height = 2, Weight = 3, Age = 1, Sex = 1),
  y = c(Height = 2, Weight = 2, Age = 1, Sex = 3))
exposures(weight_age_dag) <- "Age"
outcomes(weight_age_dag) <- "Weight"
adjustmentSets(weight_age_dag, effect = "direct")

# Plot the DAG
plot(weight_age_dag)

# ---- Generate Synthetic Data ----
# Function to generate synthetic data following the DAG structure
# Age and Sex are exogenous (no parents)
# Height depends on Age and Sex
# Weight depends on Height, Age, and Sex

sim_children <- function(n = 200,
                        # Height model parameters
                        h_intercept = 50,
                        h_age = 0.7,
                        h_sex = 5,
                        h_sigma = 10,
                        # Weight model parameters
                        w_intercept = -10,
                        w_height = 0.33,
                        w_age = 0.05,
                        w_sex = 2,
                        w_sigma = 3) {

  # Exogenous variables (no parents in DAG)
  age <- runif(n, 0, 156)  # age in months (0 to 13 years)
  sex <- factor(sample(c("Male", "Female"), n, replace = TRUE),
                levels = c("Male", "Female"))

  # Height = f(Age, Sex)
  height <- h_intercept +
    h_age * age +
    h_sex * (sex == "Male") +
    rnorm(n, 0, h_sigma)

  # Weight = f(Height, Age, Sex)
  weight <- w_intercept +
    w_height * height +
    w_age * age +
    w_sex * (sex == "Male") +
    rnorm(n, 0, w_sigma)

  data.frame(age = age, sex = sex, height = height, weight = weight)
}

# Generate synthetic data
set.seed(42)
d_sim <- sim_children(n = 200)

# ---- Visualize Synthetic Data ----
# Plot your synthetic data to verify it looks reasonable
# Separate by sex to see the confounding structure

# Age vs Height
p1 <- ggplot(d_sim, aes(x = age, y = height, color = sex)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Age → Height relationship",
       x = "Age (months)", y = "Height (cm)", color = "Sex") +
  theme_minimal()

# Weight vs Height
p2 <- ggplot(d_sim, aes(x = height, y = weight, color = sex)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Height → Weight relationship",
       x = "Height (cm)", y = "Weight (kg)", color = "Sex") +
  theme_minimal()

# Age vs Weight
p3 <- ggplot(d_sim, aes(x = age, y = weight, color = sex)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Age → Weight relationship",
       x = "Age (months)", y = "Weight (kg)", color = "Sex") +
  theme_minimal()

# Combined plot
(p1 | p2) / p3

# Density plots to check distributions
p4 <- ggplot(d_sim, aes(x = weight, fill = sex)) +
  geom_density(alpha = 0.5) +
  labs(title = "Weight distribution by sex", x = "Weight (kg)", fill = "Sex") +
  theme_minimal()

p5 <- ggplot(d_sim, aes(x = height, fill = sex)) +
  geom_density(alpha = 0.5) +
  labs(title = "Height distribution by sex", x = "Height (cm)", fill = "Sex") +
  theme_minimal()

p4 | p5
