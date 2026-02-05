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

# ---- Define True Parameters for Synthetic Data ----
# Store true parameters so we can check if model recovers them
# Using index coding: separate intercepts for each sex
# sex_id: 1 = Female, 2 = Male
true_params <- list(
  # Height model parameters
  a_height = c(60, 65),   # intercepts by sex (Female: 30cm, Male: 35cm)
  b_height_age = 0.5,     # cm per month
  sigma_height = 6,       # residual SD for height

  # Weight model parameters
  a_weight = c(-10, -8),  # intercepts by sex (Female: -10kg, Male: -8kg)
  b_weight_height = 0.20, # kg per cm of height
  b_weight_age = 0.05,    # kg per month (holding height constant)
  sigma_weight = 5        # residual SD for weight
)

# ---- Generate Synthetic Data ----
# Function to generate synthetic data following the DAG structure
# Age and Sex are exogenous (no parents)
# Height depends on Age and Sex
# Weight depends on Height, Age, and Sex

sim_children <- function(n = 200, params = true_params) {
  # Exogenous variables (no parents in DAG)
  age <- runif(n, 0, 156)  # age in months (0 to 13 years)
  sex <- factor(sample(c(1, 2), n, replace = TRUE),
                labels = c("Female", "Male"))

  # Height = f(Age, Sex)
  # Using index coding: params$a_height[sex_id]
  height <- params$a_height[sex] +
    params$b_height_age * age +
    rnorm(n, 0, params$sigma_height)

  # Weight = f(Height, Age, Sex)
  # Using index coding: params$a_weight[sex_id]
  weight <- params$a_weight[sex] +
    params$b_weight_height * height +
    params$b_weight_age * age +
    rnorm(n, 0, params$sigma_weight)

  data.frame(age = age, sex = sex,
             height = height, weight = weight)
}

# Generate synthetic data
d_sim <- sim_children(n = 200, params = true_params)
d_sim <- d_sim |>  mutate(source = "Synthetic")

# ---- Load Real Data for Comparison ----
# Load Howell1 dataset and prepare it the same way as synthetic data
data(Howell1)
d_real <- Howell1 |>
  filter(age < 13) |>
  mutate(
    sex = factor(male + 1, labels = c("Female", "Male")),
    age = age * 12,
    source = "Real (Howell1)"
  ) |>
  select(-male)

# ---- Visualize Synthetic Data ----
# Plot your synthetic data to verify it looks reasonable
# Separate by sex to see the confounding structure
# Include real data for comparison

# Combine datasets for plotting
d_combined <- rbind(
  d_sim[, c("age", "sex", "height", "weight", "source")],
  d_real[, c("age", "sex", "height", "weight", "source")]
)

# Age vs Height
p1 <- ggplot(d_combined, aes(x = age, y = height, color = sex, shape = source)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(linetype = source), method = "lm", se = FALSE) +
  labs(title = "Age → Height relationship",
       subtitle = "Comparing synthetic data (circles) to real Howell data (triangles)",
       x = "Age (months)", y = "Height (cm)",
       color = "Sex", shape = "Data", linetype = "Data") +
  theme_minimal()

# Weight vs Height
p2 <- ggplot(d_combined, aes(x = height, y = weight, color = sex, shape = source)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(linetype = source), method = "lm", se = FALSE) +
  labs(title = "Height → Weight relationship",
       subtitle = "Comparing synthetic data (circles) to real Howell data (triangles)",
       x = "Height (cm)", y = "Weight (kg)",
       color = "Sex", shape = "Data", linetype = "Data") +
  theme_minimal()

# Age vs Weight
p3 <- ggplot(d_combined, aes(x = age, y = weight, color = sex, shape = source)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(linetype = source), method = "lm", se = FALSE) +
  labs(title = "Age → Weight relationship",
       subtitle = "Comparing synthetic data (circles) to real Howell data (triangles)",
       x = "Age (months)", y = "Weight (kg)",
       color = "Sex", shape = "Data", linetype = "Data") +
  theme_minimal()

# Combined plot
print((p1 | p2) / p3)

# Density plots to check distributions
p4 <- ggplot(d_combined, aes(x = weight, fill = sex, linetype = source)) +
  geom_density(alpha = 0.3) +
  labs(title = "Weight distribution by sex",
       subtitle = "Solid = Synthetic, Dashed = Real (Howell1)",
       x = "Weight (kg)", fill = "Sex", linetype = "Data") +
  theme_minimal()

p5 <- ggplot(d_combined, aes(x = height, fill = sex, linetype = source)) +
  geom_density(alpha = 0.3) +
  labs(title = "Height distribution by sex",
       subtitle = "Solid = Synthetic, Dashed = Real (Howell1)",
       x = "Height (cm)", fill = "Sex", linetype = "Data") +
  theme_minimal()

print(p4 | p5)
