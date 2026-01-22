# Homework A03 - Problem 1
# Statistical Rethinking 2026
#
# [Restate the problem in your own words]
#

# ---- Setup ----
source(here::here("homework", "contributions", "A03", "scripts", "00_setup.R"))
source(here::here("homework", "contributions", "A03", "scripts", "01_data_prep.R"))

# ---- Step 1 ----
# generate synthetic data

# Function to simulate weight based on height
sim_weight <- function(height, beta, alpha, sd, n){
    standard_error <- rnorm(n, mean = 0, sd = sd)
    weight <- alpha +beta * height + standard_error
    return(weight)
}

n_synthetic <- 60
proportion_male <- 0.5
n_synthetic_male <- round(n_synthetic * proportion_male)
n_synthetic_female <- n_synthetic - n_synthetic_male

# I am 80 kilos, and am 182 cm tall, which I will use for the slope parameter
# I'll play around with the alpha and sd to get OK looking data
alpha_synthetic_male <- 10
beta_synthetic_male <- (80-alpha_synthetic_male) / 182
sd_synthetic_male <- 10
height_synthetic_male <- runif(n_synthetic_male, min = 140, max = 220)
weight_synthetic_male <- sim_weight(
    height_synthetic_male,
    beta = beta_synthetic_male,
    alpha = alpha_synthetic_male,
    sd = sd_synthetic_male,
    n = n_synthetic_male
    )
# plot the males to see if they look OK
plot(height_synthetic_male, weight_synthetic_male, xlab = "Height (cm)", ylab = "Weight (kg)", main = "Synthetic male Height vs Weight Data", xlim = c(130, 220), ylim = c(30, 120))




# and now generate some females with a different height, alpha, slope and sd
alpha_synthetic_female <- 0
beta_synthetic_female <- (50-alpha_synthetic_female) / 150
sd_synthetic_female <- 8
height_synthetic_female <- runif(n_synthetic_female, min = 130, max = 190)
weight_synthetic_female <- sim_weight(
    height_synthetic_female, 
    beta = beta_synthetic_female, 
    alpha = alpha_synthetic_female, 
    sd = sd_synthetic_female, 
    n = n_synthetic_female)

# a little bit unrealistic, but it seems good enough for now!
plot(height_synthetic_female, weight_synthetic_female, xlab = "Height (cm)", ylab = "Weight (kg)", main = "Synthetic female Height vs Weight Data", xlim = c(130, 220), ylim = c(30, 120))


df_synthetic <- data.frame(
    height = c(height_synthetic_male, height_synthetic_female),
    weight = c(weight_synthetic_male, weight_synthetic_female),
    sex = c(rep("male", n_synthetic_male), rep("female", n_synthetic_female))
)


ggplot(df_synthetic, aes(x = height, y = weight, color = sex)) +
    geom_point() +
    scale_color_manual(values = c("female" = "pink", "male" = "blue")) +
    labs(x = "Height (cm)", y = "Weight (kg)", title = "Synthetic Height vs Weight Data") +
    xlim(130, 220) +
    ylim(30, 120)

# We're going to apply the following model:
weight_model <- alist(
    weight ~ dnorm(mu, sigma),
    mu <- a + b_height * height,
    a ~ dnorm(0, 10),
    b_height ~ dunif(0, 1),
    sigma ~ dunif(0, 10)
    )

# fit the model using quap and synthetic data
fitted_model <- quap(weight_model, data = df_synthetic)

# Prior predictive simulation: visualize what lines the prior allows
height_seq <- seq(130, 220, length.out = 50)

prior <- extract.prior(fitted_model,n=100)
post <- extract.samples(fitted_model)
ps <- par("bty")
par(bty="n")
plot( precis(prior,2) , col.ci="gray" , bty="n" )
plot( precis(post,2) , add=TRUE , pch=16 )
par(bty=ps)



prior_samples <- extract.prior(fitted_model)
mu_prior <- link(fitted_model, post = prior_samples, data = data.frame(height = height_seq))

# Plot regression lines from the prior
plot(NULL, xlim = c(130, 220), ylim = c(-100, 600),
     xlab = "Height (cm)", ylab = "Weight (kg)",
     main = "Prior Predictive: 50 regression lines from prior")
abline(h = 0, lty = 2)  # reference line at 0 kg
for (i in 1:50) {
    lines(height_seq, mu_prior[i, ], col = col.alpha("black", 0.3))
}



summary(fitted_model)

# Plot data with fitted regression line and uncertainty
mu <- link(fitted_model, data = data.frame(height = height_seq))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI, prob = 0.89)

plot(weight ~ height, data = df_synthetic,
     col = ifelse(df_synthetic$sex == "male", "blue", "pink"),
     xlab = "Height (cm)", ylab = "Weight (kg)",
     main = "Fitted Model on Synthetic Data",
     xlim = c(130, 220), ylim = c(30, 120))
lines(height_seq, mu_mean)
shade(mu_PI, height_seq)
