
# ---- Step 2 ----
# Look at the data

ggplot(Howell1, aes(x = age, y = height)) +
    geom_point() +
    xlim(0, 60) +  # Set x-axis limits from 0 to 60 years
    ylim(50, 200) +  # Set y-axis limits from 50 to 200 cm
    labs(x = "Age", y = "Height", title = "Age vs Height")

# growth stops at around 25 years old
# I'll model growth up to 18 years old
children <- Howell1 %>% filter(age <= 18)

ggplot(children, aes(x = age, y = height)) +
    geom_point() +
    xlim(0, 18) +  # Set x-axis limits from 0 to 18 years
    ylim(50, 180) +  # Set y-axis limits from 50 to 180 cm
    labs(x = "Age", y = "Height", title = "Age vs Height")

# We're going to apply the following model:
height_model <- alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b_age * age,
    # you are born at around 30 cm
    a ~ dnorm(30, 20),
    # you grow 5 - 20 cm per year
    b_age ~ dunif(5, 20),
    # people vary by 3x sigma
    sigma ~ dunif(0, 10)
    )

# generate some predictions with the prior to see if they make sense
n_sample <- 100
age_sample <- runif(n_sample, 0, 19)
a_prior_sample <- rnorm(n_sample, 30, 20)
b_age_prior_sample <- runif(n_sample, 5, 20)
sigma_prior_sample <- runif(n_sample, 0, 10)
height_prior_sample <- a_prior_sample + b_age_prior_sample * age_sample

prior_sample <- data.frame(age = age_sample, height = height_prior_sample)  

ggplot(prior_sample, aes(x = age, y = height)) +    
    geom_point() +
    xlim(0, 20) +  # Set x-axis limits from 0 to 20 years
    ylim(0, 200) +  # Set y-axis limits from 0 to 200 cm
    labs(x = "Age", y = "Height", title = "Age vs Height")



# fit the model using quap and synthetic data
fitted_model <- quap(height_model, data = list(age = children$age, height = children$height))

# Prior predictive simulation: visualize what lines the prior allows
age_seq <- seq(0, 19, length.out = 50)

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
