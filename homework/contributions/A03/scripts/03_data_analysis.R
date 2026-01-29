
# ---- Step 2 ----
# Look at the data

ggplot(Howell1, aes(x = age, y = height)) +
    geom_point() +
    xlim(0, 90) +  # Set x-axis limits from 0 to 60 years
    ylim(50, 200) +  # Set y-axis limits from 50 to 200 cm
    labs(x = "Age", y = "Height", title = "Age vs Height")

# growth stops at around 25 years old
# I'll model growth up to 18 years old
children <- Howell1 %>% filter(age <= 18)

ggplot(children, aes(x = age, y = height)) +
    geom_point() +
    xlim(0, 20) +  # Set x-axis limits from 0 to 20 years
    ylim(0, 200) +  # Set y-axis limits from 0 to 200 cm
    labs(x = "Age", y = "Height", title = "Age vs Height")

# We're going to apply the following model:
# UK based growth charts are broadly 8 - 20 cm per year
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

    
# Create your DAG in code
height_weight_dag <- dagitty("dag {
  Age -> Height 
  Sex -> Height 
  Sex -> Age 
  Nutrition -> Height 
  Nutrition -> Age 
  Age [exposure] 
  Height [outcome] 
  Sex [confounder]
  Nutrition [unobserved] 
}")

# Plot it
plot(height_weight_dag)


# generate some predictions with the prior to see if they make sense
n_sample <- 500
age_sample <- runif(n_sample, 0, 19)
a_prior_sample <- rnorm(n_sample, 50, 20)
b_age_prior_sample <- runif(n_sample, 3, 10)
sigma_prior_sample <- runif(n_sample, 0, 10)
height_prior_sample <- a_prior_sample + b_age_prior_sample * age_sample

prior_sample <- data.frame(age = age_sample, height = height_prior_sample)  

# to adjust priors until they look reasonable
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


posterior_pred <- sim(fitted_model, data = list(age = children$age))
# Method 1: Visual check - compare actual vs predicted
plot(children$age, children$height, 
     xlab = "Age", ylab = "Height", 
     main = "Posterior Predictive Check")
for(i in 1:20) {
  points(children$age, posterior_pred[i,], col = "gray", pch = 16)
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

hist(post$a)

ps <- par("bty")
par(bty="n")
plot( precis(prior,2) , col.ci="gray" , bty="n" )
plot( precis(post,2) , add=TRUE , pch=16 )
par(bty=ps)

# surprisingly, the kids only grow about 4.5 cm per year on average, which is a lot lower than the UK growth charts suggest. This might be driving by the fact that these kids are poorly malnourished, given that the data is from a Botswana community between August 1967 and May 1969
summary(fitted_model)
plot(fitted_model)

# Plot data with fitted regression line and uncertainty
mu <- link(fitted_model, data = data.frame(age = age_seq))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI, prob = 0.89)

plot(height ~ age, data = children,
     xlab = "Age (years)", ylab = "Height (cm)",
     main = "Fitted Model on Children Data",
     xlim = c(0, 20), ylim = c(0, 200))
lines(age_seq, mu_mean)
shade(mu_PI, age_seq)

plot(precis(fitted_model))


