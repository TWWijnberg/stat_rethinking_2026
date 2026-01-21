# Globe tossing draws
# Homework A02: Posterior predictive simulation

# helper function to flip a biased coin
flip_coin <- function(n, p) {
  sample(c("W", "L"), size = n, replace = TRUE, prob = c(p, 1 - p))
}

# constants
n_draws <- 1000
predictions <-5

sides_water <- 3
sides_land <- 11

p_draws <- rbeta(n_draws, sides_water + 1, sides_land + 1)
pred_post <- sapply(p_draws, function(p) sum(flip_coin(n = predictions, p = p) == "W"))

tab_post <- table(pred_post)
plot(tab_post)

