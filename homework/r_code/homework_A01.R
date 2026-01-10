# https://github.com/rmcelreath/stat_rethinking_2026

# # Homework A1 - coin toss honesty 
# # coin toss
# sample(c("H", "T"), size = 10, replace = TRUE)
 
# Question to answer: How many ways are there to get 8 people claiming the prize, if 5 participants are honest?
# If honest: 50% chance of winning
# If dishonest: 100% chance of winning
# Assume 5 honest and 5 dishonest people 

# Q1: How many possible paths are there for 8 successes if everyone is honest?
# Q2: How many if 5 people are dishonest?
# Q3: How many honest people optimises the chance of 8 wins?

# generating a matrix of all possible outcomes
# fixed properties of the experiment
n_total <- 10
sides_of_die <- 2

n_dishonest_vals <- 0:n_total
win_total_vals <- 6:9

ways_to_get_outcome <- outer(
  n_dishonest_vals,
  win_total_vals,
  Vectorize(function(n_dishonest, win_total) {
    
    # for debugging
    # n_dishonest <- 5
    # win_total <- 8
    
    n_honest <- n_total - n_dishonest
    honest_win <- win_total - n_dishonest
    if (n_dishonest > win_total) {
      0
    } else {
      (sides_of_die ^ n_dishonest) *
        (choose(n_honest,honest_win)) 
    }
  })
)

# add readable labels
rownames(ways_to_get_outcome) <- paste0("n_dishonest=", n_dishonest_vals)
colnames(ways_to_get_outcome) <- paste0("wins=", win_total_vals)
outcome_matrix <- as.matrix(ways_to_get_outcome)


result <- round(sweep(outcome_matrix, 2, colSums(outcome_matrix), "/"),2)

# So, if we're observing 8 or more wins, there is only a 2% chance everyone is honest

# continuation of the globe tossing simulation

n_start <- 2
sides_of_globe <- 10
p <- 7 / sides_of_globe
sample_start <- rbinom(n_start, size = 1, prob = p)
water_start <- sum(sample_start)
earth_start <- n_start - water_start

water_sides_vals <- 0:sides_of_globe
paths_count <- sapply(water_sides_vals, function(water_sides) {
  p <- water_sides / sides_of_globe
  (sides_of_globe*p)^water_start * (sides_of_globe*(1-p))^earth_start
})

# applying bayes rule to get to proportions and use a prior
# (P(p_water | data) = P(data | p_water) * P(p_water)/ P(data)
weights <- c(0, 1, 10, 100, 100, 100, 50, 50, 10, 5, 1)
prior <- weights / sum(weights)

likelihood <- paths_count / sum(paths_count)
posterior_count <- (paths_count * weights) 
posterior <- posterior_count / sum(posterior_count)

x <- 0:10
plot(x, prior, type = "l",lwd = 2, lty =1,
     ylim = range(prior, likelihood, posterior))
lines(x, likelihood, lwd = 2, lty = 2)
lines(x, posterior, lwd = 2, lty = 3)

n_update <- 3
sample_update <- rbinom(n_update, size = 1, prob = p)
water_update <- sum(sample_update)
earth_update <- n_update - water_update

water_sides_vals <- 0:sides_of_globe
paths_count_update <- sapply(water_sides_vals, function(water_sides) {
  p <- water_sides / sides_of_globe
  (sides_of_globe*p)^water_update * (sides_of_globe*(1-p))^earth_update
})


likelihood_update <- paths_count_update / sum(paths_count_update)
posterior_update <- (paths_count_update * posterior_count) / sum(paths_count_update * posterior_count)

x <- 0:10
plot(x, posterior, type = "l",lwd = 2, lty =1,
     ylim = range(posterior, likelihood_update, posterior_update))
lines(x, likelihood_update, lwd = 2, lty = 2)
lines(x, posterior_update, lwd = 2, lty = 3)

posterior_count <- paths_count_update * posterior_count
posterior <- posterior_count / sum(posterior_count) 


# updating of RAG

n_start <- 10
sides_of_globe <- 3
p_dishonest <- 0.7
p_win <- 1 / sides_of_globe
p_dishonesty_steps <- 10

dishonesty_sample_start <- rbinom(n_start, size = 1, prob = p_dishonest)
result_sample_start <- dishonesty_sample_start + rbinom(n_start, size = 1, prob = p_win)
win_sample_start <- sum(result_sample_start>0)

p_dishonest_vals <- (0:p_dishonesty_steps)/p_dishonesty_steps
paths_count <- sapply(p_dishonest_vals, function(p_dishonest) {
n_dishonest <- round(p_dishonest * n_start)
n_honest <- n_start - n_dishonest
n_honest_win <- win_sample_start - n_dishonest
n_honest_loss <- n_honest - n_honest_win
if(n_dishonest>win_sample_start){
  0
  } 
else {
  (sides_of_globe)^(n_dishonest) *
    choose(
      n_honest,
      n_honest_win
    )
}
})

# applying bayes rule to get to proportions and use a prior
# (P(p_water | data) = P(data | p_water) * P(p_water)/ P(data)
weights <- rpois(p_dishonesty_steps+1,5)
prior <- weights / sum(weights)

likelihood <- paths_count / sum(paths_count)
posterior_count <- (paths_count * weights) 
posterior <- posterior_count / sum(posterior_count)

x <- 0:p_dishonesty_steps /p_dishonesty_steps
plot(x, prior, type = "l",lwd = 2, lty =1,
     ylim = range(prior, likelihood, posterior))
lines(x, likelihood, lwd = 2, lty = 2)
lines(x, posterior, lwd = 2, lty = 3)
sprintf("this sample had %i out of %i winners (%i%%)", win_sample_start, n_start,100*round(win_sample_start/n_start,1))


n_update <- 10
dishonesty_sample_update <- rbinom(n_update, size = 1, prob = p_dishonest)
result_sample_update <- dishonesty_sample_update + rbinom(n_update, size = 1, prob = p_win)
win_sample_update <- sum(result_sample_update>0)

p_dishonest_vals <- (0:p_dishonesty_steps)/p_dishonesty_steps
paths_count_update <- sapply(p_dishonest_vals, function(p_dishonest) {
  n_dishonest <- round(p_dishonest * n_update)
  n_honest <- n_update - n_dishonest
  n_honest_win <- win_sample_update - n_dishonest
  n_honest_loss <- n_honest - n_honest_win
  if(n_dishonest>win_sample_start){
    1
  } 
  else {
    ((sides_of_globe)^(n_dishonest)) *
      choose(
        n_honest,
        n_honest_win
      )
  }
})


likelihood_update <- paths_count_update / sum(paths_count_update)
posterior_update <- (paths_count_update * posterior_count) / sum(paths_count_update * posterior_count)

x <- (0:p_dishonesty_steps)/p_dishonesty_steps
plot(x, posterior, type = "l",lwd = 2, lty =1, 
     ylim = range(posterior, likelihood_update, posterior_update))
lines(x, likelihood_update, lwd = 2, lty = 2)
lines(x, posterior_update, lwd = 2, lty = 3)

posterior_count <- paths_count_update * posterior_count
posterior <- posterior_count / sum(posterior_count) 
sprintf("this sample had %i out of %i winners (%i%%)", win_sample_update, n_update,100*round(win_sample_update/n_update,1))
        