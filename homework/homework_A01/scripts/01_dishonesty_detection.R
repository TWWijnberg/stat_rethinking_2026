# https://github.com/rmcelreath/stat_rethinking_2026

# ==============================================================================
# ENVIRONMENT DOCUMENTATION
# ==============================================================================
# R version: 4.4.0
# Platform: Windows 11
# Key packages: tidyverse 2.0.0, ggplot2 3.5.0
# Date: 2026-01-14

# ==============================================================================
# DISHONESTY DETECTION EXPERIMENT WITH BAYESIAN UPDATING
# ==============================================================================
# Research Question: How many ways are there to get 8 people claiming the prize,
# given different proportions of honest vs dishonest participants? And using
# observed win rates, what's the likely proportion of dishonest participants?
#
# Assumptions:
#   - Honest participants: 50% chance of winning (fair coin) in Section 1
#   - Honest participants: 33% chance of winning (1 in 3) in Section 2
#   - Dishonest participants: 100% chance of winning (guaranteed)
#
# Section 1: Combinatorial analysis of outcomes
# Section 2: Bayesian inference from observed data with sequential updating

# ==============================================================================
# CONFIGURATION SECTION - Modify these parameters to experiment
# ==============================================================================

set.seed(123455)  # Reproducibility: set for consistent random samples

library(ggplot2)

# --- Section 1 Parameters ---
config_s1_n_total <- 10
config_s1_sides <- 2
config_s1_n_dishonest_vals <- 0:config_s1_n_total
config_s1_win_total_vals <- 0:config_s1_n_total

# --- Section 2 Parameters ---
config_s2_n_participants <- 50
config_s2_sides <- 3
config_s2_p_dishonest_true <- 0.4
config_s2_n_steps <- 100
config_s2_prior_lambda <- 5  # Poisson parameter for prior

# --- Sequential Update Configuration ---
config_s2_updates <- list(
  list(n = 100),   # Update 1: 30 new participants
  list(n = 100),   # Update 2: 30 new participants
  list(n = 50)    # Update 3: 30 more participants
)

# ==============================================================================
# SECTION 1: COIN TOSS HONESTY - COMBINATORIAL ANALYSIS
# ==============================================================================

# --- Helper Functions for Section 1 ---

#' Calculate ways to achieve observed outcomes
#'
#' @param n_dishonest Number of dishonest participants
#' @param win_total Total number of wins observed
#' @param n_total Total number of participants
#' @param sides Number of sides on the "die" (probability multiplier for honest wins)
#' @return Number of ways to achieve this outcome, 0 if impossible, NA if invalid
calculate_outcome_ways <- function(n_dishonest, win_total, n_total, sides) {
  # Validation
  if (n_dishonest < 0 || n_dishonest > n_total) {
    return(NA)
  }
  if (win_total < 0 || win_total > n_total) {
    return(NA)
  }
  
  n_honest <- n_total - n_dishonest
  honest_win <- win_total - n_dishonest
  
  if (n_dishonest > win_total) {
    return(0)  # Impossible: more dishonest people than total wins
  }
  if (honest_win < 0 || honest_win > n_honest) {
    return(NA)  # Invalid: negative honest wins or more wins than honest people
  }
  
  # Calculate ways: product of ways for dishonest (always win) and honest (binomial)
  (sides ^ n_dishonest) * 
  choose(n_honest, honest_win) *
  ((sides - 1) ^ (n_honest - honest_win))
}

#' Create outcome matrix showing ways to achieve each combination
#'
#' @param n_dishonest_vals Vector of possible dishonest participant counts
#' @param win_total_vals Vector of possible win totals
#' @param n_total Total participants
#' @param sides Probability sides
#' @return Matrix with labeled rows (n_dishonest) and columns (win_total)
create_outcome_matrix <- function(n_dishonest_vals, win_total_vals, n_total, sides) {
  ways <- outer(
    n_dishonest_vals,
    win_total_vals,
    Vectorize(function(nd, wt) calculate_outcome_ways(nd, wt, n_total, sides))
  )
  
  rownames(ways) <- paste0("n_dishonest=", n_dishonest_vals)
  colnames(ways) <- paste0("wins=", win_total_vals)
  
  return(ways)
}

#' Convert outcome counts to probabilities
#'
#' @param outcome_matrix Matrix of outcome counts
#' @param decimals Number of decimal places to round to
#' @return Probability matrix (normalized by column sums)
calculate_probabilities <- function(outcome_matrix, decimals = 2) {
  round(sweep(outcome_matrix, 2, colSums(outcome_matrix, na.rm = TRUE), "/"), decimals)
}




# ==============================================================================
# SECTION 2: DISHONESTY DETECTION - BAYESIAN INFERENCE
# ==============================================================================

# --- Helper Functions for Section 2 ---

#' Generate mixed honest/dishonest sample
#'
#' @param n_total Total participants
#' @param p_dishonest_true True proportion of dishonest participants
#' @param p_win_honest Winning probability for honest participants
#' @return Number of winners in the sample
generate_mixed_sample <- function(n_total, p_dishonest_true, p_win_honest) {
  is_dishonest <- rbinom(n_total, size = 1, prob = p_dishonest_true)
  wins <- is_dishonest + (1 - is_dishonest) * rbinom(n_total, size = 1, prob = p_win_honest)
  sum(wins > 0)
}

#' Calculate likelihood for dishonesty proportion (vectorized)
#'
#' Efficiently calculates likelihood for all parameter values at once
#'
#' @param param_vals Vector of parameter values (proportion dishonest, 0-1)
#' @param n_participants Number of participants
#' @param n_wins_observed Number of observed winners
#' @param n_sides Number of sides (probability multiplier)
#' @return Vector of likelihood values (NA for impossible/invalid cases)
calculate_dishonesty_likelihood_vec <- function(param_vals, n_participants,
                                                 n_wins_observed, n_sides) {
  # Vectorized calculation avoiding sapply loop
  likelihoods <- sapply(param_vals, function(p_dh) {
    # Validation
    if (p_dh < 0 || p_dh > 1) return(NA)
    if (n_wins_observed < 0 || n_wins_observed > n_participants) return(NA)
    n_dishonest <- round(p_dh * n_participants)
    n_honest <- n_participants - n_dishonest
    n_honest_win <- n_wins_observed - n_dishonest
    
    # Check for impossible/invalid configurations
    if (n_dishonest > n_wins_observed) return(0)
    if (n_honest_win < 0 || n_honest_win > n_honest) return(NA)
    
    # Likelihood: ways to get this outcome
    # TODO: resolve problems with incorrect posterior density
    
    # for dishonest participants, any of the n_sides outcomes lead to win
    (n_sides ^ n_dishonest) * 
    choose(n_honest, n_honest_win) *
    ( (n_sides - 1) ^ (n_honest - n_honest_win) )
  })
  
  likelihoods
}

#' Perform one Bayesian update cycle
#'
#' @param param_vals Vector of parameter values
#' @param n_wins Observed number of wins
#' @param n_participants Number of participants
#' @param prior_weights Prior weights for each parameter
#' @param n_sides Number of sides
#' @return List with likelihoods, posterior_counts, etc. for further updating
perform_bayesian_update <- function(param_vals, n_wins, n_participants,
                                    prior_weights, n_sides) {
  # Calculate likelihoods using vectorized function
  likelihoods <- calculate_dishonesty_likelihood_vec(
    param_vals, n_participants, n_wins, n_sides
  )
  
  # Handle NAs: set to 0 for invalid configurations
  likelihoods[is.na(likelihoods)] <- 0
  
  # Normalize to probabilities
  likelihood_probs <- likelihoods / sum(likelihoods)
  
  # Bayesian update: P(param | data) ∝ P(data | param) * P(param)
  posterior_counts <- likelihoods * prior_weights
  posterior_probs <- posterior_counts / sum(posterior_counts)
  prior_probs <- prior_weights / sum(prior_weights)
  
  list(
    likelihoods = likelihoods,
    likelihood_probs = likelihood_probs,
    posterior_counts = posterior_counts,
    posterior_probs = posterior_probs,
    prior_probs = prior_probs
  )
}

#' Plot Bayesian update with prior, likelihood, and posterior
#'
#' @param param_vals Parameter values to plot
#' @param prior_probs Prior probabilities
#' @param likelihood_probs Likelihood probabilities
#' @param posterior_probs Posterior probabilities
#' @param title Plot title
#' @param update_number Update number for context
plot_bayesian_results <- function(param_vals, prior_probs, likelihood_probs,
                                   posterior_probs, title = "", update_number = NULL) {
  ylim_range <- range(prior_probs, likelihood_probs, posterior_probs, na.rm = TRUE)
  
  plot(param_vals, prior_probs,
       type = "l", lwd = 2, lty = 1,
       ylim = ylim_range,
       xlab = "Proportion Dishonest", ylab = "Probability",
       main = title)
  lines(param_vals, likelihood_probs, lwd = 2, lty = 2)
  lines(param_vals, posterior_probs, lwd = 2, lty = 3)
  legend("topright", c("Prior", "Likelihood", "Posterior"), lty = 1:3, lwd = 2)
}

#' Calculate and print posterior summary statistics
#'
#' @param param_vals Parameter values
#' @param posterior_probs Posterior probability distribution
#' @param label Label for output
#' @param true_value True parameter value (for diagnostics)
print_posterior_summary <- function(param_vals, posterior_probs, label = "", true_value = NULL) {
  # Find mode
  mode_idx <- which.max(posterior_probs)
  mode_val <- param_vals[mode_idx]
  
  # Find 95% credible interval (simple version: cumulative probability)
  cum_prob <- cumsum(posterior_probs)
  lower_idx <- min(which(cum_prob >= 0.025))
  upper_idx <- max(which(cum_prob <= 0.975))
  ci_lower <- param_vals[lower_idx]
  ci_upper <- param_vals[upper_idx]
  
  cat(sprintf("%s\n", label))
  cat(sprintf("  Mode: %.2f | 95%% CI: [%.2f, %.2f]\n", mode_val, ci_lower, ci_upper))
  
  # Check if true value is in credible interval
  if (!is.null(true_value)) {
    in_ci <- (true_value >= ci_lower) && (true_value <= ci_upper)
    ci_status <- ifelse(in_ci, "✓ TRUE", "✗ FALSE")
    cat(sprintf("  True value (%.2f) in CI? %s\n", true_value, ci_status))
  }
}

#' Print formatted sample summary
#'
#' @param n_wins Winners in sample
#' @param n_total Total participants
#' @param label Description label
print_sample_summary <- function(n_wins, n_total, label = "") {
  win_rate <- round(100 * n_wins / n_total)
  cat(sprintf("%s: %d/%d winners (%d%%)\n", label, n_wins, n_total, win_rate))
}

#' Diagnostic: Check prior weights at specific values
#'
#' @param param_vals Parameter values
#' @param prior_weights Prior weights
print_prior_diagnostics <- function(param_vals, prior_weights, true_value = NULL) {
  cat("\nPrior Weight Diagnostics:\n")
  cat(sprintf("  Non-zero weights: %d / %d\n", sum(prior_weights > 0), length(prior_weights)))
  cat(sprintf("  Min weight: %.4f | Max weight: %.4f\n", min(prior_weights), max(prior_weights)))
  
  if (!is.null(true_value)) {
    # Find closest parameter value to true value
    closest_idx <- which.min(abs(param_vals - true_value))
    closest_val <- param_vals[closest_idx]
    weight_at_true <- prior_weights[closest_idx]
    cat(sprintf("  Weight at true value (%.2f): %.4f\n", closest_val, weight_at_true))
    
    if (weight_at_true == 0) {
      cat(sprintf("  ⚠ WARNING: Prior has ZERO weight at true value!\n")
      )
    }
  }
}

# ==============================================================================
# INITIALIZE SECTION 2 EXPERIMENT
# ==============================================================================

cat("================================================================================\n")
cat("SECTION 2: DISHONESTY DETECTION - BAYESIAN INFERENCE WITH SEQUENTIAL UPDATING\n")
cat("================================================================================\n\n")

if (interactive()) {
# Setup
p_win_honest <- 1 / config_s2_sides
param_vals <- (0:config_s2_n_steps) / config_s2_n_steps
prior_weights <- rpois(config_s2_n_steps + 1, lambda = config_s2_prior_lambda)

# Initialize Bayesian state
bayesian_state <- list(
  param_vals = param_vals,
  prior_weights = prior_weights,
  posterior_counts = prior_weights,
  posterior_probs = prior_weights / sum(prior_weights),
  prior_probs = prior_weights / sum(prior_weights),
  update_number = 0
)

cat(sprintf("Setup: %d participants, %d parameter steps\n", 
            config_s2_n_participants, config_s2_n_steps))
cat(sprintf("True dishonesty rate: %.0f%% (for data generation)\n",
            config_s2_p_dishonest_true * 100))
cat(sprintf("Honest win probability: %.2f\n", p_win_honest))

# Print prior diagnostics
print_prior_diagnostics(param_vals, prior_weights, config_s2_p_dishonest_true)
cat("\n")

# ==============================================================================
# SEQUENTIAL BAYESIAN UPDATING LOOP
# ==============================================================================
# Run this section multiple times to see posterior updating with new data


# --- Run Section 1 ---
exp1_outcome_matrix <- create_outcome_matrix(
  config_s1_n_dishonest_vals,
  config_s1_win_total_vals,
  config_s1_n_total,
  config_s1_sides
)

exp1_result <- calculate_probabilities(exp1_outcome_matrix, decimals = 2)

  # Extract probabilities for n_dishonest given 7 wins observed
  probs_n_dishonest_given_7wins <- exp1_result[, "wins=7"]

  # Draw 1000 samples of dishonest participants using the calculated distribution
  set.seed(42)  # For reproducibility in predictions
  sampled_n_dishonest <- sample(config_s1_n_dishonest_vals, 1000, replace = TRUE, prob = probs_n_dishonest_given_7wins)

  # For each draw of dishonest participants, simulate number of wins
  # Dishonest: 100% win, Honest: 50% win (coin flip)
  simulated_wins <- sapply(sampled_n_dishonest, function(nd) {
    nd + rbinom(1, config_s1_n_total - nd, 0.5)
  })

  # Visualization 1: Overlay of theoretical and sampled n_dishonest distributions
  df_theoretical <- data.frame(n_dishonest = 0:10, prob = probs_n_dishonest_given_7wins, type = "Theoretical")
  sampled_probs <- prop.table(table(factor(sampled_n_dishonest, levels = 0:10)))
  df_sampled <- data.frame(n_dishonest = 0:10, prob = as.numeric(sampled_probs), type = "Sampled")
  df <- rbind(df_theoretical, df_sampled)
  ggplot(df, aes(x = n_dishonest, y = prob, fill = type)) +
    geom_col(position = "identity", alpha = 0.5) +
    scale_fill_manual(values = c("Theoretical" = "lightblue", "Sampled" = "lightgreen")) +
    labs(title = "Comparison of Theoretical and Sampled n_dishonest Distributions", x = "n_dishonest", y = "Probability") +
    theme_minimal()

  # Visualization 2: Overlay of theoretical wins for n_dishonest=5/7 and simulated wins
  df_theo5 <- data.frame(wins = 5:10, prob = dbinom(0:5, 5, 0.5), type = "n_dishonest=5")
  df_theo7 <- data.frame(wins = 7:10, prob = dbinom(0:3, 3, 0.5), type = "n_dishonest=7")
  simulated_probs_wins <- prop.table(table(factor(simulated_wins, levels = 0:10)))
  df_sim <- data.frame(wins = 0:10, prob = as.numeric(simulated_probs_wins), type = "Simulated")
  df_wins <- rbind(df_theo5, df_theo7, df_sim)
  ggplot(df_wins, aes(x = wins, y = prob, fill = type)) +
    geom_col(position = "identity", alpha = 0.5) +
    scale_fill_manual(values = c("n_dishonest=5" = "lightblue", "n_dishonest=7" = "lightgreen", "Simulated" = "lightcoral")) +
    labs(title = "Theoretical Wins for n_dishonest=5/7 and Simulated Distribution", x = "Number of Wins", y = "Probability") +
    theme_minimal()

  # Display results
  cat("================================================================================\n")
  cat("SECTION 1: COIN TOSS HONESTY - COMBINATORIAL ANALYSIS\n")
  cat("================================================================================\n")
  cat("Question: How many ways are there to get 7 successes with different\n")
  cat("          numbers of dishonest participants?\n\n")
  cat("Experiment 1 - Probability of dishonesty levels given observed wins:\n")
  print(exp1_result)

  cat("\nExperiment 1 Key Finding:\n")
  cat("If observing 8 wins, only 2% chance everyone is honest\n\n")

  for (i in seq_along(config_s2_updates)) {
  n_new_participants <- config_s2_updates[[i]]$n
  bayesian_state$update_number <- i
  
  # Generate new sample
  n_wins <- generate_mixed_sample(
    n_new_participants,
    config_s2_p_dishonest_true,
    p_win_honest
  )
  
  cat(sprintf("\n%s\n", paste0("=", rep("=", 78), collapse = "")))
  cat(sprintf("UPDATE %d: New Sample with %d Participants\n", i, n_new_participants))
  cat(sprintf("%s\n", paste0("=", rep("=", 78), collapse = "")))
  
  print_sample_summary(n_wins, n_new_participants,
                       sprintf("Update %d Sample", i))
  
  # Perform Bayesian update using PREVIOUS posterior as new prior
  update_results <- perform_bayesian_update(
    bayesian_state$param_vals,
    n_wins,
    n_new_participants,
    bayesian_state$posterior_counts,  # Use previous posterior as prior!
    config_s2_sides
  )
  
  # Print posterior summary
  print_posterior_summary(bayesian_state$param_vals, update_results$posterior_probs,
                         sprintf("Posterior after Update %d:", i),
                         true_value = config_s2_p_dishonest_true)
  
  # Create plot using reusable function
  plot_bayesian_update(
    bayesian_state$param_vals,
    update_results$prior_probs,
    update_results$likelihood_probs,
    update_results$posterior_probs,
    sprintf("Dishonesty Detection: Update %d", i),
    update_number = i
  )
  
  # UPDATE STATE for next iteration
  bayesian_state$posterior_counts <- update_results$posterior_counts
  bayesian_state$posterior_probs <- update_results$posterior_probs
  }
} # end interactive guard
