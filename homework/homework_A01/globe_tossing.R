# https://github.com/rmcelreath/stat_rethinking_2026

# ==============================================================================
# GLOBE TOSSING SIMULATION WITH BAYESIAN UPDATING
# ==============================================================================
# Research Question: Given observed tosses, what can we infer about the proportion
# of water on a globe?
#
# This script demonstrates sequential Bayesian updating as we observe more data.

# ==============================================================================
# CONFIGURATION SECTION - Modify these parameters to experiment
# ==============================================================================

set.seed(123456)  # Reproducibility: set for consistent random samples

# --- Globe Tossing Parameters ---
config_globe_sides <- 100
config_globe_p_true <- 0.5
config_globe_prior_weights <- rep(1, config_globe_sides + 1)

# --- Sequential Update Configuration ---
config_globe_updates <- list(
   list(n = 3),   # Update 2: 30 tosses
    list(n = 3),   # Update 2: 30 tosses
     list(n = 3),   # Update 2: 30 tosses
      list(n = 3),   # Update 2: 30 tosses
       list(n = 3),   # Update 2: 30 tosses
  list(n = 3),    # Update 1: 20 tosses
  list(n = 30),   # Update 2: 30 tosses
  list(n = 50)    # Update 3: 50 tosses
)

# --- Helper Functions ---

#' Generate simulated data from a binomial process
#'
#' @param n_samples Number of samples
#' @param prob Probability of success
#' @return List with count of successes and failures
generate_binary_sample <- function(n_samples, prob) {
  sample_data <- rbinom(n_samples, size = 1, prob = prob)
  list(
    successes = sum(sample_data),
    failures = n_samples - sum(sample_data),
    n = n_samples
  )
}

#' Calculate likelihood for a parameter given observed data
#'
#' @param param_value Parameter value to evaluate (e.g., proportion water)
#' @param n_sides Number of possible outcomes (sides)
#' @param n_success Number of observed successes
#' @param n_failure Number of observed failures
#' @return Likelihood value
calculate_likelihood <- function(param_value, n_sides, n_success, n_failure) {
  (n_sides * param_value)^n_success * (n_sides * (1 - param_value))^n_failure
}

#' Run one Bayesian update cycle
#'
#' @param param_vals Vector of parameter values to evaluate
#' @param observed_data List with 'successes' and 'failures'
#' @param prior_weights Prior weights for each parameter value
#' @param n_sides Number of sides
#' @return List with likelihood, posterior, and posterior counts
bayesian_update <- function(param_vals, observed_data, prior_weights, n_sides) {
  # Calculate likelihood for each parameter value
  likelihoods <- sapply(param_vals, function(pv) {
    calculate_likelihood(pv, n_sides, observed_data$successes, observed_data$failures)
  })
  
  # Normalize to probabilities
  likelihood_probs <- likelihoods / sum(likelihoods)
  
  # Calculate posterior using Bayes rule
  # P(param | data) ∝ P(data | param) * P(param)
  posterior_counts <- likelihoods * prior_weights
  posterior_probs <- posterior_counts / sum(posterior_counts)
  
  list(
    likelihoods = likelihoods,
    likelihood_probs = likelihood_probs,
    posterior_counts = posterior_counts,
    posterior_probs = posterior_probs,
    prior_probs = prior_weights / sum(prior_weights)
  )
}

#' Plot Bayesian updating cycle
#'
#' @param param_vals Parameter values
#' @param update_results List from bayesian_update()
#' @param title Title for plot
plot_bayesian_update <- function(param_vals, update_results, title = "") {
  ylim_range <- range(
    update_results$prior_probs,
    update_results$likelihood_probs,
    update_results$posterior_probs
  )
  
  plot(param_vals, update_results$prior_probs,
    type = "l", lwd = 2, lty = 1,
    ylim = ylim_range,
    xlab = "Parameter Value", ylab = "Probability",
    main = title
  )
  lines(param_vals, update_results$likelihood_probs, lwd = 2, lty = 2)
  lines(param_vals, update_results$posterior_probs, lwd = 2, lty = 3)
  legend("topright", c("Prior", "Likelihood", "Posterior"), lty = 1:3, lwd = 2)
}

#' Print posterior summary statistics
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
  lower_idx <- min(which(cum_prob > 0.025))
  upper_idx <- max(which(cum_prob < 0.975))
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
#' @param n_successes Successes in sample
#' @param n_total Total trials
#' @param label Description label
print_sample_summary <- function(n_successes, n_total, label = "") {
  success_rate <- round(100 * n_successes / n_total)
  cat(sprintf("%s: %d/%d water (%d%%)\n", label, n_successes, n_total, success_rate))
}

# ==============================================================================
# INITIALIZE EXPERIMENT WITH SEQUENTIAL UPDATING
# ==============================================================================

cat("================================================================================\n")
cat("GLOBE TOSSING BAYESIAN INFERENCE WITH SEQUENTIAL UPDATING\n")
cat("================================================================================\n\n")

if (interactive()) {
  # Setup
  globe_param_vals <- 0:config_globe_sides / config_globe_sides
  
  # Initialize Bayesian state
  bayesian_state <- list(
    param_vals = globe_param_vals,
    prior_weights = config_globe_prior_weights,
    posterior_counts = config_globe_prior_weights,
    posterior_probs = config_globe_prior_weights / sum(config_globe_prior_weights),
    prior_probs = config_globe_prior_weights / sum(config_globe_prior_weights),
    update_number = 0
  )
  
  cat(sprintf("Setup: %d-sided die (parameter values: 0, 0.1, 0.2, ..., 1.0)\n", 
              config_globe_sides))
  cat(sprintf("True water proportion: %.1f%%\n\n", 
              config_globe_p_true * 100))
  
  # ==============================================================================
  # SEQUENTIAL BAYESIAN UPDATING LOOP
  # ==============================================================================
  
  for (i in seq_along(config_globe_updates)) {
    n_tosses <- config_globe_updates[[i]]$n
    bayesian_state$update_number <- i
    
    # Generate new sample
    globe_sample <- generate_binary_sample(n_tosses, config_globe_p_true)
    
    cat(sprintf("\n%s\n", paste0("=", rep("=", 78), collapse = "")))
    cat(sprintf("UPDATE %d: New Sample with %d Tosses\n", i, n_tosses))
    cat(sprintf("%s\n", paste0("=", rep("=", 78), collapse = "")))
    
    print_sample_summary(globe_sample$successes, globe_sample$n,
                         sprintf("Update %d Sample", i))
    
    # Perform Bayesian update using PREVIOUS posterior as new prior
    update_results <- bayesian_update(
      bayesian_state$param_vals,
      globe_sample,
      bayesian_state$posterior_counts,  # Use previous posterior as prior!
      config_globe_sides
    )
    
    # Print posterior summary
    print_posterior_summary(bayesian_state$param_vals, update_results$posterior_probs,
                           sprintf("Posterior after Update %d:", i),
                           true_value = config_globe_p_true)
    
    # Create plot
    options(device = windows())
    plot_bayesian_update(
      bayesian_state$param_vals,
      update_results,
      sprintf("Globe Tossing: Update %d", i)
    )
    
    # UPDATE STATE for next iteration
    bayesian_state$posterior_counts <- update_results$posterior_counts
    bayesian_state$posterior_probs <- update_results$posterior_probs
  }
} # end interactive guard
