# Diagnostic Functions for Homework A01
# Functions for validating Bayesian analysis results

#' Print posterior summary statistics with credible intervals
#'
#' @param param_vals Vector of parameter values
#' @param posterior_probs Posterior probability distribution
#' @param label Label for output
#' @param true_value True parameter value for validation (optional)
print_posterior_summary <- function(param_vals, posterior_probs, label = "",
                                   true_value = NULL) {
  # Input validation
  if (length(param_vals) != length(posterior_probs)) {
    stop("param_vals and posterior_probs must have the same length")
  }

  # Find mode (maximum a posteriori estimate)
  mode_idx <- which.max(posterior_probs)
  mode_val <- param_vals[mode_idx]

  # Calculate 95% credible interval using cumulative probability
  cum_prob <- cumsum(posterior_probs)
  lower_idx <- min(which(cum_prob >= 0.025))
  upper_idx <- max(which(cum_prob <= 0.975))

  if (length(lower_idx) == 0 || length(upper_idx) == 0) {
    warning("Could not calculate 95% credible interval")
    ci_lower <- NA
    ci_upper <- NA
  } else {
    ci_lower <- param_vals[lower_idx]
    ci_upper <- param_vals[upper_idx]
  }

  # Print results
  cat(sprintf("%s\n", label))
  cat(sprintf("  Mode: %.3f", mode_val))
  if (!is.na(ci_lower) && !is.na(ci_upper)) {
    cat(sprintf(" | 95%% CI: [%.3f, %.3f]", ci_lower, ci_upper))
  }
  cat("\n")

  # Check if true value is in credible interval (if provided)
  if (!is.null(true_value) && !is.na(ci_lower) && !is.na(ci_upper)) {
    in_ci <- (true_value >= ci_lower) && (true_value <= ci_upper)
    ci_status <- ifelse(in_ci, "✓ TRUE", "✗ FALSE")
    cat(sprintf("  True value (%.3f) in CI? %s\n", true_value, ci_status))
  }
}

#' Print sample summary statistics
#'
#' @param n_successes Number of successes observed
#' @param n_total Total number of trials
#' @param label Description label
print_sample_summary <- function(n_successes, n_total, label = "") {
  if (n_total == 0) {
    warning("Total count is zero")
    return()
  }

  success_rate <- round(100 * n_successes / n_total, 1)
  cat(sprintf("%s: %d/%d successes (%.1f%%)\n",
              label, n_successes, n_total, success_rate))
}

#' Run convergence diagnostics on posterior samples
#'
#' @param posterior_samples Matrix or data frame of posterior samples (iterations x parameters)
#' @param param_names Optional names for parameters
run_convergence_diagnostics <- function(posterior_samples, param_names = NULL) {
  if (is.null(param_names)) {
    param_names <- paste0("Parameter ", 1:ncol(posterior_samples))
  }

  cat("Convergence Diagnostics:\n")
  cat("======================\n\n")

  # R-hat (Gelman-Rubin statistic approximation)
  cat("R-hat Statistics (should be < 1.1):\n")
  for (i in 1:ncol(posterior_samples)) {
    # Simple approximation of R-hat
    chain_split <- floor(nrow(posterior_samples) / 2)
    chain1 <- posterior_samples[1:chain_split, i]
    chain2 <- posterior_samples[(chain_split + 1):nrow(posterior_samples), i]

    # Within-chain variance
    w <- (var(chain1) + var(chain2)) / 2

    # Between-chain variance
    mean1 <- mean(chain1)
    mean2 <- mean(chain2)
    b <- ((mean1 - mean2)^2) / 2

    # R-hat approximation
    r_hat <- sqrt((b / w + 1) * (nrow(posterior_samples) - 1) / nrow(posterior_samples) + 1)

    cat(sprintf("  %s: %.3f\n", param_names[i], r_hat))
  }

  # Effective sample size approximation
  cat("\nEffective Sample Size (should be > 100):\n")
  for (i in 1:ncol(posterior_samples)) {
    # Simple ESS approximation using autocorrelation
    acf_vals <- acf(posterior_samples[, i], lag.max = 100, plot = FALSE)$acf
    ess <- nrow(posterior_samples) / (1 + 2 * sum(acf_vals[2:length(acf_vals)]))

    cat(sprintf("  %s: %.0f\n", param_names[i], ess))
  }

  cat("\n")
}

#' Validate likelihood function with brute force enumeration
#'
#' @param param_vals Vector of parameter values to test
#' @param n_participants Number of participants
#' @param n_wins Number of observed wins
#' @param n_sides Number of sides on die
#' @param tolerance Tolerance for comparison
validate_likelihood_function <- function(param_vals, n_participants, n_wins, n_sides,
                                        tolerance = 1e-6) {
  cat("Likelihood Function Validation:\n")
  cat("===============================\n\n")

  for (p in param_vals) {
    n_dish <- round(p * n_participants)
    n_honest <- n_participants - n_dish
    n_honest_win <- n_wins - n_dish

    cat(sprintf("Testing p = %.2f (n_dishonest = %d, n_honest = %d)\n",
                p, n_dish, n_honest))

    # Check for impossible configurations
    if (n_dish > n_wins) {
      cat("  → Impossible: more dishonest than wins\n")
      next
    }
    if (n_honest_win < 0 || n_honest_win > n_honest) {
      cat("  → Impossible: invalid honest wins\n")
      next
    }

    # Calculate using brute force enumeration
    # Dishonest: n_sides^n_dish ways (each has n_sides outcomes, all lead to win)
    # Honest: choose(n_honest, n_honest_win) ways to assign wins ×
    #         (n_sides-1)^(n_honest - n_honest_win) ways for non-winners ×
    #         1^n_honest_win ways for winners

    bf_dishonest <- n_sides^n_dish
    bf_assignment <- choose(n_honest, n_honest_win)
    bf_honest <- (n_sides - 1)^(n_honest - n_honest_win)
    bf_total <- bf_dishonest * bf_assignment * bf_honest

    cat(sprintf("  Brute force: %d × %d × %d = %d\n",
                bf_dishonest, bf_assignment, bf_honest, bf_total))

    # Calculate using likelihood function (assuming it exists)
    if (exists("calculate_dishonesty_likelihood_vec")) {
      fn_result <- calculate_dishonesty_likelihood_vec(p, n_participants, n_wins, n_sides)
      cat(sprintf("  Function result: %d\n", fn_result))

      if (abs(bf_total - fn_result) > tolerance) {
        cat(sprintf("  ⚠ MISMATCH! Difference: %d\n", abs(bf_total - fn_result)))
      } else {
        cat("  ✓ Match within tolerance\n")
      }
    } else {
      cat("  Function calculate_dishonesty_likelihood_vec not found\n")
    }

    cat("\n")
  }
}

#' Check prior weight diagnostics
#'
#' @param param_vals Vector of parameter values
#' @param prior_weights Prior weights for each parameter
#' @param true_value True parameter value for context
print_prior_diagnostics <- function(param_vals, prior_weights, true_value = NULL) {
  cat("Prior Weight Diagnostics:\n")
  cat("========================\n")
  cat(sprintf("  Non-zero weights: %d / %d\n",
              sum(prior_weights > 0), length(prior_weights)))
  cat(sprintf("  Min weight: %.6f | Max weight: %.6f\n",
              min(prior_weights), max(prior_weights)))

  if (!is.null(true_value)) {
    closest_idx <- which.min(abs(param_vals - true_value))
    closest_val <- param_vals[closest_idx]
    weight_at_true <- prior_weights[closest_idx]
    cat(sprintf("  Weight at true value (%.3f): %.6f\n",
                closest_val, weight_at_true))

    if (weight_at_true == 0) {
      cat("  ⚠ WARNING: Prior has ZERO weight at true value!\n")
    }
  }
  cat("\n")
}

#' Comprehensive model validation report
#'
#' @param model_results List containing model fit results
#' @param true_parameters True parameter values for comparison
create_validation_report <- function(model_results, true_parameters) {
  cat("Model Validation Report\n")
  cat("=======================\n\n")

  # Parameter recovery
  cat("Parameter Recovery:\n")
  for (param_name in names(true_parameters)) {
    if (param_name %in% names(model_results$posterior_summary)) {
      estimated <- model_results$posterior_summary[[param_name]]
      true_val <- true_parameters[[param_name]]

      bias <- estimated - true_val
      relative_bias <- abs(bias) / abs(true_val) * 100

      cat(sprintf("  %s: True=%.3f, Estimated=%.3f, Bias=%.3f (%.1f%%)\n",
                  param_name, true_val, estimated, bias, relative_bias))
    }
  }

  # Credible interval coverage
  cat("\nCredible Interval Coverage:\n")
  coverage_count <- 0
  total_params <- 0

  for (param_name in names(true_parameters)) {
    if (param_name %in% names(model_results$credible_intervals)) {
      ci <- model_results$credible_intervals[[param_name]]
      true_val <- true_parameters[[param_name]]

      covered <- (true_val >= ci[1]) && (true_val <= ci[2])
      status <- ifelse(covered, "✓", "✗")

      cat(sprintf("  %s: %s [%.3f, %.3f] contains %.3f\n",
                  param_name, status, ci[1], ci[2], true_val))

      coverage_count <- coverage_count + covered
      total_params <- total_params + 1
    }
  }

  coverage_rate <- coverage_count / total_params * 100
  cat(sprintf("\nCredible Interval Coverage Rate: %.1f%% (%d/%d)\n",
              coverage_rate, coverage_count, total_params))

  # Convergence diagnostics
  if (!is.null(model_results$convergence_diagnostics)) {
    cat("\nConvergence Diagnostics:\n")
    conv <- model_results$convergence_diagnostics
    cat(sprintf("  Max R-hat: %.3f\n", conv$max_rhat))
    cat(sprintf("  Min ESS: %.0f\n", conv$min_ess))
    cat(sprintf("  All R-hat < 1.1: %s\n", ifelse(conv$all_rhat_ok, "✓", "✗")))
    cat(sprintf("  All ESS > 100: %s\n", ifelse(conv$all_ess_ok, "✓", "✗")))
  }
}
