# Plotting Functions for Homework A01
# Reusable plotting functions for Bayesian analysis

#' Plot Bayesian updating cycle with prior, likelihood, and posterior
#'
#' @param param_vals Vector of parameter values
#' @param prior_probs Prior probability distribution
#' @param likelihood_probs Likelihood probability distribution
#' @param posterior_probs Posterior probability distribution
#' @param title Plot title
#' @param update_number Update number for context (optional)
plot_bayesian_update <- function(param_vals, prior_probs, likelihood_probs,
                                 posterior_probs, title = "", update_number = NULL) {
  # Validate inputs
  if (length(param_vals) != length(prior_probs) ||
      length(param_vals) != length(likelihood_probs) ||
      length(param_vals) != length(posterior_probs)) {
    stop("All probability vectors must have the same length as param_vals")
  }

  # Calculate y-axis range
  ylim_range <- range(prior_probs, likelihood_probs, posterior_probs, na.rm = TRUE)

  # Create plot
  plot(param_vals, prior_probs,
       type = "l", lwd = 2, lty = 1,
       ylim = ylim_range,
       xlab = "Parameter Value", ylab = "Probability",
       main = title)
  lines(param_vals, likelihood_probs, lwd = 2, lty = 2)
  lines(param_vals, posterior_probs, lwd = 2, lty = 3)
  legend("topright", c("Prior", "Likelihood", "Posterior"), lty = 1:3, lwd = 2)
}

#' Plot prior vs posterior comparison
#'
#' @param param_vals Vector of parameter values
#' @param prior_probs Prior probability distribution
#' @param posterior_probs Posterior probability distribution
#' @param title Plot title
plot_prior_posterior_comparison <- function(param_vals, prior_probs, posterior_probs,
                                           title = "Prior vs Posterior") {
  ylim_range <- range(prior_probs, posterior_probs, na.rm = TRUE)

  plot(param_vals, prior_probs,
       type = "l", lwd = 2, lty = 1, col = "blue",
       ylim = ylim_range,
       xlab = "Parameter Value", ylab = "Probability",
       main = title)
  lines(param_vals, posterior_probs, lwd = 2, lty = 1, col = "red")
  legend("topright", c("Prior", "Posterior"), lty = 1, lwd = 2, col = c("blue", "red"))
}

#' Plot multiple posterior distributions for comparison
#'
#' @param param_vals Vector of parameter values
#' @param posterior_list List of posterior distributions to compare
#' @param labels Character vector of labels for each distribution
#' @param title Plot title
plot_posterior_comparison <- function(param_vals, posterior_list, labels,
                                    title = "Posterior Comparison") {
  if (length(posterior_list) != length(labels)) {
    stop("Number of posteriors must match number of labels")
  }

  ylim_range <- range(unlist(posterior_list), na.rm = TRUE)

  plot(param_vals, posterior_list[[1]],
       type = "l", lwd = 2,
       ylim = ylim_range,
       xlab = "Parameter Value", ylab = "Probability",
       main = title, col = 1)

  for (i in 2:length(posterior_list)) {
    lines(param_vals, posterior_list[[i]], lwd = 2, col = i)
  }

  legend("topright", labels, lty = 1, lwd = 2, col = 1:length(labels))
}

#' Create overlay plot of theoretical vs sampled distributions
#'
#' @param theoretical_data Data frame with theoretical distribution (cols: n_dishonest, prob, type)
#' @param sampled_data Data frame with sampled distribution (cols: n_dishonest, prob, type)
#' @param title Plot title
plot_distribution_overlay <- function(theoretical_data, sampled_data,
                                    title = "Theoretical vs Sampled") {
  combined_data <- rbind(theoretical_data, sampled_data)

  ggplot(combined_data, aes(x = n_dishonest, y = prob, fill = type)) +
    geom_col(position = "identity", alpha = 0.5) +
    scale_fill_manual(values = c("Theoretical" = "lightblue", "Sampled" = "lightgreen")) +
    labs(title = title, x = "Number of Dishonest", y = "Probability") +
    theme_minimal()
}

#' Enhanced trace plot for MCMC diagnostics
#'
#' @param param_vals Vector of parameter values
#' @param trace_matrix Matrix of MCMC traces (iterations x parameters)
#' @param title Plot title
plot_trace_diagnostic <- function(trace_matrix, title = "Trace Plot") {
  n_iterations <- nrow(trace_matrix)
  n_chains <- ncol(trace_matrix)

  # Convert to long format for plotting
  trace_df <- data.frame(
    iteration = rep(1:n_iterations, n_chains),
    value = as.vector(trace_matrix),
    chain = rep(1:n_chains, each = n_iterations)
  )

  ggplot(trace_df, aes(x = iteration, y = value, color = factor(chain))) +
    geom_line(alpha = 0.7) +
    labs(title = title, x = "Iteration", y = "Parameter Value", color = "Chain") +
    theme_minimal()
}
