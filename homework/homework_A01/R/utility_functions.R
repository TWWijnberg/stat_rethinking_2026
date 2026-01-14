# Utility Functions for Homework A01
# General-purpose helper functions used across the analysis

#' Create directory if it doesn't exist
#' @param dir_path Character string path to directory
#' @return Invisible logical indicating if directory was created
ensure_directory <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    return(TRUE)
  }
  return(FALSE)
}

#' Format percentage with specified decimal places
#' @param x Numeric value to format as percentage
#' @param digits Number of decimal places (default: 1)
#' @return Character string formatted as percentage
format_percentage <- function(x, digits = 1) {
  sprintf("%.*f%%", digits, x * 100)
}

#' Safe division that handles division by zero
#' @param numerator Numeric numerator
#' @param denominator Numeric denominator
#' @param default_value Value to return if denominator is zero (default: 0)
#' @return Numeric result of division
safe_divide <- function(numerator, denominator, default_value = 0) {
  if (denominator == 0) {
    return(default_value)
  }
  numerator / denominator
}

#' Check if a value is within a range (inclusive)
#' @param x Numeric value to check
#' @param min_val Numeric minimum value
#' @param max_val Numeric maximum value
#' @return Logical indicating if x is in range
in_range <- function(x, min_val, max_val) {
  x >= min_val && x <= max_val
}

#' Calculate mode of a numeric vector
#' @param x Numeric vector
#' @param na.rm Logical indicating whether to remove NA values (default: TRUE)
#' @return Numeric mode value
calculate_mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

#' Generate reproducible random sample with fallback
#' @param x Vector to sample from
#' @param size Sample size
#' @param replace Logical for sampling with replacement
#' @param seed Random seed for reproducibility
#' @return Sampled vector
reproducible_sample <- function(x, size, replace = FALSE, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  sample(x, size, replace = replace)
}

#' Format number with appropriate significant digits
#' @param x Numeric value
#' @param digits Number of significant digits (default: 3)
#' @return Character string of formatted number
format_number <- function(x, digits = 3) {
  format(x, digits = digits, scientific = FALSE, trim = TRUE)
}

#' Check if all elements in vector are equal
#' @param x Vector to check
#' @param na.rm Logical for removing NA values (default: FALSE)
#' @return Logical indicating if all elements are equal
all_equal <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  length(unique(x)) == 1
}

#' Calculate coefficient of variation
#' @param x Numeric vector
#' @param na.rm Logical for removing NA values (default: TRUE)
#' @return Numeric coefficient of variation
coefficient_of_variation <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  sd(x) / mean(x)
}

#' Clamp value to specified range
#' @param x Numeric value to clamp
#' @param min_val Minimum allowed value
#' @param max_val Maximum allowed value
#' @return Numeric value clamped to range
clamp <- function(x, min_val, max_val) {
  max(min_val, min(max_val, x))
}

#' Check if vector contains only finite numeric values
#' @param x Vector to check
#' @return Logical indicating if all values are finite numbers
is_finite_numeric <- function(x) {
  is.numeric(x) && all(is.finite(x))
}

#' Generate sequence with safe handling of edge cases
#' @param from Starting value
#' @param to Ending value
#' @param by Increment
#' @param length.out Desired length of sequence
#' @return Numeric sequence
safe_seq <- function(from, to, by = NULL, length.out = NULL) {
  tryCatch({
    seq(from = from, to = to, by = by, length.out = length.out)
  }, error = function(e) {
    warning("Could not create sequence: ", e$message)
    numeric(0)
  })
}

#' Calculate geometric mean
#' @param x Positive numeric vector
#' @param na.rm Logical for removing NA values (default: TRUE)
#' @return Numeric geometric mean
geometric_mean <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  if (any(x <= 0)) {
    warning("Geometric mean undefined for non-positive values")
    return(NA)
  }
  exp(mean(log(x)))
}

#' Format time duration in human-readable format
#' @param seconds Numeric seconds
#' @return Character string of formatted time
format_duration <- function(seconds) {
  if (seconds < 60) {
    sprintf("%.1f seconds", seconds)
  } else if (seconds < 3600) {
    sprintf("%.1f minutes", seconds / 60)
  } else {
    sprintf("%.1f hours", seconds / 3600)
  }
}

#' Remove outliers using IQR method
#' @param x Numeric vector
#' @param multiplier IQR multiplier (default: 1.5)
#' @return Numeric vector with outliers removed
remove_outliers <- function(x, multiplier = 1.5) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower <- q[1] - multiplier * iqr
  upper <- q[2] + multiplier * iqr
  x[x >= lower & x <= upper]
}

#' Calculate confidence interval for proportion
#' @param successes Number of successes
#' @param trials Total number of trials
#' @param conf_level Confidence level (default: 0.95)
#' @return Numeric vector with lower and upper bounds
proportion_ci <- function(successes, trials, conf_level = 0.95) {
  p <- successes / trials
  se <- sqrt(p * (1 - p) / trials)
  z <- qnorm((1 + conf_level) / 2)
  margin <- z * se

  c(lower = max(0, p - margin),
    upper = min(1, p + margin))
}
