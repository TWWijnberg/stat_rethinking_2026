# Homework A05 - Helper Functions
# Statistical Rethinking 2026
# Only 4 functions - keep analytical decisions visible

# ============================================================================
# Function 1: prepare_data()
# Why extract: Handles format differences between Howell1 and synthetic data
# ============================================================================

prepare_data <- function(data, source = "unknown") {
  d <- data

  # Howell1 has 'male' column (0/1), synthetic has 'sex' factor
  # Rethinking preference: factors with integer levels (1, 2) and descriptive labels
  if ("male" %in% names(d)) {
    d$sex <- factor(d$male + 1, levels = c(1, 2), labels = c("Female", "Male"))
    d$male <- NULL
  }

  # Ensure sex is a factor with integer levels and correct labels
  if (!is.factor(d$sex) || !identical(levels(d$sex), c("Female", "Male"))) {
    # Convert character/other to proper factor
    sex_int <- ifelse(d$sex == "Male" | d$sex == 2, 2, 1)
    d$sex <- factor(sex_int, levels = c(1, 2), labels = c("Female", "Male"))
  }

  d$source <- source
  return(d)
}

# ============================================================================
# Function 2: sim_children()
# Why extract: Core scientific model - defines data-generating process
# ============================================================================

sim_children <- function(n = 200, params) {
  # Generate in causal order: Age, Sex -> Height -> Weight

  age <- runif(n, 0, 156)  # 0 to 13 years in months

  # Rethinking preference: factors with integer levels (1, 2) and descriptive labels
  sex_int <- sample(c(1, 2), n, replace = TRUE)
  sex <- factor(sex_int, levels = c(1, 2), labels = c("Female", "Male"))

  # Height depends on Age and Sex (use integer representation for indexing)
  height <- params$a_height[sex_int] +
            params$b_height_age * age +
            rnorm(n, 0, params$sigma_height)

  # Weight depends on Height, Age, and Sex
  weight <- params$a_weight[sex_int] +
            params$b_weight_height * height +
            params$b_weight_age * age +
            rnorm(n, 0, params$sigma_weight)

  data.frame(age = age, sex = sex, height = height, weight = weight)
}

# ============================================================================
# Function 3: plot_predictions()
# Why extract: Visualization logic is complex, used for multiple outcomes
# ============================================================================

plot_predictions <- function(data, fit, outcome = "weight", title = NULL) {
  # Generate posterior predictions
  post_pred <- sim(fit, data = data)
  pred_mean <- apply(post_pred, 2, mean)
  pred_PI <- apply(post_pred, 2, PI, prob = 0.89)

  # Create plot dataframe
  plot_df <- data.frame(
    observed = data[[outcome]],
    predicted = pred_mean,
    lower = pred_PI[1, ],
    upper = pred_PI[2, ],
    sex = data$sex
  )

  ggplot(plot_df, aes(x = observed, y = predicted, color = sex)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(alpha = 0.6) +
    geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.2, width = 0) +
    labs(x = paste("Observed", outcome),
         y = paste("Predicted", outcome),
         title = title %||% paste("Posterior Predictive Check:", outcome)) +
    theme_minimal() +
    coord_equal()
}

# ============================================================================
# Function 4: plot_caterpillar()
# Why extract: Visualization logic is complex
# ============================================================================

plot_caterpillar <- function(fit, true_values = NULL, title = NULL) {
  post_summary <- precis(fit, depth = 2, prob = 0.89)

  plot_df <- data.frame(
    parameter = rownames(post_summary),
    estimate = post_summary$mean,
    lower = post_summary$`5.5%`,
    upper = post_summary$`94.5%`
  )

  p <- ggplot(plot_df, aes(x = estimate, y = parameter)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
    labs(x = "Parameter value", y = NULL,
         title = title %||% "Parameter Estimates (89% CI)") +
    theme_minimal()

  # Add true values if provided (for synthetic data validation)
  if (!is.null(true_values)) {
    true_df <- data.frame(
      parameter = names(true_values),
      true_value = unlist(true_values)
    )
    p <- p + geom_point(data = true_df, aes(x = true_value, y = parameter),
                        color = "red", shape = 4, size = 4, stroke = 2)
  }

  p
}

cat("Helper functions loaded.\n")
