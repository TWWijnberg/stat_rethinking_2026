# Smoke tests for calculate_dishonesty_likelihood_vec
# Run from project root or this folder; assumes `dishonesty_detection.R` defines the function

# Load functions without running experiments (dishonesty_detection guarded by interactive())
source("scripts/01_dishonesty_detection.R")

# 1) Brute-force enumeration vs function (small n)
# This computes: given n participants, n_sides per die, n_wins observed,
# how many outcome combinations lead to exactly n_wins total wins?
# 
# Example: n=3, n_sides=3, n_wins=2, proportion_dishonest=0.5 (2 dishonest, 1 honest)
#   - Dishonest people always win
#   - Honest people win only if they roll a 1
#   - With 2 dishonest (guaranteed 2 wins) + 1 honest person:
#     * Need 0 more wins from the honest person
#     * Honest person has 3 outcomes; rolling NOT-1 means no win
#     * So 2 ways: roll a 2 or roll a 3
#     * Total: 3^2 (dishonest outcomes) * 2 (honest outcomes with 0 wins) = 18

bruteforce_check <- function() {
  cat("\n=== Brute-Force Likelihood Check ===\n")
  param_vals <- seq(0,1,by=0.25)
  n <- 12; n_sides <- 6; n_wins <- 4
  
  bf_check <- sapply(param_vals, function(p) {
    n_dish <- round(p * n)
    n_honest <- n - n_dish
    n_honest_win <- n_wins - n_dish
    
    cat(sprintf("p=%.2f: %d dishonest, %d honest, need %d honest wins\n", 
                p, n_dish, n_honest, n_honest_win))
    
    # Check if impossible
    if (n_dish > n_wins) {
      cat("  → Impossible (more dishonest than wins)\n")
      return(0)
    }
    if (n_honest_win < 0 || n_honest_win > n_honest) {
      cat("  → Impossible (invalid honest wins)\n")
      return(NA)
    }
    
    # Count outcome combinations
    # Dishonest people: all n_sides outcomes lead to a win (n_sides ^ n_dishonest ways)
    # Honest people: only rolling 1 = win; rolling 2..n_sides = no win
    #   Need exactly n_honest_win wins: choose(n_honest, n_honest_win) ways to pick who wins
    #   Then: 1^n_honest_win * (n_sides-1)^(n_honest - n_honest_win) outcome combos
    
    dishonest_outcomes <- n_sides ^ n_dish
    honest_assignment_ways <- choose(n_honest, n_honest_win)
    honest_outcome_ways <- (n_sides - 1) ^ (n_honest - n_honest_win)
    
    total_ways <- dishonest_outcomes * honest_assignment_ways * honest_outcome_ways
    
    cat(sprintf("  → %d (dishonest) × %d (assign) × %d (outcomes) = %d\n",
                dishonest_outcomes, honest_assignment_ways, honest_outcome_ways, total_ways))
    
    total_ways
  })
  
  fn_out <- calculate_dishonesty_likelihood_vec(param_vals, n_participants=n, n_wins_observed=n_wins, n_sides=n_sides)
  
  cat("\nComparison:\n")
  print(data.frame(p = param_vals, brute_force = bf_check, function_out = fn_out))
  
  if (!all(bf_check == fn_out, na.rm = TRUE)) {
    cat("⚠ MISMATCH detected!\n")
    return(FALSE)
  }
  
  cat("✓ Brute-force check passed\n")
  return(TRUE)
}

# 2) Analytic single-person cases
analytic_check <- function() {
  param_vals <- c(0, 1)
  fn_out <- calculate_dishonesty_likelihood_vec(param_vals, 1, 1, 2)
  if (!(fn_out[1] == 1 && fn_out[2] == 2)) stop("Analytic single-person check failed")
  cat("Analytic single-person check passed\n")
}

# 3) Invalid inputs => NAs
invalid_check <- function() {
  out <- calculate_dishonesty_likelihood_vec(seq(0,1,0.1), n_participants = 5, n_wins_observed = 6, n_sides = 3)
  if (!all(is.na(out))) stop("Invalid input check failed")
  cat("Invalid input check passed\n")
}

# 4) Non-negativity & finite
numeric_check <- function() {
  v <- calculate_dishonesty_likelihood_vec(seq(0,1,0.05), 10, 4, 3)
  if (!all(v >= 0, is.finite(v))) stop("Numeric check failed")
  cat("Numeric check passed\n")
}

# 5) True-value coverage diagnostic
true_value_check <- function() {
  param_vals <- (0:20)/20
  v <- calculate_dishonesty_likelihood_vec(param_vals, 10, 7, 3)
  closest <- which.min(abs(param_vals - 0.7))
  if (!(v[closest] > 0)) stop("True-value likelihood is zero")
  cat("True-value coverage check passed\n")
}

# Run tests with error catching
tryCatch({
  cat("Running bruteforce_check...\n")
  bruteforce_check()
}, error = function(e) {
  cat("ERROR in bruteforce_check:", e$message, "\n")
})

tryCatch({
  cat("Running analytic_check...\n")
  analytic_check()
}, error = function(e) {
  cat("ERROR in analytic_check:", e$message, "\n")
})

tryCatch({
  cat("Running invalid_check...\n")
  invalid_check()
}, error = function(e) {
  cat("ERROR in invalid_check:", e$message, "\n")
})

tryCatch({
  cat("Running numeric_check...\n")
  numeric_check()
}, error = function(e) {
  cat("ERROR in numeric_check:", e$message, "\n")
})

tryCatch({
  cat("Running true_value_check...\n")
  true_value_check()
}, error = function(e) {
  cat("ERROR in true_value_check:", e$message, "\n")
})

cat("Test run complete.\n")
