#!/usr/bin/env Rscript
# Homework A01 Main Analysis Runner
# This script executes the complete analysis pipeline

# ==============================================================================
# ENVIRONMENT DOCUMENTATION
# ==============================================================================
# R version: 4.4.0
# Platform: Windows 11
# Key packages: tidyverse 2.0.0, ggplot2 3.5.0
# Date: 2026-01-14

# ==============================================================================
# WORKING DIRECTORY SETUP
# ==============================================================================

#' Set working directory to homework_A01 folder
#' @return Invisible NULL
set_working_directory <- function() {
  current_dir <- getwd()

  # Check if we're already in the homework_A01 directory
  if (basename(current_dir) == "homework_A01") {
    cat("Working directory already set to homework_A01\n")
    return(invisible(NULL))
  }

  # Try to find the homework_A01 directory
  homework_dir <- file.path(current_dir, "stat_rethinking_2026", "homework", "homework_A01")

  if (dir.exists(homework_dir)) {
    setwd(homework_dir)
    cat(sprintf("Working directory set to: %s\n", getwd()))
    return(invisible(NULL))
  }

  # Check if main.R exists in current directory (we're already there)
  if (file.exists("main.R")) {
    cat("Working directory appears to be correct (main.R found)\n")
    return(invisible(NULL))
  }

  stop("Could not locate homework_A01 directory. Please ensure you're running from the correct location.")
}

# ==============================================================================
# MAIN ANALYSIS PIPELINE
# ==============================================================================

#' Main execution function
#' @return Invisible NULL
main <- function() {
  # Ensure correct working directory
  set_working_directory()
  cat("================================================================================\n")
  cat("HOMEWORK A01: BAYESIAN ANALYSIS OF DISHONESTY DETECTION\n")
  cat("================================================================================\n\n")

  # Step 1: Setup environment
  cat("Step 1: Setting up analysis environment...\n")
  source("scripts/00_setup.R")
  cat("✓ Environment setup complete\n\n")

  # Step 2: Run main dishonesty detection analysis
  cat("Step 2: Running dishonesty detection analysis...\n")
  cat("This may take a moment and will generate plots...\n")
  source("scripts/01_dishonesty_detection.R")
  cat("✓ Dishonesty detection analysis complete\n\n")

  # Step 3: Run comparative globe tossing analysis
  cat("Step 3: Running comparative globe tossing analysis...\n")
  source("scripts/02_globe_tossing.R")
  cat("✓ Comparative analysis complete\n\n")

  # Step 4: Run validation tests
  cat("Step 4: Running validation tests...\n")
  if (file.exists("tests/smoke_likelihoods.R")) {
    source("tests/smoke_likelihoods.R")
    cat("✓ Validation tests complete\n\n")
  } else {
    cat("⚠ Validation tests not found\n\n")
  }

  cat("================================================================================\n")
  cat("ANALYSIS COMPLETE\n")
  cat("================================================================================\n")
  cat("Output files created in:\n")
  cat("  - output/figures/     : Generated plots\n")
  cat("  - output/tables/      : Generated tables\n")
  cat("  - output/reports/     : Generated reports\n")
  cat("================================================================================\n\n")

  invisible(NULL)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Run main analysis if this script is executed directly
if (!interactive()) {
  # Command line execution
  main()
} else {
  # Interactive execution - ask for confirmation
  cat("This will run the complete Homework A01 analysis pipeline.\n")
  cat("It may take several minutes and will generate multiple plots.\n\n")

  response <- readline("Do you want to continue? (y/N): ")
  if (tolower(substr(response, 1, 1)) == "y") {
    main()
  } else {
    cat("Analysis cancelled.\n")
  }
}

# ==============================================================================
# CLEANUP
# ==============================================================================

# Optional: Clean up any temporary objects
# rm(list = ls())  # Uncomment to clean workspace
