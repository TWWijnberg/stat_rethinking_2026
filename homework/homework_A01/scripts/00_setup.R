# Homework A01 Setup Script
# This script loads required packages and sets up the analysis environment
# Run this before executing any analysis scripts

# ==============================================================================
# ENVIRONMENT DOCUMENTATION
# ==============================================================================
# R version: 4.4.0
# Platform: Windows 11
# Key packages: tidyverse 2.0.0, ggplot2 3.5.0, rethinking (custom)
# Date: 2026-01-14

# ==============================================================================
# PACKAGE LOADING
# ==============================================================================

#' Check if packages are installed and install missing ones
#' @param packages Character vector of package names
#' @return Invisible NULL
check_and_install_packages <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]

  if (length(missing_packages) > 0) {
    cat(sprintf("Installing missing packages: %s\n",
                paste(missing_packages, collapse = ", ")))
    install.packages(missing_packages, repos = "https://cran.rstudio.com/")
  } else {
    cat("All required packages are already installed.\n")
  }

  invisible(NULL)
}

# Define required packages
required_packages <- c("tidyverse", "ggplot2")

# Check and install missing packages
check_and_install_packages(required_packages)

# Load packages with error handling
load_package_safely <- function(package_name) {
  tryCatch({
    library(package_name, character.only = TRUE)
    cat(sprintf("✓ Loaded package: %s\n", package_name))
  }, error = function(e) {
    stop(sprintf("Failed to load package '%s': %s", package_name, e$message))
  })
}

# Core data manipulation and visualization
load_package_safely("tidyverse")
load_package_safely("ggplot2")

# Statistical computing (base R, always available)
cat("✓ Base R stats package available\n")

# Set seed for reproducibility
set.seed(123455)

# ==============================================================================
# GLOBAL OPTIONS AND SETTINGS
# ==============================================================================

# Set default ggplot theme
theme_set(theme_minimal())

# Set options for better output
options(
  digits = 4,  # Display 4 significant digits
  scipen = 999,  # Avoid scientific notation
  stringsAsFactors = FALSE  # Don't convert strings to factors
)

# ==============================================================================
# CUSTOM FUNCTIONS AND UTILITIES
# ==============================================================================

#' Safe source function with error handling
#' @param file Path to R file to source
safe_source <- function(file) {
  if (file.exists(file)) {
    tryCatch({
      source(file, encoding = "UTF-8")
      message(sprintf("Successfully loaded: %s", file))
    }, error = function(e) {
      warning(sprintf("Failed to load %s: %s", file, e$message))
    })
  } else {
    warning(sprintf("File not found: %s", file))
  }
}

# Load custom functions if they exist
safe_source("R/plotting_functions.R")
safe_source("R/diagnostic_functions.R")
safe_source("R/utility_functions.R")

# ==============================================================================
# DIRECTORY SETUP
# ==============================================================================

# Ensure output directories exist
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/reports", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# CLEANUP
# ==============================================================================

# Remove temporary objects
rm(safe_source)

# Display setup confirmation
cat("
Homework A01 setup complete!
Loaded packages: tidyverse, ggplot2, stats
Working directory:", getwd(), "\n")
