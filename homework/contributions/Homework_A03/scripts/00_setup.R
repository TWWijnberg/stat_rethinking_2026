# Homework A03 - Setup
# Statistical Rethinking 2026
#
# Common setup script - run this first

# ---- Libraries ----
library(rethinking)
library(here)

# ---- Options ----
options(mc.cores = parallel::detectCores())

# ---- Helper Functions ----
# Source shared utilities if available
# source(here("R", "shared_utils.R"))

cat("Setup complete.\n")
