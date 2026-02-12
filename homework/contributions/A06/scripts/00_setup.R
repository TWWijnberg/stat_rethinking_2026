# =============================================================================
# 00_setup.R - A06 Homework Setup
# =============================================================================
# Purpose: Load libraries and set up paths for A06 analysis
# =============================================================================

# --- Libraries ---------------------------------------------------------------
library(rethinking)
library(cmdstanr)
library(dagitty)
library(tidyverse)
library(here)
library(patchwork)

# --- Paths -------------------------------------------------------------------
hw_path <- here::here("homework", "contributions", "A06")
fig_path <- file.path(hw_path, "outputs", "figures")
results_path <- file.path(hw_path, "outputs", "results")

# --- Verify Stan -------------------------------------------------------------
message("CmdStan version: ", cmdstanr::cmdstan_version())
message("Setup complete.")
