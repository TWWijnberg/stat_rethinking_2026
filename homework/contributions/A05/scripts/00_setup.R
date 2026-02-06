# Homework A05 - Setup
# Statistical Rethinking 2026

# ---- Libraries ----
library(rethinking)
library(here)
library(tidyverse)
library(cmdstanr)
library(dagitty)
library(patchwork)

# ---- Options ----
options(mc.cores = parallel::detectCores())

# ---- Source Helpers ----
source(here::here("homework", "contributions", "A05", "scripts", "01_helpers.R"))

cat("Setup complete.\n")
