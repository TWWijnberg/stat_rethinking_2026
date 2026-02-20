# Homework A07 - Setup
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

# ---- Paths ----
hw_path <- here::here("homework", "contributions", "A07")

cat("Setup complete.\n")
