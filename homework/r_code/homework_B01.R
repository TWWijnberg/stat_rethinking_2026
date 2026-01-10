## Homework B01 - starter script
## Create a new script for Homework B01. Workflow:
## 1) Edit locally
## 2) `git add` -> `git commit` -> optionally `git push`

# Minimal skeleton you can expand
## metadata
author <- "Thijs"
date <- Sys.Date()
assignment <- "B01"

## parameters / experiment setup
n_total <- 10
sides <- 2

## placeholder function — replace with your analysis
simulate_experiment <- function(n, p) {
  rbinom(n, size = 1, prob = p)
}

## quick test run
set.seed(1)
print(simulate_experiment(n_total, 0.5))

## TODO: implement analysis and plotting for Homework B01
