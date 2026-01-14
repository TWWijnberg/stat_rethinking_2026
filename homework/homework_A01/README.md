# Homework A01: Bayesian Analysis of Dishonesty Detection

## Overview

This homework assignment implements Bayesian inference to analyze a dishonesty detection experiment. The analysis explores how different proportions of dishonest participants affect observed win rates in a game setting, using sequential Bayesian updating to estimate the true proportion of dishonest participants from observed data.

## Research Questions

1. **Combinatorial Analysis**: Given observed win counts, how many ways are there to achieve different proportions of dishonest participants?
2. **Bayesian Inference**: Using observed win rates from sequential samples, what's the likely proportion of dishonest participants?

## Data

- **No external data files** - All data is simulated within the analysis
- **Simulation parameters**:
  - Honest participants: 50% win probability (coin flip)
  - Dishonest participants: 100% win probability (guaranteed)
  - Sample sizes vary by experimental condition

## Analysis Pipeline

1. **Setup**: Run `scripts/00_setup.R` to load packages and configure environment
2. **Combinatorial Analysis**: Run `scripts/01_dishonesty_detection.R` for Section 1
3. **Bayesian Inference**: Continue with `scripts/01_dishonesty_detection.R` for Section 2
4. **Globe Tossing**: Run `scripts/02_globe_tossing.R` for comparison analysis

## Key Results

- **Section 1**: Probability distributions over number of dishonest participants given observed wins
- **Section 2**: Sequential Bayesian updating showing how posterior beliefs evolve with new data
- **Section 3**: Comparison with globe tossing example demonstrating general Bayesian principles

## Dependencies

- R version 4.4.0 or higher
- Required packages: tidyverse, ggplot2, stats
- Custom functions in `R/` directory

## File Structure

```
homework_A01/
├── README.md                    # This file
├── main.R                       # Main analysis runner
├── .Rproj                       # RStudio project file
├── scripts/
│   ├── 00_setup.R              # Environment setup
│   ├── 01_dishonesty_detection.R # Main analysis
│   └── 02_globe_tossing.R      # Comparative analysis
├── R/
│   ├── plotting_functions.R    # Reusable plotting utilities
│   └── diagnostic_functions.R  # Validation and diagnostics
├── output/
│   ├── figures/                # Generated plots
│   ├── tables/                 # Generated tables
│   └── reports/                # Generated reports
├── data/
│   ├── raw/                    # Raw data (simulated)
│   ├── processed/              # Processed data
│   └── simulated/              # Simulation results
├── docs/                       # Documentation
└── tests/                      # Unit tests
    └── smoke_likelihoods.R     # Likelihood function tests
```

## Running the Analysis

### Quick Start (Recommended)
Run the complete analysis pipeline:
```r
# Set working directory to homework_A01/
setwd("path/to/homework_A01")

# Run everything
source("main.R")
```

### Manual Execution
Run individual components:
```r
# Set working directory to homework_A01/
setwd("path/to/homework_A01")

# Run setup
source("scripts/00_setup.R")

# Run main analysis
source("scripts/01_dishonesty_detection.R")

# Run comparative analysis
source("scripts/02_globe_tossing.R")
```

### Command Line Execution
```bash
cd path/to/homework_A01
Rscript main.R
```

## Modeling Decisions

### Section 1: Combinatorial Analysis
- **Model**: Binomial outcomes for honest participants, deterministic wins for dishonest
- **Assumptions**: Independent participants, known win probabilities
- **Validation**: Brute force enumeration vs. likelihood calculations

### Section 2: Bayesian Inference
- **Prior**: Poisson-weighted uniform distribution over dishonesty proportions
- **Likelihood**: Product of dishonest (deterministic) and honest (binomial) win probabilities
- **Updating**: Sequential application of Bayes' rule with new data
- **Validation**: Credible interval coverage, parameter recovery assessment

### Section 3: Comparative Analysis
- **Globe Tossing**: Standard binomial Bayesian updating example
- **Purpose**: Demonstrate general principles apply across different contexts

## Key Findings

1. **Low win rates strongly suggest honest populations** - High probability of all participants being honest when observed wins are low
2. **Sequential updating shows learning** - Posterior beliefs become more precise with additional data
3. **Credible intervals provide uncertainty quantification** - Bayesian approach naturally incorporates uncertainty
4. **Prior sensitivity affects early updates** - Initial beliefs matter most when data is limited

## Validation & Testing

- **Likelihood function validation**: Brute force enumeration vs. analytical calculations
- **Posterior predictive checks**: Model fit assessment through simulation
- **Parameter recovery**: Ability to recover true parameters from simulated data
- **Convergence diagnostics**: Assessment of MCMC/sequential updating stability

## References

- McElreath, R. (2020). *Statistical Rethinking: A Bayesian Course with Examples in R and Stan*. CRC Press.
- Original analysis concept from Statistical Rethinking homework assignments.

## Notes

- All analyses use reproducible seeds for consistent results
- Interactive guards prevent automatic execution in non-interactive environments
- Custom plotting and diagnostic functions promote code reusability
- Project follows best practices for reproducible Bayesian analysis
