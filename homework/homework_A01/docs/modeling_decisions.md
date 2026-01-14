# Modeling Decisions for Homework A01

## Overview

This document outlines the key modeling decisions made in the Bayesian analysis of the dishonesty detection experiment. All decisions follow the principles outlined in "Best Practices for Bayesian Analysis in R".

## Section 1: Combinatorial Dishonesty Analysis

### Research Question
Given observed win counts in a game, how many ways are there to achieve different proportions of dishonest participants?

### Model Specification

**Participants are divided into two types:**
- **Honest participants**: Win with probability 0.5 (fair coin flip)
- **Dishonest participants**: Win with probability 1.0 (guaranteed)

**Data Generating Process:**
- Total participants: N (fixed)
- Proportion dishonest: p (parameter of interest)
- Number dishonest: D = round(p × N)
- Number honest: H = N - D
- Total wins observed: W

**Likelihood Calculation:**
For a given p (→ D dishonest, H honest) and observed W wins:

```
P(W wins | p) = P(W | D dishonest, H honest)
               = (2^D) × C(H, W-D) × (1^(W-D)) × (1^(H - (W-D)))
               = (2^D) × C(H, W-D) × (1^(W-D)) × (1^(H - W + D))
```

Where:
- `2^D`: Each dishonest participant has 2 outcomes, all lead to win
- `C(H, W-D)`: Choose which honest participants win
- `1^(W-D)`: Winning honest participants have 1 way to win
- `1^(H - W + D)`: Losing honest participants have 1 way to lose

### Validation Approach
- **Brute force enumeration**: Calculate all possible outcome combinations
- **Analytical likelihood**: Use combinatorial formulas
- **Comparison**: Ensure both methods give identical results

### Assumptions
- Independent participants
- Known win probabilities (0.5 for honest, 1.0 for dishonest)
- No measurement error
- Participants don't learn or change behavior

## Section 2: Bayesian Inference with Sequential Updating

### Research Question
Using observed win rates from sequential samples, what's the likely proportion of dishonest participants?

### Prior Specification

**Parameter**: p (proportion dishonest, 0 ≤ p ≤ 1)

**Prior Distribution:**
- **Support**: 101 discrete values (0.00, 0.01, ..., 1.00)
- **Weights**: Poisson(λ=5) distributed random weights
- **Rationale**: Weakly informative prior, slight preference for lower dishonesty rates
- **Validation**: Ensures non-zero prior mass at true parameter values

### Likelihood Function

**Data**: Sequential samples of (wins, total_participants) pairs

**Likelihood for single sample:**
```
P(data | p) = P(W wins in N participants | p)
            = Same as Section 1 combinatorial calculation
```

**Sequential Updating:**
```
P(p | data₁, data₂, ..., dataₖ) ∝ P(dataₖ | p) × P(p | data₁, ..., dataₖ₋₁)
```

Where previous posterior becomes new prior.

### Validation Approach

**Parameter Recovery:**
- Generate data from known true p
- Check if posterior mode recovers true p
- Assess credible interval coverage

**Convergence Diagnostics:**
- Monitor how posterior evolves with more data
- Check if credible intervals shrink appropriately
- Validate that uncertainty decreases with sample size

**Prior Sensitivity:**
- Test different prior specifications
- Ensure results are robust to reasonable prior changes

### Assumptions
- Independent samples between updates
- Same data generating process for all samples
- No temporal changes in dishonesty rates
- Exchangeability of participants

## Section 3: Comparative Analysis (Globe Tossing)

### Purpose
Demonstrate that Bayesian principles apply across different contexts and data types.

### Model Specification

**Globe Tossing Example:**
- **Parameter**: p (proportion water on globe)
- **Data**: Binary outcomes (water/land) from tosses
- **Likelihood**: Binomial(p, N) for N tosses
- **Prior**: Uniform over p ∈ [0,1]

### Comparison with Dishonesty Detection

**Similarities:**
- Sequential Bayesian updating
- Parameter estimation from binary outcomes
- Uncertainty quantification through credible intervals

**Differences:**
- Dishonesty: Complex likelihood (mixture of deterministic + binomial)
- Globe: Simple binomial likelihood
- Dishonesty: Multiple samples with different N
- Globe: Single running total

## Implementation Decisions

### Software Choices

**R Packages:**
- **Base R**: stats, graphics - for core functionality
- **tidyverse**: dplyr, ggplot2 - for data manipulation and visualization
- **No external Bayesian packages**: Custom implementation for educational purposes

### Code Organization

**Function Structure:**
- Pure functions with clear inputs/outputs
- Comprehensive documentation
- Input validation
- Error handling

**Reproducibility:**
- Fixed seeds for all stochastic operations
- Environment documentation
- Version control integration

### Validation Strategy

**Unit Testing:**
- Likelihood function validation via brute force
- Edge case handling (impossible configurations)
- Numerical stability checks

**Integration Testing:**
- Full analysis pipeline
- Parameter recovery simulation
- Prior sensitivity analysis

## Limitations and Future Improvements

### Current Limitations

1. **Discrete Parameter Space**: p discretized to 101 values for computational tractability
2. **Fixed Win Probabilities**: Assumes exactly 50% for honest participants
3. **No Model Comparison**: Single model, no alternative specifications tested
4. **Computational Complexity**: O(N×101) for likelihood calculations

### Potential Improvements

1. **Continuous Parameter Space**: Use grid approximation or MCMC
2. **Hierarchical Models**: Account for participant-level variation
3. **Model Averaging**: Compare multiple model specifications
4. **Measurement Error**: Incorporate uncertainty in win observations
5. **Longitudinal Effects**: Model changes in dishonesty over time

## References

- McElreath, R. (2020). *Statistical Rethinking*. CRC Press.
- Original homework concept from Statistical Rethinking course materials.
- Combinatorial probability calculations based on basic probability theory.

## Decision Rationale

All modeling decisions prioritize:
- **Educational Value**: Clear illustration of Bayesian concepts
- **Computational Feasibility**: Runs efficiently on standard hardware
- **Reproducibility**: Consistent results across different runs
- **Validation**: Multiple approaches to verify correctness
- **Best Practices**: Follows recommended workflows for Bayesian analysis
