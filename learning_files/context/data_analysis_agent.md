# Human-in-the-Loop Data Analysis Agent

This document defines a structured workflow for conducting Bayesian data analysis with human review at each stage. The agent interviews the user, implements each step, and pauses for approval before proceeding.

---

## Project Setup

### Folder Structure

```
homework/contributions/{assignment}/
├── scripts/
│   ├── 00_setup.R              # Libraries, paths, sourcing
│   ├── 01_data_processing.R    # Load, clean, prepare data
│   ├── 02_eda.R                # Exploratory data analysis
│   ├── 03_dag_analysis.R       # Causal model specification
│   ├── 04_synthetic_analysis.R # Model validation on synthetic data
│   ├── 05_real_analysis.R      # Fit model to real data
│   └── 06_post_processing.R    # Contrasts, predictions, summaries
├── data/
│   ├── raw/                    # Original data (never modified)
│   └── processed/              # Cleaned/transformed data
├── outputs/
│   ├── figures/                # PNG/PDF plots
│   └── results/                # RDS files, tables
├── docs/
│   ├── planning.md             # Initial scope and decisions
│   ├── eda_report.qmd          # EDA Quarto document
│   └── analysis_report.qmd     # Final analysis Quarto document
└── README.md
```

### Core Libraries

```r
library(rethinking)
library(cmdstanr)
library(dagitty)
library(tidyverse)
library(here)
library(patchwork)
```

### Key Conventions

- **Factor variables**: Use integer levels `c(1, 2, ...)` with descriptive labels (see `rethinking_factor_variables.md`)
- **Paths**: Use `here::here()` for all file paths
- **Figures**: Save to `outputs/figures/` with numbered prefixes
- **Model syntax**: Use `a[var] ~ prior()` not `vector[n]:a`

---

## Initial Interview

Before beginning analysis, gather the following information from the user:

### Research Question
> "What is the scientific question you want to answer?"

- What outcome are you trying to explain or predict?
- What is the main exposure/treatment variable of interest?
- Is this a causal question (effect of X on Y) or predictive question?

### Data Source
> "What data will you be using?"

- Dataset name and source
- Key variables (outcome, predictors, potential confounders)
- Sample size and any known limitations
- Is subsetting needed (e.g., only children, only certain time period)?

### Causal Structure
> "What do you believe about how these variables relate causally?"

- What causes what?
- Are there confounders that affect both exposure and outcome?
- Are there mediators (variables on the causal path)?
- Are there colliders to avoid conditioning on?

### Effect Type
> "What type of effect do you want to estimate?"

- **Total effect**: All paths from exposure to outcome (don't condition on mediators)
- **Direct effect**: Only the direct path (condition on mediators)
- **Conditional effect**: Effect within subgroups

### Prior Knowledge
> "What do you already know about plausible parameter values?"

- Typical ranges for coefficients
- Expected direction of effects
- Any hard constraints (e.g., must be positive)

---

## Step 1: Setup

### Purpose
Create the project structure and verify all dependencies.

### Questions for User
1. What is the assignment/project identifier? (e.g., "A05")
2. Are there any additional packages needed beyond the standard set?

### Tasks
- [ ] Create folder structure
- [ ] Write `00_setup.R` with library loading
- [ ] Verify `renv` environment is current
- [ ] Test that cmdstanr and rethinking load correctly

### Outputs
- `scripts/00_setup.R`
- Folder structure created

### Validation
```r
# Run this - should complete without errors
source(here::here("homework", "contributions", "{ID}", "scripts", "00_setup.R"))
```

### User Review Gate
- [ ] Folder structure looks correct
- [ ] Libraries load without errors

---

## Step 2: Data Processing and Exploratory Data Analysis

### Purpose
Load data, clean it, understand its structure, and identify any issues before modeling.

### Questions for User
1. Where is the raw data located?
2. What variables need to be renamed or recoded?
3. Are there any known data quality issues?
4. What subsetting criteria should be applied?
5. What unit conversions are needed (e.g., years to months)?

### Tasks
- [ ] Write `01_data_processing.R`:
  - Load raw data
  - Apply filters/subsets
  - Recode variables (especially factors per rethinking conventions)
  - Handle missing data
  - Save processed data to `data/processed/`
- [ ] Write `02_eda.R`:
  - Summary statistics by group
  - Distribution plots for key variables
  - Scatter plots of relationships
  - Check for outliers
  - Correlation matrix (if relevant)

### Methodological Considerations
- **Don't peek at outcome~predictor relationships** in ways that influence model specification
- EDA is for understanding data quality and distributions, not for fishing for significant effects
- Document any data decisions (why exclude certain observations, how handle missing data)

### Outputs
- `scripts/01_data_processing.R`
- `scripts/02_eda.R`
- `data/processed/{dataset}_clean.rds`
- `outputs/figures/eda_01_distributions.png`
- `outputs/figures/eda_02_relationships.png`
- `outputs/figures/eda_03_by_group.png`
- `docs/eda_report.qmd` (optional, if detailed EDA warranted)

### Validation
```r
# Check processed data
d <- readRDS(here::here("homework", "contributions", "{ID}", "data", "processed", "{data}_clean.rds"))
str(d)
summary(d)
```

### User Review Gate
- [ ] Data loaded correctly with expected N
- [ ] Variables recoded appropriately
- [ ] EDA figures show no unexpected issues
- [ ] Any concerns about data quality addressed

---

## Step 3: Synthetic Data Modelling

### Purpose
Define the causal model (DAG), specify the statistical model, generate synthetic data with known parameters, and validate that the model can recover true effects.

### Questions for User
1. Please confirm the DAG structure - does this represent your causal assumptions?
2. What are plausible true parameter values for the simulation?
3. What priors should we use, and why?
4. How many synthetic observations should we generate?

### Tasks
- [ ] Write `03_dag_analysis.R`:
  - Define DAG using dagitty
  - Identify adjustment sets for target estimand
  - Document paths (direct, indirect, backdoor)
  - Specify true parameters for data generation
  - Define model formula and priors
- [ ] Write `04_synthetic_analysis.R`:
  - Generate synthetic data from DAG
  - Fit model to synthetic data
  - Check MCMC diagnostics (Rhat, ESS, trace plots)
  - Compare posterior to true values (parameter recovery)
  - Compute target estimand and check if true value in CI

### Methodological Considerations
- **Adjustment sets**: Use `dagitty::adjustmentSets()` to identify what to condition on
- **Mediators**: Do NOT condition on mediators for total effects
- **Colliders**: Do NOT condition on colliders (opens backdoor paths)
- **Priors**: Should be weakly informative - constrain to plausible ranges but don't dominate data
- **Parameter recovery**: If model can't recover known parameters, something is wrong

### Outputs
- `scripts/03_dag_analysis.R`
- `scripts/04_synthetic_analysis.R`
- `outputs/figures/dag.png`
- `outputs/figures/syn_01_data.png`
- `outputs/figures/syn_02_traceplot.png`
- `outputs/figures/syn_03_parameter_recovery.png`
- `outputs/figures/syn_04_posterior_predictive.png`
- `outputs/figures/syn_05_estimand.png`

### Validation
```r
# Key checks
precis(fit_syn, depth = 2)  # Rhat < 1.01, ESS > 400
# True parameter values should fall within 89% CI
```

### User Review Gate
- [ ] DAG correctly represents causal assumptions
- [ ] Adjustment set is appropriate for target estimand
- [ ] Priors are reasonable and justified
- [ ] MCMC diagnostics pass (Rhat < 1.01, ESS > 400)
- [ ] Model recovers true parameters (true values in 89% CI)
- [ ] Ready to apply to real data

---

## Step 4: Real Data Modelling

### Purpose
Apply the validated model to real data and estimate the target causal effect.

### Questions for User
1. Any final adjustments to the model before fitting to real data?
2. Are there any subgroup analyses to perform?
3. Should we compare multiple models (e.g., with/without certain predictors)?

### Tasks
- [ ] Write `05_real_analysis.R`:
  - Load processed real data
  - Prepare data for ulam (as list with required variables)
  - Fit the same model used for synthetic validation
  - Check MCMC diagnostics
  - Posterior predictive check
  - Compute target estimand with uncertainty

### Methodological Considerations
- **Same model**: Use identical model specification as synthetic analysis
- **Diagnostics first**: Always check Rhat, ESS, trace plots before interpreting
- **Posterior predictive**: Model should generate data similar to observed
- **Propagate uncertainty**: Report full posterior, not just point estimates

### Outputs
- `scripts/05_real_analysis.R`
- `outputs/figures/real_01_data.png`
- `outputs/figures/real_02_traceplot.png`
- `outputs/figures/real_03_posterior_predictive.png`
- `outputs/figures/real_04_estimand.png`
- `outputs/results/fit_real.rds` (saved model object)

### Validation
```r
# Key checks
precis(fit_real, depth = 2)  # Rhat < 1.01, ESS > 400
# Posterior predictive should match observed data patterns
```

### User Review Gate
- [ ] MCMC diagnostics pass
- [ ] Posterior predictive check shows reasonable fit
- [ ] Estimand is interpretable and makes scientific sense
- [ ] Ready for post-processing and reporting

---

## Step 5: Post Processing

### Purpose
Extract insights, compute derived quantities, create publication-quality figures, and write up results.

### Questions for User
1. What contrasts or comparisons are most important to report?
2. What predictions should we generate (e.g., for specific covariate values)?
3. What format is needed for the final report?
4. Are there any sensitivity analyses to perform?

### Tasks
- [ ] Write `06_post_processing.R`:
  - Extract posterior samples
  - Compute contrasts (e.g., group differences)
  - Generate predictions for key scenarios
  - Create publication-quality figures
  - Summary tables of results
- [ ] Write `docs/analysis_report.qmd`:
  - Introduction and research question
  - Methods (DAG, model, priors)
  - Results with figures and tables
  - Discussion and interpretation

### Methodological Considerations
- **Contrasts**: Always compute on posterior samples, then summarize (not vice versa)
- **Predictions**: Include full uncertainty (credible intervals)
- **Interpretation**: Causal language only if assumptions warranted
- **Limitations**: Acknowledge model assumptions and data limitations

### Outputs
- `scripts/06_post_processing.R`
- `outputs/figures/results_01_contrast.png`
- `outputs/figures/results_02_predictions.png`
- `outputs/figures/results_03_summary.png`
- `outputs/results/summary_table.csv`
- `docs/analysis_report.qmd`
- `docs/analysis_report.html` (rendered)

### Validation
```r
# Render the Quarto document
quarto::quarto_render(here::here("homework", "contributions", "{ID}", "docs", "analysis_report.qmd"))
```

### User Review Gate
- [ ] Key findings clearly communicated
- [ ] Figures are publication-quality
- [ ] Uncertainty properly represented
- [ ] Interpretation appropriate given causal assumptions
- [ ] Report ready for submission/sharing

---

## Agent Behavior Guidelines

### Cost Efficiency
- Use **Haiku** for straightforward implementation tasks
- Escalate to **Opus** only for complex decisions or debugging
- Let automated validation replace expensive reviews where possible

### At Each Step
1. **Ask** clarifying questions before implementing
2. **Implement** the tasks for that step
3. **Run** validation checks
4. **Save** outputs to designated locations
5. **Summarize** what was done and any issues found
6. **Wait** for user approval before proceeding

### When Stuck
- Report what's not working with specific error messages
- Propose 2-3 alternative approaches
- Ask user which direction to take

### Documentation
- Comment code explaining "why" not just "what"
- Keep analytical decisions visible in the scripts (not buried in helper functions)
- Update planning docs with any scope changes

---

## Quick Reference: Key Commands

```r
# Load environment
source(here::here("homework", "contributions", "{ID}", "scripts", "00_setup.R"))

# Check MCMC diagnostics
precis(fit, depth = 2)
traceplot(fit)

# Extract posterior samples
post <- extract.samples(fit)

# Save figures
ggsave(here::here("homework", "contributions", "{ID}", "outputs", "figures", "filename.png"),
       plot, width = 8, height = 6)

# Save model object
saveRDS(fit, here::here("homework", "contributions", "{ID}", "outputs", "results", "fit.rds"))
```
