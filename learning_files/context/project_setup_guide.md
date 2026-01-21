# Project Setup Guide: Statistical Rethinking 2026

*A step-by-step guide to setting up your learning project on Windows*

---

## Recommended Project Structure

This structure treats each homework week as a mini-project with its own data, scripts, and outputs. The top level provides shared code and project-wide notes. See the Weekly Workflow document for detailed explanation of each folder's purpose.

```
C:/Projects/stat_rethinking/
├── stat_rethinking.Rproj          # R project file (VSCode recognizes this)
├── .gitignore                      # Files git should ignore
├── README.md                       # What this project is
├── renv.lock                       # Package versions (created by renv)
├── renv/                           # renv's internal folder
│
├── R/                              # Shared code across all homework
│   ├── setup.R                     # Common setup (libraries, options)
│   └── shared_utils.R              # Functions used across multiple weeks
│
├── homework/                       # Each week is a self-contained mini-project
│   ├── week01/
│   │   ├── README.md
│   │   ├── data/
│   │   │   ├── raw/
│   │   │   └── processed/
│   │   ├── docs/
│   │   │   ├── planning.md
│   │   │   ├── notes.md
│   │   │   └── writeup.md
│   │   ├── scripts/
│   │   │   ├── 00_setup.R
│   │   │   ├── 01_data_prep.R
│   │   │   ├── 02_problem1.R
│   │   │   └── utils/
│   │   ├── tests/
│   │   └── outputs/
│   │       ├── figures/
│   │       └── results/
│   ├── week02/
│   │   └── ... (same structure)
│   └── ...
│
├── notes/                          # Your learning journal
│   ├── weekly/
│   │   ├── week01_notes.md
│   │   └── ...
│   └── concepts/                   # Deep-dives on specific topics
│       ├── understanding_priors.md
│       └── dag_patterns.md
│
└── explorations/                   # Experiments not tied to homework
    └── playing_with_posterior_samples.R
```

**Why this structure:** The separation into weeks with consistent subfolders means you always know where to find things. Each week is self-contained enough that you could share just that folder with someone reviewing your homework. The `R/` folder holds code that's genuinely reusable across weeks, keeping it from becoming cluttered with one-off functions.

---

## Step-by-Step Setup

### Step 1: Create the Project Folder

Open a terminal (PowerShell or Command Prompt) and run:

```powershell
mkdir C:\Projects\stat_rethinking
cd C:\Projects\stat_rethinking
```

Or just create the folder in File Explorer. Keep the path short to avoid Windows path length issues.

### Step 2: Open in VSCode

```powershell
code .
```

Or open VSCode and use File → Open Folder.

### Step 3: Create the Project File

In VSCode, create a new file called `stat_rethinking.Rproj` with this content:

```
Version: 1.0

RestoreWorkspace: No
SaveWorkspace: No
AlwaysSaveHistory: No

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

AutoAppendNewline: Yes
StripTrailingWhitespace: Yes
```

This file tells both RStudio and VSCode's R extension to treat this folder as an R project. The settings disable some defaults that hurt reproducibility (like restoring your workspace on startup).

### Step 4: Create the Folder Structure

In VSCode's terminal (Terminal → New Terminal), with R running:

```r
# Create the top-level folder structure
dir.create("R")
dir.create("homework")
dir.create("notes/weekly", recursive = TRUE)
dir.create("notes/concepts")
dir.create("explorations")

# Create starter files for shared code
file.create("R/setup.R")
file.create("R/shared_utils.R")
```

Note: You don't need to create the per-week homework structure now. The `init_homework_week()` function (defined in the Weekly Workflow document) will create each week's folders when you start that homework.

### Step 5: Create .gitignore

Create a file called `.gitignore` in the project root:

```
# R artifacts
.Rhistory
.Rdata
.RDataTmp
*.Rproj.user/

# renv (the library itself is large; lockfile is enough)
renv/library/
renv/staging/
renv/python/
renv/sandbox/

# Stan compiled models
*.exe
*.o
*.so
*.dll

# Large output files (keep structure, ignore heavy content)
# Comment these out if you want to track outputs in git
# homework/*/outputs/results/*.rds
# homework/*/outputs/figures/*.png

# OS files
Thumbs.db
.DS_Store

# VSCode
.vscode/

# Sensitive data (if any)
**/data/private/

# Temporary files
*.tmp
*~
```

### Step 6: Initialize Git

In VSCode's terminal (use PowerShell or Git Bash):

```bash
git init
git add .
git commit -m "Initial project structure"
```

### Step 7: Initialize renv

Back in R:

```r
# Initialize renv - this creates renv.lock and the renv/ folder
renv::init()
```

When prompted, choose option 1 (use the existing project structure).

### Step 8: Install Required Packages

Still in R, with renv now active:

```r
# Core packages for the course
renv::install("rethinking")       # McElreath's package
renv::install("cmdstanr")         # Modern Stan interface

# Useful extras
renv::install("here")             # Path handling
renv::install("tidyverse")        # Data manipulation (optional but handy)

# Snapshot your package state
renv::snapshot()
```

### Step 9: Install CmdStan

This step compiles the Stan engine. It takes a few minutes:

```r
# Check if your C++ toolchain is ready
cmdstanr::check_cmdstan_toolchain()

# If the above fails, install RTools first:
# https://cran.r-project.org/bin/windows/Rtools/
# Then restart R and try again

# Install CmdStan
cmdstanr::install_cmdstan()
```

### Step 10: Verify Everything Works

Create a file `explorations/test_setup.R`:

```r
# Test setup script
# Run this to verify your environment is working correctly

# ---- Environment Check ----
# These commands help you understand where R is looking for things
cat("Working directory:", getwd(), "\n")
cat("\nLibrary paths (R looks for packages in this order):\n")
for (path in .libPaths()) {
  cat("  ", path, "\n")
}

# ---- Package Check ----
# Load the rethinking package - this will fail if installation didn't work
library(rethinking)
cat("\n✓ rethinking package loaded successfully\n")

# ---- Stan Check ----
# This compiles and runs a minimal Stan model to verify the toolchain works
# First compilation is slow (30-60 seconds) - this is normal
cat("\nTesting Stan compilation (this may take a minute on first run)...\n")

model <- ulam(
  alist(
    y ~ dnorm(mu, sigma),
    mu ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ),
  data = list(y = rnorm(100)),
  chains = 1,        # Just one chain for this test
  cores = 1,
  refresh = 0        # Suppress sampling output
)

cat("✓ Stan model compiled and ran successfully\n")

# ---- Structure Check ----
# Verify the expected folders exist
expected_dirs <- c("R", "homework", "notes/weekly", "notes/concepts", "explorations")
cat("\nFolder structure check:\n")
for (d in expected_dirs) {
  status <- if (dir.exists(d)) "✓" else "✗"
  cat(" ", status, d, "\n")
}

# ---- Summary ----
cat("\n========================================\n")
cat("Setup verification complete.\n")
cat("You're ready to begin Statistical Rethinking.\n")
cat("\nNext step: Create your first homework week with:\n")
cat("  source('R/shared_utils.R')\n")
cat("  init_homework_week('week03')\n")
cat("========================================\n")
```

If this script runs without errors, your environment is ready. The first Stan compilation takes longer because it's building the model from scratch; subsequent runs of the same model are much faster.

### Step 11: Commit the Working State

```bash
git add .
git commit -m "Working setup with rethinking and cmdstanr"
```

---

## Create Your README

Create `README.md` in the project root:

```markdown
# Statistical Rethinking 2026

My work through Richard McElreath's Statistical Rethinking course (2026 edition), with a focus on building good R development practices alongside statistical understanding.

## Setup

This project uses `renv` for package management. To restore the environment:

```r
renv::restore()
```

Requires CmdStan. If not installed:

```r
cmdstanr::install_cmdstan()
```

## Structure

The project is organized as follows:

**`homework/`** contains each week's work as a self-contained mini-project. Each week has its own `data/`, `scripts/`, `outputs/`, and `docs/` folders. See any week's README for details on that specific homework.

**`R/`** holds code shared across all homework weeks. `setup.R` contains common library loads and options. `shared_utils.R` contains helper functions that proved useful across multiple weeks.

**`notes/`** contains my learning journal. `weekly/` has week-by-week reflections. `concepts/` has deeper explorations of specific topics.

**`explorations/`** is a sandbox for experiments and tangents that aren't part of homework.

## Git Workflow

Each homework week is developed on its own branch (`homework/week03`, etc.) and merged to main when complete. This keeps main stable and provides a clear history.

## Progress

- [x] Week 1: Introduction
- [x] Week 2: Garden of Forking Data
- [ ] Week 3: Geocentric Models
- [ ] Week 4: Categories & Curves
- [ ] Week 5: Elemental Confounds
- [ ] Week 6: Good and Bad Controls
- [ ] Week 7: Overfitting
- [ ] Week 8: Markov chain Monte Carlo
- [ ] Week 9: Modeling Events
- [ ] Week 10: Multilevel Models
```

---

## Creating a New Homework File

When starting each week's homework, create a file with this template:

```r
# Week 3: Geocentric Models
# Statistical Rethinking 2026 - Homework
# 
# Concepts covered:
# - Linear regression as a Bayesian model
# - Choosing priors
# - Posterior interpretation
#
# Date started: 2026-01-XX

# ---- Setup ----
library(rethinking)
library(here)
source(here("R", "helpers.R"))  # Your helper functions

# ---- Problem 1 ----
# [Restate the problem in your own words]
#
# My thinking:
# - [What approach will you take?]
# - [What concepts from the lecture apply?]
# - [What are you uncertain about?]



# ---- Problem 2 ----
# [Continue pattern...]
```

---

## Connecting to GitHub

If you want to push this to your GitHub:

```bash
# Add your remote (replace with your actual repo URL)
git remote add origin https://github.com/yourusername/stat_rethinking.git

# Push
git push -u origin main
```

---

## Troubleshooting

**"renv not found" after opening project**: The R extension might have started before loading the project's renv. Restart R: in VSCode command palette (Ctrl+Shift+P), type "R: Restart R".

**Stan compilation errors**: Make sure RTools is installed and R has been restarted after installing it. Run `cmdstanr::check_cmdstan_toolchain()` to diagnose.

**Slow first model**: Stan compiles models the first time you run them. Subsequent runs of the same model are fast. This is normal.

**"Package not found" after renv::install()**: Make sure you ran `renv::snapshot()` after installing. Check `renv::status()` to see if packages are in sync.

---

## Next Steps

Once your setup is verified:

1. **Add the `init_homework_week()` function to your shared utilities.** Copy the function definition from the Weekly Workflow document into `R/shared_utils.R`. This function creates the folder structure for each homework week automatically.

2. **Initialize your first homework week.** Since you're at week 3:
   ```r
   source("R/shared_utils.R")
   init_homework_week("week03")
   ```

3. **Create your git branch and start working:**
   ```bash
   git checkout -b homework/week03
   git add homework/week03/
   git commit -m "week03: Initialize folder structure"
   ```

4. **Follow the Weekly Workflow document** for your session rhythm and how to commit as you progress.

5. **When you complete the homework,** merge back to main:
   ```bash
   git checkout main
   git merge homework/week03
   git push
   ```

The project structure and git workflow will feel mechanical at first, but within a few weeks it becomes automatic. The consistency pays off when you want to review past work or find that useful function you wrote three weeks ago.

---

*Document version: 1.0 | Last updated: January 2026*
