# Weekly Workflow: Statistical Rethinking + Development Practice

*A practical template for integrating Bayesian learning, R development, and good habits*

---

## Purpose

This document describes a weekly workflow that uses your Statistical Rethinking homework as the vehicle for practicing all four learning goals simultaneously. The idea is not to add extra work, but to be intentional about *how* you do the work you're already doing.

---

## Project Structure

The folder structure below treats each homework as a self-contained mini-project while keeping everything organized under a single repository. This mirrors how you might structure a real analysis project at work: separate folders for inputs, processing, and outputs, with clear boundaries between exploration and final deliverables.

### Top-Level Structure

```
C:/Projects/stat_rethinking/
├── stat_rethinking.Rproj
├── .gitignore
├── README.md
├── renv.lock
├── renv/
│
├── R/                              # Shared code across all homework
│   ├── setup.R                     # Common setup (libraries, options)
│   └── shared_utils.R              # Functions used across multiple weeks
│
├── homework/                       # Each week is a mini-project
│   ├── week01/
│   ├── week02/
│   ├── week03/
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

### Homework Week Structure

Each week's homework folder follows a consistent structure. This consistency means you always know where to find things, and you practice the same organizational habits each week until they become automatic.

```
homework/week03/
│
├── README.md                       # Overview of this week's homework
│
├── data/
│   ├── raw/                        # Original data (never modify)
│   │   └── howell1.csv
│   └── processed/                  # Cleaned/transformed data
│       └── howell1_adults.rds
│
├── docs/
│   ├── planning.md                 # Initial thoughts before coding
│   ├── notes.md                    # Notes while working
│   └── writeup.md                  # Final explanations/reflections
│
├── scripts/
│   ├── 00_setup.R                  # Load libraries, source utilities
│   ├── 01_data_prep.R              # Data loading and cleaning
│   ├── 02_problem1.R               # Each problem gets its own script
│   ├── 03_problem2.R
│   ├── 04_problem3.R
│   └── utils/                      # Helper functions for this week
│       ├── model_helpers.R         # Functions for building/comparing models
│       └── plot_helpers.R          # Custom plotting functions
│
├── tests/
│   └── test_utils.R                # Tests for your helper functions
│
└── outputs/
    ├── figures/                    # Saved plots
    │   ├── problem1_posterior.png
    │   └── problem2_dag.png
    └── results/                    # Model outputs, tables
        ├── model1_summary.rds
        └── comparison_table.csv
```

### Why This Structure Matters

Let me explain the reasoning behind each folder, since understanding the "why" will help you remember and adapt the structure.

**data/raw/ vs data/processed/**: Raw data is sacred—you never modify it. Any cleaning, filtering, or transformation creates a new file in `processed/`. This means you can always trace your work back to the original, and if you discover a bug in your data prep, you can re-run from the unchanged source. The `raw/` folder is essentially read-only.

**docs/**: Separating documentation from code makes both easier to find. `planning.md` captures your initial thinking before you write code (this is valuable to review later—did your plan match reality?). `notes.md` captures insights as you work. `writeup.md` is your polished explanation of what you did and learned.

**scripts/**: The numbered prefix (`00_`, `01_`, `02_`) indicates execution order. Someone (including future you) can understand the workflow at a glance. The `00_setup.R` script is sourced by everything else—it's the single place where you load libraries and set options, ensuring consistency.

**scripts/utils/**: Functions that are specific to this homework live here. If a function proves useful across multiple weeks, promote it to the top-level `R/shared_utils.R`. This separation prevents your shared utilities from becoming a dumping ground for one-off code.

**tests/**: You don't need to test everything, but testing your utility functions catches bugs before they corrupt your analysis. Start simple: "Does this function return the expected output for a known input?"

**outputs/figures/ and outputs/results/**: Separating outputs from scripts makes it easy to regenerate everything. If you delete the `outputs/` folder and re-run your scripts, you should get the same results (reproducibility). It also makes it easy to find that plot you want to include in your notes.

---

## Git Workflow: Treating Each Homework as a Feature

Professional software development uses "feature branches"—each new feature is developed on its own branch, then merged into the main branch when complete. You'll use the same pattern for homework, which gives you several benefits: your `main` branch always contains complete, working homework; you can experiment freely on a feature branch without breaking anything; and your git history tells a clear story of what you did each week.

### The Branch Model

```
main ─────●─────────────●─────────────●─────────────●──────
          │             │             │             │
          │  week01     │  week02     │  week03     │
          └──●──●──●────┘──●──●──●────┘──●──●──●────┘
             commits       commits       commits
```

Each homework starts on a new branch, accumulates commits as you work, and merges back to `main` when complete.

### Starting a New Homework Week

When you begin a new week's homework, create a branch and set up the folder structure:

```bash
# Make sure you're on main and it's up to date
git checkout main
git pull  # if you're syncing with GitHub

# Create and switch to a new branch for this week
git checkout -b homework/week03

# The branch name 'homework/week03' uses a prefix to group related branches
# You could also use 'hw03' or 'week03' - pick a convention and stick with it
```

Then create the folder structure for this week. You can do this in R:

```r
# Run this once at the start of each homework week
week <- "week03"
base <- file.path("homework", week)

# Create the folder structure
dirs <- c(
  file.path(base, "data", "raw"),
  file.path(base, "data", "processed"),
  file.path(base, "docs"),
  file.path(base, "scripts", "utils"),
  file.path(base, "tests"),
  file.path(base, "outputs", "figures"),
  file.path(base, "outputs", "results")
)
lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

# Create starter files
file.create(file.path(base, "README.md"))
file.create(file.path(base, "docs", "planning.md"))
file.create(file.path(base, "docs", "notes.md"))
file.create(file.path(base, "scripts", "00_setup.R"))
```

Commit this initial structure:

```bash
git add homework/week03/
git commit -m "week03: Initialize folder structure"
```

### Working on the Homework

As you work, commit frequently with meaningful messages. Think of each commit as a save point you could return to if needed. Good commit messages describe *what* changed and *why*, not just *that* something changed.

```bash
# After setting up your data
git add homework/week03/scripts/01_data_prep.R
git add homework/week03/data/processed/
git commit -m "week03: Prepare Howell1 data, filter to adults only"

# After completing problem 1
git add homework/week03/scripts/02_problem1.R
git add homework/week03/outputs/
git commit -m "week03 Q1: Linear model of height~weight, uniform prior on sigma"

# After struggling and then figuring something out
git commit -m "week03 Q2: Fix prior - realized sigma must be positive, use exponential"

# After extracting a useful function
git add homework/week03/scripts/utils/plot_helpers.R
git commit -m "week03: Extract plot_posterior() to utils for reuse"
```

Notice how these messages tell a story. Months later, you can read them and understand your journey through the homework.

### Completing a Homework Week

When you've finished the homework and are satisfied with your work:

```bash
# Make sure everything is committed
git status  # Should show "nothing to commit, working tree clean"

# Switch back to main
git checkout main

# Merge your homework branch
git merge homework/week03

# The merge commit marks the completion of this homework
# Push to GitHub if you're syncing
git push
```

Your `main` branch now contains the complete week03 homework. The branch `homework/week03` still exists if you want to reference it, or you can delete it:

```bash
git branch -d homework/week03  # Delete the branch (optional)
```

### Viewing Your Progress

Git gives you tools to review your journey:

```bash
# See all your branches
git branch -a

# See commit history with a graph
git log --oneline --graph --all

# See what changed in a specific week
git log --oneline main..homework/week03  # if branch still exists

# See the diff between two weeks
git diff homework/week02..homework/week03 -- homework/
```

---

## Template Files

### 00_setup.R (Standard Setup Script)

Create this at `homework/week03/scripts/00_setup.R`:

```r
# Week 03: Geocentric Models
# Setup script - source this at the start of other scripts
#
# This script:
# - Loads required libraries
# - Sources utility functions  
# - Sets common options
# - Defines paths for this homework

# ---- Libraries ----
library(rethinking)
library(here)

# ---- Project paths ----
# Using here() makes paths work regardless of working directory
hw_root <- here("homework", "week03")
data_raw <- file.path(hw_root, "data", "raw")
data_proc <- file.path(hw_root, "data", "processed")
output_fig <- file.path(hw_root, "outputs", "figures")
output_res <- file.path(hw_root, "outputs", "results")

# ---- Source utilities ----
# Shared utilities (used across all homework)
source(here("R", "shared_utils.R"))

# Week-specific utilities
utils_dir <- file.path(hw_root, "scripts", "utils")
util_files <- list.files(utils_dir, pattern = "\\.R$", full.names = TRUE)
lapply(util_files, source)

# ---- Options ----
options(mc.cores = parallel::detectCores())  # Use all CPU cores for Stan

# ---- Confirmation ----
cat("Setup complete for Week 03\n")
cat("Working directory:", getwd(), "\n")
cat("Homework root:", hw_root, "\n")
```

### README.md Template (Per-Week)

Create this at `homework/week03/README.md`:

```markdown
# Week 03: Geocentric Models

## Topics Covered
- Linear regression as a Bayesian model
- Choosing and interpreting priors
- Posterior predictive checks

## Problems
1. [Brief description of problem 1]
2. [Brief description of problem 2]
3. [Brief description of problem 3]

## Key Learnings
<!-- Fill this in after completing the homework -->

## Files
- `scripts/01_data_prep.R` - Load and clean the Howell1 data
- `scripts/02_problem1.R` - [what this does]
- `scripts/03_problem2.R` - [what this does]

## Notes
See `docs/writeup.md` for detailed explanations of my approach.
```

### planning.md Template

Create this at `homework/week03/docs/planning.md`:

```markdown
# Week 03: Planning

## Before I Start

### What concepts from the lecture do I need to apply?
- 
- 

### What am I uncertain about?
- 
- 

### What's my initial approach to each problem?

**Problem 1:**

**Problem 2:**

**Problem 3:**

## After Completing

### Did my plan match reality?

### What would I do differently next time?
```

---

## Automating the Setup

Since you'll create this structure every week, you can automate it. Add this function to your `R/shared_utils.R`:

```r
#' Initialize a new homework week
#' 
#' Creates the standard folder structure and template files for a new week.
#' 
#' @param week Character string like "week03" or "week10"
#' @return Invisibly returns the path to the created homework folder
#' 
#' @examples
#' init_homework_week("week03")
init_homework_week <- function(week) {
  
  base <- here::here("homework", week)
  

  if (dir.exists(base)) {
    stop("Folder already exists: ", base, "\nDelete it first or choose a different week.")
  }
  
  # Create directories
  dirs <- c(
    file.path(base, "data", "raw"),
    file.path(base, "data", "processed"),
    file.path(base, "docs"),
    file.path(base, "scripts", "utils"),
    file.path(base, "tests"),
    file.path(base, "outputs", "figures"),
    file.path(base, "outputs", "results")
  )
  lapply(dirs, dir.create, recursive = TRUE)
  
  # Create README
  readme_content <- sprintf('# %s

## Topics Covered
- 

## Problems
1. 
2. 
3. 

## Key Learnings
<!-- Fill in after completing -->

## Files
- `scripts/00_setup.R` - Setup and configuration
', week)
  writeLines(readme_content, file.path(base, "README.md"))
  
  # Create planning doc
  planning_content <- sprintf('# %s: Planning

## Before I Start

### What concepts from the lecture apply?
- 

### What am I uncertain about?
- 

### Initial approach to each problem

**Problem 1:**

**Problem 2:**

**Problem 3:**
', week)
  writeLines(planning_content, file.path(base, "docs", "planning.md"))
  
  # Create empty notes and writeup
  file.create(file.path(base, "docs", "notes.md"))
  file.create(file.path(base, "docs", "writeup.md"))
  
  # Create setup script
  setup_content <- sprintf('# %s: Setup
# Source this at the start of other scripts

library(rethinking)
library(here)

# Paths
hw_root <- here("homework", "%s")
data_raw <- file.path(hw_root, "data", "raw")
data_proc <- file.path(hw_root, "data", "processed")
output_fig <- file.path(hw_root, "outputs", "figures")
output_res <- file.path(hw_root, "outputs", "results")

# Shared utilities
source(here("R", "shared_utils.R"))

# Week-specific utilities
utils_dir <- file.path(hw_root, "scripts", "utils")
util_files <- list.files(utils_dir, pattern = "\\\\.R$", full.names = TRUE)
lapply(util_files, source)

# Options
options(mc.cores = parallel::detectCores())

cat("Setup complete for %s\\n")
', week, week, week)
  writeLines(setup_content, file.path(base, "scripts", "00_setup.R"))
  
  # Create .gitkeep files for empty directories
  file.create(file.path(base, "data", "raw", ".gitkeep"))
  file.create(file.path(base, "scripts", "utils", ".gitkeep"))
  file.create(file.path(base, "tests", ".gitkeep"))
  file.create(file.path(base, "outputs", "figures", ".gitkeep"))
  file.create(file.path(base, "outputs", "results", ".gitkeep"))
  
  cat("Created homework structure at:", base, "\n")
  cat("\nNext steps:\n")
  cat("1. git checkout -b homework/", week, "\n", sep = "")
  cat("2. git add homework/", week, "/\n", sep = "")
  cat("3. git commit -m '", week, ": Initialize folder structure'\n", sep = "")
  

  invisible(base)
}
```

Now starting a new week is a single command:

```r
init_homework_week("week03")
```

---

## Weekly Rhythm

### Session 1: Engage with the Lecture (~1.5 hours)

**Before the lecture:**

Open your project in VSCode. Verify your environment with the three diagnostic commands (`getwd()`, `.libPaths()`, `renv::status()`). If starting a new week, create the folder structure:

```r
source(here::here("R", "shared_utils.R"))
init_homework_week("week03")
```

Then create your git branch:

```bash
git checkout -b homework/week03
git add homework/week03/
git commit -m "week03: Initialize folder structure"
```

**During/after the lecture:**

Take notes in `docs/planning.md` within your homework folder. Focus on:
- Key concepts in your own words, not transcription
- Things that surprised you or conflicted with prior understanding
- Questions you want to explore

**First homework attempt:**

Read the homework problems. Before looking at any hints or asking for help, attempt each problem. Create a script file for each problem (`scripts/02_problem1.R`, etc.) and write your reasoning as comments:

```r
# Problem 1: [Restate the problem in your own words]
# 
# My thinking: I need to [your reasoning]
# This connects to [concept from lecture] because [why]

source(here::here("homework", "week03", "scripts", "00_setup.R"))

# [Your code attempt here]
```

**Git checkpoint:**

```bash
git add homework/week03/scripts/
git commit -m "week03: Initial homework attempt before any help"
```

This checkpoint is valuable. It lets you see how far you got independently, which shows your actual understanding.

---

### Session 2: Work Through Problems (~1.5 hours)

**When you get stuck:**

Don't immediately ask for the answer. Instead:

First, articulate *exactly* where you're stuck. Write it as a comment in your code or in `docs/notes.md`. The act of writing often clarifies the confusion.

Second, try a different approach. Even if you don't think it'll work, the failure teaches you something about why your first approach didn't work.

Third, if still stuck after genuine effort (15+ minutes), use AI tutor mode:

> "I'm working on [problem]. I've tried [approach]. I expected [X] but got [Y]. I think the issue might be [hypothesis]. Can you give me a hint about where my thinking went wrong?"

**As you make progress:**

Commit incrementally with meaningful messages. Each commit is a save point you can return to:

```bash
git add homework/week03/
git commit -m "week03 Q1: Working linear regression, unsure about prior choice"
git commit -m "week03 Q1: Fixed prior after understanding why sigma must be positive"
git commit -m "week03 Q2: First attempt at DAG, unclear about confound direction"
```

These messages are notes to yourself. When you review later, they tell the story of your learning—including the struggles, which are often the most educational parts.

**Extract helper functions:**

If you write similar code for multiple problems, extract it to `scripts/utils/` within this week's folder:

```r
# In homework/week03/scripts/utils/plot_helpers.R

#' Plot posterior density with consistent styling
#' 
#' @param samples Vector of posterior samples
#' @param title Plot title
plot_posterior <- function(samples, title = "") {
  # Using the rethinking package's dens() as base
  dens(samples, main = title, xlab = "Parameter value", col = "steelblue")
}
```

If the function proves useful across multiple weeks, promote it to `R/shared_utils.R`.

---

### Session 3: Review and Consolidate (~1 hour)

**Compare to solutions (if available):**

Don't just check if you got the "right answer." The goal is to understand the reasoning, not just match the output. Ask yourself these questions: Where was my approach different from the solution? Was my approach *wrong* or just *different*? (Different can be fine, sometimes even better.) What did the solution do more elegantly? Is there a principle I can extract for future problems?

Write your reflections in `docs/writeup.md`. This document becomes valuable when you return to this material later.

**Refactor your code:**

Take code that works and make it clearer. This isn't about getting more points—it's about practicing the skill of recognizing and producing good code. Look for repeated patterns, unclear variable names, overly complex logic that could be simplified.

Ask yourself: Are my variable names meaningful? Would someone reading this understand what's happening without my comments? Is there repeated code I could extract into a function?

**Save outputs:**

Save any plots you want to keep to `outputs/figures/`. Save model objects or summary tables to `outputs/results/`. This makes it easy to reference your work later without re-running everything.

```r
# Save a plot
png(file.path(output_fig, "problem1_posterior.png"), width = 800, height = 600)
plot_posterior(samples, "Posterior distribution of mu")
dev.off()

# Save a model summary
saveRDS(precis(model), file.path(output_res, "model1_summary.rds"))
```

**Update your notes:**

Add a "What I Learned" section to `docs/writeup.md`:

```markdown
## What I Learned This Week

### Concepts solidified
- [Concept] finally clicked when I realized [insight]

### Still fuzzy
- [Concept] - I can use it but don't fully understand why

### Mistakes I made
- [Mistake]: Originally thought [X], but actually [Y] because [reason]

### Good patterns to remember
- [Pattern]: Useful for [situation]
```

**Reflect and update context documents:**

This step closes the learning loop. Your context documents should evolve based on what you actually experience, not just your initial guesses about how you learn. Take 10-15 minutes to reflect on the session and update your documents.

Ask yourself these questions (or have your AI agent ask you):

1. What concept clicked for me during this session that was fuzzy before?
2. Where did I get stuck longest, and what eventually unstuck me?
3. Did anything about this session's workflow feel like unnecessary friction?
4. Did anything work particularly well that I want to repeat?
5. Did I discover a learning preference I wasn't aware of before?

Based on your answers, consider updates to each context document:

**`agent_instructions.md`** — Add newly discovered preferences. For example:
- "I learn priors better through simulation than through algebraic derivation"
- "When stuck, visual diagnostics (trace plots, posterior plots) help more than re-reading theory"
- "I focus better in 45-minute bursts than 1.5-hour sessions"

**`learning_roadmap.md`** — Update your progress:
- Check off progress markers you've achieved
- Add new topics you've discovered you need to learn
- Note concepts that moved from "fuzzy" to "solid" (or vice versa)

**`weekly_workflow.md`** — Adjust the workflow itself:
- If a step consistently feels like friction without benefit, simplify or remove it
- If you discovered a useful practice, add it to the workflow
- Adjust time estimates based on how long things actually take

**`r_environment_guide.md`** — Add solutions to problems you encountered:
- If you hit an environment issue and solved it, document the symptoms and fix
- These notes help you (and your AI agent) solve similar issues faster next time

Create a brief record of these updates in `docs/writeup.md`:

```markdown
## Context Document Updates

**Date:** [today's date]

**Changes made:**
- `agent_instructions.md`: Added preference for [X]
- `learning_roadmap.md`: Marked [concept] as solidified
- [etc.]

**Reasoning:**
[Brief note on why you made these changes]
```

This record helps you track how your learning process itself is evolving over the course.

**Complete the homework and merge:**

```bash
git add .
git commit -m "week03: Complete - [brief summary of key learning]"
git checkout main
git merge homework/week03
git push  # if syncing with GitHub
```

Your `main` branch now contains completed, working homework. The homework branch has the full history of your journey through the problems.

---

## Using AI Effectively in This Workflow

### For Conceptual Questions (Tutor Mode)

> "I'm reading about [concept] in Statistical Rethinking Chapter [X]. The book says [quote/paraphrase]. I think this means [your interpretation]. Is my understanding correct? If not, where am I going wrong?"

Expect the AI to probe your understanding rather than just confirm or deny.

### For Code Problems (Debugging Mode)

> "I'm running this code: [code]. I expected [X] but got [Y]. Here's the error message: [error]. My hypothesis is that [your guess]. What am I missing?"

Expect the AI to ask clarifying questions and offer diagnostic steps, not immediately rewrite your code.

### For Code Review

After your code works:

> "This code solves [problem] and works correctly. Can you review it for clarity, efficiency, and good practices? Point out anything that could be improved, even if it works."

Expect specific, actionable feedback.

### For Environment Issues

> "I'm in VSCode with renv. When I run [command], I get [result]. My `.libPaths()` shows [paths]. My `getwd()` shows [dir]. I expected [expected behavior] but got [actual behavior]. What might be causing this?"

The diagnostic information helps the AI form accurate hypotheses.

---

## Checklists

### Before Starting Any Session

- [ ] Project open in VSCode (not just a file, the whole folder)
- [ ] Verified working directory with `getwd()`
- [ ] Verified library paths with `.libPaths()`
- [ ] If using renv: `renv::status()` shows in sync

### After Each Session

- [ ] All changes committed with meaningful message
- [ ] No uncommitted changes left hanging

### After Each Week

- [ ] All homework scripts complete and committed
- [ ] `docs/writeup.md` updated with key learnings
- [ ] Reflection questions answered and context documents updated as needed
- [ ] Any reusable code extracted (to `scripts/utils/` for week-specific, or `R/shared_utils.R` for cross-week)
- [ ] Homework branch merged to main
- [ ] `renv::snapshot()` if you installed new packages

---

## Troubleshooting Decision Tree

**Problem: Code runs differently than expected**

1. Is your working directory correct? (`getwd()`)
2. Are the right packages loaded? (`search()`)
3. Is a variable being shadowed by another object? (`ls()` to see what's in your environment)

**Problem: Package not found**

1. Is renv active? (`renv::status()`)
2. Is the package in your project library? (`.libPaths()` then check)
3. Did you install via renv? (`renv::install()`)

**Problem: Works in console, fails when running file**

1. Check working directory—it can differ between console and sourced file
2. Use `here::here()` for paths instead of relative paths
3. Ensure all `library()` calls are at the top of the file

**Problem: Git shows unexpected changes**

1. `git status` to see what changed
2. `git diff` to see exactly what's different
3. If you want to undo: `git checkout -- filename`
4. If you want to keep: `git add . && git commit -m "description"`

---

## Adjusting This Workflow

This workflow is a starting point, not a prescription. The reflection step at the end of each week is your mechanism for improving it. After 2-3 weeks, you'll have enough data to see patterns.

Questions to ask yourself periodically:

**What's working well?** Keep doing it. Document why it works so you remember.

**What feels like friction without benefit?** Either remove it or understand why it exists. Some friction is valuable (like committing frequently, which feels slow but saves you when things break). Other friction is just bureaucracy you inherited from this document. Drop what doesn't serve you.

**What's missing?** If you find yourself repeatedly doing something that isn't in the workflow, add it. Your actual practice should shape the document, not the other way around.

**Are the time estimates accurate?** The 1.5 / 1.5 / 1.0 hour split is a guess. Your actual rhythm might be different. Adjust based on what you observe.

The meta-skill of designing your own learning process is as valuable as any specific technique. By the end of the course, this workflow document should look quite different from how it started—and those differences are evidence of your growth in understanding how you learn best.

---

*Document version: 1.0 | Last updated: January 2026*
