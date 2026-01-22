# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains homework and learning materials for Richard McElreath's Statistical Rethinking 2026 course (Beginner section). The focus is on Bayesian statistics, causal inference with DAGs, and building good R development practices.

## Commands

**R environment:**
```r
# Restore package environment
renv::restore()

# Check environment status
renv::status()

# Install CmdStan (if needed)
cmdstanr::install_cmdstan()
```

**Running homework scripts:**
```r
# From project root, source a setup script first
source("homework/contributions/A01_A02/scripts/00_setup.R")
```

**Git workflow:**
```bash
# Each homework uses feature branches
git checkout -b homework/A03
# Merge to main when complete
git checkout main && git merge homework/A03
```

## Architecture

```
stat_rethinking_2026/
├── homework/
│   ├── contributions/       # Completed homework (A01_A02, A03, etc.)
│   │   └── A0X/
│   │       ├── scripts/     # Numbered R scripts (00_setup.R, 01_*.R, etc.)
│   │       ├── data/        # raw/ (immutable) and processed/
│   │       ├── outputs/     # figures/, results/
│   │       └── docs/        # planning.md, notes.md, writeup.md
│   └── README.md
├── learning_files/
│   ├── context/             # AI collaboration context documents
│   │   ├── agent_instructions.md    # Communication preferences
│   │   ├── learning_roadmap.md      # Progress tracking
│   │   └── weekly_workflow.md       # Session structure
│   └── instructions.md      # R development best practices
├── renv.lock                # Package versions (renv)
└── stat_rethinking_2026.Rproj
```

**Key conventions:**
- Scripts are numbered to indicate execution order (00_, 01_, 02_...)
- Each homework folder is self-contained with its own data, scripts, outputs
- `renv/` manages project-specific package library; use `renv::install()` for new packages
- Raw data in `data/raw/` is never modified; transformations go to `data/processed/`

## Key Packages

- **rethinking** - McElreath's course package (builds on cmdstanr)
- **cmdstanr** - Modern Stan interface for MCMC
- **tidyverse** - Data manipulation
- **here** - Portable path handling

## User Preferences

The learner has specific preferences documented in `learning_files/context/agent_instructions.md`:
- Prefers learning by doing over passive explanation
- Wants direct feedback; comfortable being corrected
- Responds well to "why" explanations before "how"
- Default to tutor mode: guide discovery through questions rather than giving answers directly
- When stuck, offer hints before solutions
