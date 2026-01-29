# How To Work: Statistical Rethinking + R Development

*Learning emerges from doing, not from following checklists*

---

## The Core Principle

Just start the homework. The learning—in Bayesian statistics, R development, environment understanding, and AI collaboration—happens through actually working, getting stuck, and figuring things out.

This document captures patterns that have emerged from experience, not prescriptions to follow before you have that experience.

---

## Project Structure: The Basics

Your repo is organized like this:

```
stat_rethinking_2026/
├── .Rproj                          # Makes this a proper R project
├── renv/ and renv.lock            # Package management
├── homework/contributions/         # Your completed homework
│   ├── A01_A02/
│   ├── A03/
│   └── ...
└── learning_files/
    ├── context/                    # AI collaboration docs
    └── instructions.md             # R best practices reference
```

Each homework folder is self-contained:
```
homework/contributions/A04/
├── scripts/         # Numbered scripts following scientific workflow
│   ├── 00_setup.R
│   ├── 01_scientific_model.R
│   ├── 02_prior_checking.R
│   ├── 03_fit_real_data.R
│   └── 04_postprocessing.R
├── data/           # raw/ (never modify) and processed/
├── outputs/        # figures/ and results/
└── docs/           # Your notes and reflections
```

**Script workflow** (following Statistical Rethinking methodology):
- **00_setup.R**: Libraries, options, helper functions
- **01_scientific_model.R**: DAG definition, synthetic data generation, initial visualizations
- **02_prior_checking.R**: Statistical model definition, prior predictive simulation, fit on synthetic data
- **03_fit_real_data.R**: Load real data, fit model, posterior predictive checks, diagnostics
- **04_postprocessing.R**: Extract causal estimates, counterfactuals, final visualizations

**Why it matters:**
- **data/raw/** is read-only. Any processing creates new files in **data/processed/**
- **Scientific workflow** ensures you validate models before fitting real data
- **Each script sources the previous** creating a reproducible pipeline
- **DAG comes first** - makes causal assumptions explicit before statistical modeling
- **Separating outputs** means you can regenerate everything by re-running scripts

---

## Git: Simple, Effective Pattern

Use branches for each homework. This keeps `main` clean and lets you experiment freely.

**Starting new homework:**
```bash
git checkout -b homework/A03
# Create your folder structure, then:
git add homework/contributions/A03/
git commit -m "A03: Initialize structure"
```

**While working:**
Commit when you make progress or learn something. Messages should capture what and why:
```bash
git commit -m "A03 Q1: Linear regression model - uniform prior on sigma"
git commit -m "A03 Q1: Fix - sigma needs positive constraint, switch to exponential"
```

**When done:**
```bash
git checkout main
git merge homework/A03
```

Your git history tells the story of your learning, including the mistakes.

---

## How Learning Actually Happens

**R development practices** emerge when you notice friction:
- Keep copying the same code? → Time to write a function
- Can't remember what this variable means? → Better naming matters
- Results changed unexpectedly? → Learn about seeds and reproducibility
- Path breaks when you move the project? → Discover `here::here()`

**Environment understanding** builds when things break:
- Package not found? → Investigate `.libPaths()` and renv
- Code works in console but not when sourced? → Learn about execution contexts
- Setting doesn't persist? → Understand `.Rprofile` and session state

**AI collaboration** improves through use:
- Vague question → Unhelpful answer → Learn to be specific
- Get stuck → Ask for answer → Miss the learning → Discover hints work better
- Provide context → Better responses → Understand what context matters

## Before Each Session

Quick environment check (makes problems catchable early):
```r
getwd()        # Where am I?
.libPaths()    # Where are packages?
renv::status() # Am I in sync?
```

## While Working

**When stuck:** First, articulate exactly where. Write it down. Often this alone unsticks you.

**Asking for help:** Provide context about what you tried and what you expected vs. got. Ask for hints before answers when learning something new.

**Committing:** Do it when you make progress or learn something. Future you will want to know what you were thinking.

## After Completing Homework

Brief reflection (10-15 minutes):
- What clicked that was fuzzy before?
- What's still confusing?
- What worked well? What felt like unnecessary friction?

Update context documents **only if** you discovered something worth capturing:
- New preference or pattern → `agent_instructions.md`
- Progress marker achieved → `learning_roadmap.md`
- Workflow adjustment → This file
- Environment solution → `r_environment_guide.md`

Don't update for the sake of updating. Update when you have something real to capture.

---

## Troubleshooting Quick Reference

**Package not found:** Check `.libPaths()`, install with `renv::install()`

**Code behaves unexpectedly:** Check `getwd()`, `search()`, `ls()`

**Git issues:** `git status` and `git diff` to see what changed

**Everything was working, now isn't:** Did R version, packages, or VSCode extension update?

---

---

## Key Concepts from A04 Session

**Causal vs Predictive Inference:**
- **Predictive**: Model doesn't need a causal structure, just needs to predict well
- **Causal**: Requires explicit scientific model (DAG) representing causal assumptions
- Same statistical model, different interpretation depending on the question

**Backdoor Paths:**
- Non-causal paths from exposure to outcome (go backward through at least one arrow)
- Create confounding - make associations appear even without causal effects
- Example: Age ← Sex → Height is a backdoor path confounding Age → Height

**Controlling for Confounders:**
- Include measured confounders in statistical model to block backdoor paths
- **Cannot** control for unmeasured confounders - acknowledge this limitation
- DAG makes you honest about what you can and cannot claim

**A04 Focus:** Estimating the causal effect of Age on Height, controlling for Sex as a measured confounder, while acknowledging unmeasured confounding from Nutrition.

---

*Version: 2.1 | Updated: 2026-01-29 | Added scientific workflow structure and causal inference concepts*
*This document evolves based on what actually works, not predictions about what should work*
