# Quick Start: Understanding Your R Environment in VSCode

*A practical mental model for the renv/VSCode/radian/R ecosystem*

---

## Why This Document Exists

You recently had a frustrating experience where renv, R project settings, R global settings, and various VSCode components interacted in unexpected ways. This document gives you a mental model to make those interactions predictable.

The goal isn't comprehensive mastery—it's enough understanding to:
1. Know what's happening when you run R code in VSCode
2. Diagnose problems when things don't work as expected
3. Make intentional changes to your setup

---

## The Components: What Each Thing Actually Does

Think of your R setup as several distinct programs that talk to each other. When something goes wrong, the question is: which program is misbehaving?

### R (the interpreter)

**What it is:** A program that reads R code and executes it. This is the engine.

**What it does:** 
- Maintains a "session" with loaded packages and variables
- Has a working directory (where it looks for files)
- Has library paths (where it looks for packages)
- Reads configuration files when it starts

**Key state to check:**
- `getwd()` — where is R looking for files?
- `.libPaths()` — where is R looking for packages?
- `search()` — what packages are currently loaded?
- `sessionInfo()` — comprehensive snapshot of the R session

### VSCode + R Extension

**What it is:** A text editor with an extension that knows about R.

**What it does:**
- Provides syntax highlighting and autocomplete
- Can send code to an R process (but doesn't run R itself)
- Has its own settings that affect how it interacts with R
- Can display plots (via httpgd) and help pages

**Key insight:** VSCode doesn't run your R code. It sends your code to a separate R process and displays the results. They're different programs.

**Key settings to know:**
- `r.rterm.xxx`: Which R/terminal to use (radian vs. vanilla R)
- `r.plot.useHttpgd`: Whether to use httpgd for plots
- `r.bracketedPaste`: Important for radian compatibility

### radian

**What it is:** An alternative R console with better features (autocomplete, syntax highlighting in the terminal, multi-line editing).

**What it does:**
- Runs R in a nicer interface
- Replaces the default R terminal experience
- Needs specific VSCode settings to work correctly

**Key insight:** radian is like a "wrapper" around R. R is still doing the work; radian just makes the interface nicer.

### httpgd

**What it is:** An R package that serves plots over HTTP so VSCode can display them.

**What it does:**
- When loaded, plots appear in VSCode's plot pane instead of a separate window
- Gets started automatically by the R extension when configured

**Key insight:** If plots aren't appearing in VSCode, either httpgd isn't installed, isn't loaded, or the VSCode setting isn't enabled.

### renv

**What it is:** An R package for project-specific package management.

**What it does:**
- Creates a project-local library (folder of installed packages)
- Maintains a lockfile recording exactly which package versions you're using
- Modifies `.libPaths()` so R looks in the project library first

**Key insight:** renv doesn't replace R's package system—it redirects where R looks for packages. When renv is active, `.libPaths()` will include a path inside your project.

---

## How They Interact: What Happens When You Start Working

Here's the sequence when you open a project in VSCode:

1. **You open a folder in VSCode** — VSCode notes if there's an `.Rproj` file or `renv` folder

2. **You open an R file or start the R terminal** — The R extension starts an R process

3. **R starts up and reads configuration:**
   - System-level R configuration (installation defaults)
   - User-level `.Rprofile` (in your home directory)
   - Project-level `.Rprofile` (in the project folder)
   - If renv is present: renv modifies `.libPaths()`

4. **You now have an R session** with:
   - A working directory (usually the project folder)
   - A set of library paths (modified by renv if present)
   - Any settings from your `.Rprofile`

5. **When you run code in VSCode** — The R extension sends code to this R session

---

## Common Confusions and How to Resolve Them

### "Package not found" after installing

**What's happening:** You installed the package, but to a different library than your current session is using.

**Diagnosis:**
```r
# Where is R looking for packages?
.libPaths()

# Where is the package actually installed?
find.package("packagename")  # Will error if not found

# What's in your renv library?
list.files(renv::paths$library())
```

**Likely cause with renv:** You installed the package to your user library (outside renv) but R is looking in the renv project library.

**Fix:** Install while renv is active: `renv::install("packagename")` or `install.packages("packagename")` with renv active (it intercepts the call).

### Settings "not sticking" between sessions

**What's happening:** Each R session starts fresh. Loading a package or setting an option only affects the current session.

**The fix:** Put things you always want in `.Rprofile`. But be cautious—project `.Rprofile` is fine; putting too much in user `.Rprofile` can cause reproducibility issues.

### VSCode seems to use wrong R or wrong settings

**What's happening:** VSCode has its own settings about which R to use, separate from what's on your system PATH.

**Diagnosis:**
- Check VSCode settings: search for "r.rterm"
- Compare to what R says: `R.home()` and `Sys.which("R")` in your R session

**Fix:** Ensure VSCode settings point to the R you intend to use.

### radian behaves differently than expected

**What's happening:** radian needs specific VSCode settings. Without them, multi-line code can break.

**The fix:** Ensure `r.bracketedPaste` is enabled in VSCode settings when using radian.

### Everything was working, then suddenly broke

**What likely changed:**
- R version updated
- Package updated (check `renv::status()`)
- VSCode extension updated
- A configuration file changed

**Diagnosis approach:**
1. Start a vanilla R session (not through VSCode): does the problem occur there?
2. If yes: the problem is in R or your packages
3. If no: the problem is in VSCode's R extension or radian

---

## Diagnostic Commands Reference

Run these when something seems off:

```r
# Where am I?
getwd()

# Where does R look for packages?
.libPaths()

# What packages are loaded?
search()

# What's my R configuration?
sessionInfo()

# Is renv active?
renv::status()  # Will error if not in a renv project

# What does renv think the state is?
renv::diagnostics()

# Where is a specific package?
find.package("dplyr")

# Full information about the session
sessioninfo::session_info()  # More detailed than sessionInfo()
```

---

## The Mental Model in One Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                        VSCode                               │
│  ┌─────────────────┐    ┌────────────────────────────┐     │
│  │  R Extension    │───▶│  R Process (or via radian)  │     │
│  │  (sends code)   │    │                            │     │
│  └─────────────────┘    │  Working dir: project/     │     │
│                         │                            │     │
│  ┌─────────────────┐    │  .libPaths():              │     │
│  │    httpgd       │◀───│   1. project/renv/library  │     │
│  │  (shows plots)  │    │   2. ~/.local/R/library    │     │
│  └─────────────────┘    │   3. /usr/lib/R/library    │     │
│                         │                            │     │
│                         │  Loaded packages:          │     │
│                         │   base, stats, rethinking  │     │
│                         └────────────────────────────┘     │
└─────────────────────────────────────────────────────────────┘
```

Key insight: The R process maintains state (working directory, library paths, loaded packages). VSCode is just the interface. When you restart VSCode, your R state survives. When you restart R, it doesn't.

---

## Action Items

1. **Before your next coding session:** Run `.libPaths()` and `getwd()` to orient yourself. Make this a habit.

2. **When something breaks:** Before trying random fixes, gather information. What does R think the state is?

3. **When using renv:** Run `renv::status()` before starting work. It tells you if your project is in sync.

4. **Keep notes:** When you solve an environment problem, write down what the symptoms were and how you fixed it. You'll encounter similar issues again.

---

## Your Specific Setup Checklist

Since you're using VSCode + R extension + radian + httpgd + renv, verify these settings:

**VSCode settings.json:**
```json
{
  "r.bracketedPaste": true,
  "r.plot.useHttpgd": true,
  "r.rterm.option": [],
  "r.rpath.xxx": "/path/to/R"  // xxx = your OS (linux/mac/windows)
}
```

**radian configuration** (optional, `~/.radian_profile`):
```python
options(radian.auto_match = TRUE)
options(radian.tab_size = 2)
```

**Project .Rprofile** (created by renv):
```r
source("renv/activate.R")
```

---

## Next Steps

Once this mental model is clear, you can:
- Intentionally modify your configuration
- Understand why reproducibility tools (renv) matter
- Debug environment issues systematically instead of guessing

The Statistical Rethinking homework is a good place to practice. Set up each assignment as a proper project with renv, and use the diagnostic commands to verify your environment before you start.

---

---

## Windows-Specific Notes

Your setup runs on Windows, which has some specific considerations:

### File Paths

Windows uses backslashes (`\`) in paths, but R can use forward slashes (`/`) on all platforms. Always use forward slashes in your R code for portability:

```r
# Good (works everywhere)
read.csv("data/myfile.csv")
here::here("data", "myfile.csv")

# Avoid (Windows-only, and backslashes need escaping)
read.csv("data\\myfile.csv")
```

### Where Things Live on Windows

| What | Typical Location |
|------|------------------|
| R installation | `C:/Program Files/R/R-4.x.x/` |
| User R library | `C:/Users/YourName/AppData/Local/R/win-library/4.x/` |
| User .Rprofile | `C:/Users/YourName/Documents/.Rprofile` |
| User .Renviron | `C:/Users/YourName/Documents/.Renviron` |
| Project renv library | `your-project/renv/library/windows/R-4.x/` |

To find your home directory in R: `Sys.getenv("HOME")` or `path.expand("~")`

### cmdstanr on Windows

The Statistical Rethinking course recommends cmdstanr over rstan. On Windows, cmdstanr needs a C++ toolchain. The setup process:

```r
# Install cmdstanr (not on CRAN, so use this method)
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# Check if you have a working toolchain
cmdstanr::check_cmdstan_toolchain()

# If not, install RTools (this gives you the C++ compiler)
# Download from: https://cran.r-project.org/bin/windows/Rtools/

# After RTools is installed, install CmdStan itself
cmdstanr::install_cmdstan()
```

RTools is separate from R itself—it provides the compilers Stan needs. After installing RTools, restart R before running `install_cmdstan()`.

### VSCode Settings for Windows

In your VSCode `settings.json`, R paths use forward slashes even on Windows:

```json
{
  "r.rpath.windows": "C:/Program Files/R/R-4.4.0/bin/R.exe",
  "r.bracketedPaste": true,
  "r.plot.useHttpgd": true
}
```

If you're not using radian (my recommendation initially), you can leave `r.rterm.windows` at its default.

### Common Windows-Specific Issues

**"RTools is required but not installed"**: Stan models need compilation. Install RTools from CRAN, then restart R.

**Path too long errors**: Windows has a 260-character path limit. Keep your project paths short. Instead of `C:/Users/YourName/Documents/Projects/Learning/Statistics/BayesianStatistics/StatisticalRethinking2026/`, use something like `C:/Projects/stat_rethinking/`.

**Permission errors with renv**: Sometimes Windows antivirus or permission settings interfere with renv creating its library. Try running VSCode as administrator once to set things up, or add your project folder to antivirus exclusions.

**Line endings**: Git on Windows can cause issues with line endings. Configure git to handle this:
```bash
git config --global core.autocrlf true
```

---

*Document version: 1.1 | Last updated: January 2026*
*Configured for: Windows with VSCode, R, cmdstanr, renv*
