# Learning Roadmap: Bayesian Statistics, R Development, Code Execution, and AI Collaboration

*A personalized progression from your current foundation to confident practitioner*

---

## How to Use This Document

This roadmap covers four interconnected learning areas. You don't need to master each area sequentially—they reinforce each other. The Statistical Rethinking course provides the backbone, and the other three areas emerge naturally as you work through homework.

Each section shows:
1. **Why This Matters** — The practical payoff
2. **Where You Are Now** — Your starting point
3. **Learning Progression** — Topics from foundational to advanced
4. **Integration Points** — How areas connect

The time spent on each area will vary week to week based on what you encounter. Don't force it—let the homework drive what you need to learn.

---

## 1. Bayesian Statistics and Scientific Workflow

### Why This Matters

You already know how to run statistical tests. What the Statistical Rethinking course offers is different: a framework for connecting statistical models to scientific questions. The Bayesian approach isn't just a different way to calculate p-values—it's a different way of thinking about what your model claims about the world, and what the data can actually tell you.

The payoff: You'll be able to build models that map directly onto your scientific hypotheses, understand exactly what assumptions you're making, and communicate uncertainty in ways that actually answer the questions stakeholders care about.

### Where You Are Now

You have a solid frequentist foundation. You understand regression, hypothesis testing, and probability concepts. Your goal is to learn the "applied statistics for science" workflow—the DAGs, the causal reasoning, the iterative model-building process.

### Learning Progression

**Foundation (Weeks 1-4): The Bayesian Worldview**

These concepts reframe what you already know through a Bayesian lens:

- How Bayesian updating works: priors, likelihoods, and posteriors as a coherent system rather than separate pieces
- Why "the probability of the hypothesis given the data" is different from "the probability of the data given the hypothesis" (and why this matters for scientific inference)
- Grid approximation and quadratic approximation as ways to understand what MCMC will later do for you automatically
- The logic of generative models: defining a data-generating process and asking "what parameters would make my data plausible?"

*Course alignment: Lectures 1-4, Chapters 1-4*

**Core Skills (Weeks 5-7): Causal Reasoning and Model Building**

This is where the course diverges most from standard statistics training:

- DAGs (Directed Acyclic Graphs) as tools for making causal assumptions explicit
- The four elemental confounds: fork, pipe, collider, descendant—and why controlling for variables can make estimates worse
- The distinction between predictive and causal inference (when to include a variable depends on your question, not just correlation)
- Information criteria and cross-validation as tools for understanding overfitting (different purpose than null hypothesis testing)

*Course alignment: Lectures 5-7, Chapters 5-7*

**Computation (Week 8): Understanding MCMC**

You'll have been using MCMC all along, but this is where you understand what it's actually doing:

- Markov chain Monte Carlo as a way to explore the posterior distribution when you can't calculate it directly
- Diagnosing when MCMC has worked (trace plots, R-hat, effective sample size) and what to do when it hasn't
- The relationship between the Stan code and the statistical model

*Course alignment: Lecture 8, Chapters 9-10*

**Applied Modeling (Weeks 9-10): Events and Hierarchies**

Building models for realistic data:

- Generalized linear models: logistic regression, Poisson regression as extensions of the linear model
- Link functions and why they matter
- Introduction to multilevel models: partial pooling as a principled way to handle grouped data

*Course alignment: Lectures 9-10, Chapters 10-12*

**Advanced (Post-Course or Course B): Complex Models**

For after you've completed the beginner section:

- Full multilevel models with varying slopes
- Gaussian processes for spatial and temporal correlation
- Measurement error and missing data
- Model comparison and averaging

*Course alignment: The "Experienced" section, Chapters 13-16*

### Integration Points

- Every homework problem is an opportunity to practice the R development skills in Section 2
- Understanding how Stan samples from the posterior connects to Section 3 (code execution)
- Explaining your model-building reasoning to an AI agent is excellent practice for clarifying your own thinking

---

## 2. R Development Best Practices

### Why This Matters

You've used R for a decade to get things done. The goal now isn't to learn R—it's to learn how to write R code that you (and others) can trust, modify, and build upon. This matters because:

1. **Debugging becomes tractable.** When code is modular, you can isolate problems. When it's a 500-line script, every bug is a mystery.

2. **Iteration becomes faster.** The "walking skeleton" approach you mentioned is excellent—but it only works if each piece of the skeleton is solid enough to build on.

3. **Your future self will thank you.** You're planning to use this methodology for work. That means returning to code months later. Well-structured code is the difference between "I can extend this" and "I need to rewrite this."

4. **AI agents work better with good code.** When your code is modular and well-named, agents can understand it, test it, and modify it reliably. A 300-line script with unclear dependencies is hard for anyone—human or AI—to work with.

### Where You Are Now

You write code that works. You've created functions and used git. But you haven't yet experienced the full benefit of these tools because you haven't built the habits that make them powerful. You're transitioning from RStudio to VSCode, which is a good opportunity to establish new patterns.

### Learning Progression

**Foundation: Project Structure and Workflow**

Start here because it affects everything else:

- The anatomy of an R project: .Rproj files, working directories, and why `setwd()` is almost always wrong
- Organizing files: separating data, scripts, outputs, and documentation
- The "one project, one purpose" principle: why mixing multiple analyses in one project creates problems
- Using `here::here()` for paths that work regardless of where the project lives

*Practice opportunity: Set up your Statistical Rethinking homework as a proper project*

**Core Skill: Writing Functions**

Functions are the building blocks of maintainable code:

- When to extract code into a function (the rule of three, but also: when naming something clarifies its purpose)
- Function design: single responsibility, clear inputs and outputs, minimal side effects
- Naming conventions that make code self-documenting
- The difference between functions for automation and functions for abstraction

*Practice opportunity: Identify repeated patterns in your homework solutions and extract them into functions*

**Core Skill: Version Control with Git**

You've used git, but perhaps not in a way that serves you:

- The mental model: git tracks *changes*, not files—and why this matters
- A minimal effective workflow: `status`, `add`, `commit`, `diff`, `log`
- Writing commit messages that help future-you understand what changed and why
- Using branches to experiment safely (try something, keep it if it works, discard if it doesn't)

*Practice opportunity: Commit your homework incrementally as you work, with messages that describe your reasoning*

**Intermediate: Testing and Validation**

This is where many people stall, but it's more approachable than it seems:

- The purpose of tests: not to prove code is correct, but to catch when it breaks
- Starting simple: "does this function return the expected output for a known input?"
- Using `testthat` for basic assertions
- When tests are worth writing (core logic, tricky edge cases) and when they're overkill

*Practice opportunity: Write tests for your extracted functions—especially the ones that do non-trivial calculations*

**Intermediate: Dependency Management with renv**

This directly addresses your frustration with environment interactions:

- What renv does: isolates your project's packages from everything else on your system
- The renv workflow: `init()`, `snapshot()`, `restore()`
- When renv helps (reproducibility, collaboration, future-proofing) and when it adds friction (quick explorations)
- Diagnosing common renv issues: lockfile conflicts, library paths, package installation failures

*Practice opportunity: Initialize renv for your homework project; understand what files it creates and why*

**Advanced: Code Review Patterns**

Reading code (yours and others') is a skill:

- What to look for: clarity, edge cases, unnecessary complexity
- Refactoring: improving code structure without changing behavior
- Using AI agents as code reviewers (prompt: "What could go wrong with this function? What edge cases am I missing?")

**Advanced: Package Development Basics**

You may not need to publish a package, but understanding package structure helps:

- Why packages are just "organized projects with metadata"
- DESCRIPTION and NAMESPACE files: what they do
- When to convert a project into a package (when you're reusing code across multiple analyses)

### Integration Points

- Apply these practices directly to your Statistical Rethinking homework
- Understanding project structure connects deeply to Section 3 (how R finds things)
- Well-structured code makes AI collaboration (Section 4) dramatically more effective

---

## 3. Understanding Code Execution

### Why This Matters

Your renv/VSCode/radian frustration comes from a gap in mental model. Right now, when things work, you don't know why. When they break, you can't debug systematically—you try random things until something works. This is exhausting and slow.

The goal isn't to become a computer scientist. It's to build a mental model accurate enough that:

1. You can predict what will happen when you run code
2. When something unexpected happens, you have hypotheses about why
3. You can make small, intentional changes to your setup and understand their effects

This is the difference between driving a car (you understand steering, brakes, accelerator) and being a passenger in a self-driving car that occasionally does something inexplicable.

### Where You Are Now

You're starting from scratch on this mental model. You've used R in RStudio, where many details are hidden. Now in VSCode, those hidden details are more exposed, and the interactions between components are more visible (and confusing).

### Learning Progression

**Foundation: What Happens When You "Run R Code"**

Start with the basics that everything else builds on:

- The R interpreter: a program that reads R code and executes it
- The difference between interactive mode (REPL) and running a script
- What "environment" means in R: a collection of name-value bindings (not the same as your OS environment)
- The search path: how R looks up names (the order matters)

*Key insight: When you type `library(dplyr)`, you're modifying R's search path so that when you later type `filter()`, R finds dplyr's filter instead of stats::filter*

**Foundation: Files, Paths, and Working Directories**

This is where many confusions originate:

- Absolute paths vs. relative paths
- The working directory: where R looks when you use a relative path
- Why the working directory is different in scripts vs. console vs. notebooks
- What `.Rproj` files actually do (spoiler: they set the working directory when you open the project)

*Key insight: "File not found" errors are almost always about the working directory or path being wrong, not the file being missing*

**Core: The R Ecosystem in VSCode**

Understanding the components you're actually using:

- R itself: the interpreter that runs your code
- The R extension for VSCode: provides syntax highlighting, code completion, sends code to R
- radian: an alternative console that replaces the default R terminal (better autocomplete, syntax highlighting)
- httpgd: a graphics device that displays plots in VSCode instead of a separate window

These are separate programs that communicate with each other. When something breaks, the question is: which component is misbehaving?

*Key insight: When you "run" code in VSCode, the extension is sending your code to a separate R process. That R process has its own working directory, its own loaded packages, its own state.*

**Core: Package Management**

Where packages live and how R finds them:

- The library: a folder containing installed packages
- Multiple libraries: R can have system-wide, user-specific, and project-specific libraries
- `.libPaths()`: shows you where R looks for packages, in order
- `renv`: creates a project-specific library and lockfile

*Key insight: When you get "package not found" after installing it, the package was probably installed to a different library than R is currently using*

**Intermediate: Processes and State**

Building a model of what's happening at the OS level:

- A process: a running program with its own memory space
- When you start R, you create a process; when you quit, it's gone (along with all variables)
- Multiple R processes: you can have several running at once (e.g., one in VSCode, one in terminal)
- Environment variables: settings the OS provides to processes (e.g., PATH, R_LIBS)

*Key insight: Your renv confusion likely involved different R processes using different library paths. Understanding this makes the behavior predictable.*

**Intermediate: Configuration Layers**

How settings cascade (from most specific to most general):

1. Code you run explicitly (`options()`, `Sys.setenv()`)
2. Project-level config (`.Rprofile` in project, `renv`)
3. User-level config (`~/.Rprofile`, `~/.Renviron`)
4. System-level config (R's installation defaults)
5. IDE settings (VSCode settings, extension settings)

When behavior is unexpected, check from most specific to most general.

**Advanced: Debugging Workflow Issues**

Systematic approaches to "why isn't this working?":

- Reproducing the problem in a clean R session
- Checking `getwd()`, `.libPaths()`, `search()` to understand current state
- Reading error messages carefully (they usually tell you exactly what's wrong)
- Using `traceback()` and `debug()` for code errors

### Integration Points

- This knowledge directly supports Section 2 (understanding why project structure matters)
- When Statistical Rethinking models fail to run, you'll be able to diagnose whether it's a code problem, environment problem, or package problem
- AI agents can help debug, but only if you can describe the situation accurately—this knowledge gives you the vocabulary

---

## 4. AI Collaboration for Learning

### Why This Matters

You're already experiencing the benefit: interactive content that adapts to your questions. But AI collaboration is a skill that improves with practice. The difference between mediocre and excellent results often comes down to how you frame requests and manage context.

The goal is to make AI agents more useful for your specific learning style—which means understanding what they're good at, what they're bad at, and how to communicate effectively with them.

### Where You Are Now

Context documents are established but felt overwhelming initially. The solution: ignore most of the workflow documentation and just start working. Update context docs after sessions with observed patterns, not hypothetical preferences. Keep it lean.

### Learning Progression

**Foundation: What AI Assistants Actually Do**

Adjusting expectations based on reality:

- LLMs generate plausible next tokens based on patterns in training data—they don't "know" things the way humans do
- They're excellent at: explaining concepts, generating examples, transforming content, catching errors, suggesting alternatives
- They're unreliable at: precise numerical calculations, guaranteeing correctness, remembering previous conversations (unless given context)
- They reflect confidence regardless of accuracy—you must verify important claims

*Key insight: AI assistants are like a very well-read collaborator who occasionally hallucinates. Use them for exploration and explanation; verify anything that matters.*

**Foundation: Effective Prompting Basics**

Getting better outputs with clearer inputs:

- Be specific about what you want (format, length, level of detail)
- Provide context about what you already know and what you're trying to accomplish
- Ask for reasoning, not just answers ("explain why" not just "what is")
- Request examples when learning concepts

*Practice opportunity: Compare responses to "explain priors" vs. "I understand probability distributions but I'm new to Bayesian statistics. Explain what priors are, why we need them, and give an example from regression."*

**Core: Context Documents**

The strategy behind what we're creating today:

- Context documents capture stable information (your background, preferences, goals) so you don't repeat yourself
- They work best for information that's relevant across many interactions
- They should be concise—AI context windows are limited; don't waste space on rarely-relevant details
- Update them when you notice patterns (e.g., "I keep having to explain that I want code comments")

**Core: Model Selection**

Different models for different tasks:

- Larger models (GPT-4, Claude Opus) are better for complex reasoning, nuanced explanations, handling ambiguity
- Smaller/faster models are fine for simple tasks: formatting, basic code generation, lookup-style questions
- Cost and speed vary—don't use an expensive model for a cheap task
- When learning something new, start with a capable model; when applying known patterns, faster models work fine

*Practice opportunity: Try the same Statistical Rethinking question on different models; notice where quality differs*

**Intermediate: Managing Context Across Sessions**

Working with the limitation that AI doesn't remember:

- Start sessions by providing relevant context (or referencing a context document)
- For ongoing projects, maintain a "project state" document you can share
- Summarize previous work when continuing from a past session
- Know when to start fresh vs. continue (fresh: new topic; continue: building on previous work)

**Intermediate: AI as Tutor vs. AI as Assistant**

Different modes for different goals:

- Tutor mode: "Help me understand X. Don't give me the answer; guide me to discover it. Ask me questions to check my understanding."
- Assistant mode: "Do X for me. Here's the context. I trust your judgment on implementation details."
- Hybrid: "Explain your reasoning as you do X, so I can learn from watching."

Your learning style (prefers struggling through problems, direct feedback, probing questions) maps well to tutor mode for new concepts.

**Advanced: Collaborative Debugging**

Using AI effectively when things break:

- Provide full context: error message, relevant code, what you expected vs. what happened
- Ask for hypotheses, not just fixes ("What might cause this?" before "How do I fix this?")
- Use the AI to explain error messages you don't understand
- When AI suggestions don't work, report back—the iteration helps narrow down the problem

**Advanced: Using AI to Improve AI Interactions**

Meta-level optimization:

- Ask AI to critique your prompts: "How could I have asked this more clearly?"
- Request feedback on your context documents: "Is there information missing that would help you help me better?"
- Experiment with different framings and notice what works

### Integration Points

- Use AI tutoring for Statistical Rethinking concepts you find confusing
- Use AI code review for your R development practice
- When you hit environment issues, describe the situation to AI and ask for diagnostic hypotheses

---

## Finding Your Rhythm

Work in the mornings when your energy is high. You can focus for extended periods when engaged.

A typical week might involve:
- Engaging with lecture material
- Attempting homework (struggle first, ask for help when genuinely stuck)
- Brief reflection on what you learned

The exact timing and breakdown will vary. Let your actual experience guide you, not a predetermined schedule.

---

## Progress Markers

How to know you're advancing:

**Bayesian Statistics**
- [ ] Can explain the difference between Bayesian and frequentist inference to a colleague
- [x] Can draw a DAG for a research question and identify confounds *(A03: built height/weight/age/sex/nutrition DAG, identified confounds)*
- [x] Can interpret posterior distributions and explain uncertainty *(A02: articulated posterior vs posterior predictive, understood prior influence)*
- [ ] Can diagnose MCMC issues and know how to address them
- [ ] Can build a multilevel model and explain why partial pooling helps

**R Development**
- [x] Projects have consistent structure; you never use `setwd()` *(set up repo with .Rproj, renv at root, contributions in subfolder)*
- [ ] You extract functions when you notice repetition
- [x] Your git history tells a story; you can revert problematic changes *(practiced commits with meaningful messages during A02 and reorganization)*
- [ ] You write at least basic tests for important functions
- [x] renv feels like a tool, not a mystery *(understand renv.lock captures versions, renv/ holds library, .Rprofile activates it)*

**Code Execution**
- [ ] Can explain what `library()` does and where packages come from
- [ ] Can diagnose "package not found" errors systematically
- [x] Understand the difference between your R process and your IDE *(VSCode sends code to R process; they're separate programs)*
- [x] Can check your current state (working directory, library paths, loaded packages) *(know to use getwd(), .libPaths())*
- [x] Configuration files don't feel like magic *(understand .Rprofile, .Rproj, renv.lock, .gitignore, .vscode/settings.json)*

**AI Collaboration**
- [ ] Can write prompts that consistently get useful responses
- [ ] Know when to provide context vs. when to let AI infer
- [ ] Can switch between tutor and assistant modes deliberately
- [ ] Use AI to debug effectively by providing good problem descriptions
- [ ] Have working context documents that improve your interactions

---

*Document version: 1.2 | Updated: 2026-01-21 | Added R development and code execution progress from config files session*
