# `bayes-workflow` — review against skill-creator and writing-for-agents

Reviewed 2026-08-20. Sources: Anthropic `skill-creator` (`SKILL.md`, `agents/grader.md`,
`agents/analyzer.md`, `references/schemas.md`) and Matt Pocock `writing-for-agents`
(`SKILL.md`, `SKILL-MECHANICS.md`, `docs/productivity/writing-for-agents.md`).
Evidence: `homework/contributions/A07_with_python/SKILL_TEST_NOTES.md` and the analysis
that run produced.

---

## 1. Verdict

This is a well-formed skill by both guides' structural standards — the invocation choice,
the sequence split, and the completion criteria are all correct, and two of them are
better than what either guide asks for. It fails on the two things both guides treat as
diagnostic of a document that was never tested: it still contains the answers to its own
test problem in three mandatory-path files, and it states the same meaning in three or
four places, which has already produced one live contradiction bug. The fixes are
mechanical and total perhaps three hours; nothing here calls the design into question.

---

## 2. Where it conforms

- **Invocation.** `disable-model-invocation: true` with a stripped, human-facing
  description is exactly SKILL-MECHANICS' user-invoked contract. Correct, and correct
  *against* skill-creator — see §3.0.
- **Split by sequence.** One invocation, one step, one step file, "and that file alone."
  This is Pocock's split-by-sequence executed across a real context boundary, which is the
  only place he says hiding later steps works.
- **Completion criteria.** "The step is complete when every **M** item has a recorded
  human response in the README" is checkable *and* exhaustive — Pocock's stated ideal.
  The per-step `Always run` lists are the demand lever applied correctly.
- **Domain organisation.** `backends/bambi.md` / `backends/brms.md` is verbatim
  skill-creator's recommended variant pattern (`cloud-deploy/references/{aws,gcp}.md`).
- **One conditional pointer done right.** `docs/examples.md` — "Do not read that file on
  every run — open it when a term needs explaining" — is the ladder used properly.
- **Length of `SKILL.md`.** 219 lines, well under skill-creator's "<500 lines ideal."

---

## 3. Where it violates the guides

Ranked by what it costs in practice.

### 3.0 First: where the guides disagree

Four real conflicts. My view on each, because you cannot satisfy both.

| Question | skill-creator | writing-for-agents | My view |
|---|---|---|---|
| Description | Make it "a little bit *pushy*"; stuff it with trigger contexts; run the description optimiser | User-invoked skills strip the description to "a one-line summary, trigger lists stripped" | **Pocock.** An agent that silently entered a ten-step sign-off workflow because someone said "fit a regression" would defeat the entire premise. Keep `disable-model-invocation`. |
| Length | "<500 lines ideal… feel free to go longer if needed" | **Sprawl** is a failure mode "even when every line is live and unique"; "the document gets shorter as it gets better" | **Pocock**, and decisively — skill-creator prices one invocation per task. This skill is invoked ten times per analysis, so every always-loaded line is paid ten times. See §4.2. |
| Explanation | "Try hard to explain the **why** behind everything you're asking the model to do" | "Its default move is deletion, not explanation… every one of those lines is a **no-op**, paying context and changing no behaviour" | **Split, and the split is testable.** Keep a *why* only when it changes behaviour against the default. §3.7 separates the ones that earned it from the ones that did not. |
| Testing | An eval harness: `evals.json`, with/without-skill baselines, an independent grader, benchmark aggregation, description optimisation | "There is no automated eval here; the check is a manual run plus the failure-mode vocabulary as a diagnostic" | **Pocock in method, skill-creator in one specific question.** You ran Pocock's check and it worked — `SKILL_TEST_NOTES.md` is a better artefact than most benchmark tables. But only skill-creator's with/without baseline answers "is step 09 earning its keep?", and you currently cannot run it. See §4.6. |

### 3.1 The skill still contains the answers to its own test problem (worst)

`SKILL_TEST_NOTES.md` §0 called this the headline problem. It is **partly** fixed — the
numeric results moved to `docs/examples.md` behind a conditional pointer, which was the
right move — and the structural leak remains, in the mandatory path:

- `steps/02-dag.md` line 20, the running example of the DAG step, is the A07 DAG with both
  its answers in the comments:
  `dt.adjustment_sets(g, "F", "W", effect="total")  # [()]  -> one set, and it is empty`
- `backends/bambi.md` line 83 — the same DAG, the same two answers.
- `tools/dag_tools.py` — the same DAG (line 364), plus all 116 rows of `foxes` as
  `_FOXES`, plus the local-test intervals hard-coded as ground truth. `SKILL.md` tells the
  agent to run this file.
- `steps/01-estimand.md` uses the fox-weight estimand verbatim as its example of a
  well-formed estimand; `steps/06-prior-check.md` uses "The average fox weighs between
  4.07 and 4.99 kg".

Pocock names this failure mode exactly, under *My skill only works on the exact task I
built it from*: "The common route — do the work once, then have the agent write it up as a
skill — over-indexes on that one run, and the exemplars come out too specific. **Keep the
run as evidence, then abstract deliberately: strip what belonged to that repo and those
files, and write for the class of task.**"

Two costs. First, the skill cannot be evaluated on the only problem it documents — every
skill-creator eval you might run on A07 is contaminated at the source. Second, and worse
in the long run, `SKILL_TEST_NOTES.md` §0 gets it right: "because the example is always
A07, it is impossible to tell which instructions are general and which were
reverse-engineered from this dataset."

**Fix.** Pick one throwaway example DAG for the whole skill and use it everywhere — one
with a confounder *and* a collider, so it teaches more than foxes does. `X <- U -> Y`,
`X -> M -> Y`, `X -> Y`, `Y -> S`. That single change fixes the step file, the backend
file, and the module docstring at once, and gives you a better teaching example. For the
fixture, see §4.5.

### 3.2 Duplication — the same meaning in three and four places

Pocock: "Keep each meaning in a **single source of truth**: one authoritative place, so
changing the behaviour is a one-place edit. **Duplication** … costs maintenance and
tokens, and inflates a meaning's prominence on the ladder past its real rank."

Counted:

| Meaning | Places |
|---|---|
| "Models answering different estimands are reported side by side, never ranked" | `SKILL.md` §Offering branches, `CONTEXT.md` §Model comparison, `steps/01`, `steps/10` — **four** |
| "Report the interval, not the p-value / not contradicted is not confirmed" | `steps/03`, `backends/bambi.md`, `dag_tools.py` docstring — **three** |
| Power-scaling: "above about 0.05 means the prior is still steering" + "weak identification is where a prior does the most work" | `steps/06`, `steps/08`, `backends/bambi.md` — **three**, near-verbatim |
| "Plots carry more than narrative. Prose only where a number misleads without it." | `steps/03`, `steps/10` — verbatim |
| "The generative model is deliberately richer than the statistical model… close the gap and recovery checks nothing but the sampler" | `steps/04`, `docs/examples.md` — verbatim |
| "Conditioning on a mediator improves a prediction and destroys a causal estimate" | `steps/01`, `adr/0003` |
| "Elapsed time, and which algorithm" | `SKILL.md` run protocol *plus* the `Always run` list of steps 06, 07, 08, 09, 10 |

This is not an abstract cost. **It has already produced a bug.** `SKILL_TEST_NOTES.md`
§1.5: the `adjustment_sets` return value was documented as `[()]` in `backends/bambi.md`
and as `[]` in `steps/02-dag.md` — opposite meanings, one character apart, one of the two
copies gone stale. That is precisely what a single source of truth prevents. You have
since fixed the copy; you have not removed the second copy, so it can drift again.

Pocock's own success test: "**Nothing is stated twice, in any form. Duplication is the
most reliable sign a document was never tested.**"

**Fix.** For each row above, pick the one file that owns the meaning and delete the
others. My picks: never-ranked → `CONTEXT.md` only (it is a vocabulary rule);
interval-not-p-value → `dag_tools.py`'s docstring only, because it is the function's own
contract; power-scaling mechanism → `steps/08` only, with `steps/06` reduced to one line
(see §3.6); elapsed time → `SKILL.md` only, deleting it from five `Always run` lists.

### 3.3 Negation without a positive target — and the evidence that it failed

`SKILL.md`, closing lines:

> **Never guess it, and never copy an absolute path out of an example.**

Pocock: "**Negation** is the failure mode beside this lever: steering by prohibition drags
the forbidden behaviour into context and makes it *more* available… Prompt the
**positive** — state the target behaviour so the banned one is never spoken."

The cold run obeyed the ban and produced this, in `A07_with_python/scripts/02_dag.py`
line 17:

```python
SKILL_TOOLS = r"C:\Users\Thijs\.claude\skills\bayes-workflow\tools"
```

The analysis folder — the skill's actual deliverable, the thing whose README documents a
sha256 of the raw data so it can be reproduced — will not run on any other machine. The
agent flagged the gap itself (`SKILL_TEST_NOTES.md` §4, first bullet: "a script that must
run standalone tomorrow has to get it from *somewhere*"). The skill said what not to do
and never said what to do.

Other bare prohibitions with no positive twin: "Do not invent a location", "Do not read
that file on every run", "Do not plan around it." Each has an obvious positive form.

### 3.4 Contradictions between files, all recorded in the cold run, all still open

`agents/analyzer.md` lists "Ambiguous instructions that led to suboptimal choices" as a
primary loser-weakness category. Four survive:

| # | Contradiction | Files |
|---|---|---|
| a | `SKILL.md` layout shows `scripts/ 01_estimand … 10_estimate`; `steps/01` says "**Always run:** Nothing. This step has no script and no data." | `SKILL.md` line 22 vs `steps/01-estimand.md` |
| b | `steps/02` `Always run`: "The DAG figure, **one panel per estimand**". `draw_dag` takes no `ax`, calls `plt.subplots` and `fig.savefig` itself — one panel per estimand is impossible in one file. | `steps/02-dag.md` vs `tools/dag_tools.py:244` |
| c | `steps/08`: "save the fit object to `data/`". `brms.md` names `saveRDS()`. `bambi.md` names nothing, and the arviz-native `to_netcdf()` raises — neither `netCDF4` nor `h5netcdf` is in the verified stack. | `steps/08-fit.md` vs `backends/bambi.md` |
| d | `steps/02` says `scripts/02_dag.py`; every other step file is extension-less (`scripts/03_data`, `scripts/05*_statistical_model`). The extension-less form is the fix that was applied for the R-filename bug — one file was missed. | `steps/02-dag.md` |

(b) and (c) each cost the cold run a debugging cycle. (a) cost an invented decision.

### 3.5 Progressive disclosure is not applied *inside* `SKILL.md`

Pocock: "**Branching is the cleanest disclosure test: inline what every branch needs, and
push behind a pointer what only some branches reach.**"

Audit `SKILL.md`'s 219 lines by which invocations need them:

| Section | Lines | Needed on |
|---|---|---|
| README template | 154–185 (**32**) | step 01 only — the README exists after that |
| Where the work lives (layout, folder location, multi-estimand suffixes) | 19–47 (**29**) | step 01 mostly; the README records the folder thereafter |
| Teaching mode | 119–133 (**15**) | only when teaching is on, and it is **off by default** |
| Iteration | 146–152 (7) | only when a step sends you back |
| Offering branches | 135–144 (10) | steps 05 and 10 |

Roughly **90 of 219 lines** are conditional, and all 90 are paid on all ten invocations.
The teaching-mode block is the sharpest case: fifteen lines describing a mode the
frontmatter says is off, loaded every time.

### 3.6 Steps 06 and 08 are split in the wrong place

`steps/06-prior-check.md` spends 14 lines on power-scaling — "Power-scaling sensitivity
turns the prior up and down and measures how far the posterior moves… Above about 0.05
means the prior is doing real work… Weak identification is where a prior does the most
work" — and then says the check runs at step 08. `steps/08-fit.md` says the same three
things again.

Pocock on co-location: "Keep a concept's definition, rules, and caveats under one heading
rather than scattered, so reading one part brings its neighbours with it." Here one concept
is split across a context boundary and stated twice on both sides of it. Step 06 needs one
sentence, not fourteen lines.

### 3.7 `why` lines that fail the no-op test

Pocock: "Hunt **no-ops** sentence by sentence: an instruction the model already obeys by
default pays load to say nothing. The test — does it change behaviour versus the default?
… When a sentence fails, delete the whole sentence rather than trim words from it."

**These earned their tokens** — the cold run's §2 shows each one changed what the agent
did, and none is default behaviour:

- `steps/04`: "The generative model is deliberately **richer** than the statistical
  model… Close the gap and recovery checks nothing but the sampler." → A07 M11.
- `steps/04`: "A true value near zero is a decision to surface." → caught that the true
  total effect is −0.03 and its recovery check is nearly vacuous (M14). The agent says
  flatly it would not have found this.
- `steps/06`: "**Does a deliberately bad prior look different?** … A check you pass
  without revising anything is a check that did not run." → produced the `Normal(0, 10)`
  comparison (M22).
- `steps/09`: "A check on something the model has a parameter for is close to circular."
  → made the 30-territory check the point of the step (M26).
- `CONTEXT.md`: "**Run the code and show the output.**" → the agent credits this with
  catching both of its own errors (§3, errors 6 and 7).

**These do not.** Motivational framing with no behavioural delta:

- `steps/01`: "Everything downstream descends from this, so it is worth being slow here."
- `steps/07`: "This is the step under time pressure that looks skippable." / "Failure is
  information, and it has three usual causes" — the three causes earn their place; the
  preamble does not.
- `steps/03`: "Describing the data is part of loading it, not a step of its own." (The
  file has no separate describe step. Nothing to prevent.)
- `SKILL.md`: "Cool?"-register asides are absent, to your credit — but "Going back is
  normal" is doing the same job for zero behavioural change.

Perhaps 15–20 lines across the step files. Small, but they are the easiest cut and the
practice is what keeps the ratio honest.

---

## 4. Structural concerns the guides raise that the skill has not considered

### 4.1 The description is inaccurate, and the one argument is invisible

```yaml
description: Run a Bayesian analysis end to end, one signed-off workflow step at a time.
```

The skill does **not** run an analysis end to end. `SKILL.md`: "One invocation runs **one
step**", and asking for several in one go is explicitly refused. For a user-invoked skill
the description is the human's index entry — the whole of the cognitive load Pocock says
you are choosing to pay — so an inaccurate one is the only failure mode available to it.

Separately: `/bayes-workflow teach` exists and appears only on line 121 of the body. A
human who has forgotten the flag has to open the file to find it. Put it in the
description; that is what a user-invoked description is *for*.

No `compatibility` field, which skill-creator lists as optional. It is warranted here —
the skill has hard runtime dependencies and an explicit "check, do not install" policy.

### 4.2 Token cost: ~7.2k tokens mandatory per invocation, ~85% of it re-read

Measured: `SKILL.md` 9.3 KB, `CONTEXT.md` 6.6 KB, `backends/bambi.md` 9.4 KB, a step file
~4 KB. That is **~29 KB ≈ 7.2k tokens** before the agent has read a line of the project's
own README, data, or prior scripts.

Across a full ten-step analysis: ~72k tokens of mandatory skill reading, of which ~62k is
the same three files re-read. That is defensible only because each invocation is a fresh
context where it *has* to be re-read — which is the point. But it means every line you
delete from the always-loaded set is worth ten times what a line deleted from a step file
is worth, and it makes §3.5's 90 conditional lines a 900-line-equivalent charge per
analysis.

Verdict on the specific question: **the backend file and the step file are defensible;
`SKILL.md` and `CONTEXT.md` together are about 90 lines of bloat.** The 213-line
`bambi.md` in particular earns every line — the cold run rates its five environment traps
as "the most valuable page in the skill" and confirms all five would have cost time. That
is Pocock's **cache** rule satisfied exactly: "Cache what the agent cannot find by looking:
the unwritten convention, the reason behind a choice, the gotcha no config confesses."

### 4.3 `CONTEXT.md` is a split that buys nothing

Pocock: "Material reached only through a pointer escapes context load at the price of the
pointer's own line." `CONTEXT.md`'s pointer — "**Read first.** `CONTEXT.md`, in this skill
directory" — fires on **every** invocation. So it escapes no context load, and costs an
extra file read and four lines of routing prose. It is in-file reference that has been
moved out of the file without being made conditional. By the ladder, that is the one move
that is never right.

The counter-argument is that `CONTEXT.md` + `docs/adr/` is the `mattpocock:domain-modeling`
convention and the naming is deliberate. Fair — but a convention is not free, and this one
is buying you nothing here because the domain model is not shared with anything else.

**My recommendation:** merge `CONTEXT.md` into `SKILL.md` and move `SKILL.md`'s README
template, folder layout and teaching mode *out*. Net effect: one always-loaded file of
~230 lines instead of two totalling 372, and ~90 conditional lines stop being paid ten
times. `SKILL.md` gets *shorter* while absorbing a whole file.

### 4.4 Is ten step files the right decomposition?

**Yes, and the evidence is unusually good.** Four separate steps each produced a finding
the others missed (cold run §2: steps 04, 06, 07, 09), and the two longest steps were the
two that turned up an unexpected result — "which is the correct place for the time to go."
That is Pocock's demand-drives-legwork lever working. Do not merge them.

Three caveats:

- **06/08 is the wrong cut** — see §3.6. Same concept, both sides.
- **07 → 10 is a forward dependency the structure does not support.** `steps/07` requires
  "the contrast code step 10 will use", written at step 07 and imported by step 10. There
  is no slot in the numbered convention for a file that belongs to neither. The cold run
  invented `scripts/_shared_estimand.py` and says so: "That is an invention, not an
  instruction." Name the file in `SKILL.md`.
- **Numbered scripts cannot import each other.** `import 05a_statistical_model` is a
  Python syntax error; `importlib.import_module` is required. The multi-estimand layout in
  `SKILL.md` guarantees you hit this. Undocumented.

### 4.5 Does `tools/dag_tools.py` belong in a skill?

**Yes — and skill-creator argues for it more strongly than you do.** From its improvement
section: "if it looks like … all 3 test cases resulted in the subagent writing a
`create_docx.py` … that's a strong signal the skill should bundle that script. Write it
once, put it in `scripts/`, and tell the skill to use it. **This saves every future
invocation from reinventing the wheel.**" D-separation is deterministic,
correctness-critical, and an agent re-deriving `_path_open` per analysis will eventually
get a collider wrong. This is the textbook case for bundling.

Four problems with *this* module, though:

1. **The 116-row `_FOXES` fixture must go.** The defence in the comment — "Embedded so the
   self-test is self-contained: it must run in any project" — is a legitimate goal reached
   with the wrong data. A seeded synthetic dataset from a known linear SEM is ~10 lines,
   leaks nothing, and is a *better* test: you know the true partial correlations
   analytically instead of pinning to R's output. It also removes ~2.3 KB of noise from a
   file an agent may have to read while debugging.
2. **The tool cannot express an unmeasured confounder.** `dag()` parses only `->` and
   `<-`. There is no `[latent]`, no `<->`. Meanwhile `CONTEXT.md` defines identification
   assumptions as "the DAG, **plus no unmeasured confounding**" — the tool cannot draw the
   one thing that assumption is about, and `adjustment_sets` will cheerfully return a set
   containing a variable you meant to be unobservable. dagitty handles this; the docstring
   claims the module "Covers what `dagitty` provides in the R backend."
3. **The validated envelope is four small DAGs, and the docstring overclaims.** "Validated
   against dagitty" reads as general; `test_against_dagitty()` covers the foxes DAG plus
   three variants. `adjustment_sets` for `effect="direct"` does not exclude descendants of
   the *outcome* from `candidates` — minimality masks it on these four graphs, but it is
   untested. Say what is validated.
4. **The self-test tolerance hides a real disagreement.** Ground truth says `0.3291`, the
   code produces `0.3289`, the tolerance is `5e-4`. The cold run caught this (§1.6). For a
   skill whose first writing rule is "Run the code and show the output. A table lands. An
   assertion invites a challenge", absorbing a known systematic difference into a tolerance
   while printing "all dagitty results reproduced" is the wrong side of your own line.

Minor: `format_local_tests` is called by `steps/03` and `backends/bambi.md` but is missing
from `__all__`.

### 4.6 The skill is close to untestable by skill-creator's method — and the fix is cheap

skill-creator's entire loop needs an executor subagent that can run the skill unattended.
This skill's gate is a human sign-off, ten times. So no `evals.json`, no with/without
baseline, no `grader.md`, no benchmark. The cold run had to invent the workaround —
"With sign-offs pre-granted it worked fine, but nothing in the files describes how to
sequence a full run" (§1.13) — and then invent the file naming, the step ordering, and
where the per-step report text goes.

Formalise what it improvised. An **autopilot** mode — sign-offs pre-granted, decision
lists written to the README as `signed off (autopilot)`, step reports to stdout — costs
about eight lines in `SKILL.md` and converts a documented gap into the one capability that
makes skill-creator's baseline comparison possible. That is the only way you will ever
answer "does step 09 change the output?" with a number rather than an anecdote.

### 4.7 The ADRs are unreachable, but the agent is told to propose new ones

`SKILL.md` instructs the agent to propose `Record this as an ADR?` and rules on where it
goes. The run protocol's reading list is CONTEXT → backend → README → "the step file …
and that file alone." So `docs/adr/` is never read. The cold run noticed and obeyed the
literal instruction: "the four existing ADRs — including `0002-agent-native-sign-off`,
which by its title is directly about the situation I was in — are never read" (§1.12).

Keeping ADRs out of the mandatory path is **correct** — they are a human decision record,
not agent instructions, and that is exactly how `mattpocock:domain-modeling` treats them.
The bug is proposing new ones blind. The fix is a conditional pointer, which is what
Pocock's pointer is for: one line, fires on one branch.

### 4.8 Naming and discoverability

`bayes-workflow` is a good name — precise, two words, and "workflow" is a genuine leading
word here (Gelman's Bayesian workflow is in the model's pretraining and the skill is
recognisably it). No router skill needed: one user-invoked skill is one thing to remember.
Nothing to change.

---

## 5. Concrete fine-tuning suggestions, by value for effort

**1. Delete `_FOXES` and re-key the running example.** (~40 min, highest value.)
One example DAG everywhere, with a confounder and a collider, replacing the foxes DAG in
`steps/02-dag.md:20`, `backends/bambi.md:83`, and `dag_tools.py`. Replace `_FOXES` and
`reference_data()` with a seeded simulation:

```python
def reference_data(n=500, seed=1):
    """Synthetic data from a known linear SEM, so the self-test needs no fixture
    and the true partial correlations are known analytically."""
    rng = np.random.default_rng(seed)
    u = rng.normal(size=n); x = 0.8*u + rng.normal(size=n)
    m = 0.7*x + rng.normal(size=n); y = 0.5*x + 0.6*m + 0.9*u + rng.normal(size=n)
    return {"U": u, "X": x, "M": m, "Y": y}
```

Also fix `steps/01-estimand.md` and `steps/06-prior-check.md`, which still use fox weight
in kg as their worked examples.

**2. Say where the tools path goes, positively.** (~5 min, highest value per minute.)
Replace, in `SKILL.md`:

> **Resolving paths inside this skill.** … Never guess it, and never copy an absolute path
> out of an example.

with:

> **Resolving paths inside this skill.** You are told this skill's base directory when it
> is invoked. Every path in these files is relative to it.
>
> An analysis must run on a machine that does not have this skill installed. So at step
> 02, **copy `tools/dag_tools.py` into the analysis folder's `scripts/`** and import it
> from there. Record in the README which version was copied. The analysis folder is then
> self-contained, and nothing in it names a path outside the project.

This kills the negation, fixes the reproducibility hole, and removes the `SKILL_TOOLS`
constant from `backends/bambi.md` entirely.

**3. Fix the four contradictions in §3.4.** (~20 min.)
- `steps/01-estimand.md`: "**Always run:** Nothing. This step has no script and no data."
  → "**Always run:** `scripts/01_estimand` — it prints the estimand, its units and its
  class, so the numbering is unbroken and the estimand has a home in code. No data, no
  fitting."
- `steps/02-dag.md`: "The DAG figure, one panel per estimand" → "One DAG figure per
  estimand — `draw_dag` writes its own file, so this is one file each, not one figure with
  panels."
- `steps/08-fit.md`: "save the fit object to `data/`" → "save the fit object to `data/` —
  the backend file names the call." Then add to `backends/bambi.md`'s Fit section:
  "`idata.to_netcdf()` needs `h5netcdf`, which is not in this stack. Pickle the
  `InferenceData` and note in the README that a pickle is not an archival format."
- `steps/02-dag.md`: `scripts/02_dag.py` → `scripts/02_dag`.

**4. Move three blocks out of `SKILL.md`, merge `CONTEXT.md` in.** (~45 min, and it is
where the token argument cashes out.)
- README template (32 lines) → `templates/README.md`, pointed at from step 01's file only.
- Folder layout and multi-estimand suffixes (29 lines) → same file, same pointer.
- Teaching mode (15 lines) → `teaching.md`, with one line left behind: "Teaching mode is
  off by default. When the README header says it is on, read `teaching.md`."
- Merge `CONTEXT.md` wholesale, dropping the "Read first" routing section.

**5. Cut the duplication in §3.2.** (~30 min.) Seven meanings, one owner each. The
elapsed-time item alone removes five lines across five step files.

**6. Collapse step 06's power-scaling section to one line.** (~5 min.)
Replace the whole "## After the fit, check whether the prior is still steering" block with:

> **Decide here that power-scaling will run at step 08.** It needs a posterior, so it
> cannot run yet; step 08 owns the mechanism. What this step owes it: a prior you can
> defend before seeing the answer.

**7. Add an autopilot mode.** (~15 min.) After "One invocation runs **one step**":

> **Autopilot.** `/bayes-workflow auto` runs every remaining step without stopping, with
> each M item recorded in the README as `signed off (autopilot)` and each step report
> written to the script's stdout. It exists so the workflow can be tested end to end, and
> for nothing else — an analysis anyone will act on is signed off step by step by a human.

**8. Add the two undocumented mechanics the cold run had to invent.** (~10 min.) In
`SKILL.md`, after the multi-estimand block:

> **Code two steps must share** goes in `scripts/_name.py` — a leading underscore keeps it
> out of the numbered sequence. Step 07's estimand contrast is the usual case.
> **Numbered scripts are not importable by name** (`05a_statistical_model` is not a Python
> identifier). Use `importlib.import_module("05a_statistical_model")`.

**9. One conditional pointer for the ADRs.** (~2 min.) In the sign-off section, before
"Where a material decision is hard to reverse":

> Before proposing an ADR, list `docs/adr/` in this skill and in the project, and read any
> whose title bears on the decision. A decision already recorded is not a new one.

**10. Two traps for `backends/bambi.md`.** (~5 min.)
> **`marginaleffects` reserves the column name `group`.** Any frame passed to it that has
> a `group` column raises `ValueError: reserved column name(s)`. Drop or rename it first —
> `group` is a near-universal name for a cluster id in multilevel data.
> **`pandas.to_markdown()` needs `tabulate`**, and a markdown cell cannot contain a raw
> `|` — the local-test claim strings (`A _||_ G | {F}`) contain three. Escape before
> writing.

**11. Honest docstrings in `dag_tools.py`.** (~10 min.) "Validated against dagitty" →
"Validated against dagitty on N DAGs, listed in `test_against_dagitty()`. Adjustment sets
for `effect='direct'` are untested against graphs containing a descendant of the outcome.
Local-test interval bounds agree with dagitty to about 2e-4; the difference is in the
normal quantile constant and is absorbed by the tolerance." Add `format_local_tests` to
`__all__`. Add a line to `dag()`: "Unmeasured variables cannot be expressed. Where the
identification assumption involves one, reason about it by hand — this module will treat
any named node as observable and conditionable."

**12. Fix the description.** (~2 min.)
> `description: Run a Bayesian analysis one signed-off workflow step at a time, from
> estimand to estimate. Add "teach" for teaching notes, "auto" to run unattended.`

**13. Cut the no-ops in §3.7.** (~15 min.) Five or six whole sentences, deleted not
trimmed.

---

## 6. What I would leave alone

- **One invocation, one step, one step file "and that file alone."** This is the best
  thing in the skill. skill-creator's shape would push toward one `SKILL.md` with all ten
  steps inline; that would put every later step in context during every earlier one, which
  is Pocock's **premature completion** lever pointed the wrong way. Your split is across a
  real context boundary — the only kind he says works — and the cold run's timing table is
  the evidence: the two seven-minute steps were the two that turned up unexpected results.
  Do not consolidate this to save tokens.

- **The M/R decision list and its completion criterion.** "The step is complete when every
  **M** item has a recorded human response in the README" is checkable and exhaustive at
  once — Pocock's stated ideal, which most documents miss. The requirement that each M
  item carry a rejected alternative *with its reason* is what stops it degenerating into a
  checklist, and A07's README (33 M items, every one with a rejected alternative) shows it
  holding under load.

- **"A list past three is a smell: check whether some are implementation details, then
  keep every one that survives."** skill-creator would probably want a hard cap for
  determinism. Resist it. A cap would make the agent hide decisions to satisfy the cap,
  which is the exact failure `adr/0002` was written to prevent.

- **`disable-model-invocation: true`.** skill-creator explicitly recommends the opposite —
  "make the skill descriptions a little bit *pushy*" — and it is wrong for this skill. See
  §3.0.

- **`CONTEXT.md`'s vocabulary and its "*Avoid:*" lines.** These are leading words earning
  their keep, and the *Avoid* lists are what makes them binding rather than decorative.
  The `identification strategy` entry — naming why an econometrician would misread it — is
  the single densest paragraph in the skill. Pocock would count this as the lever used
  correctly.

- **"Write in ASD-STE100 Simplified Technical English."** A genuine leading word: one
  named standard recruits a whole controlled-language prior for free, which is exactly
  Pocock's "reach for an existing word first." (The three sentences after it — "One idea
  per sentence. Short sentences. Active voice." — are that word spelled back out, and
  could go. Small.)

- **`backends/bambi.md` at 213 lines.** Longest single file in the mandatory path and
  worth every line. Cached gotchas no config confesses, all five confirmed real by an
  independent run. Do not trim it in a token pass.

- **`docs/examples.md` and its pointer.** The one correct application of the ladder in the
  skill, with a stated firing condition and a stated reason for the disclosure.

- **The ADRs, including superseded `0001`.** Keeping the superseded reasoning is right,
  and `0004`'s honesty about why `0001` was wrong — "the human reviewed no code at all" —
  is the kind of thing that makes a decision record worth having. They just need §5.9's
  pointer.

- **The five "why" lines in §3.7's first list.** Both guides would let you cut them on a
  length pass. Do not. There is a run's worth of evidence that each changed the output.
