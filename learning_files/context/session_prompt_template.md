# Session Start Prompt Template

Use this template at the beginning of each homework session. Copy the template below, fill in the bracketed sections with your specifics for that week, and provide it to your AI assistant along with your context documents.

---

## The Template

```
I'm working through the Statistical Rethinking 2026 course and am about to start this week's homework. Please review my context documents and help me create a structured plan for this session.

**Context Documents** (in my context folder):
- `agent_instructions.md` — My learning preferences and communication style
- `weekly_workflow.md` — My project structure and session workflow
- `learning_roadmap.md` — My overall learning goals across four areas
- `r_environment_guide.md` — My R/VSCode environment setup and troubleshooting

**This Session:**
- Lecture: [Week X: Topic Name]
- Homework: [Homework X]
- Time available: [e.g., ~1.5 hours]

**What I understood from the lecture:**
[Write 2-3 sentences about the main concepts you took away. This helps the agent identify gaps and connect homework to your understanding.]

**What I'm uncertain about:**
[Write 1-2 things that felt unclear or that you want to explore through the homework.]

Based on my workflow document and learning preferences, please:

1. Give me a step-by-step plan for this session, including which files to create/update and when to commit
2. Briefly preview what each homework problem is testing (without giving away answers)
3. Identify which concepts from the lecture each problem connects to
4. Suggest one thing to watch out for based on common misconceptions at this stage

Remember my preferences: I learn by doing, I want to struggle before getting help, and I appreciate direct feedback when my understanding is incomplete. Start me off with the plan, then let me attempt the problems before offering guidance.

**At the end of the session**, please:
5. Ask me reflective questions about what I learned, what surprised me, and where I still feel uncertain
6. Based on my answers, suggest specific updates to my context documents—for example:
   - New preferences or patterns to add to `agent_instructions.md`
   - Workflow adjustments for `weekly_workflow.md`
   - Progress updates or newly discovered learning gaps for `learning_roadmap.md`
   - Environment solutions or tips for `r_environment_guide.md`

This helps my context documents evolve to reflect my actual learning experience, not just my initial guesses about how I learn.
```

---

## Example: Week 3

Here's a filled-in example to illustrate how you might complete the template:

```
I'm working through the Statistical Rethinking 2026 course and am about to start this week's homework. Please review my context documents and help me create a structured plan for this session.

**Context Documents** (in my context folder):
- `agent_instructions.md` — My learning preferences and communication style
- `weekly_workflow.md` — My project structure and session workflow
- `learning_roadmap.md` — My overall learning goals across four areas
- `r_environment_guide.md` — My R/VSCode environment setup and troubleshooting

**This Session:**
- Lecture: Week 3: Geocentric Models
- Homework: Homework 3
- Time available: ~1.5 hours

**What I understood from the lecture:**
Linear regression describes the distribution of outcomes as a function of predictors, not a deterministic relationship. The "geocentric" framing means the model can be useful for prediction even without capturing true causal structure. Priors encode plausibility before seeing data, and prior predictive simulation lets us check whether our priors make sense.

**What I'm uncertain about:**
I'm not confident about how to choose reasonable priors when I lack strong prior knowledge. The lecture demonstrated prior predictive simulation, but I'm unsure I could set it up myself. I also don't fully grasp when to use quadratic approximation versus MCMC.

Based on my workflow document and learning preferences, please:

1. Give me a step-by-step plan for this session, including which files to create/update and when to commit
2. Briefly preview what each homework problem is testing (without giving away answers)
3. Identify which concepts from the lecture each problem connects to
4. Suggest one thing to watch out for based on common misconceptions at this stage

Remember my preferences: I learn by doing, I want to struggle before getting help, and I appreciate direct feedback when my understanding is incomplete. Start me off with the plan, then let me attempt the problems before offering guidance.

**At the end of the session**, please:
5. Ask me reflective questions about what I learned, what surprised me, and where I still feel uncertain
6. Based on my answers, suggest specific updates to my context documents—for example:
   - New preferences or patterns to add to `agent_instructions.md`
   - Workflow adjustments for `weekly_workflow.md`
   - Progress updates or newly discovered learning gaps for `learning_roadmap.md`
   - Environment solutions or tips for `r_environment_guide.md`

This helps my context documents evolve to reflect my actual learning experience, not just my initial guesses about how I learn.
```

---

## Tips for Using This Template

**Be honest about uncertainty.** The "What I'm uncertain about" section is particularly valuable. It helps the agent focus guidance where you actually need it, and it helps you practice the skill of identifying your own knowledge gaps, which is crucial for self-directed learning.

**Adjust the time estimate realistically.** If you only have 45 minutes, say so. The agent can help you prioritize which problems to tackle first or suggest a partial session plan.

**Update the template itself.** If you find yourself consistently adding or removing sections, modify this template to match your actual usage. The template should serve you, not constrain you.

**Save your filled-in prompts.** Consider saving each week's completed prompt in your `docs/` folder (e.g., `docs/session_prompt.md`). This creates a record of your starting understanding each week, which is interesting to review later to see how your thinking has evolved.

---

## Quick Reference: Course Schedule

For easy reference when filling in the template:

| Week | Lecture Topic | Chapters |
|------|---------------|----------|
| 01 | Introduction | 1 & 2 |
| 02 | Garden of Forking Data | 2 & 3 |
| 03 | Geocentric Models | 4 |
| 04 | Categories & Curves | 4 |
| 05 | Elemental Confounds | 5 |
| 06 | Good and Bad Controls | 6 |
| 07 | Overfitting | 7 |
| 08 | Markov chain Monte Carlo | 9 & 10 |
| 09 | Modeling Events | 10 & 11 |
| 10 | Multilevel Models | 12 |

---

*Template version: 1.0 | Last updated: January 2026*
