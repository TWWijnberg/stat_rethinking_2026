# Scoring Prompt Review

Use this prompt with Claude when you want to update your scoring criteria
based on accumulated feedback. Run this weekly, or whenever you have 10+
ratings to work with.

---

**How to use:**
1. Run `python review.py` — this generates `feedback_report.csv`
2. Open Claude (claude.ai)
3. Paste everything below this line into the chat
4. Then paste the contents of `feedback_report.csv` beneath it

---

I'm refining my job search scoring criteria based on feedback I've given
on past recommendations.

Below is a CSV with:
- The job details (title, company, description, etc.)
- My rating: "Good fit" or "Not a fit"
- My notes explaining why (where I added them)

## My current scoring criteria

*(This section is updated automatically — replace it with the current
contents of scoring_prompt.md each time you run this review)*

---

[PASTE CONTENTS OF scoring_prompt.md HERE]

---

## What I need from you

1. **Identify patterns** in what I rated positively vs negatively.
   Look at titles, descriptions, companies, and my notes. What distinguishes
   a "Good fit" from a "Not a fit" in ways my current criteria don't capture?

2. **Identify gaps or errors** in my current criteria.
   Are there rules that are too broad (letting in noise)?
   Rules that are too narrow (filtering out things I actually liked)?
   Anything I thumbed up that my criteria would have deprioritised?

3. **Output an updated scoring_prompt.md** that incorporates your findings.
   Be specific — change the wording of criteria, reorder priorities, or add
   new rules based on what you observed. Show me the full updated file, not
   just the changes.

4. **Summarise your reasoning** in 3–5 bullet points before showing the
   updated file. What were the key patterns that drove your changes?
