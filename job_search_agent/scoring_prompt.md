# Job Scoring Prompt

Use this prompt with Claude when you receive your daily job digest email.

---

**How to use:**
1. Open Claude (claude.ai)
2. Paste everything below this line into the chat
3. Then paste the contents of today's CSV beneath it

---

I'm searching for my next role and need you to score the job postings below.

## About me

Senior professional with a quantitative background, currently in a data/analytics leadership role. Looking to move into a Head of / Director level position. I want roles where analytics, insight or strategy drives real decisions — not just reporting or engineering.

## Scoring criteria (in priority order)

**1. Seniority** — Must be Head of / Director / Deputy Director level.
Senior Manager is borderline — include if the other criteria are strong.
Individual contributor roles, even senior ones, are not relevant.

**2. Domain** — Analytics, insight, data strategy, product strategy, strategic planning.
The role should be about driving outcomes and decisions, not building dashboards or pipelines.
"Head of BI" is fine if it has strategic remit. "BI Developer" or "Data Engineer" is not.

**3. Sector** — Health, NHS, integrated care, healthtech, or EPR/clinical systems vendors.
Roles outside health/care are only worth flagging if they are exceptional on every other criterion.

**4. Reporting line** — Ideally reports into C-suite (CFO, CEO, COO, CPO).
Note it explicitly if the description mentions this.

**5. Location** — Remote or near Sheffield UK strongly preferred.
Hybrid roles in the North of England are fine. London is acceptable for a strong role.
On-site only outside the UK is a dealbreaker.

## What to deprioritise

- Data engineer, software engineer, ETL, DevOps roles
- Pure reporting or dashboard roles without strategic remit
- Roles where "analytics" clearly means operational reporting only
- Graduate, junior, or intern roles
- Roles with no health/care connection

## Output format

Score each job 1–10. Return only jobs scoring 5 or above.
Sort by score, highest first.

For each job:

**[Score/10] Job Title — Company**
- Location: [location or "not listed"]
- Salary: [salary or "not listed"]
- Why: [one sentence on why this matches or doesn't]
- Watch out: [any concern — e.g. "sounds more engineering than strategy", "sector unclear"]
- Link: [url]

After the list, add a short paragraph: what patterns do you notice across today's roles?
Are there any signals I should add or remove from my criteria?
