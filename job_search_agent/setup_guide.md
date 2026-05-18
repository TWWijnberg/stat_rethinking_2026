# Job Search Agent — Setup Guide

This guide walks you through everything you need to get the agent running.
No technical background required. Each step says exactly what to do and why.

**Time required:** about 30–45 minutes for the full setup.

---

## How the agent works

```
Daily
─────
python main.py
  → scrapes Indeed UK, NHS Jobs, and employer career pages
  → removes jobs you've already seen
  → emails you a CSV of new roles

You
  → open the CSV, paste it into Claude with scoring_prompt.md
  → Claude returns a ranked list with reasoning
  → rate jobs you care about using 👍 / 👎 links in the email

Weekly
──────
python review.py
  → pulls your ratings from Google Sheet
  → exports feedback_report.csv

You
  → paste feedback_report.csv into Claude with review_scoring_feedback.md
  → Claude spots patterns in your feedback and outputs an updated scoring_prompt.md
  → you save the new version — criteria improve over time
```

---

## Overview of steps

1. Install Python
2. Download the code
3. Set up your Gmail App Password
4. Run the agent for the first time
5. Set up the Google Form for feedback *(optional but recommended)*
6. Set up automatic feedback reading *(optional — needed for review.py)*
7. Automate with GitHub Actions *(optional — removes need to run manually)*

---

## Step 1 — Install Python

Python is the programming language the agent is written in.

1. Go to **https://www.python.org/downloads/**
2. Click the big yellow **"Download Python 3.x.x"** button
3. Run the installer
   - **Windows:** tick the box that says **"Add Python to PATH"** before clicking Install
   - **Mac:** just click through the installer
4. When it finishes, open a terminal:
   - **Windows:** press `Win + R`, type `cmd`, press Enter
   - **Mac:** open Spotlight (Cmd + Space), type `Terminal`, press Enter
5. Confirm Python is installed:
   ```
   python --version
   ```
   You should see something like `Python 3.11.x`. If you see an error,
   try `python3 --version` — and use `python3` everywhere in this guide
   instead of `python`.

---

## Step 2 — Download the code

### Option A — Using Git (if you have it)

```
git clone https://github.com/TWWijnberg/job-search-agent.git
cd job-search-agent
```

### Option B — Download a ZIP

1. Go to **https://github.com/TWWijnberg/job-search-agent**
2. Click the green **Code** button → **Download ZIP**
3. Unzip it somewhere easy to find (e.g. your Desktop)
4. Open a terminal and navigate to that folder:
   ```
   cd Desktop/job-search-agent
   ```

### Install the required libraries

In your terminal (in the job-search-agent folder):

```
pip install -r requirements.txt
```

This may take a minute. You should see a list of packages being installed.

---

## Step 3 — Set up your Gmail App Password

The agent sends you an email each day with a CSV attached. To do this,
it needs a special "App Password" — not your normal Gmail password.

**Why not use your normal password?** Google blocks direct logins from
scripts for security reasons. App Passwords are specifically designed for
this use case and can be revoked at any time.

### Create the App Password

1. Go to your Google Account: **https://myaccount.google.com/**
2. Click **Security** in the left sidebar
3. Under **"How you sign in to Google"**, click **2-Step Verification**
   - If it's not enabled, enable it first (you'll need your phone)
4. Scroll to the bottom of that page and click **App passwords**
   *(if you don't see it, search for "App passwords" in the search bar at the top)*
5. In the **"Select app"** dropdown choose **Mail**
6. In the **"Select device"** dropdown choose **Other**, type `Job Agent`
7. Click **Generate**
8. Google shows you a **16-character password** like `abcd efgh ijkl mnop`
   — copy it now (you won't see it again)

### Add the password to the agent

1. In the job-search-agent folder, find **`.env.example`**
2. Make a copy of it and rename the copy to **`.env`** (remove `.example`)
   - **Windows:** right-click → Copy, then right-click → Paste, rename to `.env`
   - **Mac:** duplicate the file in Finder, rename to `.env`
3. Open `.env` in a text editor
4. Replace `your-16-character-app-password-here` with your password
   (no spaces — 16 characters only)
5. Save and close

Your `.env` file should look like:
```
GMAIL_APP_PASSWORD=abcdefghijklmnop
```

---

## Step 4 — Run the agent for the first time

In your terminal (in the job-search-agent folder):

```
python main.py
```

You should see:
```
========================================
  Job Search Agent
========================================

[1/3] Scraping sources...
  Indeed UK (RSS)...
    → 47 jobs
  NHS Jobs (RSS)...
    → 31 jobs
  Employer career pages...
    SystemC... → 4 listings
    Epic... → 8 listings
    ...

  Total scraped: 97
  New (not seen before): 97

[2/3] Sending email...
  Email sent to thijs.wijnberg@gmail.com with 97 jobs (new_jobs_2026-05-18.csv).

[3/3] Done. ✓
```

Check your inbox — you should receive an email with a CSV attached.

### How to score the jobs

1. Open the CSV attachment
2. Open **scoring_prompt.md** (in the job-search-agent folder) in a text editor
3. Go to **https://claude.ai** and start a new conversation
4. Paste the full contents of `scoring_prompt.md` into the chat
5. Then paste the CSV contents beneath it (or upload the file if Claude supports it)
6. Claude will return a ranked list with a score and reason for each role

The scoring criteria in `scoring_prompt.md` are yours to edit. As you use
the agent, you'll notice things to tweak — just open the file and adjust.

### If something goes wrong

**"GMAIL_APP_PASSWORD is not set"**
→ Check that your `.env` file exists (not `.env.example`) and the password
  is correct with no extra spaces.

**A scraper shows 0 jobs**
→ Usually a temporary network issue. Try again the next day. If it persists,
  see *Updating scrapers* at the bottom of this guide.

**"No new jobs today"**
→ Normal from the second run onward — the agent tracks what it has already
  sent you to avoid duplicates.

---

## Step 5 — Set up the Google Form for feedback *(optional)*

This adds 👍 / 👎 links to your daily email. Clicking one on your phone
opens a Google Form pre-filled with the job details — rate it and add a
note if you want. Ratings feed into the weekly review step.

### Create the Google Form

1. Go to **https://forms.google.com** and click the **+** (blank form)
2. Name it: `Job Search Feedback`
3. Add these three questions using the **+** button:

   **Question 1**
   - Type: **Short answer**
   - Text: `Job ID`

   **Question 2**
   - Type: **Multiple choice**
   - Text: `Rating`
   - Options: `Good fit` and `Not a fit`

   **Question 3**
   - Type: **Short answer**
   - Text: `Notes`
   - Description: `Optional — what made this a good or bad fit?`

### Get the pre-fill field IDs

1. Click the three dots (⋮) top right → **Get pre-filled link**
2. Fill in dummy values: Job ID = `TEST`, select either rating, Notes = `test`
3. Click **Get link**
4. You'll get a URL like:
   ```
   https://docs.google.com/forms/d/e/FORM_ID/viewform?usp=pp_url&entry.111111111=TEST&entry.222222222=Good+fit&entry.333333333=test
   ```
5. Note down:
   - `FORM_ID` — everything between `/d/e/` and `/viewform`
   - `entry.111111111` — Job ID field
   - `entry.222222222` — Rating field

### Update config.yaml

Open `config.yaml` in a text editor and fill in the feedback section:

```yaml
feedback:
  google_form_url: "https://docs.google.com/forms/d/e/FORM_ID/viewform?usp=pp_url"
  job_id_field: "entry.111111111"
  rating_field: "entry.222222222"
```

After this, your daily email will include 👍 / 👎 buttons for each job.

### Connect form responses to a Google Sheet

1. In the form editor, click the **Responses** tab
2. Click the green Sheets icon (📊) → **Create a new spreadsheet**
3. Name it `Job Search Feedback` → **Create**
4. The sheet URL will look like:
   ```
   https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID/edit
   ```
5. Add the Sheet ID to `config.yaml`:
   ```yaml
   google_sheet_id: "YOUR_SHEET_ID"
   ```

---

## Step 6 — Set up automatic feedback reading *(optional)*

This allows `review.py` to pull your ratings automatically. Without it,
`review.py` will skip the Google Sheet step.

### Create a service account

A service account is a special Google account for scripts — it's not a
personal account, just a way to give the agent read access to your Sheet.

1. Go to **https://console.cloud.google.com/**
2. Click the project dropdown at the top → **New Project**
   - Name it `job-agent` → **Create**
3. In the search bar, type **Google Sheets API** → click it → **Enable**
4. Go to **IAM & Admin** → **Service Accounts** → **Create Service Account**
   - Name: `job-agent-reader` → **Create and Continue**
   - Skip the optional steps → **Done**
5. Click on the service account you just created → **Keys** tab
   → **Add Key** → **Create new key** → **JSON** → **Create**
6. A JSON file downloads — rename it `google_credentials.json` and put it
   in your job-search-agent folder

### Share your Sheet with the service account

1. Open the JSON file in a text editor
2. Find the `"client_email"` field — it looks like:
   `job-agent-reader@job-agent-123456.iam.gserviceaccount.com`
3. Open your `Job Search Feedback` Google Sheet
4. Click **Share** → paste in that email → give it **Viewer** access → **Done**

### Test it

Install the Google libraries:
```
pip install gspread google-auth
```

Then run:
```
python review.py
```

If your Sheet has ratings, you'll see `feedback_report.csv` created in the folder.

---

## Step 7 — Automate with GitHub Actions *(optional)*

This runs the agent every weekday morning at 8am UK time without you
having to open a terminal. You'll need a GitHub account (free at github.com).

### Push the code to GitHub

If you haven't already:

1. Go to **https://github.com/new** — create a private repository called
   `job-search-agent`
2. In your terminal (in the job-search-agent folder):
   ```
   git init
   git add .
   git commit -m "Initial setup"
   git remote add origin https://github.com/YOUR_USERNAME/job-search-agent.git
   git push -u origin main
   ```

### Add your secrets to GitHub

Never put passwords in code. Store them as GitHub Secrets instead:

1. Go to your repository → **Settings** → **Secrets and variables** → **Actions**
2. **New repository secret**:
   - Name: `GMAIL_APP_PASSWORD`
   - Value: your 16-character app password
3. If you set up Step 6, add another secret:
   - Name: `GOOGLE_CREDENTIALS_JSON`
   - Value: paste the full contents of `google_credentials.json`

### Enable the workflow

The workflow file (`.github/workflows/daily_digest.yml`) is already in the
code. Once pushed to GitHub, it runs automatically.

To verify: go to your repository → **Actions** tab → **Daily Job Digest**
→ **Run workflow** → check it turns green and you receive an email.

---

## The weekly review

Once you have 10+ job ratings, run:

```
python review.py
```

This creates `feedback_report.csv`. Then:

1. Open Claude (https://claude.ai)
2. Open `review_scoring_feedback.md` in a text editor — paste its contents
   into Claude
3. Replace the placeholder with the current contents of `scoring_prompt.md`
4. Paste the contents of `feedback_report.csv` beneath it
5. Claude will output an updated `scoring_prompt.md` — review and save it

Do this weekly or whenever you've accumulated enough new ratings. Over time
your criteria get sharper.

---

## About Wellfound

Wellfound's website is JavaScript-rendered, which means standard scrapers
can't read it. The recommended workaround:

1. Go to **https://wellfound.com/jobs**
2. Set filters: remote, relevant role types
3. Click the bell icon 🔔 to create a **Job Alert**
4. Wellfound will email you new matches directly

*(Future upgrade: the agent will parse those alert emails automatically
and fold them into the daily digest.)*

---

## Tuning your results

### Too many irrelevant roles in the CSV?

Add more specific search queries in `config.yaml` under `indeed_searches`.
Or, just let Claude filter — the scoring prompt handles most noise.

### Claude keeps scoring a certain type of role too high/low?

Edit `scoring_prompt.md` directly. The criteria are plain English — just
update the wording to reflect what you actually want.

### The weekly review isn't catching the right patterns?

Add notes when you rate jobs. "Too technical" or "no strategic remit" is
much more useful to Claude than a bare thumbs-down.

---

## Updating scrapers if they break

Signs a scraper needs updating:
- A source consistently shows 0 results
- You receive oddly-titled jobs from one source

To fix an employer scraper, open `scrapers/employers.py`, find the employer
in `_EMPLOYERS`, and update the `url` to their current careers page.

For Indeed and NHS Jobs, 0 results usually means a temporary network issue
— try again the next day.

---

## File reference

| File | Purpose |
|------|---------|
| `main.py` | Run this daily to get your digest |
| `review.py` | Run this weekly to generate the feedback report |
| `scoring_prompt.md` | Paste into Claude with your CSV to score jobs — edit to tune |
| `review_scoring_feedback.md` | Paste into Claude with feedback_report.csv to update scoring |
| `config.yaml` | Email settings, search terms, feedback form IDs |
| `.env` | Your Gmail app password — never share or commit this |
| `jobs.db` | Database of jobs seen so far — keeps the digest fresh |
| `scrapers/` | One file per source — update here if a scraper breaks |
| `google_credentials.json` | Google API access for feedback reading (Step 6 only) |
