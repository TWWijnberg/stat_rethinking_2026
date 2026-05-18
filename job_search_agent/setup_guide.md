# Job Search Agent — Setup Guide

This guide walks you through everything you need to get the agent running.
No technical background required. Each step includes exactly what to do
and why.

**Time required:** about 30–45 minutes for the full setup.

---

## What you'll end up with

- A script that scrapes job boards daily and emails you a ranked digest
- Feedback buttons in the email (👍 / 👎) that open a Google Form on your phone
- An optional second step to run it automatically in the cloud (no laptop needed)

---

## Overview of steps

1. Install Python
2. Download the code
3. Set up your Gmail App Password
4. Run the agent for the first time
5. Set up the Google Form for feedback *(optional but recommended)*
6. Automate with GitHub Actions *(optional — removes need to run manually)*

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
5. Type the following and press Enter to confirm Python is installed:
   ```
   python --version
   ```
   You should see something like `Python 3.11.x`. If you see an error,
   try `python3 --version` instead — and use `python3` everywhere below
   instead of `python`.

---

## Step 2 — Download the code

### Option A — Using Git (recommended if you have it)

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
   *(adjust the path to wherever you unzipped it)*

### Install the required libraries

In your terminal, with the job-search-agent folder open, run:

```
pip install -r requirements.txt
```

This installs all the tools the agent needs. It may take a minute.

---

## Step 3 — Set up your Gmail App Password

The agent sends you an email each day. To do this, it needs a special
"App Password" — a one-time code that lets it send email on your behalf
without using your main Gmail password.

**Why not use your normal password?** Google blocks direct logins from
scripts for security reasons. App Passwords are specifically designed for
this use case.

### Create the App Password

1. Go to your Google Account: **https://myaccount.google.com/**
2. Click **Security** in the left sidebar
3. Scroll down to **"How you sign in to Google"** and click **2-Step Verification**
   - If it's not enabled, enable it first (you'll need your phone)
4. Scroll down to the bottom of the 2-Step Verification page
5. Click **App passwords** (you may need to search for it)
6. In the "Select app" dropdown choose **Mail**
7. In the "Select device" dropdown choose **Other** and type `Job Agent`
8. Click **Generate**
9. Google will show you a **16-character password** like `abcd efgh ijkl mnop`
   — copy it, you'll need it in a moment

### Add the password to the agent

1. In the job-search-agent folder, find the file called **`.env.example`**
2. Make a copy of it and rename the copy to **`.env`** (note: no `.example`)
3. Open `.env` in a text editor (Notepad on Windows, TextEdit on Mac)
4. Replace `your-16-character-app-password-here` with the password you just copied
   (remove the spaces — it should be 16 characters with no spaces)
5. Save and close the file

Your `.env` file should look like this:

```
GMAIL_APP_PASSWORD=abcdefghijklmnop
```

---

## Step 4 — Run the agent for the first time

In your terminal (in the job-search-agent folder), run:

```
python main.py
```

You should see output like this:

```
========================================
  Job Search Agent
========================================

[1/4] Scraping sources...
  Indeed UK (RSS)...
    → 47 jobs found
  NHS Jobs (RSS)...
    → 23 jobs found
  ...

[4/4] Sending email digest...
  Email sent to thijs.wijnberg@gmail.com (18 jobs).

Done. ✓
```

Check your inbox — you should receive the digest email within a few seconds.

### If something goes wrong

**"GMAIL_APP_PASSWORD is not set"**
→ Check that your `.env` file exists (not `.env.example`) and the password
  is correct with no extra spaces.

**"No new jobs today"**
→ This is normal on the second+ run — the agent tracks what it has already
  sent you to avoid duplicates. Try lowering `minimum_score` in `config.yaml`
  if the digest is consistently empty.

**Scraper warnings** (e.g. "Wellfound scrape failed")
→ These are usually temporary network issues. The agent continues with other
  sources. If a specific scraper fails every time, see the "Updating scrapers"
  section at the bottom of this guide.

---

## Step 5 — Set up the Google Form for feedback *(optional)*

This lets you rate jobs with 👍 / 👎 from your phone, which the agent uses
to improve future recommendations.

### Create the Google Form

1. Go to **https://forms.google.com** and click **Blank form** (the + button)
2. Name it: `Job Search Feedback`
3. Add these four questions (click the + button to add each one):

   **Question 1**
   - Question type: **Short answer**
   - Question text: `Job ID`
   - Click the three dots → tick **"Response validation"** → not needed,
     just leave it

   **Question 2**
   - Question type: **Multiple choice**
   - Question text: `Rating`
   - Options: `Good fit` and `Not a fit`

   **Question 3**
   - Question type: **Short answer**
   - Question text: `Notes`
   - Description: `Optional — explain why this role is or isn't a fit`

4. Click **Send** (top right) to get the form link — but don't share it yet.

### Find the pre-fill field IDs

This step sounds technical but it's just a few clicks:

1. In the form editor, click the three dots (⋮) in the top right → **Get pre-filled link**
2. Fill in dummy values in each field:
   - Job ID: `TESTID`
   - Rating: select either option
   - Notes: `test`
3. Click **Get link** at the bottom
4. You'll get a URL that looks like:
   ```
   https://docs.google.com/forms/d/e/LONG_FORM_ID/viewform?usp=pp_url&entry.123456789=TESTID&entry.987654321=Good+fit&entry.111222333=test
   ```
5. Note down:
   - `LONG_FORM_ID` — everything between `/d/e/` and `/viewform`
   - `entry.123456789` — this is your **Job ID field ID**
   - `entry.987654321` — this is your **Rating field ID**

### Update config.yaml

Open `config.yaml` in a text editor and fill in the feedback section:

```yaml
feedback:
  google_form_url: "https://docs.google.com/forms/d/e/LONG_FORM_ID/viewform?usp=pp_url"
  job_id_field: "entry.123456789"
  rating_field: "entry.987654321"
  google_sheet_id: ""        # leave blank for now
  google_sheet_name: "Form Responses 1"
  credentials_file: "google_credentials.json"
```

Replace `LONG_FORM_ID`, `entry.123456789`, and `entry.987654321` with your
actual values.

After this, the 👍 / 👎 buttons in the email will work on your phone.

### Link the form to a Google Sheet (for the agent to read your feedback)

By default, Google Forms saves responses to a Sheet automatically:

1. In the form editor, click the **Responses** tab
2. Click the green Sheets icon (📊) → **Create a new spreadsheet**
3. Name it `Job Search Feedback` and click Create
4. A Google Sheet will open — note the ID from the URL:
   ```
   https://docs.google.com/spreadsheets/d/THIS_IS_THE_SHEET_ID/edit
   ```
5. Add the Sheet ID to `config.yaml`:
   ```yaml
   google_sheet_id: "THIS_IS_THE_SHEET_ID"
   ```

At this point the agent will print a feedback summary each run, and over time
you'll see notes like:

```
Common words in 'not a fit' notes: ['engineering', 'technical', 'bi']
→ Consider adding these as negative_keywords in config.yaml
```

### (Advanced) Automate reading feedback with a service account

The above will show the feedback summary but won't automatically adjust scores.
For fully automatic score tuning, you need to give the agent access to your Sheet:

1. Go to **https://console.cloud.google.com/**
2. Create a new project (name it anything, e.g. `job-agent`)
3. In the search bar, search for **"Google Sheets API"** → click Enable
4. Go to **IAM & Admin → Service Accounts → Create Service Account**
   - Name: `job-agent-reader`
   - Click Create and Continue, skip the optional steps, click Done
5. Click on the service account → **Keys** tab → **Add Key → Create new key → JSON**
6. Download the JSON file, rename it `google_credentials.json`, and put it in
   your job-search-agent folder
7. Back in the Service Accounts list, copy the service account's email address
   (looks like `job-agent-reader@your-project.iam.gserviceaccount.com`)
8. Open your Google Sheet, click **Share**, paste in that email, give it **Viewer** access

Now when you run `python main.py`, the agent will read your feedback scores
automatically.

---

## Step 6 — Automate with GitHub Actions *(optional)*

Right now you run `python main.py` manually. This step sets it up to run
automatically every weekday morning at 8am UK time — no laptop needed.

You'll need a GitHub account. If you don't have one, sign up free at
**https://github.com**.

### Push the code to GitHub

If you haven't already:

1. Create a new repository at **https://github.com/new**
   - Name: `job-search-agent`
   - Set it to **Private** (keeps your config private)
   - Click **Create repository**

2. In your terminal (in the job-search-agent folder):
   ```
   git init
   git add .
   git commit -m "Initial setup"
   git remote add origin https://github.com/YOUR_USERNAME/job-search-agent.git
   git push -u origin main
   ```
   Replace `YOUR_USERNAME` with your GitHub username.

### Add your secrets to GitHub

GitHub Actions needs your Gmail password, but you should never put passwords
in code. Instead, you store them as "secrets":

1. Go to your repository on GitHub
2. Click **Settings** → **Secrets and variables** → **Actions**
3. Click **New repository secret**
4. Name: `GMAIL_APP_PASSWORD`, Value: your 16-character app password → **Add secret**
5. If you set up Google credentials (Step 5), add another secret:
   Name: `GOOGLE_CREDENTIALS_JSON`
   Value: paste the entire contents of your `google_credentials.json` file

### Enable the workflow

The workflow file (`.github/workflows/daily_digest.yml`) is already included
in the code. GitHub will pick it up automatically once the code is pushed.

To verify it's working:

1. Go to your repository → **Actions** tab
2. Click **Daily Job Digest** in the left panel
3. Click **Run workflow** → **Run workflow** to trigger it manually
4. Watch the run — it should turn green and you'll get an email

After that, it runs automatically every weekday at 8am UK time.

---

## Tuning your results over time

The agent gets better as you rate more jobs. Here's how to use that:

### Too many irrelevant results?

Open `config.yaml` and add bad keywords to `negative_keywords`:

```yaml
negative_keywords:
  "data engineer": -8
  "bi developer": -8      # add this if "BI developer" roles keep appearing
  "reporting analyst": -5  # add if too junior
```

### Not finding the right roles?

Increase the score for your most important keywords:

```yaml
seniority_keywords:
  "head of": 15      # was 10 — boost this signal
```

Or add new search terms in the `indeed_searches` section.

### Digest is empty or very short?

Lower the `minimum_score` from 8 to 5 or 6. This lets through more marginal
matches — useful early on while you're still calibrating.

---

## Updating scrapers if they break

Web scrapers occasionally break when a website changes its design. Signs that
a scraper needs updating:

- A specific source consistently shows 0 results
- You get unusual job titles in your digest from a specific source

To fix a broken employer scraper:

1. Open `scrapers/employers.py`
2. Find the employer's entry in the `_EMPLOYERS` list
3. Update the `url` to the current careers page URL
4. Update the `link_pattern` regex if the link format has changed

For Indeed and NHS Jobs (RSS scrapers), failures are usually temporary network
issues — wait a day and try again.

---

## About LinkedIn

LinkedIn does not provide a public job RSS feed and blocks automated scraping.
The reliable way to search LinkedIn is:

1. Go to **https://www.linkedin.com/jobs/search/**
2. Set your filters (seniority: Director / Senior Manager, location, keywords)
3. Click the bell icon (🔔) to set up a **Job Alert** — LinkedIn will email you
   directly when matching roles are posted

This is actually more reliable than scraping because LinkedIn's own algorithm
does the matching. Set up 2–3 alerts for your key keyword combinations.

---

## File reference

| File | Purpose |
|------|---------|
| `config.yaml` | Keywords, scoring weights, email settings — edit this to tune results |
| `.env` | Your Gmail app password — never share or commit this file |
| `main.py` | Run this to get your digest |
| `jobs.db` | Database of jobs seen so far (auto-created) |
| `google_credentials.json` | Google API access (only needed for feedback automation) |
| `scrapers/` | One file per source — update here if a scraper breaks |
| `setup_guide.md` | This file |
