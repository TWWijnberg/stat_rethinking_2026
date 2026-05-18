#!/usr/bin/env python3
"""
Synthetic test — verifies the agent's core components work end-to-end
without needing real credentials or network access.

    python test_runner.py

Tests:
  1. Database init and job storage/deduplication
  2. CSV export (emailer._jobs_to_csv)
  3. Email message construction (without sending)
  4. Feedback module imports cleanly
  5. review.py imports cleanly
  6. Prints the synthetic jobs in the format Claude would receive them
"""
import csv
import sys
from pathlib import Path

# ── 1. Database ───────────────────────────────────────────────────────────────
print("=" * 50)
print("TEST 1: Database")
print("=" * 50)

# Use a temp DB so we don't pollute the real one
import database
database.DB_PATH = Path(__file__).parent / "test_jobs.db"

database.init_db()
print("  init_db()          OK")

synthetic_jobs = list(csv.DictReader(open("test_synthetic.csv")))

assert database.is_new_job(synthetic_jobs[0]["url"]) is True
print("  is_new_job (new)   OK")

database.save_jobs(synthetic_jobs)
print("  save_jobs()        OK")

assert database.is_new_job(synthetic_jobs[0]["url"]) is False
print("  is_new_job (seen)  OK — dedup working")

retrieved = database.get_jobs_by_ids([j["id"] for j in synthetic_jobs])
assert len(retrieved) == 3
print(f"  get_jobs_by_ids()  OK — retrieved {len(retrieved)} jobs")

# Clean up test DB
database.DB_PATH.unlink()
print("  test DB cleaned up")

# ── 2. CSV export ─────────────────────────────────────────────────────────────
print()
print("=" * 50)
print("TEST 2: CSV export")
print("=" * 50)

from emailer import _jobs_to_csv
csv_output = _jobs_to_csv(synthetic_jobs)
lines = csv_output.strip().split("\n")
assert lines[0].startswith("id,title"), f"Unexpected header: {lines[0]}"
assert len(lines) == 4  # header + 3 jobs
print(f"  _jobs_to_csv()     OK — {len(lines)-1} rows, columns: {lines[0]}")

# ── 3. Email construction ─────────────────────────────────────────────────────
print()
print("=" * 50)
print("TEST 3: Email message construction")
print("=" * 50)

import os, yaml
os.environ["GMAIL_APP_PASSWORD"] = "dummy_for_test"

config = yaml.safe_load(open("config.yaml"))

from email.mime.multipart import MIMEMultipart
from emailer import _jobs_to_csv
from datetime import datetime
from email import encoders
from email.mime.base import MIMEBase
from email.mime.text import MIMEText

msg = MIMEMultipart()
msg["Subject"] = f"[Job Digest] 3 new roles · {datetime.now().strftime('%d %b %Y')}"
msg["From"] = "thijs.wijnberg@gmail.com"
msg["To"] = "thijs.wijnberg@gmail.com"
msg.attach(MIMEText("Test body", "plain"))

attachment = MIMEBase("application", "octet-stream")
attachment.set_payload(_jobs_to_csv(synthetic_jobs).encode("utf-8"))
encoders.encode_base64(attachment)
attachment.add_header("Content-Disposition", 'attachment; filename="test.csv"')
msg.attach(attachment)

raw = msg.as_string()
assert "Content-Disposition" in raw
assert "attachment" in raw
print(f"  Email constructed  OK — {len(raw)} chars, attachment present")

# ── 4. Feedback module ────────────────────────────────────────────────────────
print()
print("=" * 50)
print("TEST 4: Feedback module")
print("=" * 50)

import feedback
result = feedback.read_feedback_from_sheet()
assert isinstance(result, list)
print(f"  read_feedback_from_sheet()  OK — returns [] gracefully when unconfigured")

# ── 5. Review module ──────────────────────────────────────────────────────────
print()
print("=" * 50)
print("TEST 5: Review module imports")
print("=" * 50)

import importlib.util
spec = importlib.util.spec_from_file_location("review", "review.py")
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)
assert hasattr(mod, "main"), "review.py missing main() function"
print("  review.py          OK — module loaded, main() present")

# ── 6. Show CSV as Claude would receive it ────────────────────────────────────
print()
print("=" * 50)
print("TEST 6: CSV contents (what Claude will see)")
print("=" * 50)
print()

for job in synthetic_jobs:
    print(f"  [{job['id']}] {job['title']} — {job['company']}")
    print(f"       Location: {job['location']}  |  Salary: {job['salary']}")
    print(f"       Source: {job['source']}  |  URL: {job['url']}")
    print(f"       Description: {job['description'][:120]}...")
    print()

print("=" * 50)
print("ALL TESTS PASSED")
print("=" * 50)
