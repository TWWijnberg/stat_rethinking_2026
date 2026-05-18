#!/usr/bin/env python3
"""
Joins your feedback (from Google Sheet) with job details (from the local
database) and exports a combined CSV ready to paste into Claude.

    python review.py

The output file (feedback_report.csv) is used with review_scoring_feedback.md
to update your scoring criteria.
"""
import csv
import sys
import yaml
from pathlib import Path
from datetime import datetime
from dotenv import load_dotenv

load_dotenv(Path(__file__).parent / ".env")

from database import get_jobs_by_ids
from feedback import read_feedback_from_sheet

CONFIG_PATH = Path(__file__).parent / "config.yaml"
OUTPUT_PATH = Path(__file__).parent / "feedback_report.csv"


def load_config() -> dict:
    with open(CONFIG_PATH) as f:
        return yaml.safe_load(f)


def main():
    print("=" * 40)
    print("  Feedback Review")
    print("=" * 40)

    config = load_config()

    # ── Read feedback from Google Sheet ───────
    print("\n[1/3] Reading feedback from Google Sheet...")
    feedback = read_feedback_from_sheet()

    if not feedback:
        print("\nNo feedback found yet.")
        print("Rate some jobs first using the 👍 / 👎 links in your digest email.")
        sys.exit(0)

    good = sum(1 for f in feedback if f["rating"] == "Good fit")
    bad  = sum(1 for f in feedback if f["rating"] == "Not a fit")
    print(f"  Found {len(feedback)} ratings: {good} good fit, {bad} not a fit")

    if len(feedback) < 5:
        print("\n  Tip: you only have a few ratings so far.")
        print("  The review will be more useful with 10+ ratings.")
        print("  Continuing anyway...\n")

    # ── Join to job details ───────────────────
    print("[2/3] Joining to job database...")
    job_ids = [f["job_id"] for f in feedback]
    jobs_by_id = {j["id"]: j for j in get_jobs_by_ids(job_ids)}

    matched = 0
    rows = []
    for fb in feedback:
        job = jobs_by_id.get(fb["job_id"])
        if not job:
            continue
        matched += 1
        rows.append({
            "rating":      fb["rating"],
            "notes":       fb["notes"],
            "title":       job["title"],
            "company":     job["company"],
            "location":    job["location"],
            "salary":      job["salary"],
            "source":      job["source"],
            "url":         job["url"],
            "description": job["description"],
        })

    unmatched = len(feedback) - matched
    print(f"  Matched {matched} of {len(feedback)} feedback entries to job records")
    if unmatched:
        print(f"  ({unmatched} feedback entries had no matching job — database may have been reset)")

    if not rows:
        print("\nNo matched rows to export. Exiting.")
        sys.exit(1)

    # ── Export CSV ────────────────────────────
    print(f"[3/3] Exporting {OUTPUT_PATH.name}...")
    fields = ["rating", "notes", "title", "company", "location",
              "salary", "source", "url", "description"]

    with open(OUTPUT_PATH, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)

    print(f"\nDone. ✓  →  {OUTPUT_PATH}")
    print("\nNext steps:")
    print("  1. Open Claude (claude.ai)")
    print("  2. Paste the contents of review_scoring_feedback.md into the chat")
    print("  3. Replace [PASTE CONTENTS OF scoring_prompt.md HERE] with your current scoring_prompt.md")
    print("  4. Paste the contents of feedback_report.csv beneath it")
    print("  5. Copy Claude's updated scoring_prompt.md back into the file")


if __name__ == "__main__":
    main()
