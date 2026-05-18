#!/usr/bin/env python3
"""
Job Search Agent — main entry point.
Run this script daily to scrape, score, and email your job digest.

    python main.py

See setup_guide.md for first-time setup instructions.
"""
import sys
from pathlib import Path
from dotenv import load_dotenv

load_dotenv(Path(__file__).parent / ".env")

from database import init_db, is_new_job, save_jobs
from scorer import load_config, score_and_rank
from emailer import send_digest
from feedback import read_feedback_from_sheet, print_feedback_summary
from scrapers.rss_scraper import scrape_indeed, scrape_nhs_jobs
from scrapers.wellfound import scrape_wellfound
from scrapers.employers import scrape_all_employers


def main():
    print("=" * 40)
    print("  Job Search Agent")
    print("=" * 40)

    init_db()
    config = load_config()

    # ── Scrape ────────────────────────────────
    print("\n[1/4] Scraping sources...")

    print("  Indeed UK (RSS)...")
    indeed = scrape_indeed()
    print(f"    → {len(indeed)} jobs found")

    print("  NHS Jobs (RSS)...")
    nhs = scrape_nhs_jobs()
    print(f"    → {len(nhs)} jobs found")

    print("  Wellfound...")
    wellfound = scrape_wellfound()
    print(f"    → {len(wellfound)} jobs found")

    print("  Employer career pages...")
    employers = scrape_all_employers()
    print(f"    → {len(employers)} jobs found")

    all_jobs = indeed + nhs + wellfound + employers
    print(f"\n  Total scraped: {len(all_jobs)}")

    # ── Filter already-seen jobs ──────────────
    new_jobs = [j for j in all_jobs if is_new_job(j["url"])]
    print(f"  New (not seen before): {len(new_jobs)}")

    if not new_jobs:
        print("\nNo new jobs today. Nothing to send.")
        return

    # ── Score and rank ────────────────────────
    print("\n[2/4] Scoring jobs...")
    ranked = score_and_rank(new_jobs, config)
    print(f"  {len(ranked)} job(s) above minimum score threshold")

    # Save all new jobs (including those below threshold, for dedup purposes)
    save_jobs(new_jobs)

    # ── Feedback summary ──────────────────────
    print("\n[3/4] Reading feedback...")
    feedback = read_feedback_from_sheet()
    print_feedback_summary(feedback)

    # ── Send digest ───────────────────────────
    print("\n[4/4] Sending email digest...")
    if ranked:
        send_digest(ranked, config)
    else:
        print("  No jobs above score threshold today — no email sent.")
        print("  Tip: lower minimum_score in config.yaml if you're seeing too few results.")

    print("\nDone. ✓")


if __name__ == "__main__":
    main()
