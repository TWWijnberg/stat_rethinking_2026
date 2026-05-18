#!/usr/bin/env python3
"""
Job Search Agent — main entry point.

Scrapes job sources, removes duplicates, exports a CSV, and emails it to you.
Scoring is done by pasting the CSV into Claude using scoring_prompt.md.

    python main.py

See setup_guide.md for first-time setup instructions.
"""
from pathlib import Path
from dotenv import load_dotenv

load_dotenv(Path(__file__).parent / ".env")

from database import init_db, is_new_job, save_jobs
from emailer import send_digest
from scrapers.rss_scraper import scrape_indeed, scrape_nhs_jobs
from scrapers.employers import scrape_all_employers
import yaml


def load_config() -> dict:
    with open(Path(__file__).parent / "config.yaml") as f:
        return yaml.safe_load(f)


def main():
    print("=" * 40)
    print("  Job Search Agent")
    print("=" * 40)

    init_db()
    config = load_config()

    # ── Scrape ────────────────────────────────
    print("\n[1/3] Scraping sources...")

    print("  Indeed UK (RSS)...")
    indeed = scrape_indeed()
    print(f"    → {len(indeed)} jobs")

    print("  NHS Jobs (RSS)...")
    nhs = scrape_nhs_jobs()
    print(f"    → {len(nhs)} jobs")

    print("  Employer career pages...")
    employers = scrape_all_employers()
    print(f"    → {len(employers)} jobs")

    # Future: parse Wellfound alert emails from Gmail inbox

    all_jobs = indeed + nhs + employers
    print(f"\n  Total scraped: {len(all_jobs)}")

    # ── Deduplicate ───────────────────────────
    new_jobs = [j for j in all_jobs if is_new_job(j["url"])]
    print(f"  New (not seen before): {len(new_jobs)}")

    if not new_jobs:
        print("\nNo new jobs today. Nothing to send.")
        return

    save_jobs(new_jobs)

    # ── Email CSV ─────────────────────────────
    print("\n[2/3] Sending email...")
    send_digest(new_jobs, config)

    print("\n[3/3] Done. ✓")
    print(f"\nNext step: open the attached CSV in Claude using scoring_prompt.md")


if __name__ == "__main__":
    main()
