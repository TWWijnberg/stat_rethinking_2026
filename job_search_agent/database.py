import sqlite3
import hashlib
from pathlib import Path
from datetime import datetime

DB_PATH = Path(__file__).parent / "jobs.db"


def init_db():
    conn = sqlite3.connect(DB_PATH)
    c = conn.cursor()
    c.execute("""
        CREATE TABLE IF NOT EXISTS jobs (
            id          TEXT PRIMARY KEY,
            title       TEXT,
            company     TEXT,
            location    TEXT,
            url         TEXT UNIQUE,
            description TEXT,
            salary      TEXT,
            source      TEXT,
            posted_date TEXT,
            seen_date   TEXT
        )
    """)
    conn.commit()
    conn.close()


def is_new_job(url: str) -> bool:
    conn = sqlite3.connect(DB_PATH)
    c = conn.cursor()
    c.execute("SELECT 1 FROM jobs WHERE url = ?", (url,))
    result = c.fetchone()
    conn.close()
    return result is None


def save_jobs(jobs: list):
    conn = sqlite3.connect(DB_PATH)
    c = conn.cursor()
    for job in jobs:
        c.execute("""
            INSERT OR IGNORE INTO jobs
              (id, title, company, location, url, description,
               salary, source, posted_date, seen_date)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """, (
            job["id"],
            job["title"],
            job.get("company", ""),
            job.get("location", ""),
            job["url"],
            job.get("description", "")[:2000],
            job.get("salary", ""),
            job.get("source", ""),
            job.get("posted_date", ""),
            datetime.now().isoformat(),
        ))
    conn.commit()
    conn.close()


def get_jobs_by_ids(job_ids: list) -> list:
    """Return full job records for a list of IDs — used by review.py."""
    if not job_ids:
        return []
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    c = conn.cursor()
    placeholders = ",".join("?" * len(job_ids))
    c.execute(f"SELECT * FROM jobs WHERE id IN ({placeholders})", job_ids)
    rows = [dict(r) for r in c.fetchall()]
    conn.close()
    return rows
