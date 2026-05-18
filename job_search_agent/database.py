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
            score       REAL,
            reasons     TEXT,
            seen_date   TEXT
        )
    """)
    c.execute("""
        CREATE TABLE IF NOT EXISTS feedback (
            id        INTEGER PRIMARY KEY AUTOINCREMENT,
            job_id    TEXT,
            rating    TEXT,
            notes     TEXT,
            timestamp TEXT
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
               salary, source, posted_date, score, reasons, seen_date)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
            job.get("score", 0),
            " · ".join(job.get("reasons", [])),
            datetime.now().isoformat(),
        ))
    conn.commit()
    conn.close()
