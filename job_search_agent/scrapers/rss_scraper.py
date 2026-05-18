"""
RSS scrapers for Indeed UK and NHS Jobs.
These are the most reliable sources — RSS feeds are stable and don't require
any login or scraping of JavaScript-rendered pages.
"""
import re
import hashlib
from urllib.parse import quote_plus

import feedparser


_INDEED_SEARCHES = [
    ("head of analytics", "United Kingdom"),
    ("head of insight", "United Kingdom"),
    ("director of data", "United Kingdom"),
    ("head of strategy", "United Kingdom"),
    ("deputy director analytics", "United Kingdom"),
    ("senior manager analytics health", "United Kingdom"),
    ("head of data health", "United Kingdom"),
    ("head of product analytics", "United Kingdom"),
]

_NHS_KEYWORDS = ["analytics", "insight", "strategy", "data", "intelligence"]

_SALARY_PATTERNS = [
    r"£[\d,]+\s*[-–]\s*£[\d,]+",
    r"£[\d,]+k?\s*[-–]\s*£?[\d,]+k?",
    r"[\d,]+\s*[-–]\s*[\d,]+\s*per annum",
    r"salary[^\n]{0,40}£[\d,]+",
    r"Band \d+[a-d]?",
]


def _job_id(url: str) -> str:
    return hashlib.md5(url.encode()).hexdigest()[:12]


def _extract_salary(text: str) -> str:
    for pattern in _SALARY_PATTERNS:
        m = re.search(pattern, text, re.IGNORECASE)
        if m:
            return m.group(0).strip()
    return ""


def _dedupe(jobs: list) -> list:
    seen = set()
    out = []
    for j in jobs:
        if j["url"] not in seen:
            seen.add(j["url"])
            out.append(j)
    return out


def scrape_indeed() -> list:
    jobs = []
    for query, location in _INDEED_SEARCHES:
        url = (
            f"https://www.indeed.co.uk/rss"
            f"?q={quote_plus(query)}&l={quote_plus(location)}&sort=date"
        )
        try:
            feed = feedparser.parse(url)
            for entry in feed.entries:
                job_url = entry.get("link", "")
                if not job_url:
                    continue
                raw_title = entry.get("title", "")
                # Indeed title format: "Job Title - Company Name"
                parts = raw_title.rsplit(" - ", 1)
                title = parts[0].strip()
                company = parts[1].strip() if len(parts) == 2 else ""
                description = entry.get("summary", "")
                jobs.append({
                    "id": _job_id(job_url),
                    "title": title,
                    "company": company,
                    "location": entry.get("location", ""),
                    "url": job_url,
                    "description": description,
                    "salary": _extract_salary(description),
                    "source": "Indeed",
                    "posted_date": entry.get("published", ""),
                })
        except Exception as e:
            print(f"    Warning: Indeed search '{query}' failed — {e}")
    return _dedupe(jobs)


def scrape_nhs_jobs() -> list:
    jobs = []
    for keyword in _NHS_KEYWORDS:
        url = (
            f"https://www.jobs.nhs.uk/xi/search_vacancy/rss/"
            f"?action=search&Keyword={quote_plus(keyword)}"
            f"&sortBy=publicationDateDesc"
        )
        try:
            feed = feedparser.parse(url)
            for entry in feed.entries:
                job_url = entry.get("link", "")
                if not job_url:
                    continue
                description = entry.get("summary", "") or entry.get("description", "")
                jobs.append({
                    "id": _job_id(job_url),
                    "title": entry.get("title", "").strip(),
                    "company": entry.get("author", "NHS"),
                    "location": "",
                    "url": job_url,
                    "description": description,
                    "salary": _extract_salary(description),
                    "source": "NHS Jobs",
                    "posted_date": entry.get("published", ""),
                })
        except Exception as e:
            print(f"    Warning: NHS Jobs search '{keyword}' failed — {e}")
    return _dedupe(jobs)
