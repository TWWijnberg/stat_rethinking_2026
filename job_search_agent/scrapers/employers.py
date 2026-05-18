"""
Direct employer career page scrapers.

Each config entry defines a company's careers URL and hints for finding
job listing links on that page. These scrapers use simple HTML parsing —
they work fine for pages that render job listings in plain HTML, but may
return few or zero results for heavily JavaScript-rendered sites (Oracle
Health in particular). See setup_guide.md for how to verify and update
these if they stop working.
"""
import hashlib
import re
import time

import requests
from bs4 import BeautifulSoup

_HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/120.0.0.0 Safari/537.36"
    ),
    "Accept-Language": "en-GB,en;q=0.9",
}

# Each entry: name, url to scrape, regex pattern that job-posting links match,
# and the base URL to prepend to relative links.
_EMPLOYERS = [
    {
        "name": "SystemC",
        "url": "https://www.systemc.com/about-us/careers/",
        "link_pattern": r"/(job|career|vacancy|role)s?/",
        "base_url": "https://www.systemc.com",
    },
    {
        "name": "Epic",
        "url": "https://careers.epic.com/",
        "link_pattern": r"/jobs?/|/careers?/",
        "base_url": "https://careers.epic.com",
    },
    {
        # Cerner was acquired by Oracle in 2022. Roles appear on Oracle's careers site.
        # The URL below filters to health-related roles; adjust the keyword if needed.
        "name": "Oracle Health (Cerner)",
        "url": "https://careers.oracle.com/jobs/#en/sites/jobsearch/jobs?keyword=health+analytics",
        "link_pattern": r"/jobs/|/en/sites/jobsearch/job/",
        "base_url": "https://careers.oracle.com",
    },
    {
        # Nervecenter — verify their current careers URL; update below if it changes.
        "name": "Nervecenter",
        "url": "https://www.nervecenterhealth.com/careers",
        "link_pattern": r"/(job|career|vacancy|position)s?/",
        "base_url": "https://www.nervecenterhealth.com",
    },
]


def _job_id(url: str) -> str:
    return hashlib.md5(url.encode()).hexdigest()[:12]


def _scrape_one(employer: dict) -> list:
    jobs = []
    try:
        resp = requests.get(employer["url"], headers=_HEADERS, timeout=15)
        soup = BeautifulSoup(resp.text, "lxml")

        pattern = re.compile(employer["link_pattern"], re.IGNORECASE)
        seen_hrefs: set = set()

        for link in soup.find_all("a", href=True):
            href = link["href"]
            if not pattern.search(href):
                continue
            if href in seen_hrefs:
                continue
            seen_hrefs.add(href)

            full_url = (
                href if href.startswith("http") else employer["base_url"] + href
            )

            # Try to find the job title: prefer nearby heading, fall back to link text
            title = link.get_text(strip=True)
            if not title or len(title) < 5:
                parent = link.find_parent(["li", "article", "div"])
                if parent:
                    heading = parent.find(["h1", "h2", "h3", "h4"])
                    if heading:
                        title = heading.get_text(strip=True)
            if not title or len(title) < 5:
                title = link.get("title", "")
            if not title:
                continue

            jobs.append({
                "id": _job_id(full_url),
                "title": title[:200],
                "company": employer["name"],
                "location": "",
                "url": full_url,
                "description": "",
                "salary": "",
                "source": employer["name"],
                "posted_date": "",
            })
    except Exception as e:
        print(f"    Warning: {employer['name']} scrape failed — {e}")
    return jobs


def scrape_all_employers() -> list:
    all_jobs = []
    for employer in _EMPLOYERS:
        print(f"    {employer['name']}...")
        jobs = _scrape_one(employer)
        print(f"      → {len(jobs)} listing(s) found")
        all_jobs.extend(jobs)
        time.sleep(2)
    return all_jobs
