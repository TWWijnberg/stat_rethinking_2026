"""
Wellfound (formerly AngelList Talent) scraper.

Note: Wellfound renders its job listings with JavaScript, so a standard HTTP
request only gets the page shell, not the job cards. This scraper does a
best-effort parse and will typically return 0–5 results rather than a full
listing. The RSS sources (Indeed, NHS Jobs) are the primary reliable sources.

If you want richer Wellfound coverage, see setup_guide.md for the manual
workaround (copy-paste job URLs into a watchlist file).
"""
import hashlib
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
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
}

_SEARCH_URLS = [
    "https://wellfound.com/jobs?role=data-analyst&remote=true",
    "https://wellfound.com/jobs?role=head-of-data&remote=true",
    "https://wellfound.com/jobs?role=strategy&remote=true",
]


def _job_id(url: str) -> str:
    return hashlib.md5(url.encode()).hexdigest()[:12]


def scrape_wellfound() -> list:
    jobs = []
    session = requests.Session()
    session.headers.update(_HEADERS)

    for url in _SEARCH_URLS:
        try:
            resp = session.get(url, timeout=15)
            soup = BeautifulSoup(resp.text, "lxml")

            # Wellfound job cards typically sit inside <div> elements with
            # "job" in the class name — this may break if their HTML changes.
            cards = soup.find_all(
                "div",
                class_=lambda c: c and any("job" in cls.lower() for cls in c),
            )

            for card in cards[:20]:
                link = card.find("a", href=True)
                if not link:
                    continue
                href = link["href"]
                job_url = (
                    f"https://wellfound.com{href}"
                    if href.startswith("/")
                    else href
                )
                title_el = card.find(["h2", "h3"]) or link
                title = title_el.get_text(strip=True)
                if not title or len(title) < 5:
                    continue

                company_el = card.find(
                    "span", class_=lambda c: c and "company" in c.lower()
                )
                company = company_el.get_text(strip=True) if company_el else ""

                jobs.append({
                    "id": _job_id(job_url),
                    "title": title,
                    "company": company,
                    "location": "Remote",
                    "url": job_url,
                    "description": card.get_text(separator=" ", strip=True)[:500],
                    "salary": "",
                    "source": "Wellfound",
                    "posted_date": "",
                })
            time.sleep(2)
        except Exception as e:
            print(f"    Warning: Wellfound scrape failed — {e}")

    seen = set()
    return [j for j in jobs if j["url"] not in seen and not seen.add(j["url"])]
