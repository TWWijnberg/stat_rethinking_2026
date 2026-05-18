import yaml
from pathlib import Path

CONFIG_PATH = Path(__file__).parent / "config.yaml"


def load_config() -> dict:
    with open(CONFIG_PATH) as f:
        return yaml.safe_load(f)


def score_job(job: dict, config: dict) -> tuple:
    """Returns (score, reasons) for a single job."""
    scoring = config["scoring"]
    score = 0.0
    reasons = []

    full_text = f"{job.get('title', '')} {job.get('description', '')}".lower()
    title_lower = job.get("title", "").lower()

    # Seniority (title only — keeps signal clean)
    for keyword, points in scoring["seniority_keywords"].items():
        if keyword.lower() in title_lower:
            score += points
            reasons.append(f"'{keyword}' in title")
            break  # Only credit the best seniority match once

    # Domain keywords (title + description)
    matched_domains = []
    for keyword, points in scoring["domain_keywords"].items():
        if keyword.lower() in full_text:
            score += points
            matched_domains.append(keyword.strip())
    if matched_domains:
        reasons.append("domain: " + ", ".join(dict.fromkeys(matched_domains))[:3 * 20])

    # Reporting line (description)
    for keyword, points in scoring["reporting_keywords"].items():
        if keyword.lower() in full_text:
            score += points
            reasons.append(f"reports to {keyword.upper()}")
            break

    # Sector keywords
    matched_sectors = []
    for keyword, points in scoring["sector_keywords"].items():
        if keyword.lower() in full_text:
            score += points
            matched_sectors.append(keyword.strip())
    if matched_sectors:
        reasons.append("sector: " + ", ".join(dict.fromkeys(matched_sectors[:3])))

    # Location bonus/penalty
    location_text = (job.get("location", "") + " " + full_text).lower()
    loc_config = scoring["location"]
    location_matched = False
    for preferred in loc_config["preferred"]:
        if preferred.lower() in location_text:
            score += loc_config["preferred_bonus"]
            reasons.append(f"location: {preferred}")
            location_matched = True
            break
    if not location_matched:
        for neutral in loc_config["neutral"]:
            if neutral.lower() in location_text:
                score += loc_config["neutral_penalty"]
                break

    # Negative signals
    for keyword, points in scoring["negative_keywords"].items():
        if keyword.lower() in full_text:
            score += points  # points are already negative

    return round(score, 1), reasons


def score_and_rank(jobs: list, config: dict) -> list:
    """Score all jobs, attach score + reasons, return filtered and sorted list."""
    for job in jobs:
        score, reasons = score_job(job, config)
        job["score"] = score
        job["reasons"] = reasons

    min_score = config["scoring"]["minimum_score"]
    top_n = config["scoring"]["top_n"]

    above_threshold = [j for j in jobs if j["score"] >= min_score]
    above_threshold.sort(key=lambda x: x["score"], reverse=True)
    return above_threshold[:top_n]
