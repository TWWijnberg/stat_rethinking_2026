"""
Reads feedback from a Google Sheet (populated by your Google Form).
All functions here are optional — if credentials aren't set up, the agent
runs fine without them and just skips the feedback step.
"""
from collections import defaultdict
from pathlib import Path
import yaml

CONFIG_PATH = Path(__file__).parent / "config.yaml"


def _load_config() -> dict:
    with open(CONFIG_PATH) as f:
        return yaml.safe_load(f)


def read_feedback_from_sheet() -> list:
    """
    Returns a list of dicts: [{job_id, rating, notes}, ...]
    Returns [] silently if credentials aren't configured yet.
    """
    try:
        import gspread
        from google.oauth2.service_account import Credentials
    except ImportError:
        print("  Feedback skipped: run  pip install gspread google-auth  to enable")
        return []

    config = _load_config()
    fb = config.get("feedback", {})
    sheet_id = fb.get("google_sheet_id", "")
    sheet_name = fb.get("google_sheet_name", "Form Responses 1")
    creds_path = Path(__file__).parent / fb.get("credentials_file", "google_credentials.json")

    if not sheet_id:
        print("  Feedback skipped: google_sheet_id not set in config.yaml")
        return []
    if not creds_path.exists():
        print(f"  Feedback skipped: {creds_path.name} not found")
        return []

    try:
        scopes = [
            "https://spreadsheets.google.com/feeds",
            "https://www.googleapis.com/auth/drive",
        ]
        creds = Credentials.from_service_account_file(str(creds_path), scopes=scopes)
        client = gspread.authorize(creds)
        sheet = client.open_by_key(sheet_id).worksheet(sheet_name)
        rows = sheet.get_all_records()
        return [
            {
                "job_id": str(r.get("Job ID", "")).strip(),
                "rating": str(r.get("Rating", "")).strip(),
                "notes": str(r.get("Notes", "")).strip(),
            }
            for r in rows
            if r.get("Job ID") and r.get("Rating")
        ]
    except Exception as e:
        print(f"  Warning: could not read feedback sheet — {e}")
        return []


def print_feedback_summary(feedback: list):
    if not feedback:
        print("  No feedback found yet.")
        return

    good = sum(1 for f in feedback if f["rating"] == "Good fit")
    bad = sum(1 for f in feedback if f["rating"] == "Not a fit")
    print(f"  Feedback: {good} good fit, {bad} not a fit ({len(feedback)} total ratings)")

    bad_words = defaultdict(int)
    for item in feedback:
        if item["rating"] == "Not a fit" and item["notes"]:
            for word in item["notes"].lower().split():
                if len(word) > 4:
                    bad_words[word] += 1

    if bad_words:
        top = sorted(bad_words.items(), key=lambda x: -x[1])[:5]
        words = [w for w, _ in top]
        print(f"  Common words in 'not a fit' notes: {words}")
        print("  → Consider adding these as negative_keywords in config.yaml")
