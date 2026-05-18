import smtplib
import os
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from datetime import datetime
from urllib.parse import quote


def _feedback_buttons(job: dict, config: dict) -> str:
    fb = config.get("feedback", {})
    form_url = fb.get("google_form_url", "")
    id_field = fb.get("job_id_field", "")
    rating_field = fb.get("rating_field", "")

    if not form_url or not id_field or id_field.startswith("entry.X"):
        return ""

    up = f"{form_url}&{id_field}={quote(job['id'])}&{rating_field}={quote('Good fit')}"
    down = f"{form_url}&{id_field}={quote(job['id'])}&{rating_field}={quote('Not a fit')}"

    btn = "display:inline-block;padding:5px 14px;border-radius:5px;text-decoration:none;font-size:13px;font-weight:600;"
    return (
        f'<a href="{up}" style="{btn}background:#16a34a;color:white;">👍 Good fit</a>'
        f'&nbsp;&nbsp;'
        f'<a href="{down}" style="{btn}background:#dc2626;color:white;">👎 Not a fit</a>'
    )


def _job_row(job: dict, rank: int, config: dict) -> str:
    salary = job.get("salary") or '<span style="color:#9ca3af;">Not listed</span>'
    reasons = " &middot; ".join(job.get("reasons", [])) or "General keyword match"
    location = job.get("location") or "Not specified"
    company = job.get("company") or ""
    buttons = _feedback_buttons(job, config)

    return f"""
    <tr>
      <td style="padding:18px 12px;border-bottom:1px solid #e5e7eb;vertical-align:top;width:40px;
                 font-size:13px;color:#6b7280;text-align:center;">
        <div style="font-weight:700;">#{rank}</div>
        <div style="color:#f59e0b;font-weight:700;font-size:14px;">▲{job['score']}</div>
      </td>
      <td style="padding:18px 12px;border-bottom:1px solid #e5e7eb;vertical-align:top;">
        <a href="{job['url']}"
           style="font-size:17px;font-weight:700;color:#1d4ed8;text-decoration:none;">
          {job['title']}
        </a>
        <div style="margin-top:3px;color:#374151;font-size:14px;">{company}</div>
        <div style="margin-top:4px;color:#6b7280;font-size:13px;">
          📍 {location} &nbsp;&nbsp; 💰 {salary} &nbsp;&nbsp; 🔗 {job.get('source','')}
        </div>
        <div style="margin-top:10px;background:#eff6ff;border-left:3px solid #3b82f6;
                    padding:7px 12px;border-radius:0 4px 4px 0;font-size:13px;color:#1e40af;">
          <strong>Why this matches:</strong> {reasons}
        </div>
        {"<div style='margin-top:12px;'>" + buttons + "</div>" if buttons else ""}
      </td>
    </tr>"""


def build_html(jobs: list, config: dict) -> str:
    today = datetime.now().strftime("%A %d %B %Y")
    rows = "".join(_job_row(j, i + 1, config) for i, j in enumerate(jobs))
    return f"""<!DOCTYPE html>
<html>
<body style="margin:0;padding:0;font-family:Arial,Helvetica,sans-serif;background:#f3f4f6;">
<table style="max-width:680px;margin:20px auto;background:white;border-radius:8px;
              overflow:hidden;box-shadow:0 1px 3px rgba(0,0,0,0.1);" cellpadding="0" cellspacing="0">
  <tr>
    <td colspan="2" style="background:#1e40af;color:white;padding:20px 24px;">
      <div style="font-size:20px;font-weight:700;">Job Digest</div>
      <div style="font-size:14px;opacity:0.75;margin-top:4px;">
        {today} &middot; {len(jobs)} new role{"s" if len(jobs) != 1 else ""} found
      </div>
    </td>
  </tr>
  {rows}
  <tr>
    <td colspan="2" style="background:#f9fafb;padding:12px 16px;
                           font-size:12px;color:#9ca3af;text-align:center;">
      Tip: click a role title to view the full listing &middot;
      Rate each role to improve future digests
    </td>
  </tr>
</table>
</body>
</html>"""


def send_digest(jobs: list, config: dict):
    email_cfg = config["email"]
    sender = email_cfg["sender"]
    recipient = email_cfg["recipient"]
    password = os.environ.get("GMAIL_APP_PASSWORD", "")

    if not password:
        raise EnvironmentError(
            "GMAIL_APP_PASSWORD is not set. "
            "Add it to your .env file — see setup_guide.md Step 3."
        )

    subject = (
        f"{email_cfg['subject_prefix']} "
        f"{len(jobs)} new role{'s' if len(jobs) != 1 else ''} · "
        f"{datetime.now().strftime('%d %b %Y')}"
    )

    msg = MIMEMultipart("alternative")
    msg["Subject"] = subject
    msg["From"] = sender
    msg["To"] = recipient
    msg.attach(MIMEText(build_html(jobs, config), "html"))

    with smtplib.SMTP_SSL("smtp.gmail.com", 465) as server:
        server.login(sender, password)
        server.sendmail(sender, recipient, msg.as_string())

    print(f"  Email sent to {recipient} ({len(jobs)} jobs).")
