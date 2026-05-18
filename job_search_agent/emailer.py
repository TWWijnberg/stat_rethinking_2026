import csv
import io
import os
import smtplib
from datetime import datetime
from email import encoders
from email.mime.base import MIMEBase
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText


def _jobs_to_csv(jobs: list) -> str:
    fields = ["id", "title", "company", "location", "salary",
              "source", "posted_date", "url", "description"]
    output = io.StringIO()
    writer = csv.DictWriter(output, fieldnames=fields, extrasaction="ignore")
    writer.writeheader()
    writer.writerows(jobs)
    return output.getvalue()


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

    today = datetime.now().strftime("%d %b %Y")
    filename = f"new_jobs_{datetime.now().strftime('%Y-%m-%d')}.csv"
    subject = f"{email_cfg['subject_prefix']} {len(jobs)} new role{'s' if len(jobs) != 1 else ''} · {today}"

    body = f"""\
{len(jobs)} new role{'s were' if len(jobs) != 1 else ' was'} found today ({today}).

The attached CSV contains the full listings. To score them:
1. Open scoring_prompt.md
2. Paste its contents into Claude, followed by the CSV data
3. Claude will return a ranked list with reasoning

Sources scraped: Indeed UK, NHS Jobs, SystemC, Epic, Oracle Health, Nervecenter
"""

    msg = MIMEMultipart()
    msg["Subject"] = subject
    msg["From"] = sender
    msg["To"] = recipient
    msg.attach(MIMEText(body, "plain"))

    csv_data = _jobs_to_csv(jobs)
    attachment = MIMEBase("application", "octet-stream")
    attachment.set_payload(csv_data.encode("utf-8"))
    encoders.encode_base64(attachment)
    attachment.add_header("Content-Disposition", f'attachment; filename="{filename}"')
    msg.attach(attachment)

    with smtplib.SMTP_SSL("smtp.gmail.com", 465) as server:
        server.login(sender, password)
        server.sendmail(sender, recipient, msg.as_string())

    print(f"  Email sent to {recipient} with {len(jobs)} jobs ({filename}).")
