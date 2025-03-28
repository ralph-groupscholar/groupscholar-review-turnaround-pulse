# GroupScholar Review Turnaround Pulse

Review Turnaround Pulse is a Gauche Scheme CLI that logs scholarship review requests and tracks turnaround time by reviewer and cohort. It is designed for Group Scholar operations to surface lagging reviews and keep turnaround targets on track.

## Features
- Log review requests with requested and due timestamps.
- Close requests and record outcomes.
- List open reviews and summarize turnaround averages by reviewer.
- Triage overdue and due-soon reviews with optional cohort/reviewer filters.
- Production Postgres schema + seed data included.

## Tech
- Gauche Scheme (CLI)
- PostgreSQL (production data store)

## Setup
1. Install Gauche Scheme: `brew install gauche`
2. Ensure `psql` is available in your PATH.
3. Set production database environment variables:
   - `GS_DB_HOST`
   - `GS_DB_PORT`
   - `GS_DB_USER`
   - `GS_DB_PASSWORD`
   - `GS_DB_NAME`

> Note: This CLI is intended to connect to the production database only. Do not use local dev credentials.

## Commands
```bash
bin/review-turnaround-pulse init-db
bin/review-turnaround-pulse seed

bin/review-turnaround-pulse add-request \
  --scholar S-100 \
  --cohort Cohort-2026A \
  --reviewer "Alex Morgan" \
  --requested-at 2026-02-01 \
  --due-at 2026-02-05 \
  --notes "Priority scholarship"

bin/review-turnaround-pulse close-request --id 12 --completed-at 2026-02-04 --outcome approved
bin/review-turnaround-pulse list-open
bin/review-turnaround-pulse summary --since 2026-01-01
bin/review-turnaround-pulse triage --as-of 2026-02-08 --window-hours 48 --reviewer "Alex Morgan"
```

## Tests
```bash
cd tests
RTP_DISABLE_MAIN=1 gosh run.scm
```

## Database
Schema and seed data live in `sql/schema.sql` and `sql/seed.sql`. The CLI applies them with `init-db` and `seed` commands.
