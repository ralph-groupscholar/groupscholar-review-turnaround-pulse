# Ralph Progress Log

## Iteration 113 - 2026-02-08
- Added SLA reporting to summarize on-time, late, and overdue review performance by reviewer.
- Expanded CLI usage docs and added unit tests for SLA SQL generation.

## Iteration 115 - 2026-02-08
- Initialized the Review Turnaround Pulse project in Gauche Scheme.
- Added CLI commands for logging requests, closing requests, and summarizing turnaround.
- Added production Postgres schema + seed data and test coverage for SQL helpers.

## Iteration 116 - 2026-02-08
- Added required-flag validation for core commands.
- Added triage command to surface overdue and due-soon reviews with filters.
- Expanded tests and README to cover triage behavior.

## Iteration 119 - 2026-02-08
- Added queue command to summarize open review risk buckets by reviewer or cohort.
- Added group-by validation and SQL builder for queue reporting.
- Expanded tests and README with queue usage.
