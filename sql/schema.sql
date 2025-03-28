CREATE TABLE IF NOT EXISTS gs_review_turnaround_requests (
  id SERIAL PRIMARY KEY,
  scholar_id TEXT NOT NULL,
  cohort TEXT NOT NULL,
  reviewer TEXT NOT NULL,
  requested_at TIMESTAMP NOT NULL,
  due_at TIMESTAMP,
  completed_at TIMESTAMP,
  outcome TEXT,
  notes TEXT NOT NULL DEFAULT '',
  created_at TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS gs_review_turnaround_requests_open_idx
  ON gs_review_turnaround_requests (completed_at, requested_at);

CREATE INDEX IF NOT EXISTS gs_review_turnaround_requests_reviewer_idx
  ON gs_review_turnaround_requests (reviewer, requested_at);
