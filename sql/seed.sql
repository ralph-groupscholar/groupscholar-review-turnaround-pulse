INSERT INTO gs_review_turnaround_requests
  (scholar_id, cohort, reviewer, requested_at, due_at, completed_at, outcome, notes)
VALUES
  ('S-101', 'Cohort-2026A', 'Alex Morgan', '2026-01-10 09:00', '2026-01-15 17:00', '2026-01-14 14:30', 'approved', 'Needs quick turnaround for internship deadline.'),
  ('S-102', 'Cohort-2026A', 'Alex Morgan', '2026-01-12 10:15', '2026-01-18 17:00', NULL, NULL, 'Waiting on transcript verification.'),
  ('S-103', 'Cohort-2026B', 'Jamie Lee', '2026-01-20 13:00', '2026-01-25 12:00', '2026-01-22 09:45', 'approved', 'Strong academic record.'),
  ('S-104', 'Cohort-2026B', 'Jamie Lee', '2026-01-22 09:30', '2026-01-28 17:00', '2026-01-29 11:10', 'approved', 'Needed additional recommendation.'),
  ('S-105', 'Cohort-2026C', 'Priya Singh', '2026-02-02 08:20', '2026-02-07 17:00', NULL, NULL, 'Awaiting FAFSA updates.'),
  ('S-106', 'Cohort-2026C', 'Priya Singh', '2026-02-03 14:00', '2026-02-10 17:00', '2026-02-08 10:05', 'declined', 'Eligibility mismatch.'),
  ('S-107', 'Cohort-2026A', 'Alex Morgan', '2026-02-05 11:45', '2026-02-11 17:00', NULL, NULL, 'Priority for scholarship committee review.'),
  ('S-108', 'Cohort-2026B', 'Jamie Lee', '2026-02-06 15:10', '2026-02-12 17:00', NULL, NULL, 'Partner request.'),
  ('S-109', 'Cohort-2026C', 'Priya Singh', '2026-02-07 09:05', '2026-02-13 17:00', NULL, NULL, 'New applicant.'),
  ('S-110', 'Cohort-2026A', 'Alex Morgan', '2026-02-07 16:40', '2026-02-14 17:00', NULL, NULL, 'Late submission review.');
