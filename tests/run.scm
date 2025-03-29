(use gauche.test)

(sys-putenv "RTP_DISABLE_MAIN" "1")
(load "../src/review_turnaround_pulse.scm")

(test-start "review-turnaround-pulse")

(test* "sql-quote string" "'Hello'" (sql-quote "Hello"))
(test* "sql-quote apostrophe" "'O''Reilly'" (sql-quote "O'Reilly"))
(test* "sql-quote null string" "NULL" (sql-quote "null"))
(test* "sql-quote now" "NOW()" (sql-quote "now"))
(test* "sql-quote number" "42" (sql-quote 42))
(test* "sql-quote boolean" "TRUE" (sql-quote #t))

(let ((sql (build-insert "table" (list (cons "a" "x") (cons "b" 2)))))
  (test* "insert has table" #t (not (eq? #f (string-contains sql "INSERT INTO table"))))
  (test* "insert has columns" #t (not (eq? #f (string-contains sql "(a, b)"))))
  (test* "insert has values" #t (not (eq? #f (string-contains sql "('x', 2)")))))

(let* ((sql (build-triage-sql '()))
       (sql-filtered (build-triage-sql (list (cons "cohort" "Cohort-2026A")
                                             (cons "reviewer" "Alex Morgan")))))
  (test* "triage has default window" #t (not (eq? #f (string-contains sql "interval '48 hours'"))))
  (test* "triage marks overdue" #t (not (eq? #f (string-contains sql "OVERDUE"))))
  (test* "triage filters cohort" #t (not (eq? #f (string-contains sql-filtered "cohort='Cohort-2026A'"))))
  (test* "triage filters reviewer" #t (not (eq? #f (string-contains sql-filtered "reviewer='Alex Morgan'")))))

(let ((sql (build-list-open-sql (list (cons "as-of" "2026-02-08")
                                      (cons "cohort" "Cohort-2026A")))))
  (test* "list-open includes age" #t (not (eq? #f (string-contains sql "age_hours"))))
  (test* "list-open filters cohort" #t (not (eq? #f (string-contains sql "cohort='Cohort-2026A'")))))

(test* "deadline-expr uses target hours"
       "COALESCE(due_at, requested_at + (INTERVAL '1 hour' * 72))"
       (deadline-expr 72))

(let ((sql (build-sla-sql (list (cons "since" "2026-01-01") (cons "target-hours" "48")))))
  (test* "sla includes reviewer" #t (not (eq? #f (string-contains sql "SELECT reviewer"))))
  (test* "sla includes on time" #t (not (eq? #f (string-contains sql "completed_on_time"))))
  (test* "sla includes overdue" #t (not (eq? #f (string-contains sql "open_overdue")))))

(let ((sql (build-queue-sql (list (cons "as-of" "2026-02-08")
                                  (cons "window-hours" "24")
                                  (cons "target-hours" "36")
                                  (cons "group-by" "cohort")
                                  (cons "reviewer" "Alex Morgan")))))
  (test* "queue includes group by cohort" #t (not (eq? #f (string-contains sql "SELECT cohort"))))
  (test* "queue includes overdue" #t (not (eq? #f (string-contains sql "AS overdue"))))
  (test* "queue includes due soon" #t (not (eq? #f (string-contains sql "AS due_soon"))))
  (test* "queue filters reviewer" #t (not (eq? #f (string-contains sql "reviewer='Alex Morgan'"))))
  (test* "queue uses window" #t (not (eq? #f (string-contains sql "interval '24 hours'")))))

(test-end)
