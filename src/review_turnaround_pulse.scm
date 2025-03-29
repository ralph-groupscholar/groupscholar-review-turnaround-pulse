#!/usr/bin/env gosh
;; Review Turnaround Pulse - Gauche Scheme CLI

(use srfi-1)
(use srfi-13)
(use srfi-19)
(use gauche.process)

(define (env-or key default)
  (or (sys-getenv key) default))

(define (required-env key)
  (let ((val (sys-getenv key)))
    (if (and val (not (string-null? val)))
        val
        (begin
          (display (string-append "Missing required env: " key "\n"))
          (exit 1)))))

(define (shell-quote s)
  (string-append "'" (string-join (string-split s #\') "'\\''") "'"))

(define (sql-quote v)
  (cond
   ((eq? v #f) "NULL")
   ((string? v)
    (let ((lower (string-downcase v)))
      (cond
       ((string=? lower "null") "NULL")
       ((string=? lower "now") "NOW()")
       ((string=? lower "now()") "NOW()")
       ((string=? lower "current_timestamp") "CURRENT_TIMESTAMP")
       (else
        (string-append "'" (string-join (string-split v #\') "''") "'")))))
   ((number? v) (number->string v))
   ((boolean? v) (if v "TRUE" "FALSE"))
   (else (sql-quote (x->string v)))))

(define (build-insert table columns)
  (let* ((cols (map car columns))
         (vals (map (lambda (p) (sql-quote (cdr p))) columns)))
    (string-append
     "INSERT INTO " table " ("
     (string-join cols ", ")
     ") VALUES ("
     (string-join vals ", ")
     ") RETURNING id;")))

(define (psql-base-args)
  (list "psql"
        "-h" (required-env "GS_DB_HOST")
        "-p" (required-env "GS_DB_PORT")
        "-U" (required-env "GS_DB_USER")
        "-d" (required-env "GS_DB_NAME")))

(define (psql-run sql #:quiet? (quiet? #f))
  (let* ((pwd (required-env "GS_DB_PASSWORD"))
         (args (append (psql-base-args)
                       (if quiet? (list "-tA" "-q") '())
                       (list "-c" sql)))
         (cmd (string-append "PGPASSWORD=" (shell-quote pwd) " "
                             (string-join (map shell-quote args) " "))))
    (if quiet?
        (let* ((p (open-input-pipe cmd))
               (out (port->string p)))
          (close-input-port p)
          (string-trim-both out))
        (begin
          (sys-system cmd)
          ""))))

(define (psql-run-file path)
  (let* ((pwd (required-env "GS_DB_PASSWORD"))
         (args (append (psql-base-args) (list "-f" path)))
         (cmd (string-append "PGPASSWORD=" (shell-quote pwd) " "
                             (string-join (map shell-quote args) " "))))
    (sys-system cmd)))

(define (parse-flags args)
  (let loop ((rest args) (acc '()))
    (if (null? rest)
        acc
        (let ((key (car rest)))
          (if (and (string-prefix? "--" key) (pair? (cdr rest)))
              (loop (cddr rest) (acons (substring key 2) (cadr rest) acc))
              (loop (cdr rest) acc))))))

(define (flag flags key default)
  (let ((entry (assoc key flags)))
    (if entry (cdr entry) default)))

(define (required-flag flags key)
  (let ((val (flag flags key #f)))
    (if (and val (not (string-null? val)))
        val
        (begin
          (display (string-append "Missing required flag: --" key "\n"))
          (exit 1)))))

(define (optional-filter flags key column)
  (let ((val (flag flags key #f)))
    (if (and val (not (string-null? val)))
        (string-append " AND " column "=" (sql-quote val))
        "")))

(define (print-usage)
  (display "Review Turnaround Pulse\n\n")
  (display "Commands:\n")
  (display "  init-db                Apply schema.sql\n")
  (display "  seed                   Apply seed.sql\n")
  (display "  add-request            Add a review request\n")
  (display "  close-request          Close a request by id\n")
  (display "  list-open              List open requests\n")
  (display "  summary                Summarize turnaround metrics\n")
  (display "  triage                 List overdue and due-soon reviews\n")
  (display "  queue                  Summarize open review risk buckets\n")
  (display "  sla                    Report SLA performance by reviewer\n\n")
  (display "Examples:\n")
  (display "  review-turnaround-pulse add-request --scholar S-100 --cohort Cohort-2026A --reviewer Alex --requested-at 2026-02-01 --due-at 2026-02-05\n")
  (display "  review-turnaround-pulse close-request --id 12 --completed-at 2026-02-04 --outcome approved\n")
  (display "  review-turnaround-pulse summary --since 2026-01-01\n")
  (display "  review-turnaround-pulse triage --as-of 2026-02-08 --window-hours 48 --reviewer \"Alex Morgan\"\n")
  (display "  review-turnaround-pulse queue --as-of 2026-02-08 --window-hours 48 --target-hours 72 --group-by reviewer\n")
  (display "  review-turnaround-pulse sla --since 2026-01-01 --target-hours 72\n"))

(define (cmd-init-db)
  (let ((path (string-append (sys-getcwd) "/sql/schema.sql")))
    (psql-run-file path)))

(define (cmd-seed)
  (let ((path (string-append (sys-getcwd) "/sql/seed.sql")))
    (psql-run-file path)))

(define (cmd-add-request flags)
  (let* ((scholar (required-flag flags "scholar"))
         (cohort (required-flag flags "cohort"))
         (reviewer (required-flag flags "reviewer"))
         (requested-at (flag flags "requested-at" "now"))
         (due-at (flag flags "due-at" #f))
         (notes (flag flags "notes" ""))
         (sql (build-insert
               "gs_review_turnaround_requests"
               (list (cons "scholar_id" scholar)
                     (cons "cohort" cohort)
                     (cons "reviewer" reviewer)
                     (cons "requested_at" requested-at)
                     (cons "due_at" due-at)
                     (cons "notes" notes))))
         (id (psql-run sql #:quiet? #t)))
    (display (string-append "Created request id: " id "\n"))))

(define (cmd-close-request flags)
  (let* ((req-id (required-flag flags "id"))
         (completed-at (flag flags "completed-at" "now"))
         (outcome (flag flags "outcome" "completed"))
         (sql (string-append
               "UPDATE gs_review_turnaround_requests SET completed_at="
               (sql-quote completed-at)
               ", outcome=" (sql-quote outcome)
               " WHERE id=" (sql-quote req-id) ";")))
    (psql-run sql)
    (display "Request closed.\n")))

(define (cmd-list-open)
  (psql-run
   (string-append
    "SELECT id, scholar_id, cohort, reviewer, requested_at, due_at\n"
    "FROM gs_review_turnaround_requests\n"
    "WHERE completed_at IS NULL\n"
    "ORDER BY requested_at ASC;")))

(define (cmd-summary flags)
  (let* ((since (flag flags "since" "1970-01-01"))
         (sql (string-append
               "SELECT reviewer,\n"
               "       count(*) AS total,\n"
               "       count(completed_at) AS completed,\n"
               "       round(avg(extract(epoch from (completed_at - requested_at)))/3600, 1) AS avg_hours\n"
               "FROM gs_review_turnaround_requests\n"
               "WHERE requested_at >= " (sql-quote since) "\n"
               "GROUP BY reviewer\n"
               "ORDER BY avg_hours NULLS LAST;")))
    (psql-run sql)))

(define (deadline-expr target-hours)
  (string-append
   "COALESCE(due_at, requested_at + (INTERVAL '1 hour' * "
   (sql-quote target-hours)
   "))"))

(define (build-sla-sql flags)
  (let* ((since (flag flags "since" "1970-01-01"))
         (target-hours (flag flags "target-hours" "72"))
         (deadline (deadline-expr target-hours)))
    (string-append
     "SELECT reviewer,\n"
     "       count(*) AS total,\n"
     "       count(completed_at) AS completed,\n"
     "       count(*) FILTER (WHERE completed_at IS NOT NULL AND completed_at <= "
     deadline ") AS completed_on_time,\n"
     "       count(*) FILTER (WHERE completed_at IS NOT NULL AND completed_at > "
     deadline ") AS completed_late,\n"
     "       count(*) FILTER (WHERE completed_at IS NULL AND NOW() > "
     deadline ") AS open_overdue,\n"
     "       round(avg(extract(epoch from (completed_at - requested_at)))/3600, 1) AS avg_hours\n"
     "FROM gs_review_turnaround_requests\n"
     "WHERE requested_at >= " (sql-quote since) "\n"
     "GROUP BY reviewer\n"
     "ORDER BY open_overdue DESC, completed_late DESC, avg_hours NULLS LAST;")))

(define (cmd-sla flags)
  (psql-run (build-sla-sql flags)))

(define (validate-group-by value)
  (cond
   ((string=? value "reviewer") value)
   ((string=? value "cohort") value)
   (else
    (display (string-append "Invalid --group-by value: " value "\n"))
    (exit 1))))

(define (build-queue-sql flags)
  (let* ((as-of (flag flags "as-of" "now"))
         (window (flag flags "window-hours" "48"))
         (target-hours (flag flags "target-hours" "72"))
         (group-by (validate-group-by (flag flags "group-by" "reviewer")))
         (deadline (deadline-expr target-hours))
         (filters (string-append
                   (optional-filter flags "cohort" "cohort")
                   (optional-filter flags "reviewer" "reviewer"))))
    (string-append
     "SELECT " group-by ",\n"
     "       count(*) AS open_total,\n"
     "       count(*) FILTER (WHERE " deadline " <= " (sql-quote as-of) ") AS overdue,\n"
     "       count(*) FILTER (WHERE " deadline " > " (sql-quote as-of)
     " AND " deadline " <= " (sql-quote as-of) " + interval '" window " hours') AS due_soon,\n"
     "       round(avg(extract(epoch from (" (sql-quote as-of) " - requested_at)))/3600, 1) AS avg_age_hours\n"
     "FROM gs_review_turnaround_requests\n"
     "WHERE completed_at IS NULL\n"
     filters "\n"
     "GROUP BY " group-by "\n"
     "ORDER BY overdue DESC, due_soon DESC, open_total DESC;")))

(define (cmd-queue flags)
  (psql-run (build-queue-sql flags)))

(define (build-triage-sql flags)
  (let* ((as-of (flag flags "as-of" "now"))
         (window (flag flags "window-hours" "48"))
         (base (string-append
                "SELECT id, scholar_id, cohort, reviewer, due_at,\n"
                "       CASE WHEN due_at < " (sql-quote as-of)
                " THEN 'OVERDUE' ELSE 'DUE_SOON' END AS risk,\n"
                "       CASE WHEN due_at < " (sql-quote as-of)
                " THEN 2 ELSE 1 END AS risk_rank\n"
                "FROM gs_review_turnaround_requests\n"
                "WHERE completed_at IS NULL\n"
                "  AND due_at IS NOT NULL\n"
                "  AND due_at <= " (sql-quote as-of)
                " + interval '" window " hours'"))
         (filters (string-append
                   (optional-filter flags "cohort" "cohort")
                   (optional-filter flags "reviewer" "reviewer")))
         (suffix "\nORDER BY risk_rank DESC, due_at ASC;"))
    (string-append base filters suffix)))

(define (cmd-triage flags)
  (psql-run (build-triage-sql flags)))

(define (main args)
  (if (null? args)
      (begin (print-usage) (exit 0))
      (let* ((cmd (car args))
             (flags (parse-flags (cdr args))))
        (cond
         ((string=? cmd "init-db") (cmd-init-db))
         ((string=? cmd "seed") (cmd-seed))
         ((string=? cmd "add-request") (cmd-add-request flags))
         ((string=? cmd "close-request") (cmd-close-request flags))
         ((string=? cmd "list-open") (cmd-list-open))
         ((string=? cmd "summary") (cmd-summary flags))
         ((string=? cmd "triage") (cmd-triage flags))
         ((string=? cmd "queue") (cmd-queue flags))
         ((string=? cmd "sla") (cmd-sla flags))
         (else (begin (print-usage) (exit 1)))))))

(define (maybe-run)
  (if (not (sys-getenv "RTP_DISABLE_MAIN"))
      (main (cdr (command-line)))))

(maybe-run)
