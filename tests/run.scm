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

(test-end)
