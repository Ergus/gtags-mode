;;; coverage.el --- Report LCOV coverage to GitHub -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage, from the repository root, after a test run with undercover:
;;   emacs -Q --batch -l .github/coverage.el
;;
;; Appends a Markdown table of coverage.info to GITHUB_STEP_SUMMARY.
;; undercover writes coverage.info when its Emacs exits, so this runs in
;; a separate Emacs.

;;; Code:

(defun coverage--table (file)
  "Return the Markdown report of the LCOV FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((table "## Coverage\n\n| File | Lines | Coverage | Missed lines |\n|---|---|---|---|\n"))
      (while (re-search-forward "^SF:\\(.*\\)" nil t)
	(let ((source (file-name-nondirectory (match-string 1)))
	      (end (save-excursion (re-search-forward "^end_of_record" nil t)))
	      (total 0) (missed nil))
	  (while (re-search-forward "^DA:\\([0-9]+\\),\\([0-9]+\\)" end t)
	    (setq total (1+ total))
	    (when (string= (match-string 2) "0")
	      (push (match-string 1) missed)))
	  (setq table (concat table (format "| %s | %d/%d | %.1f%% | %s |\n"
					    source (- total (length missed)) total
					    (/ (* 100.0 (- total (length missed))) (max total 1))
					    (string-join (nreverse missed) ", "))))))
      table)))

(write-region (coverage--table "coverage.info") nil
	      (or (getenv "GITHUB_STEP_SUMMARY") (error "GITHUB_STEP_SUMMARY is not set"))
	      t 'silent)

;;; coverage.el ends here
