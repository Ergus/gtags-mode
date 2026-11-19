;;; report.el --- Run the tests and report them to GitHub -*- lexical-binding: t; -*-

;;; Commentary:

;; Usage, from the repository root:
;;   emacs -Q --batch -L . -l .github/report.el
;;
;; Runs gtags-mode-tests.el.  When GITHUB_STEP_SUMMARY is set, appends
;; a Markdown table of the tests to it.  Prints a GitHub ::error
;; annotation per failed test.  Exits with status 1 on unexpected
;; results.

;;; Code:

(require 'ert)
(load "gtags-mode-tests" nil t)

(defun report--result-name (result)
  "Return the name of the ERT test RESULT."
  (cond ((ert-test-passed-p result) "passed")
	((ert-test-skipped-p result) "skipped")
	((ert-test-failed-p result) "failed")
	(t "aborted")))

(defun report--tests ()
  "Return the Markdown report of the tests; print failure annotations."
  (let ((rows "") (details "") (counts nil))
    (dolist (test (ert-select-tests t t))
      (let* ((result (ert-test-most-recent-result test))
	     (name (report--result-name result))
	     (ok (ert-test-result-expected-p test result)))
	(push name counts)
	(setq rows (concat rows (format "| %s `%s` | %s | %.3f s |\n"
					(if ok "✅" "❌") (ert-test-name test) name
					(ert-test-result-duration result))))
	(unless ok
	  (let ((condition (if (ert-test-result-with-condition-p result)
			       (format "%S" (ert-test-result-with-condition-condition result))
			     "Unexpected result")))
	    (setq details (concat details (format "<details><summary><code>%s</code></summary>\n\n```\n%s\n```\n</details>\n\n"
						  (ert-test-name test) condition)))
	    ;; Workflow command values encode %, CR and LF.
	    (princ (format "::error file=%s,title=%s::%s\n"
			   (if-let* ((file (ert-test-file-name test)))
			       (file-relative-name file)
			     "gtags-mode-tests.el")
			   (ert-test-name test)
			   (replace-regexp-in-string
			    "[%\r\n]" (lambda (c) (format "%%%02X" (string-to-char c)))
			    condition)))))))
    (concat (format "## Tests (Emacs %s)\n\n" emacs-version)
	    (mapconcat (lambda (name) (format "%s %d" name (seq-count (apply-partially #'equal name) counts)))
		       (seq-uniq counts) ", ")
	    "\n\n| Test | Result | Time |\n|---|---|---|\n" rows "\n" details)))

(let ((stats (ert-run-tests-batch))
      (summary (getenv "GITHUB_STEP_SUMMARY")))
  (let ((report (report--tests)))
    (when summary
      (write-region report nil summary t 'silent)))
  (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))

;;; report.el ends here
