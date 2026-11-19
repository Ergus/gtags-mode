;;; gtags-mode-tests.el --- Tests for gtags-mode. -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l gtags-mode-tests.el -f ert-run-tests-batch-and-exit
;;
;; Requires GNU Global.  Tests defined with `gtags-mode-tests-deftest'
;; run twice: on a local project and through TRAMP.  The remote variant
;; uses a "mock" method that logs into a local sh, so it needs no server.

;;; Code:

(require 'ert)
(require 'tramp)
(require 'gtags-mode)

(add-to-list 'tramp-methods
	     '("mock"
	       (tramp-login-program "sh")
	       (tramp-login-args (("-i")))
	       (tramp-remote-shell "/bin/sh")
	       (tramp-remote-shell-args ("-c"))
	       (tramp-connection-timeout 10)))
(setq tramp-verbose 0
      tramp-persistency-file-name nil)

(defconst gtags-mode-tests--bin (make-temp-file "gtags-mode-bin-" t)
  "Directory of the gtags wrapper used on the mock host.")
(add-hook 'kill-emacs-hook (lambda () (delete-directory gtags-mode-tests--bin t)))

;; The mock host shares the local executables.  The wrapper leaves a
;; marker so tests can tell which executable ran.
(let ((wrapper (expand-file-name "gtags-remote" gtags-mode-tests--bin)))
  (write-region "#!/bin/sh\ntouch .remote-gtags\nexec gtags \"$@\"\n" nil wrapper nil 'silent)
  (set-file-modes wrapper #o755)
  (connection-local-set-profile-variables
   'gtags-mode-tests--mock `((gtags-mode-gtags-executable . ,wrapper)))
  (connection-local-set-profiles
   '(:application tramp :protocol "mock") 'gtags-mode-tests--mock))

(defvar gtags-mode-tests-remote nil
  "Non-nil when the current test runs through TRAMP.")

(defconst gtags-mode-tests--prefix
  ;; TRAMP fills in the default host: "/mock::" becomes "/mock:HOST:".
  (file-remote-p (expand-file-name "/mock::/"))
  "Remote prefix of the mock method.")

(defun gtags-mode-tests--write (file content)
  "Write CONTENT to FILE, creating its directory."
  (make-directory (file-name-directory file) t)
  (write-region content nil file nil 'silent))

(defun gtags-mode-tests--gtags (dir)
  "Create a Global database in local DIR."
  (let ((default-directory dir))
    (should (eq 0 (call-process "gtags" nil nil nil)))))

(defun gtags-mode-tests--wait ()
  "Wait for the async gtags or global process, at most 10 seconds."
  (with-timeout (10 (error "Async process timeout"))
    (while gtags-mode--pr-async
      (accept-process-output nil 0.05))))

(defun gtags-mode-tests--lines (xrefs)
  "Return the sorted line numbers of XREFS."
  (sort (mapcar (lambda (x) (xref-location-line (xref-item-location x))) xrefs)))

(defmacro gtags-mode-tests--with-project (&rest body)
  "Run BODY with a fresh project; bind `root', `other' and `dir'.
`dir' is the local temporary directory.  `root' is the indexed
project, `other' a directory without database; both are remote file
names when `gtags-mode-tests-remote' is non-nil."
  (declare (indent 0) (debug t))
  `(let* ((dir (file-name-as-directory (file-truename (make-temp-file "gtags-mode-" t))))
	  (prefix (if gtags-mode-tests-remote gtags-mode-tests--prefix ""))
	  (root (concat prefix dir "proj/"))
	  (other (concat prefix dir "other/"))
	  (gtags-mode--alist nil)
	  (gtags-mode--no-root-dirs (make-hash-table :test #'equal))
	  (gtags-mode-verbose-level 0))
     (ignore root other)
     (unwind-protect
	 (progn
	   (gtags-mode-tests--write (concat dir "proj/src/a.c")
				    "int alpha_one(void) { return 1; }\nint alpha_two(void) { return alpha_one(); }\n")
	   (gtags-mode-tests--write (concat dir "proj/src/sub/b.c")
				    "int Beta(void) { return alpha_two(); }\n")
	   (gtags-mode-tests--write (concat dir "other/d.c")
				    "int delta(void) { return 0; }\n")
	   (gtags-mode-tests--gtags (concat dir "proj/"))
	   (gtags-mode 1)
	   ,@body)
       (gtags-mode -1)
       (dolist (buff (buffer-list))
	 (when (and (buffer-file-name buff) (string-search dir (buffer-file-name buff)))
	   (with-current-buffer buff (set-buffer-modified-p nil))
	   (kill-buffer buff)))
       (delete-directory dir t))))

(defun gtags-mode-tests--tramp-p ()
  "Return non-nil when the mock TRAMP method works."
  (ignore-errors (file-directory-p "/mock::/")))

(defmacro gtags-mode-tests-deftest (name doc &rest body)
  "Define tests NAME/local and NAME/remote with DOC running BODY in a project."
  (declare (indent 1) (doc-string 2))
  `(progn
     (ert-deftest ,(intern (format "%s/local" name)) ()
       ,doc
       (let ((gtags-mode-tests-remote nil))
	 (gtags-mode-tests--with-project ,@body)))
     (ert-deftest ,(intern (format "%s/remote" name)) ()
       ,doc
       (skip-unless (gtags-mode-tests--tramp-p))
       (let ((gtags-mode-tests-remote t))
	 (gtags-mode-tests--with-project ,@body)))))

;; project ===========================================================

(gtags-mode-tests-deftest gtags-mode-tests-project
  "File buffers of a project share its plist but not the true file name."
  (with-current-buffer (find-file-noselect (concat root "src/sub/b.c"))
    (let ((project (project-current)))
      (should (equal (project-root project) root))
      (should (eq project gtags-mode--plist))
      (should (equal gtags-mode--true-file-name (concat root "src/sub/b.c")))
      (with-current-buffer (find-file-noselect (concat root "src/a.c"))
	(should (eq (project-current) project))
	(should (equal gtags-mode--true-file-name (concat root "src/a.c")))))))

(gtags-mode-tests-deftest gtags-mode-tests-project-find-dir
  "`gtags-mode--project-find' uses DIR, not the buffer, and caches misses."
  (with-temp-buffer
    (should (equal (project-root (gtags-mode--project-find (concat root "src/"))) root))
    (should-not (gtags-mode--project-find other))
    (should (gethash other gtags-mode--no-root-dirs))
    (should-not (local-variable-p 'gtags-mode--plist))))

(gtags-mode-tests-deftest gtags-mode-tests-nested-roots
  "The innermost root wins, regardless of creation order."
  (gtags-mode-tests--gtags (concat dir "proj/src/sub/"))
  (let* ((inner (gtags-mode--project-find (concat root "src/sub/")))
	 (outer (gtags-mode--project-find root)))
    (should (equal (project-root inner) (concat root "src/sub/")))
    (should (equal (project-root outer) root))
    (should (eq (gtags-mode--project-find (concat root "src/sub/")) inner))))

(gtags-mode-tests-deftest gtags-mode-tests-project-files
  "`project-files' returns absolute or relative names."
  (let ((project (gtags-mode--project-find root)))
    (should (equal (sort (project-files project))
		   (list (concat root "src/a.c") (concat root "src/sub/b.c"))))
    (let ((project-files-relative-names t))
      (should (equal (sort (project-files project)) '("src/a.c" "src/sub/b.c"))))))

;; xref ==============================================================

(gtags-mode-tests-deftest gtags-mode-tests-xref
  "Definitions, references and apropos."
  (with-current-buffer (find-file-noselect (concat root "src/a.c"))
    (let ((backend (xref-find-backend)))
      (should (eq backend gtags-mode--plist))
      (let ((defs (xref-backend-definitions backend "alpha_one")))
	(should (equal (gtags-mode-tests--lines defs) '(1)))
	(should (equal (xref-location-group (xref-item-location (car defs)))
		       (concat root "src/a.c"))))
      (should (equal (gtags-mode-tests--lines (xref-backend-references backend "alpha_one")) '(2)))
      ;; Symbols starting with "-" are not options.
      (should-not (xref-backend-definitions backend "-x"))
      (should (equal (gtags-mode-tests--lines (xref-backend-apropos backend "alpha")) '(1 2)))
      (should (equal (gtags-mode-tests--lines (xref-backend-apropos backend "alpha two")) '(2)))
      (should (equal (gtags-mode-tests--lines (xref-backend-apropos backend "^alpha_o")) '(1)))
      (let ((case-fold-search t))
	(should (equal (gtags-mode-tests--lines (xref-backend-apropos backend "ALPHA")) '(1 2))))
      (let ((case-fold-search nil))
	(should-not (xref-backend-apropos backend "ALPHA"))))))

;; completion ========================================================

(gtags-mode-tests-deftest gtags-mode-tests-completions
  "Completions honor `completion-ignore-case' and are cached per project."
  (with-current-buffer (find-file-noselect (concat root "src/a.c"))
    (should (equal (sort (gtags-mode--list-completions nil)) '("Beta" "alpha_one" "alpha_two")))
    (should (plist-get gtags-mode--plist :cache))
    (let ((completion-ignore-case nil))
      (should (equal (sort (gtags-mode--list-completions "alpha")) '("alpha_one" "alpha_two")))
      (should-not (gtags-mode--list-completions "ALPHA")))
    (let ((completion-ignore-case t))
      (should (equal (sort (gtags-mode--list-completions "ALPHA")) '("alpha_one" "alpha_two"))))
    ;; The cache lives in the shared plist, not in the buffer.
    (with-current-buffer (find-file-noselect (concat root "src/sub/b.c"))
      (gtags-mode--local-plist)
      (should (gethash '("alpha" . nil) (plist-get gtags-mode--plist :prefix-cache))))))

(ert-deftest gtags-mode-tests-capf ()
  "The capf completes the symbol at point."
  (gtags-mode-tests--with-project
    (with-current-buffer (find-file-noselect (concat root "src/a.c"))
      (goto-char (point-max))
      (insert "alpha_")
      (let ((capf (gtags-mode-completion-function)))
	(should (equal (all-completions "alpha_" (nth 2 capf)) '("alpha_one" "alpha_two")))))))

;; imenu =============================================================

(gtags-mode-tests-deftest gtags-mode-tests-imenu
  "The index comes from Global, also for files in subdirectories."
  (with-current-buffer (find-file-noselect (concat root "src/sub/b.c"))
    (should (equal (mapcar (lambda (e) (list (car e) (cadr e)))
			   (funcall imenu-create-index-function))
		   '(("Beta" 1))))
    (gtags-mode -1)
    (should-not (advice-function-member-p #'gtags-mode--imenu-advice
					  imenu-create-index-function))))

;; async =============================================================

(gtags-mode-tests-deftest gtags-mode-tests-after-save
  "Saving a file updates the database and clears the caches."
  (with-current-buffer (find-file-noselect (concat root "src/a.c"))
    (gtags-mode--list-completions nil)
    (goto-char (point-max))
    (insert "int gamma(void) { return 0; }\n")
    (let ((inhibit-message t)) (save-buffer))
    (gtags-mode-tests--wait)
    (should-not (plist-get gtags-mode--plist :cache))
    (should (equal (gtags-mode--list-completions "gam") '("gamma")))))

(gtags-mode-tests-deftest gtags-mode-tests-create
  "`gtags-mode-create' from an unrelated buffer registers open buffers.
For a remote root, the local buffer must not resolve the executable."
  (with-current-buffer (find-file-noselect (concat other "d.c"))
    (should-not (gtags-mode--local-plist))
    (should (local-variable-p 'gtags-mode--plist))
    (with-temp-buffer
      (setq default-directory temporary-file-directory)
      (gtags-mode-create other)
      (gtags-mode-tests--wait))
    (should (file-exists-p (concat other "GTAGS")))
    (should (eq gtags-mode-tests-remote (file-exists-p (concat other ".remote-gtags"))))
    (should (equal (project-root gtags-mode--plist) other))))

(ert-deftest gtags-mode-tests-update-args ()
  "`gtags-mode-update-args' is split with shell syntax."
  (gtags-mode-tests--with-project
    (with-temp-buffer
      (setq gtags-mode-update-args "--skip-unreadable '--skip-symlink=a'")
      (gtags-mode-create other)
      (should (equal (cdr (process-get gtags-mode--pr-async :command))
		     '("--skip-unreadable" "--skip-symlink=a")))
      (gtags-mode-tests--wait))
    (should (file-exists-p (concat other "GTAGS")))))

(ert-deftest gtags-mode-tests-async-failure ()
  "A failed async process kills its buffer and releases the lock."
  (gtags-mode-tests--with-project
    (let ((count (length (buffer-list)))
	  (default-directory root))
      (should (gtags-mode--exec-async 'gtags-mode--global "--no-such-option"))
      (gtags-mode-tests--wait)
      (should-not gtags-mode--pr-async)
      (should (= count (length (buffer-list)))))))

;; mode line =========================================================

(ert-deftest gtags-mode-tests-lighter ()
  "The lighter shows only in project buffers."
  (gtags-mode-tests--with-project
    (let ((form (cadr (assq 'gtags-mode minor-mode-alist))))
      (with-current-buffer (find-file-noselect (concat root "src/a.c"))
	(gtags-mode--local-plist)
	(should (equal (eval (cadr form) t) gtags-mode-lighter)))
      (with-current-buffer (find-file-noselect (concat other "d.c"))
	(gtags-mode--local-plist)
	(should-not (eval (cadr form) t))))))

(provide 'gtags-mode-tests)
;;; gtags-mode-tests.el ends here
