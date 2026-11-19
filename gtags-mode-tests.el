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

;; Coverage, when undercover is installed.  It must instrument
;; gtags-mode.el before it is loaded.  It is active only on CI or with
;; UNDERCOVER_FORCE set.
(when (require 'undercover nil t)
  (undercover "gtags-mode.el"
	      (:report-format 'lcov)
	      (:report-file "coverage.info")
	      (:send-report nil)))

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

(defun gtags-mode-tests--sync-calls (fn)
  "Call FN; return the argument lists of its `gtags-mode--exec-sync' calls."
  (let* ((calls nil)
	 (spy (lambda (&rest args) (push args calls))))
    (advice-add 'gtags-mode--exec-sync :before spy)
    (unwind-protect (funcall fn)
      (advice-remove 'gtags-mode--exec-sync spy))
    (nreverse calls)))

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

;; Regressions from the history ====================================

(gtags-mode-tests-deftest gtags-mode-tests-spaces-in-path
  "Paths with spaces and code with \" NAME NUMBER \" parse (333b0ac, b68128a)."
  (gtags-mode-tests--write (concat dir "proj/src/my dir/e f.c")
			   "int eps(int a) { return a + 1 ; }\n")
  (gtags-mode-tests--gtags (concat dir "proj/"))
  (with-current-buffer (find-file-noselect (concat root "src/a.c"))
    (let ((loc (xref-item-location
		(car (xref-backend-definitions (xref-find-backend) "eps")))))
      (should (equal (xref-location-group loc) (concat root "src/my dir/e f.c")))
      (should (= (xref-location-line loc) 1)))))

(ert-deftest gtags-mode-tests-gtagslibpath ()
  "Definitions and completions include GTAGSLIBPATH (87245a0, 86a846e)."
  (gtags-mode-tests--with-project
    (gtags-mode-tests--gtags (concat dir "other/"))
    (let ((process-environment (cons (concat "GTAGSLIBPATH=" dir "other")
				     process-environment)))
      (with-current-buffer (find-file-noselect (concat root "src/a.c"))
	(let ((loc (xref-item-location
		    (car (xref-backend-definitions (xref-find-backend) "delta")))))
	  (should (equal (expand-file-name (xref-location-group loc))
			 (concat other "d.c"))))
	(should (equal (gtags-mode--list-completions "del") '("delta")))))))

(gtags-mode-tests-deftest gtags-mode-tests-undefined-references
  "References include symbols without definition (5c09390)."
  (gtags-mode-tests--write (concat dir "proj/src/u.c")
			   "int use(void) { return undefined_fn(); }\n")
  (gtags-mode-tests--gtags (concat dir "proj/"))
  (with-current-buffer (find-file-noselect (concat root "src/a.c"))
    (should (equal (gtags-mode-tests--lines
		    (xref-backend-references (xref-find-backend) "undefined_fn"))
		   '(1)))))

(ert-deftest gtags-mode-tests-dash-prefix ()
  "Prefixes made of dashes are not options (816093f)."
  (gtags-mode-tests--with-project
    (with-current-buffer (find-file-noselect (concat root "src/a.c"))
      (should-not (gtags-mode--list-completions "-"))
      (should-not (gtags-mode--list-completions "----")))))

(gtags-mode-tests-deftest gtags-mode-tests-save-outside-project
  "Saving outside a project queries global once and starts nothing (d5c597d, 17c7c7c)."
  (with-current-buffer (find-file-noselect (concat other "d.c"))
    (let ((calls (gtags-mode-tests--sync-calls
		  (lambda ()
		    (dotimes (_ 2)
		      (insert " ")
		      (let ((inhibit-message t)) (save-buffer))
		      (should-not gtags-mode--pr-async))))))
      (should (equal calls '(("--print-dbpath")))))))

(gtags-mode-tests-deftest gtags-mode-tests-non-file-buffer
  "Non-file buffers do not query global, but reuse known projects (a4346a0, 38c4812)."
  (with-temp-buffer
    (setq default-directory root)
    (should-not (gtags-mode-tests--sync-calls
		 (lambda () (should-not (gtags-mode--local-plist))))))
  (with-current-buffer (find-file-noselect (concat root "src/a.c"))
    (gtags-mode--local-plist))
  (with-temp-buffer
    (setq default-directory (concat root "src/"))
    (should-not (gtags-mode-tests--sync-calls
		 (lambda () (should (equal (project-root (gtags-mode--local-plist)) root)))))))

(gtags-mode-tests-deftest gtags-mode-tests-symlink
  "Files visited through a symlinked directory use the true name (8c6dafd).
The known project is found without querying global."
  (make-symbolic-link (concat dir "proj/src") (concat dir "link"))
  (with-current-buffer (find-file-noselect (concat root "src/sub/b.c"))
    (gtags-mode--local-plist))
  (with-current-buffer (find-file-noselect (concat prefix dir "link/a.c"))
    (should-not (gtags-mode-tests--sync-calls
		 (lambda ()
		   (should (equal (project-root (gtags-mode--local-plist)) root)))))
    (should (equal gtags-mode--true-file-name (concat root "src/a.c")))
    (should (equal (mapcar #'car (funcall imenu-create-index-function))
		   '("alpha_one" "alpha_two")))))

(ert-deftest gtags-mode-tests-imenu-fallback ()
  "Outside a project the major mode builds the index (076ae4a)."
  (gtags-mode-tests--with-project
    (with-current-buffer (find-file-noselect (concat other "d.c"))
      (should (assoc "delta" (funcall imenu-create-index-function))))))

(ert-deftest gtags-mode-tests-single-process ()
  "Only one async process runs at a time (c4ed450)."
  (gtags-mode-tests--with-project
    (let ((default-directory root))
      (should (gtags-mode--exec-async 'gtags-mode--global "--update"))
      (should-not (gtags-mode--exec-async 'gtags-mode--global "--update"))
      (gtags-mode-tests--wait)
      (should (gtags-mode--exec-async 'gtags-mode--global "--update"))
      (gtags-mode-tests--wait))))

(gtags-mode-tests-deftest gtags-mode-tests-update
  "`gtags-mode-update' registers buffers whose database appeared later (41e9847)."
  (let ((buffer (find-file-noselect (concat other "d.c"))))
    (with-current-buffer buffer
      (should-not (gtags-mode--local-plist)))
    (gtags-mode-tests--gtags (concat dir "other/"))
    (with-current-buffer (find-file-noselect (concat root "src/a.c"))
      (gtags-mode-update)
      (gtags-mode-tests--wait))
    (with-current-buffer buffer
      (should (equal (project-root gtags-mode--plist) other)))))

(gtags-mode-tests-deftest gtags-mode-tests-project-buffers
  "`project-buffers' lists only the project's buffers (3c68518)."
  (let ((a (find-file-noselect (concat root "src/a.c")))
	(b (find-file-noselect (concat root "src/sub/b.c"))))
    (find-file-noselect (concat other "d.c"))
    ;; One buffer with the plist already set, one still unset.
    (with-current-buffer a (gtags-mode--local-plist))
    (should (equal (sort (project-buffers (gtags-mode--project-find root))
			 :key #'buffer-name)
		   (sort (list a b) :key #'buffer-name)))))

(ert-deftest gtags-mode-tests-features ()
  "`gtags-mode-features' selects the hooks; disabling removes them all."
  (gtags-mode-tests--with-project
    (let ((hooks '(project-find-functions xref-backend-functions
		   completion-at-point-functions after-save-hook
		   after-change-major-mode-hook)))
      (let ((gtags-mode-features '(project)))
	(gtags-mode -1)
	(gtags-mode 1)
	(should (memq #'gtags-mode--project-find project-find-functions))
	(should-not (memq #'gtags-mode--local-plist xref-backend-functions))
	(should-not (memq #'gtags-mode--imenu-setup after-change-major-mode-hook)))
      (gtags-mode -1)
      (dolist (hook hooks)
	(should-not (seq-some (lambda (f) (and (symbolp f)
					       (string-prefix-p "gtags-mode-" (symbol-name f))))
			      (default-value hook)))))))

;; Remaining paths ================================================

(ert-deftest gtags-mode-tests-messages ()
  "Messages up to `gtags-mode-verbose-level' go to *Messages*."
  (let ((gtags-mode-verbose-level 2)
	(inhibit-message t))
    (gtags-mode--message 2 "level %s" 2)
    (gtags-mode--message 3 "level %s" 3)
    (with-current-buffer (messages-buffer)
      (goto-char (point-max))
      (should (search-backward "gtags-mode: level 2" nil t))
      (should-not (search-forward "gtags-mode: level 3" nil t)))))

(ert-deftest gtags-mode-tests-missing-executable ()
  "Processes do not start without executable."
  (gtags-mode-tests--with-project
    (with-temp-buffer
      ;; No local binding here: `let' binds the default value, which
      ;; the temporary buffers of the exec functions see.
      (let ((gtags-mode--gtags nil)
	    (gtags-mode--global nil)
	    (default-directory root))
	(should-not (gtags-mode--exec-async 'gtags-mode--gtags))
	(should-not gtags-mode--pr-async)
	(should-not (gtags-mode--exec-sync "--version"))))))

(ert-deftest gtags-mode-tests-connection-local-global ()
  "A connection-local `gtags-mode-global-executable' is used as is."
  (skip-unless (gtags-mode-tests--tramp-p))
  ;; A separate method: the profile of "mock" sets the gtags executable
  ;; and is applied once per host.
  (add-to-list 'tramp-methods (cons "mock2" (cdr (assoc "mock" tramp-methods))))
  (let ((global (executable-find "global")))
    (connection-local-set-profile-variables
     'gtags-mode-tests--mock2 `((gtags-mode-global-executable . ,global)))
    (connection-local-set-profiles
     '(:application tramp :protocol "mock2") 'gtags-mode-tests--mock2)
    (with-temp-buffer
      (setq default-directory "/mock2::/")
      (gtags-mode--set-connection-locals)
      (should (equal gtags-mode--global global))
      ;; Not connection-local: resolved by name on the remote PATH.
      (should (equal (file-name-nondirectory gtags-mode--gtags) "gtags")))))

(ert-deftest gtags-mode-tests-negative-cache-cleared ()
  "Creating a plist removes the cached misses under its root."
  (gtags-mode-tests--with-project
    (should-not (gtags-mode--project-find other))
    (gtags-mode-tests--gtags (concat dir "other/"))
    ;; Still cached: the database was created outside Emacs.
    (should-not (gtags-mode--project-find other))
    (with-current-buffer (find-file-noselect (concat other "d.c"))
      (should (gtags-mode--local-plist)))
    (should-not (gethash other gtags-mode--no-root-dirs))
    (should (equal (project-root (gtags-mode--project-find other)) other))))

(ert-deftest gtags-mode-tests-no-project-errors ()
  "Functions that need a project signal outside one."
  (gtags-mode-tests--with-project
    (with-temp-buffer
      (setq default-directory other)
      (should-error (gtags-mode--list-completions "x"))
      (should-error (gtags-mode--filter-find-symbol '("--definition") "x" #'list)))))

(ert-deftest gtags-mode-tests-identifier-completion-table ()
  "The xref completion table lists all symbols."
  (gtags-mode-tests--with-project
    (with-current-buffer (find-file-noselect (concat root "src/a.c"))
      (should (equal (sort (xref-backend-identifier-completion-table (xref-find-backend)))
		     '("Beta" "alpha_one" "alpha_two"))))))

(ert-deftest gtags-mode-tests-imenu-goto ()
  "Selecting an imenu entry moves to its line."
  (gtags-mode-tests--with-project
    (with-current-buffer (find-file-noselect (concat root "src/a.c"))
      (imenu (assoc "alpha_two" (funcall imenu-create-index-function)))
      (should (= (line-number-at-pos) 2)))))

(gtags-mode-tests-deftest gtags-mode-tests-project-files-dirs
  "`project-files' with several DIRS removes duplicates."
  (should (equal (sort (project-files (gtags-mode--project-find root)
				      (list (concat root "src/") (concat root "src/sub/"))))
		 (list (concat root "src/a.c") (concat root "src/sub/b.c")))))

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
