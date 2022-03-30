;;; gtags-mode.el --- GNU Global integration with xref, project and imenu. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; URL: https://github.com/Ergus/gtags-mode
;; Keywords: xref, project, imenu, gtags, global
;; Version: 1.0 alpha
;; Package-Requires: ((emacs "28"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; GNU Global integration with xref, project and imenu.

;;; Code:

(require 'xref)
(require 'cl-generic)
(require 'files-x)

(defgroup gtags-mode nil
  "GNU Global group for xref."
  :group 'xref)

(defcustom gtags-mode-global-executable "global"
  "GNU Global executable."
  :type 'string
  :local t)

(defcustom gtags-mode-gtags-executable "gtags"
  "GNU Gtags executable."
  :type 'string
  :local t)

(defcustom gtags-mode-lighter "Gtags"
  "Gtags executable."
  :type 'string
  :risky t)

(defvar gtags-mode--alist nil
  "Full list of Global roots.
The address is absolute for remote hosts.")
(put 'gtags-mode--alist 'risky-local-variable t)

(defvar-local gtags-mode--global (executable-find gtags-mode-global-executable))
(defvar-local gtags-mode--gtags (executable-find gtags-mode-gtags-executable))
(defvar-local gtags-mode--root nil
  "Project Global root for this buffer.
The address is relative on remote hosts and includes the remote prefix.")

(defconst gtags-mode--output-format-regex
  "^\\([^[:blank:]]+\\)[[:blank:]]+\\([[:digit:]]+\\)[[:blank:]]+\\([^[:blank:]]+\\)[[:blank:]]+\\(.*\\)"
  "Regex to filter the output with `gtags-mode--output-format-options'.")

(defconst gtags-mode--output-format-options
  '("--result=ctags-x" "--path-style=absolute")
  "Command line options to use with `gtags-mode--output-format-regex'.")

;; Connection functions
(defun gtags-mode--set-connection-locals ()
  "Set GLOBAL connection local variables when possible and needed."
  (when-let* ((remote (file-remote-p default-directory))
	      (criteria (connection-local-criteria-for-default-directory))
	      ((not (and (local-variable-p 'gtags-mode--global)
			 (local-variable-p 'gtags-mode--gtags))))
	      (symvars (intern (concat "gtags-mode--" remote "-vars")))
	      (enable-connection-local-variables t))
    (unless (alist-get symvars connection-local-profile-alist)
      (with-connection-local-variables
       (let ((global (if (local-variable-p 'gtags-mode-global-executable)
			 gtags-mode-global-executable
		       (file-name-nondirectory gtags-mode-global-executable)))
	     (gtags (if (local-variable-p 'gtags-mode-gtags-executable)
			gtags-mode-gtags-executable
		      (file-name-nondirectory gtags-mode-gtags-executable))))
	 (connection-local-set-profile-variables
	  symvars
	  `((gtags-mode--global . ,(executable-find global t))
	    (gtags-mode--gtags . ,(executable-find gtags t))))
	 (connection-local-set-profiles criteria symvars))))
    (hack-connection-local-variables-apply criteria)))

;; Async functions
(defun gtags-mode--exec-async-sentinel (process event)
  "Sentinel to run when PROCESS emits EVENT.
This is the sentinel set in `gtags-mode--exec-async'."
  (let ((temp-buffer (process-buffer process))
	(parent-buffer (process-get process :buffer)))
    (if (and (eq (process-status process) 'exit)   ;; if success
	     (eq (process-exit-status process) 0))
	(and (buffer-name temp-buffer)             ;; kill temp buffer
	     (kill-buffer temp-buffer))
      (with-current-buffer temp-buffer             ;; else print error
	(while (accept-process-output process))
	(message "Global error output:\n%s" (buffer-string))))
    (when (buffer-live-p parent-buffer)            ;; Always clear the cache
      (with-current-buffer parent-buffer
	(gtags-mode--get-plist gtags-mode--root
			       (lambda (p) (plist-put p :cache nil))))))
  (message "Async %s: %s" (process-command process) event)) ;; Notify

(defun gtags-mode--exec-async (cmd args)
  "Run CMD with ARGS asynchronously and set SENTINEL to process.
Starts an asynchronous process and sets
`gtags-mode--exec-async-sentinel' as the process sentinel if
SENTINEL is nil or not specified.  Returns the process object."
  (when cmd
    (let ((pr (make-process :name (format "%s-async" cmd)
			    :buffer (generate-new-buffer " *temp*" t)
			    :command (append (list cmd) args)
			    :sentinel #'gtags-mode--exec-async-sentinel
			    :file-handler t)))
      (process-put pr :buffer (current-buffer))
      pr)))

(defun gtags-mode--exec-sync (args)
  "Run CMD with ARGS synchronously, on success call SENTINEL.
Starts a sync process; on success call SENTINEL or
`gtags-mode--sync-sentinel' if SENTINEL is not specified or nil.
Returns the output of SENTINEL or nil if any error occurred."
  (when cmd
    (with-temp-buffer ;; When sync
      (let ((status (apply #'process-file gtags-mode--global nil (current-buffer) nil args)))
	(if (eq status 0)
	    (string-lines (buffer-string) t)
	  (message "Global error output:\n%s" (buffer-string))
	  (message "Sync %s %s: exited abnormally with code %s" cmd args status)
	  nil)))))

;; Utilities functions (a bit less low level) ========================
(defun gtags-mode--get-plist (file action)
  "Apply ACTION on a plist with known prefix FILE from `gtags-mode--alist'."
  (let ((truename (file-truename file)))
    (catch 'found
      (mapc (lambda (plist)
	      (when (string-prefix-p (plist-get plist :root) truename)
		(throw 'found (funcall action plist))))
	    gtags-mode--alist)
      nil)))

(defun gtags-mode--find-root ()
  "Return the GLOBAL project root.  Return nil if none."
  (when-let ((root (car (gtags-mode--exec-sync '("--print-dbpath")))))
    (setq root (concat (file-remote-p default-directory)
		       (file-truename root)))
    (add-to-list 'gtags-mode--alist `(:root ,root :cache nil)
		 nil (lambda (o1 o2)
		       (string= (plist-get o1 :root) (plist-get o2 :root))))
    root))

(defun gtags-mode--list-completions (prefix)
  "Get the list of completions for PREFIX.
When PREFIX is nil or empty; return the entire list of
completions usually from the cache when possible."
  (if (and (stringp prefix) (not (string-blank-p prefix))) ;; not use cache
      (gtags-mode--exec-sync (append '("--ignore-case" "--completion")
				     `(,(shell-quote-argument prefix))))
    (gtags-mode--get-plist
     gtags-mode--root
     (lambda (plist)    ;; set and return the cache
       (if-let ((cache (plist-get plist :cache)))
	   cache
	 (setq cache (gtags-mode--exec-sync '("--completion")))
	 (plist-put plist :cache cache)
	 cache)))))

(defun gtags-mode--buffers-in-root (root)
  "Return a list of buffers which variable `buffer-file-name' is inside ROOT."
  (mapcan (lambda (buf)
	    (when-let* ((bname (buffer-local-value 'buffer-file-name buf))
			(tname (file-truename bname))
			((string-prefix-p root tname)))
	      (list buf)))
	  (buffer-list)))

(defun gtags-mode--filter-find-symbol (args symbol creator)
  "Run `gtags-mode--exec-sync' with ARGS on SYMBOL and filter output with CREATOR.
Returns the results as a list of CREATORS outputs similar to
`mapcar'.  Creator should be a function with 4 input arguments:
name, code, file, line."
  (delete nil
	  (mapcar
	   (lambda (line)
	     (when (string-match gtags-mode--output-format-regex line)
	       (funcall creator
			(match-string 1 line)   ;; name
			(match-string 4 line)   ;; code
			(match-string 3 line)   ;; file
			(string-to-number (match-string 2 line))))) ;; line
	   (gtags-mode--exec-sync
	    (append args gtags-mode--output-format-options
		    (unless (string-blank-p symbol)
		      (list (shell-quote-argument symbol))))))))

;; Interactive commands ==============================================
(defun gtags-mode-create (root-dir)
  "Create a GLOBAL GTAGS file in ROOT-DIR asynchronously."
  (interactive "DCreate db in directory: ")
  (let ((default-directory root-dir))
    (gtags-mode--exec-async gtags-mode--gtags nil)))

(defun gtags-mode-update ()
  "Update GLOBAL project database."
  (interactive)
  (if gtags-mode--root
      (gtags-mode--exec-async gtags-mode--global '("--update"))
    (error "Not under a GLOBAL project")))

;; Hooks =============================================================
(defun gtags-mode--after-save-hook ()
  "After save hook to update GLOBAL database with changed data."
  (when (and buffer-file-name gtags-mode--root)
    (gtags-mode--exec-async
     gtags-mode--global
     (list "--single-update"
	   (file-name-nondirectory buffer-file-name)))))

(defun gtags-mode--find-file-hook ()
  "Try to enable `gtags' when opening a file.
Check the roots and enable `gtags' if the found-file is in
one of them."
  (when (gtags-mode--get-plist buffer-file-name #'identity)
    (gtags-mode 1)))

;; xref integration ==================================================
(defun gtags-mode--xref-find-symbol (args symbol)
  "Run GNU Global to create xref input list with ARGS on SYMBOL.
Return the results as a list of xref location objects.  ARGS are
any additional command line arguments to pass to GNU Global."
  (gtags-mode--filter-find-symbol
   args symbol
   (lambda (_name code file line)
     (xref-make code (xref-make-file-location
		      (concat (file-remote-p default-directory) file)
		      line 0)))))

(defun gtags-xref-backend ()
  "Gtags backend for Xref."
  (and gtags-mode--root 'gtags))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql gtags)))
  "List all symbols."
  (gtags-mode--list-completions nil))

(cl-defmethod xref-backend-definitions ((_backend (eql gtags)) symbol)
  "List all definitions for SYMBOL."
  (gtags-mode--xref-find-symbol '("--definition") symbol))

(cl-defmethod xref-backend-references ((_backend (eql gtags)) symbol)
  "List all referenced for SYMBOL."
  (gtags-mode--xref-find-symbol '("--reference") symbol))

(cl-defmethod xref-backend-apropos ((_backend (eql gtags)) symbol)
  "List grepped list of candidates SYMBOL."
  (gtags-mode--xref-find-symbol '("--grep") symbol))

;; imenu integration =================================================
(defvar-local gtags-mode--imenu-default-function nil)

(defun gtags-mode--imenu-goto-function (_name line)
  "Function to goto with imenu when LINE info."
  (funcall-interactively #'goto-line line))

(defun gtags-mode-imenu-create-index-function ()
  "Make imenu use Global."
  (when buffer-file-name
    (gtags-mode--filter-find-symbol
     '("--file") (file-name-nondirectory buffer-file-name)
     (lambda (name _code _file line)
       (list name line #'gtags-mode--imenu-goto-function)))))

;; project integration ===============================================
(defun gtags-mode-project-backend (dir)
  "Return the project for DIR as an array."
  (when-let ((plist (gtags-mode--get-plist dir #'identity)))
    (list 'gtags (plist-get plist :root))))

(cl-defmethod project-root ((project (head gtags)))
  "Root for PROJECT."
  (cadr project))

(cl-defmethod project-files ((project (head gtags)) &optional dirs)
  "List files inside all the PROJECT or in if specified DIRS ."
  (let* ((root (project-root project))
	 (remote (file-remote-p root))
	 (results (mapcan
		   (lambda (dir)
		     (when-let* ((tdir (file-truename dir))
				 ((string-prefix-p root tdir)))
		       (gtags-mode--filter-find-symbol
			'("--path") (string-remove-prefix root tdir)
			(lambda (_name _code file _line)
			  (concat remote file)))))
		   (or dirs (list root)))))
    (if (> (length dirs) 1) (delete-dups results) results)))

(cl-defmethod project-buffers ((project (head gtags)))
  "Return the list of all live buffers that belong to PROJECT."
  (gtags-mode--buffers-in-root (project-root project)))

;; Completion-at-point-function (capf) ===============================
(defun gtags-mode-completion-function ()
  "Generate completion list."
  (when-let (bounds (bounds-of-thing-at-point 'symbol))
    (list (car bounds) (cdr bounds)
	  (completion-table-dynamic #'gtags-mode--list-completions)
	  :exclusive 'no)))

;;;###autoload
(define-minor-mode gtags-mode
  "Use GNU Global as backend for several Emacs features in this buffer."
  :global nil
  :lighter gtags-mode-lighter
  (cond
   (gtags-mode
    (gtags-mode--set-connection-locals)
    (setq gtags-mode--root (gtags-mode--find-root))
    (add-hook 'find-file-hook #'gtags-mode--find-file-hook)
    (add-hook 'project-find-functions #'gtags-mode-project-backend)
    (add-hook 'xref-backend-functions #'gtags-xref-backend nil t)
    (add-hook 'after-save-hook #'gtags-mode--after-save-hook nil t)
    (add-hook 'completion-at-point-functions #'gtags-mode-completion-function nil t)
    (setq gtags-mode--imenu-default-function imenu-create-index-function)
    (setq imenu-create-index-function #'gtags-mode-imenu-create-index-function)
    ;; Enable the mode in all the files inside `gtags-mode--root'
    (when (called-interactively-p 'all)
      (mapc (lambda (buff)
	      (unless (buffer-local-value 'gtags-mode buff)
		(with-current-buffer buff
		  (gtags-mode 1))))
	    (gtags-mode--buffers-in-root gtags-mode--root))))
   (t
    (setq gtags-mode--root nil)
    (remove-hook 'find-file-hook #'gtags-mode--find-file-hook)
    (remove-hook 'project-find-functions #'gtags-mode-project-backend)
    (remove-hook 'xref-backend-functions #'gtags-xref-backend t)
    (remove-hook 'after-save-hook #'gtags-mode--after-save-hook t)
    (remove-hook 'completion-at-point-functions #'gtags-mode-completion-function t)
    (setq imenu-create-index-function gtags-mode--imenu-default-function))))

(provide 'gtags-mode)
;;; gtags-mode.el ends here
