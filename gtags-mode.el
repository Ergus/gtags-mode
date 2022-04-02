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
(defvar-local gtags-mode--plist nil
  "Project Global root for this buffer.")

(defconst gtags-mode--output-format-regex
  "^\\([^[:blank:]]+\\)[[:blank:]]+\\([[:digit:]]+\\)[[:blank:]]+\\([^[:blank:]]+\\)[[:blank:]]+\\(.*\\)"
  "Regex to filter the output with `gtags-mode--output-format-options'.")

(defconst gtags-mode--output-format-options
  '("--result=ctags-x" "--path-style=absolute")
  "Command line options to use with `gtags-mode--output-format-regex'.")

;; Connection functions
(defun gtags-mode--set-connection-locals ()
  "Set connection local variables when possible and needed."
  (when-let* ((remote (file-remote-p default-directory))
	      ((not (and (local-variable-p 'gtags-mode--global)
			 (local-variable-p 'gtags-mode--gtags))))
	      (criteria (connection-local-criteria-for-default-directory))
	      (symvars (intern (concat "gtags-mode--" remote "-vars")))
	      (enable-connection-local-variables t))
    (unless (alist-get symvars connection-local-profile-alist)
      (with-connection-local-variables  ;; because *-executable can be set as connection local
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
  (if (and (eq (process-status process) 'exit)         ;; On success
	   (eql (process-exit-status process) 0))
      (let ((temp-buffer (process-buffer process))
	    (parent-buffer (process-get process :parent-buffer))
	    (extra-sentinel (process-get process :extra-sentinel)))
	(when (buffer-name temp-buffer)                ;; kill temp buffer
	  (while (accept-process-output process))
	  (kill-buffer temp-buffer))
	(when (buffer-live-p parent-buffer)            ;; work on parent buffer
	  (with-current-buffer parent-buffer
	    (when (functionp extra-sentinel)           ;; run extra sentinel
	      (funcall extra-sentinel))
	    (when gtags-mode--plist                    ;; clear cache
	      (plist-put gtags-mode--plist :cache nil)))))
    (with-current-buffer (process-buffer process)      ;; In failure print error
      (while (accept-process-output process))
      (message "Global async error output:\n%s" (buffer-string))))
  (message "Async %s: %s" (process-get process :command) event)) ;; Notify always

(defsubst gtags-mode--quote (args symbol)
  "Pre-process ARGS and quote SYMBOL."
  (append args (and (stringp symbol) (not (string-blank-p symbol))
		    `(,(shell-quote-argument symbol)))))

(defun gtags-mode--exec-async (cmd args &optional target)
  "Run CMD with ARGS on TARGET asynchronously.
Start an asynchronous process and sets
`gtags-mode--exec-async-sentinel' as the process sentinel.
Returns the process object."
  (if-let* ((cmd (buffer-local-value cmd (current-buffer)))
	    (command (gtags-mode--quote (append `(,cmd) args) target))
	    (pr (make-process :name (format "%s-async" cmd)
			      :buffer (generate-new-buffer " *temp*" t)
			      :command command
			      :sentinel #'gtags-mode--exec-async-sentinel
			      :file-handler t)))
      (progn
	;; In future not needed with `remote-commands'.
	(set-process-plist pr `(:parent-buffer ,(current-buffer) :command ,command))
	pr)
    (message "Can't start async %s subprocess" cmd)
    nil))

(defun gtags-mode--exec-sync (args &optional target)
  "Run global with ARGS on TARGET synchronously.
On success return a list of strings or nil if any error occurred."
  (if-let ((cmd gtags-mode--global) ;; Required for with-temp-buffer
	   (cargs (gtags-mode--quote args target)))
      (with-temp-buffer
	(let ((status (apply #'process-file cmd nil (current-buffer) nil cargs)))
	  (if (eq status 0)
	      (string-lines (string-trim (buffer-substring-no-properties
					  (point-min)
					  (point-max))) t)
	    (message "Global sync error output:\n%s" (buffer-string))
	    (message "Sync global %s: exited abnormally with code %s" cargs status)
	    nil)))
    (message "Can't start sync %s subprocess" cmd)
    nil))

;; Utilities functions (a bit less low level) ========================
(defun gtags-mode--get-plist (directory)
  "Return the plist for DIRECTORY from `gtags-mode--alist'."
  (let ((truename (file-truename directory)))
    (catch 'found
      (dolist (plist gtags-mode--alist)
	(when (string-prefix-p (plist-get plist :gtagsroot) truename)
	  (throw 'found plist)))
      nil)))

(defun gtags-mode--create-plist (directory)
  "Return dbpath for DIRECTORY or nil if none."
  (when-let* ((default-directory (file-truename directory))
	      (root (car (gtags-mode--exec-sync '("--print-dbpath")))))
    (setq root (file-truename (concat (file-remote-p default-directory) root)))
    (or (gtags-mode--get-plist root)   ;; already exist
	(car (push `(:gtagsroot ,root :cache nil) gtags-mode--alist)))))

(defun gtags-mode--local-plist ()
  "Set and return the buffer local value of `gtags-mode--plist'."
  (if (local-variable-p 'gtags-mode--plist)
      gtags-mode--plist
    (gtags-mode--set-connection-locals)
    (setq-local gtags-mode--plist (or (gtags-mode--get-plist default-directory)
				      (gtags-mode--create-plist default-directory)))))

(defun gtags-mode--list-completions (prefix)
  "Get the list of completions for PREFIX.
When PREFIX is nil or empty; return the entire list of
completions usually from the cache when possible."
  (cond ;; TODO: use with-memoization in the future it will be on emacs 29.1
   ((not (gtags-mode--local-plist))
    (error "Calling `gtags-mode--list-completions' with no gtags-mode--plist"))
   ((and (stringp prefix) (not (string-blank-p prefix))
	 (gtags-mode--exec-sync '("--ignore-case" "--completion") prefix)))
   ((plist-get gtags-mode--plist :cache))
   (t (plist-put gtags-mode--plist :cache (gtags-mode--exec-sync '("--completion")))
      (plist-get gtags-mode--plist :cache))))

(defun gtags-mode--filter-find-symbol (args symbol creator)
  "Run `gtags-mode--exec-sync' with ARGS on SYMBOL and filter output with CREATOR.
Returns the results as a list of CREATORS outputs similar to
`mapcar'.  Creator should be a function with 4 input arguments:
name, code, file, line."
  (delete nil (mapcar
	       (lambda (line)
		 (when (string-match gtags-mode--output-format-regex line)
		   (funcall creator
			    (match-string-no-properties 1 line)   ;; name
			    (match-string-no-properties 4 line)   ;; code
			    (match-string-no-properties 3 line)   ;; file
			    (string-to-number (match-string-no-properties 2 line))))) ;; line
	       (gtags-mode--exec-sync
		(append args gtags-mode--output-format-options) symbol))))

(defun gtags-mode--update-buffers-plist ()
  "Actions to perform after creating a database.
This iterates over the buffers and tries to reset
`gtags-mode--plist' when it is nil."
  (dolist (buff (buffer-list))
    (unless (buffer-local-value 'gtags-mode--plist buff)
      (with-current-buffer buff
	(gtags-mode--set-connection-locals)
	(kill-local-variable 'gtags-mode--plist) ;; kill the local to reset it
	(gtags-mode--local-plist)))))

;; Interactive commands ==============================================
(defun gtags-mode-create (root-dir)
  "Create a GLOBAL GTAGS file in ROOT-DIR asynchronously."
  (interactive "DCreate GLOBAL files in directory: ")
  (when-let* ((default-directory root-dir)
	      (pr (gtags-mode--exec-async 'gtags-mode--gtags nil)))
    (process-put pr :extra-sentinel #'gtags-mode--update-buffers-plist)))

(defun gtags-mode-update ()
  "Update GLOBAL project database."
  (interactive)
  (when (gtags-mode--local-plist)
    (gtags-mode--exec-async 'gtags-mode--global '("--update"))))

;; Hooks =============================================================
(defun gtags-mode--after-save-hook ()
  "After save hook to update GLOBAL database with changed data."
  (when (and buffer-file-name (gtags-mode--local-plist))
    (gtags-mode--exec-async
     'gtags-mode--global
     '("--single-update") (file-name-nondirectory buffer-file-name))))

;; xref integration ==================================================
(defun gtags-mode--xref-find-symbol (args symbol)
  "Run GNU Global to create xref input list with ARGS on SYMBOL.
Return as a list of xref location objects."
  (gtags-mode--filter-find-symbol
   args symbol
   (lambda (_name code file line)
     (xref-make code (xref-make-file-location
		      (concat (file-remote-p default-directory) file)
		      line 0)))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (head :gtagsroot)))
  "List all symbols."
  (gtags-mode--list-completions nil))

(cl-defmethod xref-backend-definitions ((_backend (head :gtagsroot)) symbol)
  "List all definitions for SYMBOL."
  (gtags-mode--xref-find-symbol '("--definition") symbol))

(cl-defmethod xref-backend-references ((_backend (head :gtagsroot)) symbol)
  "List all referenced for SYMBOL."
  (gtags-mode--xref-find-symbol '("--reference") symbol))

(cl-defmethod xref-backend-apropos ((_backend (head :gtagsroot)) symbol)
  "List grepped list of candidates SYMBOL."
  (gtags-mode--xref-find-symbol '("--grep") symbol))

;; imenu integration =================================================
(defun gtags-mode--imenu-goto-function (_name line)
  "Function to goto with imenu when LINE info."
  (funcall-interactively #'goto-line line))

(defun gtags-mode--imenu-advice ()
  "Make imenu use Global."
  (when (and buffer-file-name (gtags-mode--local-plist))
    (gtags-mode--filter-find-symbol
     '("--file") (file-name-nondirectory buffer-file-name)
     (lambda (name _code _file line)
       (list name line #'gtags-mode--imenu-goto-function)))))

;; project integration ===============================================
(defun gtags-mode-project-backend (dir)
  "Return the project for DIR as an array."
  (gtags-mode--get-plist dir))

(cl-defmethod project-root ((project (head :gtagsroot)))
  "Root for PROJECT."
  (plist-get project :gtagsroot))

(cl-defmethod project-files ((project (head :gtagsroot)) &optional dirs)
  "List files inside all the PROJECT or in DIRS if specified."
  (let* ((root (project-root project))
	 (remote (file-remote-p root))
	 (results (mapcan
		   (lambda (dir)
		     (when-let* ((tdir (file-truename dir))
				 ((string-prefix-p root tdir)))
		       (mapcar (lambda (file)
				 (concat remote file)) ;; Add remote prefix
			       (gtags-mode--exec-sync
				'("--path-style=absolute" "--path")
				(string-remove-prefix root tdir)))))
		   (or dirs `(,root)))))
    (if (> (length dirs) 1) (delete-dups results) results)))

(cl-defmethod project-buffers ((project (head :gtagsroot)))
  "Return the list of all live buffers that belong to PROJECT."
  (mapcar (lambda (buff)
	    (cond
	     ((eq (buffer-local-value 'gtags-mode--plist buff) project) buff)
	     ((local-variable-p 'gtags-mode--plist buff) nil)
	     (t (with-current-buffer buff
		  (when (eq (gtags-mode--local-plist) project)
		    (current-buffer))))))
	  (buffer-list)))

;; Completion-at-point-function (capf) ===============================
(defun gtags-mode-completion-function ()
  "Generate completion list."
  (if (gtags-mode--local-plist)
      (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
	(list (car bounds) (cdr bounds)
	      (completion-table-dynamic #'gtags-mode--list-completions)
	      :exclusive 'no))))

;;;###autoload
(define-minor-mode gtags-mode
  "Use GNU Global as backend for several Emacs features in this buffer."
  :global t
  :lighter gtags-mode-lighter
  (cond
   (gtags-mode
    (add-hook 'project-find-functions #'gtags-mode-project-backend)
    (add-hook 'xref-backend-functions #'gtags-mode--local-plist)
    (add-hook 'completion-at-point-functions #'gtags-mode-completion-function)
    (add-hook 'after-save-hook #'gtags-mode--after-save-hook)
    (advice-add imenu-create-index-function :before-until #'gtags-mode--imenu-advice))
   (t
    (remove-hook 'project-find-functions #'gtags-mode-project-backend)
    (remove-hook 'xref-backend-functions #'gtags-mode--local-plist)
    (remove-hook 'completion-at-point-functions #'gtags-mode-completion-function)
    (remove-hook 'after-save-hook #'gtags-mode--after-save-hook)
    (advice-remove imenu-create-index-function #'gtags-mode--imenu-advice))))

(provide 'gtags-mode)
;;; gtags-mode.el ends here
