;;; gtags-mode.el --- GNU Global integration with xref, project and imenu. -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Jimmy Aguilar Mena
;; URL: https://github.com/Ergus/gtags-mode
;; Keywords: xref, project, imenu, gtags, global
;; Version: 1.3
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

;; GNU Global integration with xref, project, completion-at-point
;; (capf) and imenu.

;; There are many other packages with their own approach and set of
;; more complete/complex features, maps and functionalities; like
;; ggtags, gtags.el, gxref, agtags and some others referenced in:
;; https://www.gnu.org/software/global/links.html

;; This package let all the work to the EMACS tools available for such
;; functions and avoids external dependencies.  Unlike the other
;; packages; this module does not create extra special maps, bindings
;; or menus, but just adds support to the mentioned features to use
;; gtags/global as a backend when possible.  We do special emphasis on
;; minimalism, simplicity, efficiency and tramp support.

;; This package may be extended in the future with new features and to
;; support other tools, but only if they are required and included in
;; an EMACS distribution and don't need external dependencies.

;;; Code:

(require 'xref)
(require 'files-x)
(eval-when-compile (require 'subr-x))

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

(defcustom gtags-mode-lighter " Gtags"
  "The text displayed in the mode line."
  :type 'string
  :risky t)

(defcustom gtags-mode-verbose 2
  "The text displayed in the mode line."
  :type 'natnum
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
  "^\\([^ ]+\\) \\([^ ]+\\) \\([[:digit:]]+\\) \\(.*\\)"
  "Regex to filter the output with `gtags-mode--output-format-options'.")

(defconst gtags-mode--output-format-options
  '("--result=cscope" "--path-style=through" "--color=never")
  "Command line options to use with `gtags-mode--output-format-regex'.")

(defsubst gtags-mode--message (level format-string &rest args)
  "Print log messages when the `gtags-mode-verbose' is greater than LEVEL."
  (when (>= gtags-mode-verbose level)
    (apply #'message (concat "gtags-mode: " format-string) args)))

;; Connection functions
(defun gtags-mode--set-connection-locals ()
  "Set connection local variables when possible and needed."
  (when-let* ((remote (file-remote-p default-directory))
	      ((not (and (local-variable-p 'gtags-mode--global)
			 (local-variable-p 'gtags-mode--gtags))))
	      (criteria (connection-local-criteria-for-default-directory))
	      (symvars (intern (format "gtags-mode--%s-vars" remote)))
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
      (gtags-mode--message 1 "Global async error output:\n%s"
			   (string-trim
			    (buffer-substring-no-properties (point-min) (point-max))))))
  (gtags-mode--message 2 "Async %s: %s"
		       (process-get process :command) (string-trim event))) ;; Always notify

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
    (gtags-mode--message 1 "Can't start async %s subprocess" cmd)
    nil))

(defun gtags-mode--exec-sync (args &optional target)
  "Run global with ARGS on TARGET synchronously.
On success return a list of strings or nil if any error occurred."
  (if-let ((cmd gtags-mode--global) ;; Required for with-temp-buffer
	   (cargs (gtags-mode--quote args target)))
      (with-temp-buffer
	(let* ((status (apply #'process-file cmd nil (current-buffer) nil cargs))
	       (output (string-trim
			(buffer-substring-no-properties (point-min) (point-max)))))
	  (if (eq status 0)
	      (string-lines output t)
	    (gtags-mode--message 1 "Global sync error output:\n%s" output)
	    (gtags-mode--message 1 "Sync %s %s: exited abnormally with code %s" cmd cargs status)
	    nil)))
    (gtags-mode--message 1 "Can't start sync %s subprocess" cmd)
    nil))

;; Utilities functions (a bit less low level) ========================
(defun gtags-mode--get-plist (dir)
  "Return the plist for DIR from `gtags-mode--alist'."
  (seq-find (lambda (plist)
	      (string-prefix-p (plist-get plist :gtagsroot) dir))
	    gtags-mode--alist))

(defun gtags-mode--create-plist (dir)
  "Return dbpath for DIR or nil if none."
  (when-let* ((default-directory dir)
	      (root (car (gtags-mode--exec-sync '("--print-dbpath")))))
    (setq root (concat (file-remote-p default-directory) ;; add remote prefix if remote
		       (file-name-as-directory root)))   ;; add a / at the end is missing
    (display-warning 'gtags-mode "Option HEEE" :info)
    (or (gtags-mode--get-plist root)   ;; already exist
	(car (push `(:gtagsroot ,root :cache nil) gtags-mode--alist)))))

(defun gtags-mode--local-plist (&optional dir)
  "Set and return the buffer local value of `gtags-mode--plist'."
  (if (local-variable-p 'gtags-mode--plist)
      gtags-mode--plist
    (let ((default-directory (or dir default-directory)))
      (gtags-mode--set-connection-locals)
      (setq-local gtags-mode--plist
		  (or (gtags-mode--get-plist default-directory)
		      ;; Heuristic to create new plists only when visiting real files
		      ;; This optimizes when there is not tags file to avoid calling
		      ;; the external process repeatedly i.e in magit buffers that are
		      ;; regenerated every time and forgets the local variables
		      (and (buffer-file-name)
			   (gtags-mode--create-plist default-directory)))))))

(defun gtags-mode--list-completions (prefix)
  "Get the list of completions for PREFIX.
When PREFIX is nil or empty; return the entire list of
completions usually from the cache when possible."
  (cond ;; TODO: use with-memoization in the future it will be on emacs 29.1
   ((not (gtags-mode--local-plist default-directory))
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
  (if-let ((root (plist-get (gtags-mode--local-plist default-directory) :gtagsroot)))
      (delete nil (mapcar
		   (lambda (line)
		     (when (string-match gtags-mode--output-format-regex line)
		       (funcall creator
				(match-string-no-properties 2 line)   ;; name
				(match-string-no-properties 4 line)   ;; code
				(concat root (substring-no-properties
					      line (1+ (match-beginning 1)) (match-end 1))) ;; file
				(string-to-number (match-string-no-properties 3 line))))) ;; line
		   (gtags-mode--exec-sync
		    (append args gtags-mode--output-format-options) symbol)))
    (error "Calling gtags-mode--filter-find-symbol without GTAGSROOT")
    nil))

(defun gtags-mode--update-buffers-plist ()
  "Actions to perform after creating a database.
This iterates over the buffers and tries to reset
`gtags-mode--plist' when it is nil."
  (dolist (buff (buffer-list))
    (unless (buffer-local-value 'gtags-mode--plist buff)
      (with-current-buffer buff
	(gtags-mode--set-connection-locals)
	(kill-local-variable 'gtags-mode--plist) ;; kill the local to reset it
	(gtags-mode--local-plist default-directory)))))

;; Interactive commands ==============================================

;;;###autoload
(defun gtags-mode-create (root-dir)
  "Create a GLOBAL GTAGS file in ROOT-DIR asynchronously."
  (interactive "DCreate GLOBAL files in directory: ")
  (when-let* ((default-directory root-dir)
	      (pr (gtags-mode--exec-async 'gtags-mode--gtags nil)))
    (process-put pr :extra-sentinel #'gtags-mode--update-buffers-plist)))

(defun gtags-mode-update ()
  "Update GLOBAL project database."
  (interactive)
  (when (gtags-mode--local-plist default-directory)
    (gtags-mode--exec-async 'gtags-mode--global '("--update"))))

;; Hooks =============================================================
(defun gtags-mode--after-save-hook ()
  "After save hook to update GLOBAL database with changed data."
  (when (and buffer-file-name (gtags-mode--get-plist buffer-file-name))
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
     (xref-make code (xref-make-file-location file line 0)))))

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
  (when (and buffer-file-name (gtags-mode--local-plist default-directory))
    (gtags-mode--filter-find-symbol
     '("--file") (file-name-nondirectory buffer-file-name)
     (lambda (name _code _file line)
       (list name line #'gtags-mode--imenu-goto-function)))))

;; project integration ===============================================
(defun gtags-mode-project-backend (dir)
  "Return the project for DIR as an array."
  (gtags-mode--get-plist (file-truename dir)))

(cl-defmethod project-root ((project (head :gtagsroot)))
  "Root for PROJECT."
  (plist-get project :gtagsroot))

(cl-defmethod project-files ((project (head :gtagsroot)) &optional dirs)
  "List files inside all the PROJECT or in DIRS if specified."
  (let* ((root (project-root project))
	 (default-directory root)
	 (results (mapcan
		   (lambda (dir)
		     (when (string-prefix-p root dir)
		       (mapcar (lambda (file)
				 (expand-file-name file root))
			       (gtags-mode--exec-sync
				'("--path-style=through" "--path")
				(string-remove-prefix root dir)))))
		   (or dirs `(,root)))))
    (if (> (length dirs) 1) (delete-dups results) results)))

(cl-defmethod project-buffers ((project (head :gtagsroot)))
  "Return the list of all live buffers that belong to PROJECT."
  (delq nil
	(mapcar (lambda (buff)
		  (cond
		   ((eq (buffer-local-value 'gtags-mode--plist buff) project) buff)
		   ((local-variable-p 'gtags-mode--plist buff) nil)
		   (t (with-current-buffer buff
			(when (eq (gtags-mode--local-plist default-directory) project)
			  (current-buffer))))))
		(buffer-list))))

;; Completion-at-point-function (capf) ===============================
(defun gtags-mode-completion-function ()
  "Generate completion list."
  (if (gtags-mode--local-plist default-directory)
      (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
	(list (car bounds) (point)
	      (completion-table-dynamic #'gtags-mode--list-completions)
	      :exclusive 'no))))

;;;###autoload
(define-minor-mode gtags-mode
  "Use GNU Global as backend for project, xref, capf and imenu.
When the buffer is not in a global-project, then all these tools
rely on their original or user configured default behavior."
  :global t
  :lighter gtags-mode-lighter
  (cond
   (gtags-mode
    (add-hook 'project-find-functions #'gtags-mode--local-plist)
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
