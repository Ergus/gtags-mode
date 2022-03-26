;;; global-xref.el --- GNU Global integration with xref and imenu. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; URL: https://github.com/Ergus/global-xref
;; Keywords: xref, imenu, gtags, global
;; Version: 1.0 alpha
;; Package-Requires: ((emacs "28"))

;; Copyright (C) 2022  Jimmy Aguilar Mena

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

;; GNU Global integration with xref and imenu.

;;; Code:

(require 'xref)
(require 'cl-generic)

(defgroup global-xref nil
  "GNU Global grup for xref."
  :group 'xref)

(defcustom global-xref-global "global"
  "GNU Global executable."
  :type 'string)

(defcustom global-xref-gtags "gtags"
  "Gtags executable."
  :type 'string)

(defcustom global-xref-lighter "Global-Xref"
  "Gtags executable."
  :type 'string)

(defun global-xref--buffer-to-list ()
  "Return lines in current buffer in a list."
  (let ((lines))
    (goto-char (point-min))
    (while (not (eobp))
      (push (buffer-substring-no-properties
	     (line-beginning-position) (line-end-position))
	    lines)
      (forward-line 1))
    (nreverse lines)))

(defvar global-xref--roots-list nil)

(defvar-local global-xref--global (executable-find global-xref-global))
(defvar-local global-xref--gtags (executable-find global-xref-gtags))
(defvar-local global-xref--project-root nil
  "Project Global root for this buffer.")

(defun global-xref--exec (command args async postfunction)
  "Run COMMAND-SYM with and ARGS, in ASYNC way.
When ASYNC is 'nil' executes command synchronously; returns the
output of the command as a string or calls POSTFUNCTION in the
command's buffer and returns the result.  returns nil if an error
occurred.
When ASYNC is non-nil starts an async process and executes the
POSTFUNCTION in a sentinel.  Returns the process handler in all
the cases"
  (with-connection-local-variables
   (when-let (cmd (symbol-value command))
     (if async ;; When async
	 (let ((process (apply #'start-file-process
			       (format "%s-async" cmd)
			       (generate-new-buffer " *temp*" t) cmd args)))
	   (set-process-sentinel
	    process
	    (lambda (process event)
	      (if-let* (((eq (process-status process) 'exit))
			(temp-buffer (process-buffer process)))
		(with-current-buffer temp-buffer
		  (while (accept-process-output process))
		  (when (functionp postfunction)
		    (unwind-protect
			(funcall postfunction)
		      (and (buffer-name temp-buffer)
			   (kill-buffer temp-buffer)))))
		(let ((inhibit-message t))
		  (message "global error output:\n%s" (buffer-string))))
	      (message "Async %s: %s" (process-command process) event)))
	   process)

       (with-temp-buffer ;; When sync
	 (let ((status (apply #'process-file cmd nil (current-buffer) nil args)))
	   (if (eq status 0)
	       (if (functionp postfunction)
		   (funcall postfunction)
		 (string-trim (buffer-substring-no-properties (point-min) (point-max))))
	     (error "Sync %s: exited abnormally with code %s" cmd status)
	     (let ((inhibit-message t))
	       (message "global error output:\n%s" (buffer-string)))
	     nil)))))))

(defsubst global-xref--to-list (args)
  "Run GNU Global with `process-file' and ARGS return a list."
  (global-xref--exec 'global-xref--global args nil #'global-xref--buffer-to-list))

(defun global-xref--set-connection-locals ()
  "Set GLOBAL connection local variables when possible."
  (when-let* ((host (file-remote-p default-directory 'host))
	      (symvars (intern (concat "global-xref-" host "-vars")))
	      (criteria `(:machine ,host))
	      (connection-local-variables-alist t))
    (hack-connection-local-variables criteria)
    (unless (alist-get 'global-xref--global connection-local-variables-alist)
      (connection-local-set-profile-variables
       symvars
       `((global-xref--global . ,(executable-find (file-name-base global-xref-global) t))
	 (global-xref--gtags . ,(executable-find (file-name-base global-xref-gtags) t))))
      (connection-local-set-profiles criteria symvars))))

(defun global-xref--find-root ()
  "Return the GLOBAL project root.  Return nil if none."
  (let ((root (global-xref--exec 'global-xref--global '("--print-dbpath") nil nil)))
    (when root
      (add-to-list 'global-xref--roots-list root)
      root)))

(defun global-xref--filter-find-symbol (creator args symbol)
  "Run GNU Global and apply CREATOR to global-xref--to-list output.
Return the results as a list."
  (remove
   nil
   (mapcar (lambda (gtags-x-line)
	     (when (string-match
		    "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t\]+\\)[ \t]+\\(.*\\)"
		    gtags-x-line)
	       (funcall creator
			(match-string 1 gtags-x-line)   ;; name
			(match-string 4 gtags-x-line)   ;; code
			(match-string 3 gtags-x-line)   ;; file
			(string-to-number (match-string 2 gtags-x-line)) ;; line
			)))
	   (global-xref--to-list
	    (append args (list "--result=ctags-x" "--path-style=absolute"
			       (shell-quote-argument symbol)))))))

;; Interactive commands.
(defun global-xref-create-db (root-dir)
  "Create a GLOBAL database in ROOT-DIR asynchronously."
  (interactive "DCreate db in directory: ")
  (let ((default-directory root-dir))
    (global-xref--exec
     'global-xref--gtags '("--update") t
     (lambda ()
       (when (not global-xref--project-root)
	 (setq global-xref--project-root (global-xref--find-root))
	 (message "Set GLOBAL project-root: %s" global-xref--project-root))))))

(defun global-xref-update ()
  "Update GLOBAL project database."
  (interactive)
  (if global-xref--project-root
      (global-xref--exec 'global-xref--global '("--update") t t)
    (error "Not under a GLOBAL project")))

(defun global-xref-update-file (file)
  "Update GLOBAL project database for single FILE."
  (interactive "fFile: ")
  (if (and global-xref--project-root
	   (file-exists-p file)
	   (string-prefix-p global-xref--project-root (expand-file-name file)))
      (global-xref--exec 'global-xref--global (list "--single-update" file) t t)
    (when (called-interactively-p 'all)
      (error "Database no updated Root: %s Buffer file: %s"
	     global-xref--project-root file))))

(defun global-xref--after-save-hook ()
  "After save hook to update GLOBAL database with changed data."
  (when buffer-file-name
    (global-xref-update-file buffer-file-name)))

(defun global-xref--find-file-hook ()
  "After save hook to update GLOBAL database with changed data."
  (when buffer-file-name
    (global-xref-update-file buffer-file-name)))

;; xref integration
(defun global-xref--find-symbol (args symbol)
  "Run GNU Global to create xref input list with ARGS on SYMBOL.
Return the results as a list of xref location objects.  ARGS are
any additional command line arguments to pass to GNU Global."
  (global-xref--filter-find-symbol
   (lambda (_name code file line)
     (xref-make code (xref-make-file-location
		      (concat (file-remote-p default-directory) file)
		      line 0)))
   args symbol))

(defun global-xref-xref-backend ()
  "Global-Xref backend for Xref."
  (and global-xref--project-root 'global-xref))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql global-xref)))
  "List all symbols."
  (global-xref--to-list '("--completion")))

(cl-defmethod xref-backend-definitions ((_backend (eql global-xref)) symbol)
  "List all definitions for SYMBOL."
  (global-xref--find-symbol '("--definition") symbol))

(cl-defmethod xref-backend-references ((_backend (eql global-xref)) symbol)
  "List all referenced for SYMBOL."
  (global-xref--find-symbol '("--reference") symbol))

(cl-defmethod xref-backend-apropos ((_backend (eql global-xref)) symbol)
  "List grepped list of candidates SYMBOL."
  (global-xref--find-symbol '("--grep") symbol))

;; imenu integration
(defvar-local global-xref--imenu-default-function nil)

(defun global-xref--imenu-goto-function (_name line)
  "Function to goto with imenu when LINE info."
  (funcall-interactively #'goto-line line))

(defun global-xref-imenu-create-index-function ()
  "Make imenu use gtags."
  (when-let ((commandfile (or (file-remote-p buffer-file-name 'localname)
			      buffer-file-name)))
    (global-xref--filter-find-symbol
     (lambda (name _code _file line)
       (list name line #'global-xref--imenu-goto-function))
     '("--file")
     commandfile)))

;;;###autoload
(define-minor-mode global-xref-mode
  "Use GNU Global as backend for several Emacs features in this buffer."
  :global nil
  :lighter global-xref-lighter
  (cond
   (global-xref-mode
    (global-xref--set-connection-locals)
    (setq global-xref--project-root (global-xref--find-root))
    (add-hook 'xref-backend-functions #'global-xref-xref-backend nil t)
    (add-hook 'after-save-hook #'global-xref--after-save-hook nil t)
    (setq global-xref--imenu-default-function imenu-create-index-function)
    (setq imenu-create-index-function #'global-xref-imenu-create-index-function))
   (t
    (setq global-xref--project-root nil)
    (remove-hook 'xref-backend-functions #'global-xref-xref-backend t)
    (remove-hook 'after-save-hook #'global-xref--after-save-hook t)
    (setq imenu-create-index-function global-xref--imenu-default-function))))

(provide 'global-xref)
;;; global-xref.el ends here
