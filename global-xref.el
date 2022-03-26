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

(defvar-local global-xref--global (executable-find global-xref-global))
(defvar-local global-xref--gtags (executable-find global-xref-gtags))

(defvar-local global-xref--project-root nil
  "Project Global root for this buffer.")

(defun global-xref--process-file (args &optional postfunction)
  "Run GNU Global with `process-file' and ARGS.
Return the output as a string or passes it to POSTFUNCTION.
Return nil if an error occurred."
  (with-connection-local-variables
   (when global-xref-global
     (with-temp-buffer
       (let ((status (apply #'process-file global-xref--global nil (current-buffer) nil args)))
	 (if (eq status 0)
	     (if (functionp postfunction)
		 (funcall postfunction)
	       (string-trim (buffer-substring-no-properties (point-min) (point-max))))
	   (error "%s exited with status %s" global-xref--global status)
	   (let ((inhibit-message t))
	     (message "global error output:\n%s" (buffer-string)))
	   nil))))))

(defsubst global-xref--to-list (args)
  "Run GNU Global with `process-file' and ARGS return a list."
  (global-xref--process-file args #'global-xref--buffer-to-list))

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
  (global-xref--process-file '("--print-dbpath")))

(defun global-xref--filter-find-symbol (creator args symbol)
  "Run GNU Global and apply CREATOR to global-xref--to-list output.
Return the results as a list."
  (remove nil
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
  "Create a GLOBAL database in ROOT-DIR."
  (interactive "DCreate db in directory: ")
  (let ((default-directory root-dir)
	(buffer (get-buffer-create " *Global-Xref create*")))
    (with-connection-local-variables
     (set-process-sentinel
      (start-file-process "global-xref-create-db" buffer global-xref--gtags)
      (lambda (process event)
	(message "Global created %s: %s" root-dir (string-trim event))
	;; If not GLOBAL root, maybe we can set it after this.
	(when (and (eq (process-status process) 'exit)
		   (not global-xref--project-root))
	  (setq global-xref--project-root (global-xref--find-root))
	  (message "Set GLOBAL project-root: %s" global-xref--project-root)))))))

(defun global-xref-update ()
  "Update GLOBAL project database."
  (interactive)
  (if global-xref--project-root
      (global-xref--process-file '("--update"))
    (error "Not under a GLOBAL project")))

(defun global-xref-update-file (file)
  "Update GLOBAL project database for single FILE."
  (interactive "fFile: ")
  (if (and global-xref--project-root
	   (file-exists-p file)
	   (string-prefix-p global-xref--project-root (expand-file-name file)))
      (global-xref--process-file (list "--single-update" file))
    (error "Root: %s Buffer file: %s" global-xref--project-root file)))

(defun global-xref--after-save-hook ()
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
    (setq imenu-create-index-function #'global-xref-imenu-create-index-function)
    )
   (t
    (setq global-xref--project-root nil)
    (remove-hook 'xref-backend-functions #'global-xref-xref-backend t)
    (remove-hook 'after-save-hook #'global-xref--after-save-hook t)
    (setq imenu-create-index-function global-xref--imenu-default-function))))

(provide 'global-xref)
;;; global-xref.el ends here
