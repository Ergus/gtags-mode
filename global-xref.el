;;; global-xref.el --- GNU Global integration with xref and imenu. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Jimmy Aguilar Mena

;; Author: Jimmy Aguilar Mena
;; URL: https://github.com/Ergus/global-xref
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

(defgroup global-xref nil
  "GNU Global grup for xref."
  :group 'xref)

(defcustom global-xref-global "global"
  "GNU Global executable."
  :type 'string
  :local t)

(defcustom global-xref-gtags "gtags"
  "Gtags executable."
  :type 'string
  :local t)

(defcustom global-xref-lighter "Global-Xref"
  "Gtags executable."
  :type 'string
  :risky t)

(defvar global-xref--roots-list nil
  "Full list of Global roots.
The address is absolute for remote hosts.")
(put 'global-xref--roots-list 'risky-local-variable t)

(defvar-local global-xref--global (executable-find global-xref-global))
(defvar-local global-xref--gtags (executable-find global-xref-gtags))
(defvar-local global-xref--project-root nil
  "Project Global root for this buffer.
the address is relative on remote hosts.")

(defconst global-xref--output-format-regex
  "^\\([^[:blank:]]+\\)[[:blank:]]+\\([[:digit:]]+\\)[[:blank:]]+\\([^[:blank:]]+\\)[[:blank:]]+\\(.*\\)"
  "Regex to filter the output with `global-xref--output-format-options'.")

(defconst global-xref--output-format-options
  '("--result=ctags-x" "--path-style=absolute")
  "Command line options to use with `global-xref--output-format-regex'.")

;; Connection functions
(defun global-xref--set-connection-locals ()
  "Set GLOBAL connection local variables when possible and needed."
  (when-let* ((remote (file-remote-p default-directory))
	      (criteria (connection-local-criteria-for-default-directory))
	      ((not (and (local-variable-p 'global-xref--global)
			 (local-variable-p 'global-xref--gtags))))
	      (symvars (intern (concat "global-xref--" remote "-vars")))
	      (enable-connection-local-variables t))
    (unless (alist-get symvars connection-local-profile-alist)
      (with-connection-local-variables
       (let ((xref-global (if (local-variable-p 'global-xref-global)
			      global-xref-global
			    (file-name-nondirectory global-xref-global)))
	     (xref-gtags (if (local-variable-p 'gtags-xref-global)
			     global-xref-gtags
			   (file-name-nondirectory global-xref-gtags))))
	 (connection-local-set-profile-variables
	  symvars
	  `((global-xref--global . ,(executable-find xref-global t))
	    (global-xref--gtags . ,(executable-find xref-gtags t))))
	 (connection-local-set-profiles criteria symvars))))
    (hack-connection-local-variables-apply criteria)))

;; Async functions
(defun global-xref--exec-async-sentinel (process event)
  "Sentinel to run when PROCESS emits EVENT.
This is the sentinel set in `global-xref--exec-async'."
  (let ((temp-buffer (process-buffer process)))
    (if (and (eq (process-status process) 'exit)
	     (eq (process-exit-status process) 0))
	(and (buffer-name temp-buffer)
	     (kill-buffer temp-buffer))
      (with-current-buffer temp-buffer
	(while (accept-process-output process))
	(message "Global error output:\n%s" (buffer-string)))))
  (message "Async %s: %s" (process-command process) event))

(defun global-xref--exec-async (command args &optional sentinel)
  "Run COMMAND with ARGS asynchronously and set SENTINEL to process.
Starts an asynchronous process and sets
`global-xref--exec-async-sentinel' as the process sentinel if
SENTINEL is nil or not specified.  Returns the process object."
  (when-let* ((cmd (symbol-value command))
	      (process (apply #'start-file-process
			      (format "%s-async" cmd)
			      (generate-new-buffer " *temp*" t)
			      cmd args))
	      (sentinel (or sentinel
			    #'global-xref--exec-async-sentinel)))
    (set-process-sentinel process sentinel)
    process))

(defun global-xref--exec-sync (command args)
  "Run COMMAND with ARGS synchronously, on success call SENTINEL.
Starts a sync process; on success call SENTINEL or
`global-xref--sync-sentinel' if SENTINEL is not specified or nil.
Returns the output of SENTINEL or nil if any error occurred."
  (when-let ((cmd (symbol-value command)))
    (with-temp-buffer ;; When sync
      (let ((status (apply #'process-file cmd nil (current-buffer) nil args)))
	(if (eq status 0)
	    (string-lines (buffer-string) t)
	  (message "Global error output:\n%s" (buffer-string))
	  (message "Sync %s %s: exited abnormally with code %s" cmd args status)
	  nil)))))

;; Api functions
(defun global-xref--find-root ()
  "Return the GLOBAL project root.  Return nil if none."
  (when-let ((root (car (global-xref--exec-sync 'global-xref--global
						'("--print-dbpath")))))
    (setq root (concat (file-remote-p default-directory)
		       (file-truename root)))
    (add-to-list 'global-xref--roots-list root)
    root))

(defun global-xref--filter-find-symbol (args symbol creator)
  "Run `global-xref--exec-sync' with ARGS on SYMBOL and filter output with CREATOR.
Returns the results as a list of CREATORS outputs similar to
`mapcar'.  Creator should be a function with 4 input arguments:
name, code, file, line."
  (delete nil
	  (mapcar
	   (lambda (line)
	     (when (string-match global-xref--output-format-regex line)
	       (funcall creator
			(match-string 1 line)   ;; name
			(match-string 4 line)   ;; code
			(match-string 3 line)   ;; file
			(string-to-number (match-string 2 line))))) ;; line
	   (global-xref--exec-sync
	    'global-xref--global
	    (append args global-xref--output-format-options
		    (unless (string-blank-p symbol)
		      (list (shell-quote-argument symbol))))))))

;; Interactive commands ==============================================
(defun global-xref-create (root-dir)
  "Create a GLOBAL GTAGS file in ROOT-DIR asynchronously."
  (interactive "DCreate db in directory: ")
  (let ((default-directory root-dir))
    (global-xref--exec-async 'global-xref--gtags nil)))

(defun global-xref-update ()
  "Update GLOBAL project database."
  (interactive)
  (if global-xref--project-root
      (global-xref--exec-async 'global-xref--global '("--update"))
    (error "Not under a GLOBAL project")))

(defun global-xref--after-save-hook ()
  "After save hook to update GLOBAL database with changed data."
  (when (and buffer-file-name global-xref--project-root)
    (global-xref--exec-async
     'global-xref--global
     (list "--single-update"
	   (file-name-nondirectory buffer-file-name)))))

(defun global-xref--has-open-root (file)
  "Check for a known prefix for FILE in `global-xref--roots-list'."
  (let ((truename (file-truename file)))
    (catch 'found
      (mapc (lambda (root)
	      (when (string-prefix-p root truename)
		(throw 'found root)))
	    global-xref--roots-list)
      nil)))

(defun global-xref--find-file-hook ()
  "Try to enable `global-xref' when opening a file.
Check the roots and enable `global-xref' if the found-file is in
one of them."
  (when (global-xref--has-open-root buffer-file-name)
    (global-xref-mode 1)))

(defun global-xref--buffers-in-root (root)
  "Return a list of buffers which variable `buffer-file-name' is inside ROOT."
  (mapcan (lambda (buf)
	    (when-let* ((bname (buffer-local-value 'buffer-file-name buf))
			(tname (file-truename bname))
			((string-prefix-p root tname)))
	      (list buf)))
	  (buffer-list)))

;; xref integration ==================================================
(defun global-xref--find-symbol (args symbol)
  "Run GNU Global to create xref input list with ARGS on SYMBOL.
Return the results as a list of xref location objects.  ARGS are
any additional command line arguments to pass to GNU Global."
  (global-xref--filter-find-symbol
   args symbol
   (lambda (_name code file line)
     (xref-make code (xref-make-file-location
		      (concat (file-remote-p default-directory) file)
		      line 0)))))

(defun global-xref-xref-backend ()
  "Global-Xref backend for Xref."
  (and global-xref--project-root 'global-xref))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql global-xref)))
  "List all symbols."
  (global-xref--exec-sync 'global-xref--global '("--completion")))

(cl-defmethod xref-backend-definitions ((_backend (eql global-xref)) symbol)
  "List all definitions for SYMBOL."
  (global-xref--find-symbol '("--definition") symbol))

(cl-defmethod xref-backend-references ((_backend (eql global-xref)) symbol)
  "List all referenced for SYMBOL."
  (global-xref--find-symbol '("--reference") symbol))

(cl-defmethod xref-backend-apropos ((_backend (eql global-xref)) symbol)
  "List grepped list of candidates SYMBOL."
  (global-xref--find-symbol '("--grep") symbol))

;; imenu integration =================================================
(defvar-local global-xref--imenu-default-function nil)

(defun global-xref--imenu-goto-function (_name line)
  "Function to goto with imenu when LINE info."
  (funcall-interactively #'goto-line line))

(defun global-xref-imenu-create-index-function ()
  "Make imenu use Global."
  (when buffer-file-name
    (global-xref--filter-find-symbol
     '("--file") (file-name-nondirectory buffer-file-name)
     (lambda (name _code _file line)
       (list name line #'global-xref--imenu-goto-function)))))

;; project integration ===============================================
(defun global-xref-project-backend (dir)
  "Return the project for DIR as an array."
  (when-let ((root (global-xref--has-open-root dir)))
    (list 'global-xref root)))

(cl-defmethod project-root ((project (head global-xref)))
  "Root for PROJECT."
  (cadr project))

(cl-defmethod project-files ((project (head global-xref)) &optional dirs)
  "Root for PROJECT."
  (let* ((root (project-root project))
	 (remote (file-remote-p root)))
    (mapcan (lambda (dir)
	      (when-let* ((tdir (file-truename dir))
			  ((string-prefix-p root tdir)))
		(global-xref--filter-find-symbol
		 '("--path") (string-remove-prefix root tdir)
		 (lambda (_name _code file _line)
		   (concat remote file)))))
	    (or dirs (list root)))))

(cl-defmethod project-buffers ((project (head global-xref)))
  "Return the list of all live buffers that belong to PROJECT."
  (global-xref--buffers-in-root (project-root project)))

;;;###autoload
(define-minor-mode global-xref-mode
  "Use GNU Global as backend for several Emacs features in this buffer."
  :global nil
  :lighter global-xref-lighter
  (cond
   (global-xref-mode
    (global-xref--set-connection-locals)
    (setq global-xref--project-root (global-xref--find-root))
    (add-hook 'find-file-hook #'global-xref--find-file-hook)
    (add-hook 'project-find-functions #'global-xref-project-backend)
    (add-hook 'xref-backend-functions #'global-xref-xref-backend nil t)
    (add-hook 'after-save-hook #'global-xref--after-save-hook nil t)
    (setq global-xref--imenu-default-function imenu-create-index-function)
    (setq imenu-create-index-function #'global-xref-imenu-create-index-function)
    ;; Enable the mode in all the files inside `global-xref--project-root'
    (when (called-interactively-p 'all)
      (mapc (lambda (buff)
	      (unless (buffer-local-value 'global-xref-mode buff)
		(with-current-buffer buff
		  (global-xref-mode 1))))
	    (global-xref--buffers-in-root global-xref--project-root))))
   (t
    (setq global-xref--project-root nil)
    (remove-hook 'find-file-hook #'global-xref--find-file-hook)
    (remove-hook 'project-find-functions #'global-xref-project-backend)
    (remove-hook 'xref-backend-functions #'global-xref-xref-backend t)
    (remove-hook 'after-save-hook #'global-xref--after-save-hook t)
    (setq imenu-create-index-function global-xref--imenu-default-function))))

(provide 'global-xref)
;;; global-xref.el ends here
