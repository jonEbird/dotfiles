; -*- emacs-lisp -*-

(require 'dired)

(defun my-make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name'.\n
   This customized version of this function is useful to keep all backups
   in one place, instead of all over the filesystem."
  (let ((file-name-sanitized
         (dired-replace-in-string "%" "" file-name)))
    (message (concat "make-backup: " file-name-sanitized))
    (if (not (file-exists-p "~/.backups"))
        (make-directory (expand-file-name "~/.backups/") 't))
    (concat (expand-file-name "~/.backups/")
            (dired-replace-in-string "/" "|" file-name-sanitized))))

;; Set different methods depending on what system
(if (eq system-type 'windows-nt)
    (setq
     backup-by-copying              t  ; don't clobber symlinks
     backup-directory-alist         '(("." . "~/.backups"))  ; don't litter my fs tree
     delete-old-versions            t
     kept-new-versions              6
     kept-old-versions              2
     version-control                t  ; use versioned backups
     )
  (setq
   make-backup-file-name-function   'my-make-backup-file-name
   vc-make-backup-files             t  ;; Backup version controlled files too
   ))

;; Do not make backups for GnuPG files
;;   Thanks - http://anirudhsasikumar.net/blog/2005.01.21.html
(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
	;; disable backups
	(set (make-local-variable 'backup-inhibited) t)
	;; disable auto-save
	(if auto-save-default
	    (auto-save-mode -1)))
    ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;resort to default auto save setting
    (if auto-save-default
	(auto-save-mode 1))))

(setq auto-mode-alist
      (append '(("\\.gpg$" . sensitive-mode))
	      auto-mode-alist))
