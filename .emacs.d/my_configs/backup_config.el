; -*- emacs-lisp -*-

(defun my-make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name'.\n
   This customized version of this function is useful to keep all backups
   in one place, instead of all over the filesystem."
  (require 'dired)
  (message (concat "make-backup: " file-name))
  (if (not (file-exists-p "~/.backups"))
      (make-directory (expand-file-name "~/.backups/") 't))
  (concat (expand-file-name "~/.backups/")
	  (dired-replace-in-string "/" "|" file-name)))

;; Set different methods depending on what system
(if (eq system-type 'windows-nt)
    (setq
     backup-by-copying t      ; don't clobber symlinks
     backup-directory-alist
     '(("." . "~/.backups"))  ; don't litter my fs tree
     delete-old-versions t
     kept-new-versions 6
     kept-old-versions 2
     version-control t)       ; use versioned backups
  (setq make-backup-file-name-function 'my-make-backup-file-name)
  )
