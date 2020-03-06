; -*- emacs-lisp -*-

(defun my-sh-mode-hook ()
  "Minor setup when entering Shell mode."
  (interactive)
  (flyspell-prog-mode)
  (flycheck-mode 1)
  (hs-minor-mode)
  (git-gutter-mode 1)
  (highlight-thing-mode))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)

;; Would like to setup a key to automatically send this command to current shell
;; PS1="\W $([ $EUID -eq 0 ] && printf '#' || printf '$') "
