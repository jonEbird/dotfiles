; -*- emacs-lisp -*-

(defun my-sh-mode-hook ()
  "Minor setup when entering Shell mode."
  (interactive)
  (flyspell-prog-mode)
  (flycheck-mode 1)
  (highlight-thing-mode))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)
