; -*- emacs-lisp -*-

(defun my-perl-mode-hook ()
  "Minor setup when entering Perl mode."
  (interactive)
  (hs-minor-mode)
  (flyspell-prog-mode)
  (flycheck-mode 1)
  (git-gutter-mode 1)
  (highlight-thing-mode)
  (setq fill-column 90)
  (ac-flyspell-workaround))

(add-hook 'perl-mode-hook 'my-perl-mode-hook)
