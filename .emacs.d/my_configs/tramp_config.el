; -*- emacs-lisp -*-

(require 'tramp)

(setq tramp-remote-path
      '(tramp-default-remote-path
        "/usr/sbin" "/usr/local/bin" "/local/bin" "/local/freeware/bin"
        "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin"
        "/usr/contrib/bin" "/usr/bin")
      tramp-default-method "ssh")

(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))

(if (eq system-type 'windows-nt)
    (setq tramp-default-method "pscp")
  )
;(setq tramp-debug-buffer t)
