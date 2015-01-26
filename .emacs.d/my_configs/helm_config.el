;; Mostly following  http://tuhdo.github.io/helm-intro.html
;; Then took things from https://github.com/thierryvolpiatto/emacs-tv-config/blob/master/init-helm-thierry.el

;; Cheat sheet for myself
;;  C-c h b     - Resume last helm session
;;  C-c h r     - Test and use regexp
;;  C-c h s     - surfraw and and 'g' for google

(require 'helm-config)
(helm-mode 1)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-split-window-default-side        'below  ; considered right
      helm-ff-file-name-history-use-recentf t
      helm-quick-update                     t
      helm-idle-delay                       0.01
      helm-input-idle-delay                 0.01
      helm-ff-file-name-history-use-recentf t
      helm-buffers-fuzzy-matching           t
      helm-M-x-fuzzy-match                  t
      helm-move-to-line-cycle-in-source     t
      ido-use-virtual-buffers               t
      )

; (add-to-list 'helm-boring-file-regexp-list "")
(add-to-list 'completion-ignored-extensions ".snapshot/")

; Global key bindings
(global-set-key (kbd "M-x")       'helm-M-x)

(global-set-key (kbd "M-y")       'helm-show-kill-ring)  ; where have you been all my emacs life
(if (fboundp 'session-yank)
    (setq session-save-print-spec     '(t nil 40000)))   ; https://github.com/emacs-helm/helm/issues/94

(global-set-key (kbd "C-x b")     'helm-mini)

(global-set-key (kbd "C-x C-f")   'helm-find-files)

(global-set-key (kbd "C-c h o")   'helm-occur)

(global-set-key (kbd "C-h SPC")   'helm-all-mark-rings)  ; This is ridiculous
(global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)

(global-set-key (kbd "C-c h g")   'helm-google-suggest)  ; More ridiculous

(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)

(global-set-key (kbd "C-h r")     'helm-info-emacs)

(global-set-key (kbd "C-h d")     'helm-info-at-point)

(global-set-key (kbd "<f1>")      'helm-resume)

(global-set-key (kbd "C-c h i")   'helm-semantic-or-imenu)

;; Issue a C-u C-s while in helm-find-files to perform a recursive grep (or ack)
(when (executable-find "ack")
  (setq helm-grep-default-command         "ack -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack -H  --no-group --no-color %e %p %f"))

; Help to find man page entries at point via "C-c h m"
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; External Package Interface
; --------------------------------------------------
(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(if (functionp 'helm-projectile-on)
    (helm-projectile-on))

;; Fixing mu4e for helm
(setq mu4e-completing-read-function 'completing-read)

(require 'helm-mu)
(define-key mu4e-headers-mode-map (kbd "M-/") 'helm-mu)

;; Shell
(require 'helm-eshell)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))

;; gtags
;;--------------------------------------------------
;; See https://github.com/syohex/emacs-helm-gtags

;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 '(helm-gtags-path-style 'relative)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-prefix-key "C-t")
 '(helm-gtags-suggested-key-mapping t))

;; key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
     (define-key helm-gtags-mode-map (kbd "C-c h") nil )))

(require 'helm-gtags)

;; helm-swoop - https://github.com/ShingoFukuyama/helm-swoop
(require 'helm-swoop)
;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
