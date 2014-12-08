; Mostly following  http://tuhdo.github.io/helm-intro.html

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
)

; Global key bindings
(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)  ; where have you been all my emacs life

(global-set-key (kbd "C-x b") 'helm-mini)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)  ; This is ridiculous
(global-set-key (kbd "C-c h SPC") 'helm-all-mark-rings)

(global-set-key (kbd "C-c h g") 'helm-google-suggest)  ; More ridiculous

(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)

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
(helm-projectile-on)
;(helm-projectile-on)

;; Fixing mu4e for helm
(setq mu4e-completing-read-function 'completing-read)

(require 'helm-mu)

(defun jsm/helm-mu-contacts-init ()
  "Retrieves contacts from mu."
  (let ((cmd (concat
              "mu cfind --format=mutt-ab"
              (if helm-mu-contacts-personal " --personal" "")
              (format
                " --after=%d"
                (truncate (float-time (date-to-time helm-mu-contacts-after))))
              "| sed -n '/@/s/\\([^\t]*\\)\t\\([^\t]*\\).*/\\2 <\\1>/p'"
              "| egrep -v ' <logwatch@| <buzz|@txt.voice.google|@plus.google.com'")))
    (cdr (split-string (shell-command-to-string cmd) "\n"))))

(setq jsm/my-contacts (jsm/helm-mu-contacts-init))

(defun jsm/complete-address ()
  "Complete address at point if possible"
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'symbol)))
         (eoh ;; end-of-headers
             (save-excursion
               (goto-char (point-min))
               (search-forward-regexp mail-header-separator nil t))))
    (if (and bounds
               (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p)))
      (let* ((text (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (address (helm-comp-read "Address: " 'jsm/my-contacts
                                      :initial-input text)))
        (delete-region (car bounds) (cdr bounds))
        (insert address))
      (insert "\t"))))

(define-key message-mode-map (kbd "<tab>") 'jsm/complete-address)

;; TODO: Find a key to bind helm-mu to for advanced email searching / narrowing

;; Shell
(require 'helm-eshell)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))
