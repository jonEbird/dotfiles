;;; ivy_config --- Summary

;;; Commentary:

;; Enabling Ivy mode
;; ------------------------------

;;; Code:

;; recentf integration already exists by-default in the 'ivy-switch-buffer
;; Just creating an explicit call for recentf file opens
(defun ivy-recentf-open ()
  "Use `ivy-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ivy-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(use-package ivy
  :custom ((projectile-completion-system 'ivy)
           (magit-completing-read-function 'ivy-completing-read)
           (mu4e-completing-read-function 'ivy-completing-read)
           (counsel-find-file-at-point t)
           (ivy-use-selectable-prompt t))
  :config (progn
            (ivy-mode 1)
            (ivy-set-sources
             'counsel-find-file
             '((original-source)
               (recentf-subset)
               (my-config-files))))
  :bind (("<f9>" . ivy-resume)
         ("C-x C-r" . ivy-recentf-open)
         ("C-x C-f" . counsel-find-file)
         ("M-i" . counsel-imenu)
         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)
         ("TAB" . ivy-alt-done)
         ("C-j" . ivy-partial-or-done)
         :map org-mode-map
         ("C-c h i" . counsel-org-goto)))

;; Swap TAB and C-j. The `ivy-alt-done' will immediately select the current
;; target and continue wheras the `ivy-partial-or-done' will only take it
;; if it is the only remaining selection. Essentially, Abo thinks you
;; should get used to used C-j when you've decided what to select but I
;; still like TAB for that, so I'm switching these.
;; (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)
;; (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-partial-or-done)


;; (defun jsm/swipe-headers ()
;;   (interactive)
;;   (swiper "^\* "))
;; (define-key org-mode-map (kbd "C-c h i") 'jsm/swipe-headers)

;; With Ivy v0.10.0 there is a new function I can use
; (define-key org-mode-map (kbd "C-c h i") 'counsel-org-goto)

;; The bindings for this function is set with the rest of the projectile config
(defun jsm/projectile-counsel-ag ()
  "Run counsel-ag within projectile root while ignoring git submodule paths."
  (interactive)
  (let ((default-directory (projectile-project-root))
        (ignore-cmd "git config --file .gitmodules --get-regexp path | awk '{ print $2 }' > .agignore"))
    (shell-command agignore-cmd nil nil)
    (counsel-ag (thing-at-point 'symbol) default-directory)))

(defun jsm/projectile-counsel-rg ()
  "Run counsel-rg within projectile root while ignoring git submodule paths."
  (interactive)
  (let ((default-directory (projectile-project-root))
        (ignore-cmd "git config --file .gitmodules --get-regexp path | awk '{ print $2 }' > .ignore"))
    (shell-command ignore-cmd nil nil)
    (counsel-rg (thing-at-point 'symbol) default-directory "-S -U")))

;; Switching and opening files should always include recentf values
;; (kbd "C-x b")
;; (kbd "C-x C-f")
(defun recentf-subset ()
  (cl-subseq recentf-list 0 30))

(defun my-config-files ()
  "List my elisp config files."
  (file-expand-wildcards (expand-file-name "~/.emacs.d/my_configs/*.el") t))

;; Better command issuing
;; ------------------------------
(use-package smex
  :bind ("M-x" . counsel-M-x))

;; Via Ben Maughan on mu4e ML
;; Based on http://kitchingroup.cheme.cmu.edu/blog/2015/03/14/A-helm-mu4e-contact-selector/
(defun ivy-select-and-insert-contact (&optional start)
  (interactive)
  (let ((mail-abbrev-mode-regexp mu4e~compose-address-fields-regexp)
        (eoh ;; end-of-headers
         (save-excursion
           (goto-char (point-min))
           (search-forward-regexp mail-header-separator nil t))))
    (when (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
      (let* ((end (point))
             (start
              (or start
                  (save-excursion
                    (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                    (goto-char (match-end 0))
                    (point))))
             (contact
              (ivy-read "Contact: "
                        mu4e~contacts-for-completion
                        :re-builder #'ivy--regex
                        :sort nil
                        :initial-input (buffer-substring-no-properties start end))))
        (unless (equal contact "")
          (kill-region start end)
          (insert contact))))))

(provide 'ivy-config)
;;; ivy_config.el ends here
