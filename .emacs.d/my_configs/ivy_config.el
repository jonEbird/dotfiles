;;; ivy_config --- Summary

;;; Commentary:

;; Enabling Ivy mode
;; ------------------------------

;;; Code:

(require 'ivy)

(ivy-mode 1)

;; Some standard key bindings
(global-set-key (kbd "<f9>") 'ivy-resume)
(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)

;; Integration with other Modes
(setq projectile-completion-system 'ivy
      magit-completing-read-function 'ivy-completing-read
      mu4e-completing-read-function 'ivy-completing-read)

;; recentf integration already exists by-default in the 'ivy-switch-buffer
;; Just creating an explicit call for recentf file opens
(defun ivy-recentf-open ()
  "Use `ivy-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ivy-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ivy-recentf-open)

;; Counsel Setting
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; ;; Found the helm-org-*-headings functions via comments discussion at:
;; ;; http://irreal.org/blog/?p=4170
;; (defun helm-org-in-buffer-headings ()
;;   "Preconfigured helm for org buffer headings."
;;   (interactive)
;;   (let ((helm-org-headings--nofilename t))
;;     (helm :sources (helm-source-org-headings-for-files
;;                     (list (current-buffer)))
;;           :candidate-number-limit 99999
;;           :buffer "*helm org inbuffer*")))

;; ;; http://irreal.org/blog/?p=4170
;; (defun helm-org-search-headers (arg)
;;   (interactive "P")
;;   (cond ((equal arg nil) (call-interactively 'helm-semantic-or-imenu))
;;         ((equal arg '(4)) (helm-org-in-buffer-headings))
;;         ((equal arg '(16)) (helm-org-agenda-files-headings))))
;; (define-key org-mode-map (kbd "C-c h i") 'helm-org-search-headers)

;; (org-map-region (lambda () t) (point-min) (point-max))
;; (call (lambda () t))


;; (ivy-read "Heading:"
;;           (org-element-map (org-element-parse-buffer) 'headline (lambda (hl) hl))
;;           )
;; (setq org-goto-max-level 5)

;; (cfunc (if (and org-refile-use-outline-path
;;                 org-outline-path-complete-in-steps)
;;            'org-olpath-completing-read
;;          'org-icompleting-read))


;; (org-element-map
;;     (org-element-parse-buffer 'headline) 'headline
;;   (lambda (headline)
;;     (let ((title  (org-element-property :title headline)))
;;       title)))

;; (defun helm-org-search-headers (arg)
;;   (interactive "P")
;;   (cond ((equal arg nil) (call-interactively 'helm-semantic-or-imenu))
;;         ((equal arg '(4)) (helm-org-in-buffer-headings))
;;         ((equal arg '(16)) (helm-org-agenda-files-headings))))
;; (define-key org-mode-map (kbd "C-c h i") 'helm-org-search-headers)

(defun jsm/swipe-headers ()
  (interactive)
  (swiper "^\* "))

(define-key org-mode-map (kbd "C-c h i") 'jsm/swipe-headers)

(define-key projectile-command-map (kbd "s s")
  (lambda () (interactive) (counsel-ag nil (projectile-project-root))))

;; Better command issuing
;; ------------------------------
(require 'smex)
(global-set-key (kbd "M-x") 'counsel-M-x)

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
