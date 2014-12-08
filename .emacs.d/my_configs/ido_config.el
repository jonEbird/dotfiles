
;; Enabling ido Mode
;; ------------------------------
; Used http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/ for guidelines.
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess
      ido-create-new-buffer 'always
      ido-save-directory-list-file (concat (expand-file-name "~/.ido.last.") hostname)
      ido-file-extensions-order '(".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf")
      ido-ignore-buffers '("\\` " "^*mu4e-"))
(add-to-list 'ido-ignore-files "\\.emacs\\.desktop")
(ido-mode 1)

;; I can never remember which key to split vertically and horizontally as
;; well as thinking of it the opposite way
(global-set-key (kbd "C-x |") (lambda () (interactive) (split-window-right) (ido-switch-buffer-other-window)))
(global-set-key (kbd "C-x _") (lambda () (interactive) (split-window-below) (ido-switch-buffer-other-window)))

;; ido-imenu - More navigation help
;; ------------------------------
(require 'idomenu)
(global-set-key (kbd "C-x C-i") 'idomenu)

;; Improved IDO match support
;; ------------------------------
(require 'flx-ido)
(flx-ido-mode 1)
;; Disable default ido faces to see flx highlights
(setq ido-use-faces nil)
; Finally, with the improved highlighting, it is also nice to view matches vertically
(ido-vertical-mode)

;; Better command issuing
;; ------------------------------
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
