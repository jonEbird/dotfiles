;;; package --- Summary

;;; Commentary:

;;; Mac Configuration --- Summary
;;;   Setting up settings only for Macintosh

;; --------------------------------------------------
;; Now actually set the variables and more complicated setups
;; --------------------------------------------------

;;; Code:
(defun font-exists-p (font)
  "Check if FONT exists."
  (if (null (x-list-fonts font)) nil t))

(defvar jsm:hiding-others t
  "Toggle variable to control `jsm:hide-other-windows' from firing or not.")

;; TODO: Move the list of applications to NOT hide into a list and
;; dynamically (macro) insert the extra 'and not \"<name>\"' text into the
;; AppleScript
(defun jsm:hide-other-windows ()
  "Hide all other windows."
  (interactive)
  (if (eq jsm:hiding-others t)
      (do-applescript "
tell application \"System Events\"
    set visible of every process whose ¬
        visible is true and ¬
        frontmost is false and ¬
        name is not \"Jabber Video\" and ¬
        name is not \"Cisco Jabber\" ¬
    to false
end tell")))

(defun jsm:toggle-hiding ()
  "Toggle `jsm:hiding-others' variable between nil and t."
  (interactive)
  (if jsm:hiding-others
      (progn
        (setq jsm:hiding-others nil)
        (message "Disabled hiding other windows"))
    (setq jsm:hiding-others t)
    (message "Enabled hiding other windows")
    (jsm:hide-other-windows)))

;; http://qwan.org/2011/09/01/reloading-a-page-in-chrome-from-aquamacs/
(defun mayoff:open-url-in-chrome (url)
  "Open URL in Google Chrome.  I use AppleScript to do several things:

  1. I tell Chrome to come to the front.  If Chrome wasn't launched, this
     will also launch it.
  2. If Chrome has no windows open, I tell it to create one.
  3. If Chrome has a tab showing URL, I tell it to reload the tab, make
     that tab the active tab in its window, and bring its window to the front.
  4. If Chrome has no tab showing URL, I tell Chrome to make a new tab (in
     the front window) showing URL."
  (when (symbolp url)
    ;; User passed a symbol instead of a string.  Use the symbol name.
    (setq url (symbol-name url)))
  (do-applescript (format "
tell application \"Google Chrome\"
        activate
        set theUrl to %S

        if (count every window) = 0 then
                make new window
        end if

        set found to false
        set theTabIndex to -1
        repeat with theWindow in every window
                set theTabIndex to 0
                repeat with theTab in every tab of theWindow
                        set theTabIndex to theTabIndex + 1
                        if theTab's URL = theUrl then
                                set found to true
                                exit
                        end if
                end repeat

                if found then
                        exit repeat
                end if
        end repeat

        if found then
                tell theTab to reload
                set theWindow's active tab index to theTabIndex
                set index of theWindow to 1
        else
                tell window 1 to make new tab with properties {URL:theUrl}
        end if
end tell
  " url)))

(when (eq system-type 'darwin)
  (message "Setting up specific settings for Mac")

  ;; Consider the following setting if you end up not using
  ;; `exec-path-from-shell-copy-env'
  ;; (add-to-list 'exec-path "/usr/local/bin")

  ;; (setq browse-url-generic-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
  ;;       browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program nil
        browse-url-browser-function 'browse-url-default-browser)

  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'control)

  ;; Every other Mac program supports a "hide others" command and that is
  ;; being captured by Emacs. Binding the same, normal keyboard to a
  ;; applescript program to do the same.
  (global-set-key (kbd "M-s-h") 'jsm:hide-other-windows)
  (add-hook 'focus-in-hook 'jsm:hide-other-windows)

  ;; Have a big monitor but still like a vertical split
  (setq split-height-threshold nil
        split-width-threshold 160)

  ;; Better mouse scrolling
  (setq mouse-wheel-scroll-amount '(0.01))

  ;; (setq org-file-apps '((auto-mode . emacs)
  ;;       		("\\.mm\\'" . default)
  ;;       		("\\.x?html?\\'" . default)
  ;;       		("\\.pdf\\'" . default)
  ;;       		("\\.xlsx?" . default)
  ;;       		("\\.docx?" . default)))

  ;; Need to properly set a Font
  ;; (set-face-attribute 'default nil
  ;;       	      :family "Consolas" :height 130)
  ;; (cond
  ;;  ((find-font (font-spec :name "DejaVu Sans Mono"))
  ;;   (set-frame-font "DejaVu Sans Mono-13"))
  ;;  ((find-font (font-spec :name "Monaco"))
  ;;   (set-frame-font "monaco-13"))
  ;;  ((find-font (font-spec :name "inconsolata"))
  ;;   (set-frame-font "inconsolata-16"))
  ;;  ((find-font (font-spec :name "Lucida Console"))
  ;;   (set-frame-font "Lucida Console-12"))
  ;;  ((find-font (font-spec :name "courier"))
  ;;   (set-frame-font "courier-12")))

  ;; (set-face-attribute 'default nil
  ;;                     :family "Monaco"
  ;;                     :height 130)
  ;; (set-face-attribute 'default nil
  ;;                     :family "Inconsolata"
  ;;                     :height 160)
  ;; (set-face-attribute 'default nil
  ;;                     :family "Menlo"
  ;;                     :height 140)
  )

(font-exists-p "Inconsolata")

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; nil for dark text
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(provide 'mac_config)
;;; mac_config.el ends here
