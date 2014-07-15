; -*- emacs-lisp -*-

(require 'erc)
(require 'netrc)

; Set which modules to load
(setq erc-modules '( pcomplete autojoin button completion fill
                               irccontrols list log match menu move-to-prompt
                               netsplit networks noncommands readonly ring stamp track))

(setq erc-echo-notices-in-minibuffer-flag t)
; Ohio Linux Fest planning @ irc.oftc.net #ohiolinux on Thursdays at 20:00 EST

; Logging the sessions
(setq erc-log-channels-directory "~/.erc/logs/"
      erc-save-buffer-on-part t
      erc-hide-list nil ; '("JOIN" "PART" "QUIT")
      jonebird-irc-creds (netrc-machine (netrc-parse "~/.netrc.gpg") "jonebirdirc")
      erc-server-flood-margin 1000
      )

; Assign the common C-x C-s to act as you'd expect in erc mode
(define-key erc-mode-map (kbd "C-x C-s") 'erc-save-buffer-in-logs)

(add-hook 'erc-mode-hook 'turn-on-flyspell 'append)

;; Improving the display output of text.
(defun my-erc-ansi-colors ()
  ; First need to remove some control characters
  (while (re-search-forward "[]" nil t)
    (replace-match "" nil nil))
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook 'erc-insert-modify-hook 'my-erc-ansi-colors 'append)

; Nick notify-send Notification
; Modified from http://www.emacswiki.org/emacs/ErcOSD
(defun jsm/xml-escape (s)
  (setq s (replace-regexp-in-string "'" "&apos;"
  (replace-regexp-in-string "\"" "&quot;"
  (replace-regexp-in-string "&" "&amp;"
  (replace-regexp-in-string "<" "&lt;"
  (replace-regexp-in-string ">" "&gt;" s)))))))

(defun jsm/notify-send (summary msg)
  "Display a message msg using notify-send."
  (save-window-excursion
    (shell-command
     (format
      "notify-send -a IRC -u low \"%s\" \"%s\"" summary msg))))

(defun jsm/erc-notify-send (matched-type nick msg)
  (interactive)
  "Hook to add into erc-text-matched-hook in order to remind the user that a message from erc has come their way."
  (when (and (string= matched-type "current-nick") (string-match "\\([^:]*\\).*:\\(.*\\)" msg))
    (let(
   (text (match-string 2 msg))
	 (from (erc-extract-nick nick)))
      (when text
	(let ((maxlength 128))
	  (if ( > (length msg) maxlength )
	      (setq msg (concat (substring msg 0 20) ".. *snip* .. " (substring msg (- 30)) "."))))
	(jsm/notify-send (format "IRC-%s" from) msg))
      )))

; (add-hook 'erc-text-matched-hook 'jsm/erc-notify-send)
(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

; Typical connection
(defun irc-work ()
  (interactive)
  (erc-tls :server "localhost" :port 6667 :full-name (netrc-get jonebird-irc-creds "login")
           :password (netrc-get jonebird-irc-creds "password") :nick "jonEbird")
  )
(defun irc-home ()
  (interactive)
  (erc-tls :server "jonebird.com" :port 6667 :full-name (netrc-get jonebird-irc-creds "login")
           :password (netrc-get jonebird-irc-creds "password") :nick "jon")
  )
(defun irc-qualcomm ()
  (interactive)
  (erc-tls :server "chat-irc.qualcomm.com" :port 9999 :full-name "Jon Miller"
           :password "blah" :nick "jsmiller"))
