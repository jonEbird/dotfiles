; -*- emacs-lisp -*-

; Set which modules to load
(setq erc-modules '( pcomplete autojoin button completion fill
                               irccontrols list log match menu move-to-prompt
                               netsplit networks noncommands readonly ring stamp track))

(setq erc-echo-notices-in-minibuffer-flag t)
; Ohio Linux Fest planning @ irc.oftc.net #ohiolinux on Thursdays at 20:00 EST

; Logging the sessions
(setq erc-log-channels-directory "~/.erc/logs/"
      erc-save-buffer-on-part t
      jonebird-irc-creds (netrc-machine (netrc-parse "~/.netrc.gpg") "jonebirdirc" t)
      )

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

(add-hook 'erc-text-matched-hook 'jsm/erc-notify-send)

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
