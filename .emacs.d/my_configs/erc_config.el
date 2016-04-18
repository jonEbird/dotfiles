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
      jonebird-irc-creds nil
      erc-server-flood-margin 1000
      )

;; Retrieve my irc credentials
(defun jsm/set-irc-creds ()
  (unless jonebird-irc-creds
    (setq jonebird-irc-creds (netrc-machine (netrc-parse "~/.netrc.gpg") "jonebirdirc"))))

; Assign the common C-x C-s to act as you'd expect in erc mode
(define-key erc-mode-map (kbd "C-x C-s") 'erc-save-buffer-in-logs)

(add-hook 'erc-mode-hook 'turn-on-flyspell 'append)

;; Improving the display output of text.
(defun my-erc-ansi-colors ()
  ; First need to remove some control characters
  (while (re-search-forward (format "[%s]" (kbd "C-o")) nil t)
    (replace-match "" nil nil))
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook 'erc-insert-modify-hook 'my-erc-ansi-colors 'append)

; Colorize Nicks
; ------------------------------
; http://www.emacswiki.org/emacs/ErcNickColors

;; Pool of colors to use when coloring IRC nicks.
(setq erc-colors-list '("green" "blue" "red"
                        "dark gray" "dark orange"
                        "dark magenta" "maroon"
                        "indian red" "black" "forest green"
                        "midnight blue" "dark violet"))

;; special colors for some people
(setq erc-nick-color-alist '(("shuff" . "orange")
                             ("tim" . "OrangeRed2")
                             ("mitchelh" . "orange")
                             ))

(defun erc-get-color-for-nick (nick)
  "Gets a color for NICK. If NICK is in erc-nick-color-alist, use that color, else hash the nick and use a random color from the pool"
  (or (cdr (assoc nick erc-nick-color-alist))
      (nth
       (mod (string-to-number
             (substring (md5 (downcase nick)) 0 6) 16)
            (length erc-colors-list))
       erc-colors-list)))

(defun erc-put-color-on-nick ()
  "Modifies the color of nicks according to erc-get-color-for-nick"
  (save-excursion
    (goto-char (point-min))
    (while (forward-word 1)
      (setq bounds (bounds-of-thing-at-point 'word))
      (setq word (buffer-substring-no-properties
                  (car bounds) (cdr bounds)))
      (when (or (and (erc-server-buffer-p) (erc-get-server-user word))
                (and erc-channel-users (erc-get-channel-user word)))
        (put-text-property (car bounds) (cdr bounds)
                           'face (cons 'foreground-color
                                       (erc-get-color-for-nick word)))))))

; (add-hook 'erc-insert-modify-hook 'erc-put-color-on-nick)
(add-hook 'erc-insert-post-hook 'erc-put-color-on-nick)

; Nick notify-send Notification
; ------------------------------
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
        (jsm/notify-send (format "IRC-%s" from) msg)))))

; (add-hook 'erc-text-matched-hook 'jsm/erc-notify-send)
(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

; Typical connection
(defun irc-work ()
  (interactive)
  (jsm/set-irc-creds)
  (erc-tls :server "localhost" :port 6667 :full-name (netrc-get jonebird-irc-creds "login")
           :password (netrc-get jonebird-irc-creds "password") :nick "jonEbird"))

(defun irc-home ()
  (interactive)
  (jsm/set-irc-creds)
  (erc-tls :server "jonebird.com" :port 6667 :full-name (netrc-get jonebird-irc-creds "login")
           :password (netrc-get jonebird-irc-creds "password") :nick "jon"))

(defun irc-qualcomm ()
  (interactive)
  (jsm/set-irc-creds)
  (erc-tls :server "chat-irc.qualcomm.com" :port 9999 :full-name "Jon Miller"
           :password "blah" :nick "jsmiller"))

; https://freenode.net/irc_servers.shtml
; chat.freenode.net
(defun irc-freenode ()
  (interactive)
  (jsm/set-irc-creds)
  (erc-tls :server "chat.freenode.net" :port 6697 :full-name "Jon Miller"
           :nick "jonEbird"))
