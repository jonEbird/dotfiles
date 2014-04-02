(require 'mu4e)
(require 'org-mu4e)

; Not specifically needed for mu4e but helpful to set
(setq
 user-mail-address  "jsmiller@qti.qualcomm.com"
 user-full-name     "Jon Miller"
 mail-user-agent    'mu4e-user-agent
 mu4e-user-mail-address-list '("jsmiller@qti.qualcomm.com" "jonEbird@gmail.com"))

;; These are actually the defaults
(setq
 mu4e-maildir       "~/Maildir"   ;; top-level Maildir
 mu4e-sent-folder   "/Sent"       ;; folder for sent messages
 mu4e-drafts-folder "/Drafts"     ;; unfinished messages
 mu4e-trash-folder  "/Trash"      ;; trashed messages
 mu4e-refile-folder "/Archives")  ;; saved messages

(defun file-string (file)
  "Read the contents of a file and return as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defvar my-mu4e-account-alist
  '(("Qualcomm"
     (mu4e-sent-folder "/Qualcomm/Sent Items")
     (mu4e-drafts-folder "/Qualcomm/Drafts")
     (mu4e-trash-folder "/Qualcomm/Deleted Items")
     (mu4e-refile-folder "/Qualcomm/Archives")
     (user-mail-address "jsmiller@qti.qualcomm.com")
     (mu4e-compose-signature (file-string "~/.Qualcomm-sig.txt")))
    ("Gmail"
     (mu4e-sent-folder "/Gmail/Sent Items")
     (mu4e-drafts-folder "/Gmail/Drafts")
     (mu4e-trash-folder "/Gmail/Trash")
     (mu4e-refile-folder "/Gmail/Archives")
     (user-mail-address "jonEbird@gmail.com")
     (mu4e-compose-signature (file-string "~/.Gmail-sig.txt")))
    ))

(setq
 mu4e-get-mail-command            "offlineimap"  ;; calling offlineimap separately
 mu4e-update-interval             120            ;; Not needed with offlineimap hooks
 mu4e-use-fancy-chars             t              ;; Pretty symbols in the view
 mu4e-view-show-images            t              ;; Show images inline
 mu4e-view-image-max-width        800            ;; Limit too big photos
 mu4e-view-prefer-html            t              ;; I get a lot of HTML emails
 mu4e-compose-dont-reply-to-self  t              ;; Do not include myself in replies
 ;mu4e-html2text-command "w3m -dump -S -cols 100 -T text/html" ;; Good text representation
 ;mu4e-html2text-command "html2text -utf8 -width 72"
 mu4e-html2text-command           'html2text
 org-mu4e-convert-to-html         t              ;; Oh yeah, exactly what I wanted
 mu4e-attachment-dir              "~/Downloads"  ;; Match browser default
 mu4e-headers-skip-duplicates     t              ;; Eliminate Gmail dups
 mu4e-headers-show-threads        nil            ;; Keep non-threaded by default 'P' to change
 mu4e-hide-index-messages         t              ;; No messages in mini-buffer about updates
 )
;; mu4e-hide-index-messages - set once you've updated mu4e

(setq mu4e-headers-fields
      '( (:human-date    .  13)
         (:flags         .   6)
         (:maildir       .  30)
         (:from-or-to    .  22)
         (:subject       .  nil)))
;          (:mailing-list  .  15)

;; https://groups.google.com/forum/#!topic/mu-discuss/xlZegBifdaA
(defun html2text ()
  "Replacement for standard html2text using shr."
  (interactive)
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

(defun jsm:shr-browse-url ()
  "For mu4e messages, browse the URL under point or advance the message"
  (interactive)
  (let ((url (get-text-property (point) 'shr-url)))
    (if (not url)
        (mu4e-scroll-up)
      (shr-browse-url))))
(define-key mu4e-view-mode-map (kbd "RET") 'jsm:shr-browse-url)

(defun jsm:mailing-list-mode (&optional enable)
  "Change the headers-fields to include mailing-list and enable
threading when enabling mailing-list-mode. Go back to
non-threaded and regular headers when disabling"
  (interactive "P")
  (if enable
      (setq mu4e-headers-fields
            '( (:human-date    .  13)
               (:flags         .   6)
               (:mailing-list  .  30)
               (:from-or-to    .  22)
               (:subject       .  nil))
            mu4e-headers-show-threads t)
    (setq mu4e-headers-fields
          '( (:human-date    .  13)
             (:flags         .   6)
             (:maildir       .  30)
             (:from-or-to    .  22)
             (:subject       .  nil))
          mu4e-headers-show-threads nil))
  )

(defun jsm:gmail-mailing-lists ()
  "Toggle between going to my bookmarked Gmail mailing list
collection while also enabling my mailing-list viewing mode or
disable my mailing-list viewing mode and returning to previous
query"
  (interactive)
  (if (not (boundp 'jsm:ml-mode)) (setq jsm:ml-mode nil))
  (if jsm:ml-mode
      (progn
        (setq jsm:ml-mode nil)
        (jsm:mailing-list-mode nil)
        (mu4e-headers-query-prev))
    (progn
      (setq jsm:ml-mode t)
      (jsm:mailing-list-mode t)
      (mu4e-headers-search-bookmark "m:/Gmail/INBOX and list:* and flag:unread"))
    ))

(define-key mu4e-headers-mode-map (kbd "G") 'jsm:gmail-mailing-lists)


(defun jsm:narrow-to-mailing-list ()
  "Filter the list of mail based on current mailing list of
  message at point. If already narrowed, remove filter."
  (interactive)
  (if (not (boundp 'jsm:narrowed-ml)) (setq jsm:narrowed-ml nil))
  (let ((ml (mu4e-message-field-at-point :mailing-list)))
    (if ml
        (if jsm:narrowed-ml
            (progn
              (setq jsm:narrowed-ml nil)
              (mu4e-headers-query-prev))
          (setq jsm:narrowed-ml ml)
          (mu4e-headers-search-narrow ml)))
    (message "Message is not a mailing-list email")))

(define-key mu4e-headers-mode-map (kbd "L") 'jsm:narrow-to-mailing-list)

;; I like being able to use C-Return to also send a message
(define-key mu4e-compose-mode-map [C-return] 'message-send-and-exit)

;; Hit 'a' then 'V' to view the message in an external browser
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

; My mailing lists
(setq mu4e-user-mailing-lists
      '( ("emacs-orgmode.gnu.org"           . "OrgMode")
         ("mu-discuss.googlegroups.com"     . "Mu Group")
         ("devel.lists.fedoraproject.org"   . "Fedora Devel")
         ("colug-432.colug.net"             . "COLUG")
         ("spacewalk-list.redhat.com"       . "Spacewalk")
         ("mu.djcb.github.com"              . "Mu Github")
         ("PythonSD-list.meetup.com"        . "Python SD")
         ("kplug-list.kernel-panic.org"     . "KPLUG")
         ("linux-s390.vger.kernel.org"      . "Linux s390")
         ("linux-rt-users.vger.kernel.org"  . "Linux RT")))

(setq my-mailing-lists-filter (mapconcat (lambda (x) (concat "list:" (car x))) mu4e-user-mailing-lists " OR "))

;; When 'j'umping to a Maildir, you can set these shortcuts
(setq mu4e-maildir-shortcuts
      '(("/Qualcomm/INBOX"       .  ?q)
        ("/Qualcomm/Root Mail"   .  ?r)
        ("/Qualcomm/Sent Items"  .  ?s)
        ("/GMail/INBOX"          .  ?g)
        ("/GMail/Sent Items"     .  ?S)))

;; Here you can use the full power of a "mu find" command. Try playing
;; around with the CLI version and then incorporate that search as a
;; bookmark.
(setq mu4e-bookmarks
      '( ("flag:unread AND NOT flag:trashed AND m:/Qualcomm/*"    "Unread messages"           ?u)
         ("\"Maildir:/Qualcomm/INBOX\""                           "Qualcomm Inbox"            ?q)
         ("m:/Gmail/INBOX and not list:*"                         "Gmail Inbox (no groups)"   ?g)
         ("m:/Gmail/INBOX and list:*"                             "Gmail Groups"              ?G)
         ("date:today..now"                                       "Today's messages"          ?t)
         ("date:7d..now"                                          "Last 7 days"               ?w)
         ("mime:image/*"                                          "Messages with images"      ?i)))
;; (add-to-list 'mu4e-bookmarks (list (concat "Maildir:/Gmail/INBOX AND NOT (" my-mailing-lists-filter ")") "Gmail Inbox (no groups)" ?g) t)
;; (add-to-list 'mu4e-bookmarks (list (concat "Maildir:/Gmail/INBOX AND (" my-mailing-lists-filter ")")     "Gmail Groups"            ?G) t)

; Sending Email - Using msmtp
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp")

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;;-Helping-Functions--------------------------------

;; Choose account label to feed msmtp -a option based on From header in Message buffer;
;; This function must be added to message-send-mail-hook for on-the-fly change of From address
;; before sending message since message-send-mail-hook is processed right before sending message.
(defun cg-feed-msmtp ()
  (if (message-mail-p)
      (save-excursion
	(let* ((from
		(save-restriction
		  (message-narrow-to-headers)
		  (message-fetch-field "from")))
	       (account
		(cond
		 ;; I use email address as account label in ~/.msmtprc
		 ((string-match "jsmiller@qti.qualcomm.com" from) "qualcomm")
		 ;; Add more string-match lines for your email accounts
		 ((string-match "jonEbird@gmail.com" from) "gmail"))))
	  (setq message-sendmail-extra-arguments (list '"-a" account))))))

(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'cg-feed-msmtp)

;; Select the correct account based on email message and prompt for which
;; account to use when composing a new message
(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(defun my-mu4e-compose-settings ()
  "Set some custom variables when composing an email"
  (progn
    (setq
     fill-column 77) ; Was set to 99 before
    ))
(add-hook 'mu4e-compose-mode-hook 'my-mu4e-compose-settings)

;; Awesome way to mark & attach files to a new email message. Mark the
;; file(s) in dired you would like to attach and press C-c RET C-a, and
;; you'll be asked whether to attach them to an existing message, or create
;; a new one.
(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
	(set-buffer buffer)
	(when (and (derived-mode-p 'message-mode)
		(null message-sent-message-via))
	  (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; My way to switch between my email window configuration and other ongoing work
(defun switch-between-mu4e ()
  "Either switch to mu4e or return to previous window configuration"
  (interactive)
  ; They all start with "mu4e": mu4e-main-mode, mu4e-headers-mode, mu4e-view-mode
  (if (string= "mu4e" (substring (symbol-name major-mode) 0 4))
      (progn
        (window-configuration-to-register ?m nil)
        (unless (ignore-errors (jump-to-register ?e))
          (find-file (expand-file-name "~/org/projects.org"))))
    (progn
      (window-configuration-to-register ?e nil)
      (delete-other-windows)
      (unless (ignore-errors (jump-to-register ?m))
        (mu4e)))))
(global-set-key (kbd "<f11>") 'switch-between-mu4e)

(defun compose-reply-spacing ()
  "Create two newlines worth of spacing for replying to emails in
internet style formatting. Also move cursor to the first reply
location."
  (interactive)
  (progn
    (save-excursion
      (replace-regexp "^\\(> .*
> .*
\\)>>" "\\1
" nil))
    (search-forward-regexp "^> .*

" nil t)))
(add-hook 'mu4e-compose-mode-hook 'compose-reply-spacing)

; Next two functions are courtesy of sabof https://github.com/djcb/mu/issues/128
(defun mu4e-headers-mark-all-unread-read ()
  "Put a ! \(read) mark on all visible unread messages"
  (interactive)
  (mu4e-headers-mark-for-each-if
   (cons 'read nil)
   (lambda (msg param)
     (memq 'unread (mu4e-msg-field msg :flags)))))

(defun mu4e-headers-flag-all-read ()
  "Flag all visible messages as \"read\""
  (interactive)
  (mu4e-headers-mark-all-unread-read)
  (mu4e-mark-execute-all t))
(defalias 'mark-all-read 'mu4e-headers-flag-all-read)

; Use the helper functions to mark all read in annoying maildirs
(defun jsm:mu4e-mark-noisy-maildirs-all-read ()
  "Mark all read for some of my noisy maildirs such as Root Email"
  (interactive)
  (let ((noisy-maildirs (list "/Qualcomm/Root Mail" "/Qualcomm/Projects.Spacewalk"))
        (mu4e-headers-leave-behavior 'ask))
    (mapc (lambda (maildir)
            (save-window-excursion (mu4e~headers-jump-to-maildir maildir)
                                   (mu4e-headers-flag-all-read)
                                   ; (mu4e-mark-handle-when-leaving)
                                   (mu4e-mark-execute-all t)
                                   ; (sleep-for 2)
                                   (mu4e-headers-query-prev)))
          noisy-maildirs)))
(define-key mu4e-headers-mode-map (kbd "A") 'jsm:mu4e-mark-noisy-maildirs-all-read)

(if (not (functionp 'delete-all-overlays))
  (defun delete-all-overlays ()
    (remove-overlays))
  )

;; Abbrev for mu4e composing - Only used once and then used M-x edit-abbrevs
;; (define-abbrev-table 'mu4e-compose-mode-abbrev-table
;;   '(
;;     ("nickname" "\"Lastname, Firstname\" <nickname@nowhere.com>" nil 0)
;;     ))

; https://groups.google.com/forum/#!topic/mu-discuss/VRt6ZIegrrM
(defun jmg/ido-select-recipient ()
  "Inserts a contact from the mu cache. Uses ido to select the
contact from all those present in the database."
  (interactive)
  (insert
   (ido-completing-read
    "Recipient: "
    (mapcar (lambda (contact-string)
              (let* ((data (split-string contact-string ","))
                     (name (when (> (length (car data)) 0)
                             (car data)))
                     (address (cadr data)))
                (if name
                    (format "%s <%s>" name address)
                  address)))
            (remove-if (lambda (string) (= 0 (length string)))
                       (split-string (shell-command-to-string "mu cfind --format=csv") "\n"))))))
(define-key message-mode-map (kbd "C-c t") 'jmg/ido-select-recipient)

; Notes on using mbsync
;  Need to set mu4e-change-filenames-when-moving to t
;  Sample mbsync config(s)
;   https://groups.google.com/d/msg/mu-discuss/AhgmBAcv-ww/vgWKlBmxXsMJ

;; mu4e TODO
; crypto - http://www.djcbsoftware.nl/code/mu/mu4e/MSGV-Crypto.html#MSGV-Crypto
; org-mode emails - http://www.djcbsoftware.nl/code/mu/mu4e/Rich_002dtext-messages-with-org_002dmode.html#Rich_002dtext-messages-with-org_002dmode
; notifications - http://www.djcbsoftware.nl/code/mu/mu4e/Getting-new-mail-notifications-with-Sauron.html#Getting-new-mail-notifications-with-Sauron
; Tweak citation - http://www.djcbsoftware.nl/code/mu/mu4e/Citations-with-mu_002dcite.html#Citations-with-mu_002dcite
; DONE - Calendaring - http://doughellmann.com/2007/10/working-with-imap-and-icalendar-2.html - doesn't work with our Exchange server
; RPM packaging - http://pastebin.com/5Ja8SJsB
