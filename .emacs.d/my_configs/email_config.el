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

(defvar my-mu4e-account-alist
  '(("Qualcomm"
     (mu4e-sent-folder "/Qualcomm/Sent Items")
     (mu4e-drafts-folder "/Qualcomm/Drafts")
     (mu4e-trash-folder "/Qualcomm/Deleted Items")
     (mu4e-refile-folder "/Qualcomm/Archives")
     (user-mail-address "jsmiller@qti.qualcomm.com")
     (message-signature-file "~/.Qualcomm-sig.txt"))
    ("Gmail"
     (mu4e-sent-folder "/Gmail/Sent Items")
     (mu4e-drafts-folder "/Gmail/Drafts")
     (mu4e-trash-folder "/Gmail/Trash")
     (mu4e-refile-folder "/Gmail/Archives")
     (user-mail-address "jonEbird@gmail.com")
     (message-signature-file "~/.Gmail-sig.txt"))))

(setq
 mu4e-get-mail-command            "offlineimap"  ;; calling offlineimap separately
 mu4e-update-interval             300            ;; Not needed with offlineimap hooks
 mu4e-use-fancy-chars             t              ;; Pretty symbols in the view
 mu4e-view-show-images            t              ;; Show images inline
 mu4e-view-image-max-width        800            ;; Limit too big photos
 mu4e-view-prefer-html            t              ;; I get a lot of HTML emails
 mu4e-compose-dont-reply-to-self  t              ;; Do not include myself in replies
 mu4e-html2text-command "w3m -dump -T text/html" ;; Good text representation
 org-mu4e-convert-to-html         t              ;; Oh yeah, exactly what I wanted
 mu4e-attachment-dir              "~/Downloads"  ;; Match browser default
 )

(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(setq mu4e-maildir-shortcuts
      '(("/Qualcomm/INBOX" . ?Q)
        ("/GMail/INBOX"    . ?g)))

(setq mu4e-bookmarks
      '( ("flag:unread AND NOT flag:trashed" "Unread messages"      ?u)
         ("\"Maildir:/Qualcomm/INBOX\""      "Qualcomm Inbox"       ?q)
         ("\"Maildir:/Gmail/INBOX\""         "Gmail Inbox"          ?g)
         ("date:today..now"                  "Today's messages"     ?t)
         ("date:7d..now"                     "Last 7 days"          ?w)
         ("mime:image/*"                     "Messages with images" ?i)))

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
        (window-configuration-to-register 109 nil)
        (jump-to-register 101))
    (progn
      (window-configuration-to-register 101 nil)
      (delete-other-windows)
      (if (string= "Register a is empty" (view-register 109))
          (mu4e)
        (jump-to-register 109)))))
(global-set-key (kbd "<f11>") 'switch-between-mu4e)

;; mu4e TODO
; crypto - http://www.djcbsoftware.nl/code/mu/mu4e/MSGV-Crypto.html#MSGV-Crypto
; org-mode emails - http://www.djcbsoftware.nl/code/mu/mu4e/Rich_002dtext-messages-with-org_002dmode.html#Rich_002dtext-messages-with-org_002dmode
; notifications - http://www.djcbsoftware.nl/code/mu/mu4e/Getting-new-mail-notifications-with-Sauron.html#Getting-new-mail-notifications-with-Sauron
; Tweak citation - http://www.djcbsoftware.nl/code/mu/mu4e/Citations-with-mu_002dcite.html#Citations-with-mu_002dcite
; Calendaring - http://doughellmann.com/2007/10/working-with-imap-and-icalendar-2.html
; RPM packaging - http://pastebin.com/5Ja8SJsB
