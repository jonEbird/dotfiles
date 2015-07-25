(require 'mu4e)
(require 'org-mu4e)
(require 'mu4e-contrib)

; Main email configuration is all keyed off this alist of account information
;  First account values are automatically set and then you are prompted to
;  select other accounts when composing

(defvar my-mu4e-account-alist
  '(("Qualcomm"
     (msmtp-account "qualcomm")
     (mu4e-sent-folder "/Qualcomm/Sent Items")
     (mu4e-drafts-folder "/Qualcomm/Drafts")
     (mu4e-trash-folder "/Qualcomm/Deleted Items")
     (mu4e-refile-folder "/Qualcomm/Archives")
     (user-mail-address "jsmiller@qti.qualcomm.com")
     (user-full-name "Jon Miller")
     (mu4e-compose-signature-auto-include nil)
     (message-signature-file "~/.Qualcomm-sig.txt")
     (message-cite-reply-position above)
     (message-cite-style message-cite-style-outlook))
    ("Gmail"
     (msmtp-account "gmail")
     (mu4e-sent-folder "/Gmail/Sent Items")
     (mu4e-drafts-folder "/Gmail/Drafts")
     (mu4e-trash-folder "/Gmail/Trash")
     (mu4e-refile-folder "/Gmail/Archives")
     (user-mail-address "jonEbird@gmail.com")
     (user-full-name "Jon Miller")
     (mu4e-compose-signature-auto-include nil)
     (message-signature-file "~/.Gmail-sig.txt")
     (message-cite-reply-position below)
     (message-cite-style message-cite-style-gmail))
    ))

; (setq mu4e-html2text-command 'mu4e-shr2text) ; TODO: enable this with mu4e update

(setq
 mu4e-maildir                     "~/Maildir"    ;; top-level Maildir
 mu4e-get-mail-command            "true"         ;; calling offlineimap separately
 mu4e-update-interval             120            ;; Not needed with offlineimap hooks
 mu4e-use-fancy-chars             nil            ;; Pretty symbols in the view
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
 message-kill-buffer-on-exit      t              ;; Don't keep around messages
 message-sendmail-envelope-from   'header
 mail-interactive                 nil            ;; quiets msmtp false-positive errors
 message-interactive              nil            ;; quiets msmtp false-positive errors
 )

; Default headers used in normal mode
(setq mu4e-headers-fields
      '( (:human-date    .  13)
         (:flags         .   6)
         (:from-or-to    .  22)
         (:thread-subject       .  nil)))

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

; Support imagemagick types
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; Hit 'a' then 'V' to view the message in an external browser
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

; Sending Email - Using msmtp
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp")

; By default, set the values for the first account specified in `my-mu4e-account-alist'
(mapc #'(lambda (var)
          (set (car var) (cadr var)))
      (cdar my-mu4e-account-alist))

(setq mu4e-user-mail-address-list
      (mapcar (lambda (account) (cadr (assq 'user-mail-address account))) my-mu4e-account-alist))

;; Prevent mail buffers from being included in project lists
(if (boundp 'projectile-mode)
    (add-to-list 'projectile-globally-ignored-modes "mu4e.*"))

;-Viewing-Email------------------------------------

;; https://groups.google.com/forum/#!topic/mu-discuss/xlZegBifdaA
(defun html2text ()
  "Replacement for standard html2text using shr."
  (interactive)
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

;; Orig: "\\(\\(https?\\://\\|mailto:\\)[-+[:alnum:].?_$%/+&#@!*~,:;=/()]+\\)"
; https://github.com/djcb/mu/issues/408
(defconst mu4e~view-url-regexp
  "\\(\\(https?\\://\\|mailto:\\)[-+\[:alnum:\].?_$%/+&#@!*~,:;=/()]*[-+\[:alnum:\].?_$%/+&#@!*~,:;=/]\\)"
  "Regexp that matches http:/https:/mailto: URLs; match-string 1 will contain the matched URL, if any.")

;-Composing-Email----------------------------------

; Reset global C-x m to mu4e variant that will prompt for specifc account
(global-set-key (kbd "C-x m") 'mu4e-compose-new)

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


; Used to use a much larger function but it used hard coded email addresses
; in there. Instead have introduced a variable `msmtp-account' to track the
; specific msmtp account name to use when sending an email.
(defun cg-feed-msmtp ()
  (let* ((from
          (save-restriction
            (message-narrow-to-headers)
            (message-fetch-field "from")))
         (account-vars
          (car
           (delq 'nil
                 (mapcar (lambda (account)
                           (if (string-match (cadr (assq 'user-mail-address account)) from)
                               (cdr account)))
                         my-mu4e-account-alist)))))
    (if account-vars
        (progn
          (mapc #'(lambda (var)
                    (set (car var) (cadr var)))
                account-vars)
          (setq message-sendmail-extra-arguments (list "-a" msmtp-account))))))

(add-hook 'message-send-mail-hook 'cg-feed-msmtp)


; Be smart about inserting signature for either cite-reply-position used
(defun insert-signature ()
  "Insert signature where you are replying"
  ; Do not insert if already done - needed when switching modes back/forth
  (unless (save-excursion (message-goto-signature))
    (save-excursion
      (if (eq message-cite-reply-position 'below)
          (goto-char (point-max))
        (message-goto-body))
      (insert-file-contents message-signature-file)
      (save-excursion (insert "\n-- \n")))))

(add-hook 'mu4e-compose-mode-hook 'insert-signature)


; Final custom function for any other customization
(defun my-mu4e-compose-settings ()
  "Set some custom variables when composing an email"
  (setq fill-column 77))

(add-hook 'mu4e-compose-mode-hook 'my-mu4e-compose-settings)

;; ; Per ML https://groups.google.com/forum/#!topic/mu-discuss/XChrOWBNzO4
;; (add-hook 'mu4e-compose-mode-hook
;;           (defun cpb-compose-setup ()
;;             "Use hard newlines, so outgoing mails will have format=flowed."
;;             (use-hard-newlines t 'guess)))

;-Mailing-Lists------------------------------------

; My mailing lists
(setq mu4e-user-mailing-lists
      '( ("emacs-orgmode.gnu.org"                . "OrgMode")
         ("mu-discuss.googlegroups.com"          . "Mu Group")
         ("devel.lists.fedoraproject.org"        . "Fedora Devel")
         ("colug-432.colug.net"                  . "COLUG")
         ("spacewalk-list.redhat.com"            . "Spacewalk")
         ("mu.djcb.github.com"                   . "Mu Github")
         ("PythonSD-list.meetup.com"             . "Python SD")
         ("kplug-list.kernel-panic.org"          . "KPLUG")
         ("linux-s390.vger.kernel.org"           . "Linux s390")
         ("linux-rt-users.vger.kernel.org"       . "Linux RT")
         ("systemd-devel.lists.freedesktop.org"  . "systemd Devel")
         ("scons-users.scons.org"                . "SCons")))

(setq my-mailing-lists-filter (mapconcat (lambda (x) (concat "list:" (car x))) mu4e-user-mailing-lists " OR "))

(defun jsm/toggle-mailing-list-headers-fields (&optional enable)
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
               (:thread-subject       .  nil))
            mu4e-headers-show-threads t)
    (setq mu4e-headers-fields
          '( (:human-date    .  13)
             (:flags         .   6)
             (:from-or-to    .  22)
             (:thread-subject       .  nil))
          mu4e-headers-show-threads nil)))

(defun jsm/toggle-view-mailing-lists ()
  "Toggle between going to my bookmarked Gmail mailing list
collection while also enabling my mailing-list viewing mode or
disable my mailing-list viewing mode and returning to previous
query"
  (interactive)
  ; Initialize our state variable if first time
  (if (not (boundp 'jsm/ml-mode)) (setq jsm/ml-mode nil))
  (if jsm/ml-mode
      (progn
        (setq jsm/ml-mode nil)
        (jsm/toggle-mailing-list-headers-fields nil)
        (ignore-errors (mu4e-headers-query-prev)))
    (progn
      (setq jsm/ml-mode t)
      (jsm/toggle-mailing-list-headers-fields t)
      ; Removed the extra "and flag:unread" from the search
      (mu4e-headers-search-bookmark "m:/Gmail/INBOX AND list:* AND flag:unread")
      ; (mu4e-headers-search-narrow "flag:unread")
      )))

(define-key mu4e-headers-mode-map (kbd "G") 'jsm/toggle-view-mailing-lists)

; Support narrowing to just a select set mailing list
(defun jsm/narrow-to-mailing-list ()
  "Filter the list of mail based on current mailing list of
  message at point. If already narrowed, remove filter."
  (interactive)
  ; Initialize our state variable if first time
  (if (not (boundp 'jsm/narrowed-ml)) (setq jsm/narrowed-ml nil))
  (if jsm/narrowed-ml
      (progn
        ; TODO: Need to iterate this query-prev until ml is no longer in
        ; the current search query
        (mu4e-headers-query-prev)
        (setq jsm/narrowed-ml nil
              mu4e-headers-auto-update t))
    (let ((ml (mu4e-message-field-at-point :mailing-list)))
      (if ml
          (progn
            (mu4e-headers-search-narrow ml)
            (setq jsm/narrowed-ml ml
                  mu4e-headers-auto-update nil))
        (message "Message is not a mailing-list email")))))

(define-key mu4e-headers-mode-map (kbd "L") 'jsm/narrow-to-mailing-list)

; Thanks to a solution provided in github - https://github.com/djcb/mu/issues/516#issuecomment-73392975 
(defun stefan/mu4e-next-thread ()
  (interactive)
  (mu4e-headers-find-if-next
   (lambda (msg)
     (let ((thread (mu4e-message-field msg :thread)))
        (or
         (eq 0 (plist-get thread :level))
         (plist-get thread :empty-parent))))))

(define-key 'mu4e-headers-mode-map (kbd "TAB") 'stefan/mu4e-next-thread)

;-Helping-Functions--------------------------------

(defun jsm/shr-browse-url ()
  "For mu4e messages, browse the URL under point or advance the message"
  (interactive)
  (let ((url (get-text-property (point) 'shr-url)))
    (if (not url)
        (mu4e-scroll-up)
      (shr-browse-url))))
(define-key mu4e-view-mode-map (kbd "RET") 'jsm/shr-browse-url)

;; Scroll mu4e-header along with next/prev messages
(defadvice mu4e-view-headers-next (around scroll-down-mu4e-header activate)
  "Scroll down the mu4e-header window when moving onto next email"
  (save-excursion
    (other-window 1)
    (recenter))
  ad-do-it)

(defadvice mu4e-view-headers-prev (around scroll-up-mu4e-header activate)
  "Scroll up the mu4e-header window when moving onto prev email"
  (save-excursion
    (other-window 1)
    (recenter))
  ad-do-it)

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
  (if (or (string= "mu4e" (substring (symbol-name major-mode) 0 4))
          (string-match (expand-file-name "~/Maildir") (or buffer-file-name "")))
      (progn ;; Currently with email
        (window-configuration-to-register ?m nil)
        (when (= (length (window-list)) 1)
          (bury-buffer))
        (unless (ignore-errors (jump-to-register ?e))
          (find-file (expand-file-name "~/org/projects.org"))))
    (progn ;; Currently not looking at an email buffer
      (window-configuration-to-register ?e nil)
      (delete-other-windows)
      (unless (ignore-errors (jump-to-register ?m))
        (mu4e)))))
(global-set-key (kbd "<f11>") 'switch-between-mu4e)

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
(define-key mu4e-headers-mode-map (kbd "M") 'mu4e-headers-flag-all-read)

; Use the helper functions to mark all read in annoying maildirs
; FIXME: Timing issue when switching between queries
(defun jsm/mu4e-mark-noisy-maildirs-all-read ()
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
(define-key mu4e-headers-mode-map (kbd "A") 'jsm/mu4e-mark-noisy-maildirs-all-read)

(if (not (functionp 'delete-all-overlays))
  (defun delete-all-overlays ()
    (remove-overlays)))

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

;; Help for attaching buffers and files
; I find it annoying that I have to move to the end of the email to attach
; or suffer the recepients getting the 2nd part of my email in a
; mime-attached generic part
(defun attach-buffer ()
  "Call mml-attach-buffer but only after moving to the end of the message"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (call-interactively 'mml-attach-buffer)))
(define-key message-mode-map (kbd "C-c <return> b") 'attach-buffer)

(defun attach-file ()
  "Call mml-attach-file but only after moving to the end of the message"
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (call-interactively 'mml-attach-file)))
(define-key message-mode-map (kbd "C-c <return> f") 'attach-file)

;-Looking-up-Contacts--------------------

; Inspired from helm-mu
(defun jsm/mu-contact-lookup (name)
  "Searches mu contacts for name"
  (let ((cmd (concat
              (format "mu cfind --format=mutt-ab %s" name)
              "| sed -n '/@/s/\\([^\t]*\\)\t\\([^\t]*\\).*/\"\\2\" <\\1>/p'"
              "| sed 's/\"\" //g'"
              "| egrep -v 'root@| <logwatch@| <buzz|@txt.voice.google|@plus.google.com'")))
    (cdr (split-string (shell-command-to-string cmd) "\n"))))

;; Inspired from mailabbrev.el
(defun my-mail-aliases ()
  "Grab just the mail alias names from ~/.mailrc to be included in completion"
  (with-temp-buffer
    (insert-file-contents (or mail-personal-alias-file "~/.mailrc"))
    (let ((my-aliases '()))
      (while (re-search-forward
              "^a\\(lias\\)?[ \t]+" nil t)
        (beginning-of-line)
        (re-search-forward "[ \t]+\\([^ \t\n]+\\)")
        (let ((name (buffer-substring
                     (match-beginning 1) (match-end 1))))
          (end-of-line)
          (add-to-list 'my-aliases name)))
      my-aliases)))

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
               (candidates (append (jsm/mu-contact-lookup text) (my-mail-aliases))))
          (delete-region (car bounds) (cdr bounds))
          (insert (funcall my-address-completion-func candidates text)))
      (insert "\t"))))

(defun my-ido-email-address-complete (addresses initial)
  "Wrapping function to call IDO completion on my filtered email addresses"
  (ido-completing-read "Address: " addresses nil nil initial))

(defun my-helm-email-address-complete (addresses initial)
  (helm-comp-read "Address: " addresses :initial-input initial))

(cond ((boundp 'helm-mode)
       (setq my-address-completion-func 'my-helm-email-address-complete))
      ((boundp 'ido-mode)
       (setq my-address-completion-func 'my-ido-email-address-complete)))

(define-key message-mode-map (kbd "<tab>") 'jsm/complete-address)

;-Sending-HTML-Emails--------------------

;; http://orgmode.org/worg/org-contrib/org-mime.html
(require 'org-mime)
(setq org-mime-library 'mml)

(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))

(defun org-mime-html-styling ()
  "Help add styling to your html elements. Intended to be called via
`org-mime-html-hook`"
  (save-excursion
    (beginning-of-buffer)
    (org-mime-change-element-style
     "pre" (concat "padding: 0.5em;")))
  (save-excursion
    (beginning-of-buffer)
    (org-mime-change-element-style
     "blockquote" (concat "font: 14px/22px normal helvetica, sans-serif;"
                          "margin-top: 10px;"
                          "margin-bottom: 10px;"
                          "margin-left: 50px;"
                          "padding-left: 15px;"
                          "border-left: 3px solid #ccc;")))
  (save-excursion
    (beginning-of-buffer)
    (org-mime-change-class-style
     "example" (concat "padding: 0.5em;")))
  (save-excursion
    (beginning-of-buffer)
    (org-mime-change-element-style
     "p" (concat "padding-bottom: 15px;"))))

(add-hook 'org-mime-html-hook 'org-mime-html-styling)

(defun jsm/html-compose (&optional key)
  "Helper function for switching between normal message mode and org-mode
  composition.

If in message mode, quote your signature in an \"EXAMPLE\" org block and
switch mode to org-mode. Otherwise switch back to message mode and htmlize
the body."
  (interactive)
  (let* ((my-sig
         (with-temp-buffer
           (message-insert-signature)
           (buffer-substring-no-properties (+ 1 (point-min)) (point-max))))
         (goback-key (or key (kbd "<f12>"))))
    ; If in email compose mode, then prep body and switch to org-mode;
    ; Otherwise, switch back to compose mode and generate the html
    (if (derived-mode-p 'message-mode)
        (progn
          (setq jsm/html-compose-prevmode (symbol-name major-mode))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward my-sig nil t)
              (replace-match (format "#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE\n" my-sig)))
            (org-mode)
            (local-set-key goback-key 'jsm/html-compose)))
      ; Otherwise return to message-mode and htmlize
      (funcall (intern jsm/html-compose-prevmode))
      (org-mime-htmlize nil))
    ; (org-mu4e-compose-org-mode)
    ; (add-hook 'message-send-hook 'org~mu4e-mime-convert-to-html-maybe nil t)
    ))
; Assign the desired key for executing jsm/html-compose and for switching back
;; (let ((key (kbd "<f12>")))
;;   `(define-key message-mode-map ,key
;;      (lambda () (interactive) (jsm/html-compose ,key))))
(define-key message-mode-map [f12] (lambda () (interactive) (jsm/html-compose [f12])))
;(lookup-key message-mode-map (kbd "<f12>"))

;-Notes--------------------------------------------

; Notes on using mbsync
;  Need to set mu4e-change-filenames-when-moving to t
;  Sample mbsync config(s)
;   https://groups.google.com/d/msg/mu-discuss/AhgmBAcv-ww/vgWKlBmxXsMJ

; crypto - http://www.djcbsoftware.nl/code/mu/mu4e/MSGV-Crypto.html#MSGV-Crypto
; org-mode emails - http://www.djcbsoftware.nl/code/mu/mu4e/Rich_002dtext-messages-with-org_002dmode.html#Rich_002dtext-messages-with-org_002dmode
; notifications - http://www.djcbsoftware.nl/code/mu/mu4e/Getting-new-mail-notifications-with-Sauron.html#Getting-new-mail-notifications-with-Sauron
; Tweak citation - http://www.djcbsoftware.nl/code/mu/mu4e/Citations-with-mu_002dcite.html#Citations-with-mu_002dcite
; DONE - Calendaring - http://doughellmann.com/2007/10/working-with-imap-and-icalendar-2.html - doesn't work with our Exchange server
; RPM packaging - http://pastebin.com/5Ja8SJsB
