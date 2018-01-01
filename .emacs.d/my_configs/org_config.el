;; -*- emacs-lisp -*-

;; org configurations
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Other packages you should have installed
; 1. htmlize
;    Good for syntax highlighting code blocks when exporting to HTML
; 2. org-checklist
;    Good for repeating tasks where you've used checkboxes
;    and want them de-selected when marking the task as done.
;    http://orgmode.org/worg/org-contrib/#repofile-contrib-lisp-org-checklist.el
;; (load-file (expand-file-name "~/.emacs.d/org-checklist.el"))

;; ;; org-jira
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/org-jira/"))
;; (require 'org-jira)
;; (setq jiralib-url "https://crd-jira.qualcomm.com/jira")

;; --------------------------------------------------
;; Basic org configuration
;; --------------------------------------------------

;; Activation basics for org
;; http://orgmode.org/manual/Activation.html#Activation
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\222"  'org-capture)      ; aka C-M-r
(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; And allow ace-jump-mode within my org files
(define-key org-mode-map (kbd "C-c SPC") 'ace-jump-mode)

;; Other Org variables
; Typical work estimates for a task
(setq org-global-properties '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                              ("STYLE_ALL" . "habit")))
; columns shown in column mode (defaults to "%25ITEM %TODO %3PRIORITY %TAGS")
(setq org-columns-default-format "%80ITEM %TODO %3PRIORITY %10Effort(Effort){:} %TAGS"
      org-tags-column -100)
; logging
(setq org-log-into-drawer t)
(setq org-clock-into-drawer t)
; Face it, your spelling sucks. You /need/ flyspell-mode on!
; FIXME: flyspell is killing org-archive on C-c $
(use-package org-bullets)

;; Let's make the document look nicer while editing
(setq org-hide-emphasis-markers t)

(defun my-org-mode-settings ()
  (interactive)
  ;; Spelling
  ;; http://endlessparentheses.com/ispell-and-org-mode.html
  (turn-on-flyspell)
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("`" "`"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (setq fill-column 100)
  ;; Other hacks
  (flycheck-mode -1)
  (org-bullets-mode 1))
(add-hook 'org-mode-hook 'my-org-mode-settings 'append)

; Hide all blank lines inside folded contents of headings
(setq org-cycle-separator-lines 0)
; Allow alphabetic lists to be recognized for lists
(setq org-alphabetical-lists t)
; Change ellipsis from the three dots to something more visually distinctive
(setq org-ellipsis "⤵")
; Speed commands
(setq org-use-speed-commands t)
(setq org-speed-commands-user '(("P" . org-property-action)
                                ("z" . org-add-note)
                                ("N" . org-narrow-to-subtree)
                                ("W" . widen)))
; Increase the amount of levels available to imenu
(setq org-imenu-depth 4)
; RET follows links
(setq org-return-follows-link t)
; Export settings
(setq org-use-sub-superscripts nil
      org-export-with-sub-superscripts nil
      org-export-headline-levels 8
      org-html-htmlize-output-type 'inline-css)
; Don't remove the highlighting after an occur search (C-c / /)
(setq org-remove-highlights-with-change nil)
; Setup additional colors for the various TODO states
; #+TODO: TODO(t) STARTED(s) DELEGATED(d@) WAITING(w@) | DONE(o@) CANCELED(c@)
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("STARTED" :foreground "steel blue" :weight bold)
        ("INFO" :foreground "blue" :weight bold)
        ("DELEGATED" :background "red" :foreground "black" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("CANCELED" :foreground "yellow" :weight bold)
        ("PHONE" :foreground "forest green" :weight bold)))
; language recognition for #+begin_src blocks
(setq org-src-lang-modes
      '(("ocaml" . tuareg)
        ("elisp" . emacs-lisp)
        ("ditaa" . artist)
        ("asymptote" . asy)
        ("dot" . graphviz-dot)
        ("shell" . sh)
        ("python" . python)
        ("javascript" . js)
        ("c" . c)
        ("sql" . sql)
        ("scheme" . scheme)
        ("exlixir" . elixir)))
(setq org-src-preserve-indentation t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil)

;; Add a new quick template
;; For the record, I hate the new template system (C-c C-x w)
(add-to-list 'org-structure-template-alist
             '(?S . "src shell"))
;; (add-to-list 'org-structure-template-alist
;;              '("sh" "#+begin_src shell\n?\n#+end_src" "<src lang=\"shell\">\n\n</src>"))
;; (add-to-list 'org-structure-template-alist
;;              '("cj" "#+begin_src clojure\n?\n#+end_src" "<src lang=\"clojure\">\n\n</src>"))
;; (add-to-list 'org-structure-template-alist
;;              '("java" "#+BEGIN_SRC java -n -r\n?\n#+END_SRC" "<src lang=\"java\">\n\n</src>"))

;; In an effort to fix the poor new template system, introducing my own fix:
(defun jsm/org-src-block ()
  "Better src block completion experience"
  (interactive)
  (org-insert-structure-template
   (concat "src " (completing-read "Source type: " org-src-lang-modes))))
(define-key org-mode-map (kbd "C-c s") 'jsm/org-src-block)

; org-babel configuration
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (dot . t)
   (ditaa . t)
   (R . t)
   (python . t)
   (ruby . t)
   (gnuplot . t)
   (clojure . t)
   (sh . t)
   (shell . t)
   (ledger . t)
   (org . t)
   (plantuml . t)
   (latex . t)
   (scheme . t)))

(setq org-ditaa-jar-path "/usr/share/java/ditaa.jar")
(setq org-plantuml-jar-path "~/bin/java/plantuml.jar")

;; Exporting - Extras
;; (require 'ox-taskjuggler)
(use-package ox-md)
(use-package ox-rst)

;; Custom HTML exporting
(setq org-html-postamble t
      org-html-checkbox-type 'unicode
      org-export-with-smart-quotes t
      org-html-postamble-format  '(("en" "<hr/><p><b>Exported by</b> %a <b>on</b> %d</p>"))
      org-html-head-extra "
<style type=\"text/css\">
  <!--/*--><![CDATA[/*><!--*/

blockquote {
  font-size: 0.9em;
  margin-top: 10px;
  margin-bottom: 10px;
  margin-left: 50px;
  padding-left: 15px;
}
blockquote:before {
  color: #ccc;
  content: open-quote;
  font-size: 4em;
  line-height: 0.1em;
  margin-right: 0.25em;
  vertical-align: -0.4em;
}
blockquote p {
  display: inline;
  padding-bottom: 15px;
}
.signature {
  font-family: monospace;
  white-space: pre-wrap;
  white-space: -moz-pre-wrap;
  word-wrap: break-word;
}
a {
  color: #666;
}
a:visited {
  color: #666;
}
a:hover {
  color: #c00;
}
p code, li code {
  color: darkgreen;
  font-weight: bold;
}
.figure p {
  text-align: left;
}

#content {
  margin: 0 auto;
  padding: 30px;
  width: 297mm;
  size: A4;
  margin: 0;
}

@page {
  size: A4;
  margin: 0;
}
@media print {
  html, body {
    width: 210mm;
    height: 297mm;
  }
}

  /*]]>*/-->
</style>")
;; http://stackoverflow.com/questions/16649943/css-to-set-a4-paper-size
;; Previously had: #content width: 1200px;

; Update any dblocks before exporting
; (add-hook 'org-export-first-hook 'org-update-all-dblocks 'append)
;   (org-update-all-dblocks) -> kills large regions of my text file when region set using "C-c @"
;   Or rather (org-map-dblocks 'org-update-dblock) does

; Agenda Info
(setq org-agenda-todo-ignore-scheduled "all"
      org-agenda-todo-ignore-deadlines "near"
      org-deadline-warning-days 30
      org-agenda-text-search-extra-files (list 'agenda-archives)
      org-agenda-show-future-repeats 'next
      )
; Refiling C-c C-w
;  This allows for file like pathing for refiling and lets me pick heading and subheading (level <= 2)
(setq org-refile-use-outline-path t)
;; (setq org-refile-targets (quote ((nil :maxlevel . 2))))
(setq org-refile-targets '((nil :maxlevel . 2)
                           ("~/org/info.org" :maxlevel . 2)
                           ("~/org/projects.org" :maxlevel . 3)
                           ("~/personal/org/personal.org" :maxlevel . 2)))

;; --------------------------------------------------
;; Customize Capture and Agenda
;; --------------------------------------------------

;; (From http://doc.norang.ca/org-mode.html)
;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; Capture new TODO items via org-capture
(setq org-directory "~/org"
      org-default-notes-file (concat org-directory "/notes.org"))

(setq org-capture-templates
      '(("t" "Todo Item" entry (file+headline "~/org/tasks.org" "Tasks")
         (file "~/org/tasks.tmplt") )
        ("v" "Vendor or Product" entry (file+olp "~/org/projects.org" "Vendors" "Misc Vendor or Product")
         (file "~/org/vendor_product.tmplt") :clock-in t :clock-resume t)
        ("p" "Projects or Repeating" entry (file+headline "~/org/projects.org" "Projects")
         (file "~/org/projects.tmplt") )
        ("m" "Meeting or Consultation" entry (file+headline "~/org/meetings.org" "Meetings")
         (file "~/org/meetings.tmplt") )
        ("s" "Support Production" entry (file+olp "~/org/projects.org" "Support Production")
         (file "~/org/support.tmplt") )
        ("o" "Oncall Entry" entry (file+olp+datetree "~/org/projects.org" "Oncall")
         (file "~/org/oncall.tmplt") :clock-in t :tree-type 'week)
        ("i" "Information or Ideas" entry (file+headline "~/org/info.org" "Incoming Ideas")
         (file "~/org/info.tmplt") )
        ("k" "Kudos to You" entry (file+olp "~/org/info.org" "Development Planning" "Kudos")
         (file "~/org/kudos.tmplt") )
        ("h" "Home Personal Item" entry (file+headline "~/personal/org/personal.org" "Personal")
         (file "~/org/personal.tmplt") :clock-in t :clock-resume t :kill-buffer t)))

; Leaving "~/personal/org/personal.org" out of my org-agenda-files. Can narrow ('<') for home Agenda work.
(setq org-agenda-files '("~/org/projects.org" "~/org/info.org" "~/org/meetings.org" "~/org/tasks.org" "~/personal/org/personal.org"))

;; Custom agenda block views
;; Stuff I want to see:
;; 1. Any TODO or DELEGATED  - Best collection of items that need work.
;;    From sections: Projects, Repeating Projects, Supporting Production,
;; 2. tags :needsrefile:     - I have set this on certain heading properties to be inherited by sub-headings.
;; 3. DONE or CANCELED tasks - They should be refiled.
;; 4. Other things I should have focus on? Perhaps strategic topics which need thought vs. action?
;;    tags "strategic-personal"
;; 5. Everything in my Tasks list
;;    tags "Tasks"
;; 6. Stuff that needs archived
;; Other Questions:
;; 1. What to do about Meetings?
;; 2. Personal Development items?
(setq org-agenda-custom-commands
      '(("w" "All my work-place items"
         ((agenda "" ((org-agenda-span 'week)
                      (org-agenda-files '("~/org/projects.org" "~/org/info.org" "~/org/meetings.org" "~/org/tasks.org"))))
          (tags-todo "ProjectsFile|TasksFile"
                     ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if 'todo '("STARTED"))))))
          (agenda "" ((org-agenda-span 'day)
                      (org-agenda-files '("~/personal/org/personal.org"))))
          (todo "TODO")
          (tags "+needsrefile")
          ))
        ("W" "Work meeting start of notes" occur-tree "^[[][0-9-]* [A-Za-z]* [0-9:]*[]] /[^/]*/.*$"
         ((org-agenda-overriding-header "Sparse tree regexp for start of meeting")))))

; Controlling how the windows are setup during Agenda views
(setq org-agenda-window-setup "other-window" ; Defaults to "reorganize-frame"
      org-agenda-restore-windows-after-quit t)

;; --------------------------------------------------
;; Publishing
;; --------------------------------------------------
(setq org-publish-project-alist
      '(("org"
         :auto-sitemap t
         :sitemap-title "Notes Index"
         :base-directory "~/org/"
         :publishing-directory "/usr2/jsmiller/public_html"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :exclude "tasks.org"
         :with-toc nil
         :html-head "<link rel=\"stylesheet\"
                    href=\"../other/mystyle.css\"
                    type=\"text/css\"/>")
        ("images"
         :base-directory "~/org/"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "/usr2/jsmiller/public_html"
         :publishing-function org-publish-attachment)))

;; --------------------------------------------------
;; Blogging
;; --------------------------------------------------
;; Setting up org2blog
;; ; Installed via ELPA
;; (require 'org2blog-autoloads)
;; (require 'netrc)
;; ; netrc entry would look like:
;; ; machine jonebird login admin password myrealpassword
;; (setq blog (netrc-machine (netrc-parse "~/.netrc.gpg") "jonebird" t))
;; (setq org2blog/wp-blog-alist
;;       '(("jonebird.com"
;;          :url "https://jonebird.com/xmlrpc.php"
;;          :username (netrc-get blog "login")
;;          :password (netrc-get blog "password")
;;          :tags-as-categories t)))
;; ;; (org2blog/wp-login)
;; ;; (setq org-insert-heading-hook nil) ; messes up org2blog
;; ;; Preview: M-x org2blog/wp-post-subtree
;; ;; Publish: C-u M-x org2blog/wp-post-subtree

;; --------------------------------------------------
;; Additional Hacks
;; --------------------------------------------------

;; Fold current subtree
;;  I like to fold a piece of text right from the middle of it...
;;  Manually, it would be: C-c C-p TAB
(defun jsm/org-fold-here()
  "Fold current subtree"
  (interactive)
  (outline-previous-visible-heading 1)
  (org-cycle))
(define-key org-mode-map (kbd "C-S-f") 'jsm/org-fold-here)

;; Narrow to parent element
(defun jsm/org-narrow-to-parent (&optional N)
  (interactive "p")
  (let ((levelup (or N 1)))
    (save-excursion
      (widen)
      (outline-up-heading levelup)
      (org-narrow-to-subtree))))
(define-key org-mode-map (kbd "C-x n p") 'jsm/org-narrow-to-parent)

;; Thanks norang - Exactly what I like to do
;;  http://doc.norang.ca/org-mode.html#sec-15-21
(defun jsm/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun jsm/insert-heading-inactive-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (jsm/insert-inactive-timestamp)))

(add-hook 'org-insert-heading-hook 'jsm/insert-heading-inactive-timestamp 'append)

;; Want to be able to narrow to subtree and clockin at the same time,
;;  then be able to do the opposite with a clockout and widen
(defun org-work-checkin ()
  (interactive)
  (org-clock-in)
  (org-narrow-to-subtree))
(defun org-work-checkout ()
  (interactive)
  (widen)
  (org-clock-out))
(define-key org-mode-map "\C-ci" 'org-work-checkin)
(define-key org-mode-map "\C-co" 'org-work-checkout)

;; I often like to create an indented sub-item and shift from '-' to '+'
; (defun jsm/org-indent-subitem)

;; Presentations via S5 for your org file
;; (load-file (expand-file-name "~/.emacs.d/org-S5/org-export-as-s5.el"))
;; Find further S5 themes:
;;   Official Site   - http://meyerweb.com/eric/tools/s5/themes/
;;   Nice Collection - http://atuan.com/s5/
;; Extract these into your ~/org/ui/ directory since that is where you store org files
(setq org-s5-theme "mitaine") ; dark and slighty odd
(setq org-s5-theme "yatil")   ; clean and bright

;; A near direct copy of org-export-as-html-and-open from org-html.el
(defun org-export-as-s5-and-open (arg)
  "Export the outline as an S5 presentation and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (org-export-as-s5 arg 'hidden)
  (org-open-file buffer-file-name)
  (when org-export-kill-product-buffer-when-displayed
    (kill-buffer (current-buffer))))
(defalias 'org-present 'org-export-as-s5-and-open)

;; Trying Reveal.js for presentations
;; See https://github.com/yjwen/org-reveal/blob/master/Readme.org
(use-package ox-reveal
  :custom (org-reveal-root "http://qualnet.qualcomm.com/~jsmiller/org-reveal/"))

;; Currently using a single inactive date followed by a italicized comment to denote the beginning of meeting notes
;;  Idea is that a project would be at the 2nd level (under top-level "Projects"), then
;;  any logical effort within the project would be the 3rd level heading along with other siblings
;;  Within a particular project's effort, I may participate in several meetings.
;;  E.g. [2012-01-05 Thu 16:10] /Meeting Started/
(defun jsm/org-insert-meeting-heading ()
  "Inserts a new line containing an inactive datetime stamp
followed by italicized meeting heading which is specified by the user"
  (interactive)
  (let ((prefix "\n")
        (cur-sentence (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (meeting-title (read-string "Meeting Title [Meeting Notes]: " nil '() "Meeting Notes")))
    (if (equal cur-sentence "")
        (setq prefix ""))
    (org-end-of-line)
    (org-insert-time-stamp nil t t prefix (concat " /" meeting-title "/\n") nil)))
(define-key org-mode-map "\M-i" 'jsm/org-insert-meeting-heading)
;; Alternatively accomplished via an org-capture:
; ("." "Current working heading" plain (clock) "%U - /%^{Meeting Title}/" :immediate-finish t :unnarrowed t)

(defun jsm/org-project-properties ()
  "Takes the current heading title, denoting the project, and
sets the :EXPORT_TITLE: and :CATEGORY: properties to the same."
  (interactive)
  (save-excursion
    (save-window-excursion
      (org-goto-marker-or-bmk org-capture-last-stored-marker)
      ;(org-capture-goto-last-stored) ; Aka C-u C-u org-capture
      (let ((project-title (org-get-heading t t)))
      ;(message (concat "DEBUG: project-title = \"" project-title "\""))
        (org-set-property "EXPORT_TITLE" project-title)
        (org-set-property "CATEGORY" project-title)))))
;(add-hook 'org-capture-before-finalize-hook 'jsm/org-project-properties)
;(add-hook 'org-capture-after-finalize-hook 'jsm/org-project-properties)

; Per Marc-Oliver via org-mode ML
(define-key org-mode-map
  [(f10)]
  (lambda () (interactive)
    (progn
      (occur (concat "^\\*+ .*"
                     (read-from-minibuffer
                      "Occur for toplevel headlines containing: "))
             nil)
      (pop-to-buffer "*Occur*")
      ;(use-local-map (copy-keymap (current-local-map)))
      (local-set-key (kbd "RET")
                     (lambda () (interactive)
                       (progn
                         (occur-mode-goto-occurrence)
                         (delete-other-windows)))))))

;; I like to export to html and immediately open in a browser tab
;;   Most of the time it is with a subtree. A tadbit annoying that
;;   certain properties are used only when you select the region
;;   vs. simplying having it narrowed.
;; See http://orgmode.org/manual/Export-options.html for other properties
(defun jsm/org-export-subtree-as-html-and-open ()
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (org-export-as-html-and-open 3)))
;; (define-key org-mode-map (kbd "<f12>") 'jsm/org-export-subtree-as-html-and-open)

;; (?o "As HTML file and open"
;;     (lambda (a s v b)
;;       (if a (org-html-export-to-html t s v b)
;;     (org-open-file (org-html-export-to-html nil s v b)))))))


;; (defun jsm/org-export-subtree-as-html-with-subtree-name-and-open (arg)
;;   (interactive "P")
;;   (let ((new-file-name (concat
;;                         (replace-regexp-in-string "[^a-zA-Z0-9]+" "_" (substring-no-properties (org-get-heading t t)))
;;                         ".html") ))
;;     (save-excursion
;;       (org-mark-subtree)
;;       (org-export-as-html arg 'hidden nil new-file-name nil "~/published")
;;       (switch-to-buffer new-file-name)
;;       (write-file "~/published" nil)
;;       (kill-buffer))
;;     new-file-name))
;; (define-key org-mode-map (kbd "<f9>") 'jsm/org-export-subtree-as-html-with-subtree-name-and-open)

(defun jsm/org-export-subtree-attach-to-email (arg)
  (interactive "P")
  (let ((exported-file-name
         (expand-file-name
          (concat "~/published/"
                  (jsm/org-export-subtree-as-html-with-subtree-name-and-open 3)))))
    ;; Need to create a plist with :path to file and :subject for file
    (mu4e-action-capture-message
     (list :path exported-file-name
           :subject "Exported by Org"))
    (mu4e-compose-attach-captured-message)))

;; Hack to change the appearance of the checkboxes [X] via the ML
 (font-lock-add-keywords
  'org-mode `(("\\[X\\]"
               (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                         "☑") ; ✔ ☐ ☑
                         nil)))
              ("\\[ \\]"
               (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                         "☐")
                         nil)))))

;; Allow linking to manual pages
(use-package org-man)

;; org-protocol setup
(use-package org-protocol)

;; org-drill support
(use-package org-drill)

;; Add the wonderful org-mac-link
(use-package org-mac-link)

(if (eq system-type 'darwin)
    (add-hook 'org-mode-hook
              (lambda ()
                (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))))

;; Improved org-mac apple script functions
(defun org-as-get-selected-mail ()
  "AppleScript to create links to selected messages in Mail.app."
  (do-applescript
   (concat
    "tell application \"Mail\"\n"
    "set odate to date \"Friday, January 12, 2001 at 12:00:01 AM\"\n"
    "set selectedMessages to selection\n"
    "repeat with i from 1 to (count of selectedMessages)\n"
      "tell item i of selectedMessages\n"
        "set messageid to message id\n"
        "set subjectstr to subject\n"
        "set dstr to date received\n"
        "if dstr > odate then\n"
          "set theLink to \"message://\" & messageid & \"::split::\" & subjectstr\n"
          "set odate to dstr\n"
        "end if\n"
        "end tell\n"
    "end repeat\n"
    "return theLink\n"
    "end tell")))

;; org-download
;; (require 'org-download)

;; Calendaring support with org-caldav. I created an el-get recipe to
;; ensure this package is installed.
;; (require 'org-caldav)
;; (setq
;;  org-caldav-url "http://localhost:1080/users/jsmiller@qti.qualcomm.com/calendar"
;;  org-caldav-calendar-id ""
;;  org-icalendar-timezone "America/Los_Angeles"
;;  org-caldav-inbox "~/org/qualcal.org"
;;  org-caldav-files '("~/org/appointments.org")    ; list of org files to sync
;;  org-icalendar-alarm-time  15                    ; minutes before alarm
;;  )
; Run M-x org-caldav-sync to start the sync

;; Org-drill hacking
; org-drill-card-type-alist

;; TODO
; org-screen -- http://orgmode.org/w/?p=org-mode.git;a=blob_plain;f=contrib/lisp/org-screen.el;hb=HEAD
; ox-confluence -- http://orgmode.org/w/?p=org-mode.git;a=blob_plain;f=contrib/lisp/ox-confluence.el;hb=HEAD
