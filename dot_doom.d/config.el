;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Matthew Sampson"
      user-mail-address "matthew@graywolfai.com")
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;;(setq doom-font (font-spec :family "Attribute Mono" :size 22))
;;(unless (find-font doom-font)
;;  (setq doom-font (font-spec :family "Cascadia Code PL" :size 20)))
;;(setq doom-unicode-font (font-spec :family "Symbola" :size 20))
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")
(setq org-index-file (concat org-directory "getting-things-done.org" ))
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file org-index-file)))
(global-set-key (kbd "C-x c") #'copy-region-as-kill)
(global-set-key (kbd "C-x v") #'org-yank)
(setq org-default-notes-file (concat org-directory "inbox.org"))
(setq org-roam-directory "~/Dropbox/org/roam")
(setq org-agenda-files (list org-index-file))
;; from here https://stackoverflow.com/a/18209748/16154075
(global-set-key (kbd "C-c C-l") 'org-insert-link)

;;(after! org
  (setq org-todo-keywords '((sequence "WAITING" "TODO(@)" "NEXT" "FIGURING-IT-OUT" "MAKING-IT-HAPPEN" "|" "DONE" "CANCELLED")))
  (setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("NEXT" . "red")
        ("DONE" . "green")
        ("WAITING" . "yellow")
        ("CANCELLED" . "orange")
        ("FIGURING-IT-OUT" . "red")
        ("MAKING-IT-HAPPEN" . "red")))
;;  )
;;(after! org-capture
;;  (setq org-default-notes-file (concat org-directory "inbox.org"))
;;  (setq org-capture-templates
;;    '(("t" "Todo" entry (file org-default-notes-file)
;;       "* TODO %?\n%U" :empty-lines 1)
;;         )))

(defun my/log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
;;(add-hook 'org-after-todo-state-change-hook #'my/log-todo-next-creation-date)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq deft-directory org-directory
      deft-extensions '("org")
      deft-recursive t)
(map! "C-c n l" #'org-roam-buffer-toggle
       "C-c n f" #'org-roam-node-find
       "C-c n i"  #'org-roam-node-insert
       "C-c n d" #'org-roam-dailies-capture-today
       "C-c n o" #'org-id-get-create
       "C-c n t" #'org-roam-tag-add
       "C-c n a" #'org-roam-alias-add)
(setq x-selection-timeout 10)

;;(set-email-account!
;; "gmail"
;; '((mu4e-sent-folder       . "/[Gmail]/Sent Mail")
;;   (mu4e-trash-folder      . "/[Gmail]/Bin")
;;   (smtpmail-smtp-user     . "example@gmail.com"))
;; t)
;;(setq org-refile-targets '((org-index-file :maxlevel . 3)))
;;(after! mu4e (setq mu4e-get-mail-command "mbsync gmail"
      ;; get emails and index every 5 minutes
;;      mu4e-update-interval 300
	  ;; send emails with format=flowed
;;	  mu4e-compose-format-flowed t
	  ;; don't need to run cleanup after indexing for gmail
;;	  mu4e-index-cleanup nil
;;	  mu4e-index-lazy-check t
      ;; more sensible date format
;;      mu4e-headers-date-format "%d.%m.%y"))
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;;(setq message-send-mail-function 'smtpmail-send-it
;;     smtpmail-stream-type 'starttls
;;     smtpmail-default-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-service 587)
;;(setq auth-sources (quote (macos-keychain-internet macos-keychain-generic)))

(map! "C-c s" #'evil-save)
;;(setq display-line-numbers-type t) ;; <2022-01-17 Mon 17:06> uncommenting while debugging org capture freezing
(setq vterm-shell "/usr/local/bin/zsh")

(defun my/insert-clipboard-image (filename) ;; I had to add this to paste images in org-roam. I found it online on stackoverflow
  "Inserts an image from the clipboard"
  (interactive "sFilename to paste: ")
  (let ((file
         (concat
          (file-name-directory (buffer-file-name (buffer-base-buffer)))
          "images/"
          (format-time-string "%Y%m%d_%H%M%S_-_")
          (if (bound-and-true-p my/insert-clipboard-image-use-buffername)
              (concat (s-replace "-" "_"
                                 (downcase (file-name-sans-extension (buffer-name)))) "_-_")
            "")
          (if (bound-and-true-p my/insert-clipboard-image-use-headername)
              (concat (s-replace " " "_" (downcase (nth 4 (org-heading-components)))) "_-_")
            "")
          filename ".png")))

    ;; create images directory
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file)))

    ;; paste file from clipboard
    (shell-command (concat "pngpaste " file))
    (insert (concat "[[./images/" (file-name-nondirectory file) "]]"))))

(map! :desc "Insert clipboard image"
      :n "C-M-y" #'my/insert-clipboard-image)

(add-hook 'org-mode-hook #'org-display-inline-images)
;; enable autosave
(setq auto-save-default t
      make-backup-files t)
;; better mapping for unfo-fu redo
(after! undo-fu
  (map! :map undo-fu-mode-map "C-?" #'undo-fu-only-redo))
;; shortcut to go to main orgmode file
(defun my/insert-timestamp ()
  (interactive)
  (org-insert-time-stamp (current-time) t)
  )
(global-set-key (kbd "C-.") #'my/insert-timestamp)
(after! org-roam
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
)

(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))
(after! org
  (add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-todo-to-today)))))

(after! (org org-capture)
      (setq org-default-notes-file (concat org-directory "inbox.org"))
      (setq org-capture-templates
       '(("t" "Personal todo" entry (file+headline org-default-notes-file "Inbox") "* TODO %?\n:LOGBOOK:\n:CREATED_AT: %T\n:END:\n" :prepend t)
         ("m" "Meeting" entry (file+headline org-default-notes-file "Meetings") "* %T %^{meeting title} :MEETING:\n %?" :clock-in t :clock-resume t)
         ("i" "Idea" entry (file+headline org-default-notes-file "Ideas") "* %? :IDEA: \n:LOGBOOK:\n:CREATED_AT: %T\n:END:")))
)

(setq org-log-done 'note)

;; don't keep message buffers around
;;(setq message-kill-buffer-on-exit t)
;;(use-package! org-sort-tasks)
;;(call-process-shell-command "dura serve &" nil 0)

(setq org-refile-allow-creating-parent-nodes 'confirm)

;;(defun my/refile-meeting-todo )

;;(use-package org-noter
;;:after org
;;  :ensure t)

;;(defun my-skip-unless-waiting ()
;;  "Skip trees that are not waiting"
;;  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
;;    (if (re-search-forward ":waiting:" subtree-end t)
;;        nil          ; tag found, do not skip
;;      subtree-end))) ; tag not found, continue after end of subtree
;;(setq org-agenda-custom-commands
;;      '(("c" "Simple agenda view"
;;         ((tags "PRIORITY=\"A\""
;;                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
;;
;;          (agenda "")
;;          (alltodo "")))))

;;(setq parinfer-rust-check-before-enable nil) ;; stops the annoying parinfer "do you want to fix indentation y or n" prompt
(use-package yequake
  :custom
    (yequake-frames '(("org-capture" (buffer-fns . (yequake-org-capture)) (width . 0.75) (height . 0.75)(alpha . 0.95)(frame-parameters . ((undecorated . t) (skip-taskbar . t)(sticky . t)))))))

(setq org_notes "~/Dropbox/org/notes/")
;;(setq zot_bib) ;; not sure if I need this, I think its just a private variables
(setq bibtex-completion-bibliography "~/Dropbox/org/zotLib.bib")
(after! citar
  (setq org-cite-global-bibliography
        '("~/Dropbox/org/zotLib.bib" "~/Dropbox/org/gw-zotLib.bib"))
(setq org-cite-insert-processor 'citar)
(setq org-cite-follow-processor 'citar)
(setq org-cite-activate-processor 'citar)
(setq citar-bibliography org-cite-global-bibliography)
(setq citar-notes-paths `("~/Dropbox/Org/roam"))
)
(citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook)) ;; following https://github.com/bdarcus/citar#refreshing-the-library-display

;;(setq citar-bibliography '("~/Dropbox/org/zotLib.bib" "~/Dropbox/org/gw-zotLib.bib"))

;;(use-package! org-roam-bibtex
;;  :after org-roam)
;;(setq orb-roam-ref-format "org-cite") ;; sets citation key format style to org-cite @element format

;;(after! citar
;;(setq citar-file-note-org-include 'org-roam-ref))

;;(defun my/citar-org-open-notes (key entry)
;;  (let* ((bib (string-join (list my/bibtex-directory key ".bib")))
;;         (org (string-join (list my/bibtex-directory key ".org")))
;;         (new (not (file-exists-p org))))
;;    (funcall citar-file-open-function org)
;;    (when (and new (eq (buffer-size) 0))assoc-default "title" entry)
;;                      user-full-name
;;                      user-mail-address
;;                      bib
;;                      (with-temp-buffer
;;                        (insert-file-contents bib)
;;                        (buffer-string))))
;;      (search-backward "|")
;;      (delete-char 1)

;;(setq-default citar-open-note-function 'my/citar-org-open-notes)
(defun my-citar-org-open-notes (key entry)
  (let* ((bib (string-join (list my/bibtex-directory key ".bib")))
         (org (string-join (list my/bibtex-directory key ".org")))
         (new (not (file-exists-p org))))
    (funcall citar-file-open-function org)
    (when (and new (eq (buffer-size) 0))
      (insert (format template
                      (assoc-default "title" entry)
                      user-full-name
                      user-mail-address
                      bib
                      (with-temp-buffer
                        (insert-file-contents bib)
                        (buffer-string))))
      (search-backward "|")
      (delete-char 1))))

;;(setq-default citar-open-note-function 'my-citar-org-open-notes)
(defun my-citar-org-format-note-function (key entry filepath)
  "Format a note FILEPATH from KEY and ENTRY."
    (let* ((template (citar-get-template 'note))
           (note-meta
            (when template
              (citar--format-entry-no-widths
               entry
               template)))
           (buffer (find-file filepath)))
      (with-current-buffer buffer
        ;; This just overrides other template insertion.
        (erase-buffer)
        (citar-org-roam-make-preamble key)
        (insert ":PROPERTIES:\n:ID:     @")
        (insert key)
        (insert"\n:END:\n")
        (insert "#+title: ")
        (when template (insert note-meta))
        (insert "\n|\n")
        (search-backward "|")
        (delete-char 1)
        (when (fboundp 'evil-insert)
          (evil-insert 1)))))
(setq citar-format-note-function #'my-citar-org-format-note-function)
