;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Matthew Sampson"
      user-mail-address "matthew@graywolfai.com")

;;https://github.com/hlissner/doom-emacs-private/blob/064c19884c7a10f2facffda1e33f9ef54880bde6/config.el#L13
(setq display-line-numbers nil)

(setq doom-theme 'doom-vibrant)

;; makes autocomplete manual
(setq company-idle-delay nil)

;; Disable invasive lsp-mode features
(setq lsp-ui-sideline-enable nil
      lsp-ui-doc-enable nil
      lsp-enable-symbol-highlighting nil)

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq fancy-splash-image (concat doom-private-dir "splash.png"))
;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'exwm)                                                                                   ;;
;; (require 'exwm-config)                                                                            ;;
;; (exwm-config-default)                                                                             ;;
;; (require 'exwm-randr)                                                                             ;;
;; (setq exwm-randr-workspace-output-plist '(0 "Virtual-1"))                                         ;;
;; (add-hook 'exwm-randr-screen-change-hook                                                          ;;
;;         (lambda ()                                                                                ;;
;;           (start-process-shell-command()                                                          ;;
;;            "xrandr" nil "xrandr --output Virtual-1 --mode 1920x1080 --pos 0x0 --rotate normal"))) ;;
;; (exwm-randr-enable)                                                                               ;;
;; (require 'exwm-systemtray)                                                                        ;;
;; (exwm-systemtray-enable)                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; :tools magit
(setq magit-repository-directories '(("~/Projects" . 2))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t
      transient-values '((magit-rebase "--autosquash" "--autostash")
                         (magit-pull "--rebase" "--autostash")))
;; when using magit, it takes forever
(setq vc-handled-backends nil)

(setq org-directory "/Volumes/org/"
      org-index-file (concat org-directory "getting-things-done.org" ))



(global-set-key (kbd "C-x c") #'copy-region-as-kill)
(global-set-key (kbd "C-x v") #'org-yank)
(setq org-default-notes-file (concat org-directory "inbox.org")
      org-roam-directory (concat org-directory "roam/")
      org-roam-db-location (concat org-directory ".org-roam.db")
      org-agenda-files (list org-index-file))
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))
;; from here https://stackoverflow.com/a/18209748/16154075
(global-set-key (kbd "C-c C-l") 'org-insert-link)

(setq projectile-project-search-path '("~/code/" org-directory)
      projectile-sort-order 'recentf)

(require 'time-stamp)
(after! org
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (setq org-todo-keywords '((sequence "WAITING" "TODO(@)" "NEXT" "FIGURING-IT-OUT" "MAKING-IT-HAPPEN" "|" "DONE" "CANCELLED"))) ;;
  ;; (setq org-todo-keyword-faces                                                                                                  ;;
  ;;     '(("TODO" . org-warning)                                                                                                  ;;
  ;;       ("NEXT" . "red")                                                                                                        ;;
  ;;       ("DONE" . "green")                                                                                                      ;;
  ;;       ("WAITING" . "yellow")                                                                                                  ;;
  ;;       ("CANCELLED" . "orange")                                                                                                ;;
  ;;       ("FIGURING-IT-OUT" . "red")                                                                                             ;;
  ;;       ("MAKING-IT-HAPPEN" . "red"))))                                                                                         ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trying from http://doc.norang.ca/org-mode.html#HowToUseThisDocument
 (setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

 (setq org-todo-keyword-faces
       (quote (("TODO" :foreground "red" :weight bold)
               ("NEXT" :foreground "blue" :weight bold)
               ("DONE" :foreground "forest green" :weight bold)
               ("WAITING" :foreground "orange" :weight bold)
               ("HOLD" :foreground "magenta" :weight bold)
               ("CANCELLED" :foreground "forest green" :weight bold)
               ("MEETING" :foreground "forest green" :weight bold)
               ("PHONE" :foreground "forest green" :weight bold))))
 (setq org-use-fast-todo-selection t)
 (setq time-stamp-active t
       time-stamp-start "#\\+last_modified: [ \t]*"
       time-stamp-end "$"
       time-stamp-format "\[%04y-%02m-%02d %3a %02H:%02M\]")
 (add-hook 'before-save-hook 'time-stamp nil))

;; from http://doc.norang.ca/org-mode.html#HowToUseThisDocument
;; allows changing todo states with S-left and S-right skipping all of the normal processing when entering or leaving a todo state. This cycles through the todo states but skips setting timestamps and entering notes which is very convenient when all you want to do is fix up the status of an entry.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)


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

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i");

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq org-refile-targets '((org-index-file :maxlevel . 3)))                          ;;
;;                                                                                      ;;
;; (set-email-account!                                                                  ;;
;;  "matthew@graywolfai.com"                                                            ;;
;;  '((mu4e-sent-folder       . "/[Gmail]/Sent Mail")                                   ;;
;;    (mu4e-trash-folder      . "/[Gmail]/Bin")                                         ;;
;;    (smtpmail-smtp-user     . "matthew@graywolfai.com"))                              ;;
;;  t)                                                                                  ;;
;; (after! mu4e (setq mu4e-get-mail-command "mbsync gmail"                              ;;
;;       ;; get emails and index every 5 minutes                                        ;;
;;       mu4e-update-interval 300                                                       ;;
;; 	  ;; send emails with format=flowed                                             ;;
;; 	  mu4e-compose-format-flowed t                                                  ;;
;; 	  ;; don't need to run cleanup after indexing for gmail                         ;;
;; 	  mu4e-index-cleanup nil                                                        ;;
;; 	  mu4e-index-lazy-check t                                                       ;;
;;       ;; more sensible date format                                                   ;;
;;       mu4e-headers-date-format "%d.%m.%y"))                                          ;;
;; ;; This determines the style of line numbers in effect. If set to `nil', line        ;;
;; ;; numbers are disabled. For relative line numbers, set this to `relative'.          ;;
;; (setq message-send-mail-function 'smtpmail-send-it                                   ;;
;;      smtpmail-stream-type 'starttls                                                  ;;
;;      smtpmail-default-smtp-server "smtp.gmail.com"                                   ;;
;;      smtpmail-smtp-server "smtp.gmail.com"                                           ;;
;;      smtpmail-smtp-service 587)                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map! (:after evil-org
       :map evil-org-mode-map
       :n "c" 'evil-copy
       :n "r" 'evil-redo))
(map! (:after evil-org
       :n "C-c s" 'evil-save
       :n "Q" 'evil-window-next))
(map! (:n "C-c /" 'comment-box))
;;(setq display-line-numbers-type t) ;; <2022-01-17 Mon 17:06> uncommenting while debugging org capture freezing
(setq vterm-shell "/usr/local/bin/zsh")
;;(evil-multiedit-default-keybinds) ;; I think this needs to run in an after block? It's error out

;; turns on indent more
(add-hook 'org-mode-hook 'org-indent-mode)
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

(if (require 'toc-org nil t)
    (progn
      (add-hook 'org-mode-hook 'toc-org-mode)
      ;;enable in markdown, too
      (add-hook 'markdown-mode-hook 'toc-org-mode))
  (warn "toc-org note found"))

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

;;Ideas lead to potentially new projects
;;Notes are tied to existing projects
(after! (org org-capture)
      (setq org-default-notes-file (concat org-directory "inbox.org"))
      (setq org-capture-templates
       '(("t" "todo" entry (file+headline org-default-notes-file "Inbox")
          "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
         ("m" "meeting" entry (file+headline org-default-notes-file "Meetings")
          "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
         ("i" "idea" entry (file+headline org-default-notes-file "Ideas")
          "* %? :IDEA:\n%U\n%a\n:END:")
         ("n" "note" entry (file+headline org-default-notes-file "Notes")
          "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t))))



(setq org-log-done 'note)

(setq org-refile-allow-creating-parent-nodes 'confirm)

;;(defun my/refile-meeting-todo )

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

(setq org_notes (concat org-directory "notes/"))
(setq bibliography-directory "~/bibliography/" )
;;(setq zot_bib) ;; not sure if I need this, I think its just a private variables
(setq bibtex-completion-bibliography (list (concat bibliography-directory "zotLib.bib") (concat bibliography-directory "gw-zotLib.bib")))
(after! citar
  (setq org-cite-global-bibliography bibtex-completion-bibliography)
(setq org-cite-insert-processor 'citar)
(setq org-cite-follow-processor 'citar)
(setq org-cite-activate-processor 'citar)
(setq citar-bibliography org-cite-global-bibliography)
(setq citar-notes-paths (list (concat org-directory "roam/")))
)
;;(citar-filenotify-setup '(LaTeX-mode-hook org-mode-hook)) ;; following https://github.com/bdarcus/citar#refreshing-the-library-display commenting this out because it's raising errors when I open org files


;;(setq citar-bibliography '("~/Dropbox/org/zotLib.bib" "~/Dropbox/org/gw-zotLib.bib"))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'lsp)                                                           ;;
;; (after! lsp                                                              ;;
;;   (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))       ;;
;;   (lsp-register-client                                                   ;;
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp")) ;;
;;                   :major-modes '(nix-mode)                               ;;
;;                   :server-id 'nix)))                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/current-time-string ()
  "calls current-time, then loops through each integer value and converts to string"
  (cl-loop for val in (current-time)
         concat (int-to-string val)))
(defun my/citar-notes-gen-roam-id (key)
  "generate a unique hash id by concating the input 'key' with the string version of current-time"
  (secure-hash 'md5 (concat key (my/current-time-string))))


(after! org-roam
    (org-roam-setup))
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
        (insert ":PROPERTIES:\n:ID:     ")
        (insert (my/citar-notes-gen-roam-id key))
        (insert"\n:END:\n")
        (insert "#+title: ")
        (when template (insert note-meta))
        (insert "\n|\n")
        (search-backward "|")
        (delete-char 1)
        (when (fboundp 'evil-insert)
          (evil-insert 1)))))
;;(setq citar-format-note-function #'my-citar-org-format-note-function)

(map! (:n "C-c o" 'citar-open-notes
       :map org-mode-map
       :n "C-c b" 'org-cite-insert
       :n "C-c r" 'citar-refresh))
(map! (:n "C-c /" 'comment-box))
