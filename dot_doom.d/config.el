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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-roam-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org/"))
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq deft-directory "~/Dropbox/org/"
      deft-extensions '("org","txti")
      deft-recursive t)
(map! "C-c n l" #'org-roam-buffer-toggle
       "C-c n f" #'org-roam-node-find
       "C-c n i"  #'org-roam-node-insert
       "C-c n d" #'org-roam-dailies-capture-today
       "C-c n o" #'org-id-get-create
       "C-c n t" #'org-roam-tag-add
       "C-c n a" #'org-roam-alias-add)
;(org-roam-setup)
(set-email-account!
 "gmail"
 '((mu4e-sent-folder       . "/[Gmail]/Sent Mail")
   (mu4e-trash-folder      . "/[Gmail]/Bin")
   (smtpmail-smtp-user     . "example@gmail.com"))
 t)
(setq mu4e-get-mail-command "mbsync gmail"
      ;; get emails and index every 5 minutes
      mu4e-update-interval 300
	  ;; send emails with format=flowed
	  mu4e-compose-format-flowed t
	  ;; don't need to run cleanup after indexing for gmail
	  mu4e-index-cleanup nil
	  mu4e-index-lazy-check t
      ;; more sensible date format
      mu4e-headers-date-format "%d.%m.%y")
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(map! "C-c s" #'evil-save)
(setq display-line-numbers-type t)
(setq vterm-shell "/usr/local/bin/zsh")
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; Overview
;; --------
;; Inserts an image from the clipboard by prompting the user for a filename.
;; Default extension for the pasted filename is .png

;; A ./images directory will be created relative to the current Org-mode document to store the images.

;; The default name format of the pasted image is:
;; filename: <yyyymmdd>_<hhmmss>_-_<image-filename>.png

;; Important
;; --------
;; This function depends on 'pngpaste' to paste the clipboard image
;; -> $ brew install pngpaste

;; Basic Customization
;; -------------------
;; Include the current Org-mode header as part of the image name.
;; (setq my/insert-clipboard-image-use-headername t)
;; filename: <yyyymmdd>_<hhmmss>_-_<headername>_-_<image-filename>.png

;; Include the buffername as part of the image name.
;; (setq my/insert-clipboard-image-use-buffername t)
;; filename: <yyyymmdd>_<hhmmss>_-_<buffername>_-_<image-filename>.png
;; Org-download-clipboard was not working for in in org-roamv2 (not sure if it'd work in org-roamv1 either..never tested), the below functions does work however, and I like it better because I can provide a customer name to each file pasted from clipboard
;; Full name format
;; filename: <yyyymmdd>_<hhmmss>_-_<buffername>_-_<headername>_-_<image-filename>.png
(defun my/insert-clipboard-image (filename)
  "Inserts an image from the clipboard"
  (interactive "sFilename to paste: ")
  (let ((file
         (concat
          (file-name-directory buffer-file-name)
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
      :n "C-M-y" 'my/insert-clipboard-image)

(after! org
  (setq org-image-actual-width (/ (display-pixel-width) 2)))
;; auto turns on org-displayin-inline-images
(add-hook 'org-mode-hook 'org-display-inline-images)
;; enable autosave
(setq auto-save-default t
      make-backup-files t)
;; better mapping for unfo-fu redo
(after! undo-fu
  (map! :map undo-fu-mode-map "C-?" #'undo-fu-only-redo))
;; shortcut to go to main orgmode file
(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file (concat org-directory "index.org"))))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
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
(setq debug-on-error t)
(after! org
  (add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/org-roam-copy-todo-to-today)))))
;(use-package org-download
;  :after org
;  :bind
;  (:map org-mode-map
;        (("s-Y" . org-download-screenshot)
 ;        ("s-y" . org-download-yank))))
