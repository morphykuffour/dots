;;; init.el --- Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; emacs os config for writing and productivity
; MacOS X
; brew tap d12frosted/emacs-plus
; brew install emacs-plus@31 --with-mailutils --with-modern-black-dragon-icon

;;; Code:

;; Measure startup time
(defun morph/display-startup-time ()
  "Display Emacs startup time in the minibuffer."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'morph/display-startup-time)

;; Reset garbage collection settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024) ; 16mb
                  gc-cons-percentage 0.1)
            ;; Restore file-name-handler-alist
            (setq file-name-handler-alist morph--file-name-handler-alist)))

;; GC when focus is lost (helps keep things snappy)
(add-hook 'focus-out-hook #'garbage-collect)

;; straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure use-package with straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(use-package org :straight (:type built-in))

(defconst user-init-dir
  (cond ((and (boundp 'user-init-file) user-init-file)
         (file-name-directory (file-truename user-init-file)))
        ((boundp 'user-emacs-directory) user-emacs-directory)
        ((boundp 'user-init-directory) user-init-directory)
        (t "~/.emacs.d/")))

;; Helper function to load config files
(defun load-user-file (file)
  "Load a file in current user's configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "config/font-resize.el")
(load-user-file "config/keymaps.el")
(load-user-file "config/utils.el")
(load-user-file "config/vterm-config.el")
;; (load-user-file "config/org-mode.el")

;; sensible settings from hrs
(add-to-list  'load-path (expand-file-name "config" user-init-dir))
(require 'sensible-defaults)
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

;; snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :defer 5
  :config
  (setq yas-snippet-dirs (list (expand-file-name "~/.emacs.d/snippets" )))
  (yas-global-mode 1))

;; Silences the warning when running a snippet with backticks (runs a command in the snippet)
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;; Smex for M-x command history (required by nano-counsel)
(use-package smex)

(use-package counsel
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-x b" . 'counsel-switch-buffer)
  ("C-x C-f" . 'counsel-find-file)
  ("C-s" . 'swiper)

  :config
  (use-package flx)

  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)

  ;; Use exact matching for M-x
  ;; (setq ivy-re-builders-alist
  ;;       '((counsel-M-x . ivy--regex-plus)
  ;;         (swiper . ivy--regex-plus)
  ;;         (t . ivy--regex-fuzzy)))

  ;; Prioritize exact matches for counsel-M-x
  ;; (setq ivy-sort-matches-functions-alist
  ;;       '((counsel-M-x . ivy--prefix-sort)
  ;;         (t . nil)))

  ;; Disable fuzzy matching for M-x completely (if needed)
  (setq counsel-M-x-use-fuzzy nil))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package ivy
  :diminish
  :bind (("C-f" . swiper)
         :map ivy-minibuffer-map
         ("ESC" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode))
;; (pdf-tools-install)

(use-package undo-tree
  :ensure t
  :init
  ;; globally turn on undo-tree
  (global-undo-tree-mode))

(setq evil-undo-system 'undo-tree)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)

  ;; make C-r redo
  (define-key evil-normal-state-map (kbd "C-r") 'evil-redo)
  (define-key evil-insert-state-map (kbd "C-r") 'evil-redo))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; macOS: Set Alt/Option as Meta, Command as Super
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; macOS: Command+Q to quit Emacs (without confirmation)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)

;; Install mu4e extensions from rougier's repos
(straight-use-package
 '(mu4e-dashboard :type git :host github :repo "rougier/mu4e-dashboard"))

(straight-use-package
 '(mu4e-thread-folding :type git :host github :repo "rougier/mu4e-thread-folding"))

;; Load experimental modules (theme colors already initialized above)
;; nano-mu4e is experimental and can cause issues - commented out for stability
;; Uncomment if you want SVG tags and enhanced mu4e styling
;; (with-eval-after-load 'mu4e
;;   (require 'nano-mu4e))

; ;; ui tweaks
(tooltip-mode -1)
(column-number-mode)
; (evil-commentary-mode)
; (setq visible-bell nil)
; (tool-bar-mode -1)
; (set-fringe-mode 10)
(setq confirm-kill-emacs nil)
;; (pixel-scroll-precision-mode)
(setq inhibit-startup-message t)
(global-prettify-symbols-mode t)
(setq shell-command-switch "-ic")
(setq counsel-find-file-at-point t)
(setq ring-bell-function 'ignore)
(global-hl-line-mode)
(set-window-scroll-bars (minibuffer-window) nil nil)
(setq frame-title-format '((:eval (projectile-project-name))))
(setq-default indent-tabs-mode nil)
(setq save-place-forget-unreadable-files nil)
(save-place-mode 1)
;; (set-face-attribute 'mode-line nil :height 80)
;; (set-face-attribute 'mode-line-inactive nil :height 150)
(set-face-attribute 'default nil :height 160)
(global-visual-line-mode t)

;; hide minor modes
;; (use-package moody
;;   :demand t
;;   :custom
;;   (x-underline-at-descent-line t)
;;   :config
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode))
;;
;; (use-package minions
;;   :config
;;   (setq minions-mode-line-lighter "?"
;;         minions-mode-line-delimiters (cons "" ""))
;;   (minions-mode 1))

;; Disabled doom-modeline in favor of nano-modeline
;; (use-package doom-modeline
;;   :ensure t
;;   :init
;;   (doom-modeline-mode 1)
;;   :config
;;   ;; set the height of the mode-line
;;   (set-face-attribute 'mode-line nil :height 160)
;;   (set-face-attribute 'mode-line-inactive nil :height 160))

(use-package deadgrep
  :defer t
  :commands (deadgrep)
  :config
  (evil-define-key 'motion deadgrep-mode-map (kbd "C-p") 'project-find-file)

  (defun deadgrep--include-args (rg-args)
    (push "--hidden" rg-args)
    (push "--glob=!.git/" rg-args))
  (advice-add 'deadgrep--arguments
              :filter-return #'deadgrep--include-args))

;; git
(use-package magit
  :hook (with-editor-mode . evil-insert-state)
  :bind ("C-x g" . magit-status)

  :config
  (use-package git-commit)
  (use-package magit-section)
  (use-package with-editor)

  (require 'git-rebase)

  (setq magit-push-always-verify nil
        git-commit-summary-max-length 50))

(use-package magit-popup :ensure t :demand t)

(use-package magit-delta
  ;; :ensure-system-package (delta . "cargo install git-delta")
  :hook (magit-mode . magit-delta-mode))

(use-package exec-path-from-shell)

(getenv "SHELL")
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package eat
  :config
  (eat-eshell-mode)
  (setq eshell-visual-commands '()))

;; terminal setup
(use-package better-shell
  :straight t)

(use-package eterm-256color

  :straight t
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :straight t

  :bind*(:map vterm-mode-map
  ("C-x C-k" . vterm-copy-mode)

  :map vterm-copy-mode-map
   ("C-x C-k" . vterm-copy-mode))

  :config
  (setq vterm-max-scrollback 100000))

(use-package multi-vterm
  :straight t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(require 'rainbow-delimiters)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))

(use-package olivetti
  :defer t
  :commands (olivetti-mode))
(auto-image-file-mode 1)

;; https://github.com/d12frosted/homebrew-emacs-plus/issues/383#issuecomment-899157143
(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

(use-package dired
  :straight nil
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (defun hrs/dired-slideshow ()
    (interactive)
    (start-process "dired-slideshow" nil "s" (dired-current-directory)))

  (evil-define-key 'normal dired-mode-map (kbd "o") 'dired-find-file-other-window)
  (evil-define-key 'normal dired-mode-map (kbd "v") 'hrs/dired-slideshow)

  (setq-default dired-listing-switches
                (combine-and-quote-strings '("-l" "-v" "-g"
                                             "--no-group"
                                             "--human-readable"
                                             "--time-style=+%Y-%m-%d"
                                             "--almost-all")))
  (setq dired-clean-up-buffers-too t
        dired-dwim-target t
        dired-recursive-copies 'always
        delete-by-moving-to-trash t
        dired-recursive-deletes 'top
        global-auto-revert-non-file-buffers t))


(use-package dired-single
  :commands (dired dired-jump))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "." 'dired-hide-dotfiles-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;; perform dired actions asynchronously
(use-package async
  :config
  (dired-async-mode 1))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

;; Keep gruvbox-theme available as backup
(use-package autothemer)  ; Required dependency for gruvbox-theme
(use-package gruvbox-theme)

(use-package circadian
  :ensure t
  :config
  ;; Set your location for sunrise/sunset times
  ;; Find your coordinates at: https://www.latlong.net/
  (setq calendar-latitude 40.0)   ; Replace with your latitude
  (setq calendar-longitude -70.0) ; Replace with your longitude
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  ;; (setq circadian-themes '((:sunrise . gruvbox-light-medium)
  ;;                          (:sunset  . gruvbox-dark-hard)))
  (circadian-setup))

;; colemak dh
;; (use-package evil-colemak-basics
;;   :init
;;   (setq evil-colemak-basics-layout-mod 'mod-dh)
;;   :config
;;   (global-evil-colemak-basics-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (C . t)
   (shell . t)
   (python . t)
   (js . t)
   (emacs-lisp . t)))

(setq org-babel-python-command "python3")
(setq org-confirm-babel-evaluate nil)
(setq org-startup-with-inline-images t)

;; https://github.com/org-roam/org-roam/issues/397#issuecomment-611751481
;; https://github.com/org-roam/org-roam-ui/issues/213
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Org/zettelkasten"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n a" . org-roam-alias-add)
         ("C-c n f" . org-roam-node-find)
	 ("C-c n g" . org-roam-ui-open)
         ("C-c n j" . org-roam-jump-to-index)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n d" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(setq org-default-notes-file "~/Org/agenda/tasks.org")
;; (load-user-file "config/agenda.el")

(require 'org-agenda)

(setq org-default-notes-file "~/Org/agenda/tasks.org")

(setq org-return-follows-link t
      org-agenda-tags-column 75
      org-deadline-warning-days 30
      org-use-speed-commands t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

;; agenda files
(setq org-agenda-files
      '("~/Org/agenda/tasks.org"
	"~/Org/agenda/school.org"
	"~/Org/agenda/birthdays.org"
	"~/Org/agenda/habits.org"))

(setq initial-major-mode 'org-mode)
;; org-agenda setup
(setq calendar-week-start-day 1)
(setq org-startup-folded t)

(defun my-org-ret-at-col-0 ()
  "In Org buffers, disable auto‑indent on RET and always insert newline at col 0."
  (electric-indent-local-mode -1)        ; turn off electric-indent just here
  (setq-local org-adapt-indentation nil) ; don’t auto-indent text under headings
  ;; Make RET in insert-state just do a raw newline (no indent)
  (when (boundp 'evil-insert-state-map)
    (define-key evil-insert-state-local-map (kbd "RET") #'newline)))

(add-hook 'org-mode-hook #'my-org-ret-at-col-0)

;; icloud syncing
(setq auto-save-default t
      auto-revert-use-notify nil
      auto-revert-verbose nil)
(global-auto-revert-mode 1)

(defun reset-org-config ()
  "Reset all org-mode global variables to their default state."
  (interactive)
  (org-reset-all-org-global-variables))

;; ls $(brew --prefix)/share/emacs/site-lisp/mu/mu4e
;; find $(brew --prefix) -type d -name "mu4e"
;; sudo ln -s $(brew --prefix)/share/emacs/site-lisp/mu/mu4e /usr/local/share/emacs/site-lisp/mu/mu4e
;; (use-package mu4e
;;   :load-path  "/usr/local/share/emacs/site-lisp/mu/mu4e/")

(use-package mu4e
  :load-path  "/usr/local/share/emacs/site-lisp/mu/mu4e/"
  :straight nil
  :commands (mu4e)
  :config

  ;; Set your email address (consider using custom.el or environment variable)
  ;; (setq user-mail-address "your-email@protonmail.com")
  (setq user-mail-address (or (getenv "EMAIL") user-mail-address))

  (setq mu4e-change-filenames-when-moving t ; avoid sync conflicts
      mu4e-update-interval (* 5 60) ; check mail every 5 minutes (faster updates)
      mu4e-compose-format-flowed t ; re-flow mail so it's not hard wrapped
      mu4e-get-mail-command "mbsync --config ~/.mbsyncrc protonmail"
      mu4e-maildir "~/mail/protonmail"
      ;; Performance optimizations
      mu4e-index-cleanup nil ; don't cleanup on every index (faster)
      mu4e-index-lazy-check t ; only check maildirs that have changed
      mu4e-headers-skip-duplicates t ; faster header view
      mu4e-headers-include-related nil ; don't include related messages (faster)
      mu4e-headers-results-limit 500 ; limit results for faster rendering
      mu4e-search-results-limit 500 ; limit search results
      mu4e-attachment-dir "~/Downloads" ; faster attachment handling
      ;; Error handling - don't show annoying errors
      mu4e-hide-index-messages t ; hide indexing messages
      mu4e-update-mail-and-index-run-in-background t) ; run updates in background

  (setq mu4e-drafts-folder "/Drafts"
      mu4e-sent-folder   "/Sent"
      mu4e-refile-folder "/All Mail"
      mu4e-trash-folder  "/Trash")

   (setq mu4e-headers-auto-update t                ; avoid to type `g' to update
      mu4e-view-show-images t                   ; show images in the view buffer
      mu4e-use-fancy-chars nil                  ; disable fancy chars for better compatibility
      mu4e-compose-signature-auto-include nil   ; I don't want a message signature
      mu4e-compose-reply-ignore-address `("no-?reply" ,user-mail-address)
      ;; Async mail fetching
      mu4e-index-update-in-background t ; update index in background
      mu4e-hide-index-messages t ; hide indexing messages for cleaner UI
      mu4e-modeline-support t ; show sync status in modeline
      ;; HTML email rendering - prefer text when possible
      mu4e-view-prefer-html nil ; prefer plain text over HTML
      mu4e-view-show-addresses t) ; show full email addresses

    ;; signature
   (setq message-signature "bgc")

   (setq mu4e-maildir-shortcuts
       '(("/INBOX"     . ?i)
         ("/Sent"      . ?s)
         ("/Trash"     . ?t)
         ("/Drafts"    . ?d)
         ("/All Mail"  . ?a)))

   (setq mu4e-bookmarks
     '((:name  "Unread messages"
        :query "flag:unread and maildir:/INBOX"
        :key   ?u)
       (:name  "Today's messages"
        :query "date:today..now"
        :key ?t)
       (:name  "Last 7 days"
        :query "date:7d..now"
        :key ?7)
       (:name  "Messages with Word docs"
        :query "mime:application/msword OR mime:application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        :key ?w)
       (:name  "Messages with PDF"
        :query "mime:application/pdf"
        :key ?p)
       (:name  "Messages with calendar event"
        :query "mime:text/calendar"
        :key ?e)))

   (setq message-send-mail-function 'smtpmail-send-it
       auth-sources '("~/.authinfo.gpg") ;need to use gpg version but only local smtp stored for now
       smtpmail-smtp-server "127.0.0.1"
       smtpmail-smtp-service 1025
       smtpmail-stream-type  'starttls)

  ;; Async mail fetching function with error handling
  (defun my-mu4e-update-mail-async ()
    "Update mail asynchronously without blocking Emacs."
    (interactive)
    (let ((proc (start-process "mbsync" "*mbsync-output*" "mbsync" "--config" (expand-file-name "~/.mbsyncrc") "protonmail")))
      (set-process-sentinel
       proc
       (lambda (process event)
         (cond
          ((string= event "finished\n")
           (message "✓ Mail synced")
           (mu4e-update-index nil)
           (when (and (boundp 'mu4e-headers-mode) mu4e-headers-mode)
             (mu4e-headers-rerun-search)))
          (t
           (with-current-buffer "*mbsync-output*"
             (let ((output (buffer-string)))
               (if (string-match "gpg\\|agent\\|timeout" output)
                   (message "⚠ GPG issue - run: gpg-connect-agent reloadagent /bye")
                 (message "⚠ Sync failed - check *mbsync-output* buffer"))))))))
      (message "Syncing mail...")))

  ;; Hook to auto-update after sync
  (add-hook 'mu4e-index-updated-hook 'mu4e-headers-rerun-search)

  ;; Use mu server for faster queries (if available)
  (when (executable-find "mu")
    (setq mu4e-mu-binary (executable-find "mu")))

  ;; Keybinding for async mail update
  (when (boundp 'mu4e-main-mode-map)
    (define-key mu4e-main-mode-map (kbd "U") 'my-mu4e-update-mail-async))
  (when (boundp 'mu4e-headers-mode-map)
    (define-key mu4e-headers-mode-map (kbd "U") 'my-mu4e-update-mail-async))

  )


(use-package org-msg
  :straight t
  :after mu4e
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (require 'org-msg)
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
      org-msg-startup "hidestars indent inlineimages"
      org-msg-default-alternatives '((new		. (text html))
                                     (reply-to-html	. (text html))
                                     (reply-to-text	. (text)))
      org-msg-convert-citation t)
  (org-msg-mode))

;; EAF removed - was causing issues with Emacs 31

;; (use-package markdown-mode
;;   :ensure t
;;   :mode ("README\\.md\\'" . gfm-mode)
;;   :init
;;   (setq markdown-command "multimarkdown")
;;   (setq markdown-header-scaling nil)
;;   :bind (:map markdown-mode-map
;;          ("C-c C-e" . markdown-do)))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode))
  :init
  (setq markdown-header-scaling nil)   ;; don't let markdown-mode scale headers
  (setq markdown-fontify-code-blocks-natively nil))  ;; disable syntax highlighting in code blocks


(use-package slime)
(setq slime-contribs '(slime-fancy))
(setq slime-net-coding-system 'utf-8-unix)

;; helper to connect to your RISC-V VM’s Swank
(defun my/slime-connect-riscv ()
  "Connect to SBCL/Swank on localhost:4005 (RISC-V VM)."
  (interactive)
  (slime-connect "localhost" 4005))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" default))
 '(org-agenda-files
   '("~/dots/emacs/.emacs.d/init.el" "~/Sync/Org/Todo.org"
     "~/Org/agenda/tasks.org" "~/Org/agenda/school.org"
     "~/Org/agenda/birthdays.org" "~/Org/agenda/habits.org"))
 '(send-mail-function 'smtpmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant italic))))
 '(markdown-code-face ((t (:inherit default))))
 '(markdown-header-face ((t (:inherit default :weight normal))))
 '(markdown-header-face-1 ((t (:inherit default :weight normal))))
 '(markdown-header-face-2 ((t (:inherit default :weight normal))))
 '(markdown-header-face-3 ((t (:inherit default :weight normal))))
 '(markdown-header-face-4 ((t (:inherit default :weight normal))))
 '(markdown-header-face-5 ((t (:inherit default :weight normal))))
 '(markdown-header-face-6 ((t (:inherit default :weight normal))))
 '(markdown-inline-code-face ((t (:inherit default))))
 '(markdown-pre-face ((t (:inherit default)))))

;;; Performance optimizations

;; Reduce rendering workload
(setq-default bidi-display-reordering nil  ; Disable bidirectional text for performance
              bidi-paragraph-direction 'left-to-right)

;; Make scrolling smoother
(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil
      scroll-margin 0
      scroll-preserve-screen-position t)

;; Improve LSP performance
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; Reduce unnecessary work
(setq idle-update-delay 1.0)  ; Update UI less frequently when idle

;; Don't compact font caches during GC
(setq inhibit-compacting-font-caches t)

;;; init.el ends here
