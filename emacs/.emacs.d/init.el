;;; init.el --- Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Nix-managed Emacs config for writing and productivity

;;; Code:

;;; --- DELAYED EXECUTION SYSTEM (from Kyure-A) ---

(defvar morph/delayed-priority-high-configurations '())
(defvar morph/delayed-priority-low-configurations '())

(defmacro with-delayed-execution (priority &rest body)
  "Execute BODY with delayed execution based on PRIORITY (:high or :low)."
  (declare (indent 1))
  `(cond
    ((eq ,priority :high)
     (push (lambda () ,@body) morph/delayed-priority-high-configurations))
    ((eq ,priority :low)
     (push (lambda () ,@body) morph/delayed-priority-low-configurations))
    (t
     (error "Unknown priority: %s" ,priority))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-timer 0.1 nil
                            (lambda ()
                              (dolist (fn (nreverse morph/delayed-priority-high-configurations))
                                (funcall fn))))))

(run-with-timer 0.3 0.001
                (lambda ()
                  (when morph/delayed-priority-low-configurations
                    (let ((fn (pop morph/delayed-priority-low-configurations)))
                      (when fn (funcall fn))))))

;;; --- STARTUP TIME ---

(defun morph/display-startup-time ()
  "Display Emacs startup time."
  (message "Emacs loaded in %.2f seconds with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'morph/display-startup-time)

;;; --- GC ---

(add-hook 'focus-out-hook #'garbage-collect)

;;; --- PACKAGE MANAGEMENT ---

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-defer t
      use-package-always-ensure nil
      use-package-expand-minimally t
      use-package-compute-statistics nil)

;;; --- GCMH ---

(use-package gcmh
  :demand t
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 128 1024 1024)
        gcmh-low-cons-threshold (* 16 1024 1024))
  (gcmh-mode 1)
  (when (boundp 'morph--file-name-handler-alist)
    (setq file-name-handler-alist morph--file-name-handler-alist)))

;;; --- TAB BAR ---

(use-package tab-bar
  :ensure nil
  :demand t
  :init
  (dotimes (n 5)
    (global-unset-key (kbd (format "C-%d" n)))
    (global-unset-key (kbd (format "M-%d" n))))
  :config
  (tab-bar-mode t)
  (global-set-key (kbd "M-0") 'tab-bar-switch-to-tab)
  (global-set-key (kbd "M-1") (lambda () (interactive) (tab-bar-select-tab 1)))
  (global-set-key (kbd "M-2") (lambda () (interactive) (tab-bar-select-tab 2)))
  (global-set-key (kbd "M-3") (lambda () (interactive) (tab-bar-select-tab 3)))
  (global-set-key (kbd "M-4") (lambda () (interactive) (tab-bar-select-tab 4))))

;;; --- CONFIG FILE LOADING ---

(defconst user-init-dir
  (cond ((and (boundp 'user-init-file) user-init-file)
         (file-name-directory (file-truename user-init-file)))
        ((boundp 'user-emacs-directory) user-emacs-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  "Load FILE from user configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))

;; keymaps.el is read-only (Nix); keybindings consolidated below
;; (load-user-file "config/keymaps.el")
(load-user-file "config/utils.el")
;; vterm-config.el keybindings moved below for consistency
;; (load-user-file "config/vterm-config.el")

;;; --- CONSOLIDATED KEYBINDINGS ---
;; All keybindings in one place for easy management.
;; Prefix key organization:
;;   C-c e - Emacs/editing commands
;;   C-c n - Notes (org-roam) - MUST use global-set-key, not :bind
;;   C-c t - Terminal (vterm)
;;   C-c g - Git
;;   C-c f - Files/utilities
;;   C-c d - Date/time
;;   C-c a - Applications (atomic-chrome)
;;   C-c r - Remote (TRAMP)

;; Autoloads for deferred packages
(autoload 'counsel-load-theme "counsel" nil t)
(autoload 'mu4e "mu4e" nil t)
(autoload 'olivetti-mode "olivetti" nil t)
(autoload 'vterm "vterm" nil t)
(autoload 'multi-vterm "multi-vterm" nil t)
(autoload 'multi-vterm-next "multi-vterm" nil t)
(autoload 'multi-vterm-prev "multi-vterm" nil t)
(autoload 'magit-status "magit" nil t)
(autoload 'git-timemachine-show-previous-revision "git-timemachine" nil t)
(autoload 'git-timemachine-show-next-revision "git-timemachine" nil t)
(autoload 'git-timemachine-show-current-revision "git-timemachine" nil t)
(autoload 'deadgrep "deadgrep" nil t)
(autoload 'project-find-file "project" nil t)
(autoload 'org-roam-node-find "org-roam" nil t)
(autoload 'org-roam-node-insert "org-roam" nil t)
(autoload 'org-roam-capture "org-roam" nil t)
(autoload 'org-roam-buffer-toggle "org-roam" nil t)
(autoload 'org-roam-alias-add "org-roam" nil t)
(autoload 'org-roam-ui-open "org-roam-ui" nil t)
(autoload 'org-roam-dailies-capture-today "org-roam-dailies" nil t)

;; Helper functions (must be defined before keybindings)
(defun my/vterm-window-split ()
  "Split window and open vterm in the bottom split."
  (interactive)
  (split-window-below -15)
  (other-window 1)
  (vterm))

(defun org-roam-jump-to-index ()
  "Jump to the org-roam index file."
  (interactive)
  (require 'org-roam)
  (org-roam-node-find nil "Index"))

;; Define nested prefix keymaps for C-c
;; Each prefix (e, t, g, f, d, n) needs its own keymap for proper nesting
(defvar morph/emacs-map (make-sparse-keymap) "C-c e - Emacs/editing commands")
(defvar morph/terminal-map (make-sparse-keymap) "C-c t - Terminal commands")
(defvar morph/git-map (make-sparse-keymap) "C-c g - Git commands")
(defvar morph/file-map (make-sparse-keymap) "C-c f - File utilities")
(defvar morph/date-map (make-sparse-keymap) "C-c d - Date/time commands")
(defvar morph/notes-map (make-sparse-keymap) "C-c n - Notes (org-roam)")

;; C-c e - Emacs/editing commands
(define-key morph/emacs-map (kbd "i") (lambda () (interactive)
                                         (find-file (expand-file-name "init.el" user-emacs-directory))))
(define-key morph/emacs-map (kbd "k") (lambda () (interactive)
                                         (find-file (expand-file-name "init.el" user-emacs-directory))
                                         (goto-char (point-min))
                                         (search-forward "CONSOLIDATED KEYBINDINGS" nil t)))
(define-key morph/emacs-map (kbd "R") #'reload-config)
(define-key morph/emacs-map (kbd "p") #'package-install)
(define-key morph/emacs-map (kbd "t") #'counsel-load-theme)
(define-key morph/emacs-map (kbd "m") #'mu4e)
(define-key morph/emacs-map (kbd "o") #'olivetti-mode)
(define-key morph/emacs-map (kbd "b") #'eval-buffer)
(define-key morph/emacs-map (kbd "e") #'eval-region)
(define-key morph/emacs-map (kbd "r") #'org-babel-execute-src-block)

;; C-c t - Terminal commands
(define-key morph/terminal-map (kbd "t") #'multi-vterm)
(define-key morph/terminal-map (kbd "s") #'my/vterm-window-split)
(define-key morph/terminal-map (kbd "n") #'multi-vterm-next)
(define-key morph/terminal-map (kbd "p") #'multi-vterm-prev)

;; C-c g - Git commands
(define-key morph/git-map (kbd "g") #'magit-status)
(define-key morph/git-map (kbd "p") #'git-timemachine-show-previous-revision)
(define-key morph/git-map (kbd "n") #'git-timemachine-show-next-revision)
(define-key morph/git-map (kbd "c") #'git-timemachine-show-current-revision)

;; C-c f - File utilities
(define-key morph/file-map (kbd "y") #'copy-filename)
(define-key morph/file-map (kbd "s") #'org-screenshot)
(define-key morph/file-map (kbd "g") #'deadgrep)
(define-key morph/file-map (kbd "f") #'project-find-file)

;; C-c d - Date/time
(define-key morph/date-map (kbd "i") #'insert-current-date)

;; C-c n - Notes (org-roam)
(define-key morph/notes-map (kbd "f") #'org-roam-node-find)
(define-key morph/notes-map (kbd "i") #'org-roam-node-insert)
(define-key morph/notes-map (kbd "c") #'org-roam-capture)
(define-key morph/notes-map (kbd "l") #'org-roam-buffer-toggle)
(define-key morph/notes-map (kbd "a") #'org-roam-alias-add)
(define-key morph/notes-map (kbd "g") #'org-roam-ui-open)
(define-key morph/notes-map (kbd "d") #'org-roam-dailies-capture-today)
(define-key morph/notes-map (kbd "j") #'org-roam-jump-to-index)

;; Bind prefix maps to C-c using global-set-key
(global-set-key (kbd "C-c e") morph/emacs-map)
(global-set-key (kbd "C-c t") morph/terminal-map)
(global-set-key (kbd "C-c g") morph/git-map)
(global-set-key (kbd "C-c f") morph/file-map)
(global-set-key (kbd "C-c d") morph/date-map)
(global-set-key (kbd "C-c n") morph/notes-map)

;; General keybindings (not under C-c prefix)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key [C-mouse-wheel-up-event]   'text-scale-increase)
(global-set-key [C-mouse-wheel-down-event] 'text-scale-decrease)

;; Legacy bindings (muscle memory)
(global-set-key (kbd "C-x g") #'magit-status)

(add-to-list 'load-path (expand-file-name "config" user-init-dir))
(require 'sensible-defaults)
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

;; Load font-resize AFTER sensible-defaults to override its font keybindings
(load-user-file "config/font-resize.el")

;;; --- SNIPPETS ---

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 5
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-global-mode 1))

(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;;; --- COMPLETION (Ivy/Counsel) ---

(use-package counsel
  :demand t
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-x b" . 'counsel-switch-buffer)
  ("C-x C-f" . 'counsel-find-file)
  ("C-s" . 'swiper)
  :config
  (use-package flx)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-initial-inputs-alist nil
        counsel-M-x-use-fuzzy nil))

(use-package counsel-tramp
  :defer t
  :commands (counsel-tramp)
  :config
  (setq counsel-tramp-default-directory "~/"
        counsel-tramp-custom-connections '()))

;; C-c r - Remote (TRAMP)
(if (locate-library "counsel-tramp")
    (progn
      (autoload 'counsel-tramp "counsel-tramp" nil t)
      (global-set-key (kbd "C-c r") #'counsel-tramp))
  ;; Fallback TRAMP connect if counsel-tramp unavailable
  (defun my/tramp-connect ()
    "Connect to SSH host interactively using TRAMP."
    (interactive)
    (require 'tramp)
    (let* ((hosts (delete-dups
                   (mapcar #'car
                           (append
                            (when (file-exists-p "~/.ssh/config")
                              (tramp-parse-sconfig "~/.ssh/config"))
                            (when (file-exists-p "/etc/ssh/ssh_config")
                              (tramp-parse-sconfig "/etc/ssh/ssh_config"))))))
           (host (completing-read "SSH host: " hosts))
           (default-directory (format "/ssh:%s:~/" host))
           (remote-dir (read-directory-name "Remote directory: " default-directory)))
      (find-file remote-dir)))
  (global-set-key (kbd "C-c r") #'my/tramp-connect))

(use-package ivy
  :demand t
  :diminish
  :bind (;; Removed C-f binding - conflicts with Evil page-down
         ;; Use C-s for swiper instead (already bound in counsel)
         :map ivy-minibuffer-map
         ("ESC" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;;; --- PDF ---

(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode))

;;; --- EVIL ---

(use-package undo-tree
  :demand t  ;; Evil needs undo-tree loaded immediately
  :init
  ;; Store undo history centrally, not in working directories
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree-history/" user-emacs-directory))))
  (setq undo-tree-auto-save-history t)
  :config
  ;; Enable globally for Evil undo/redo to work everywhere
  (global-undo-tree-mode 1))

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (define-key evil-normal-state-map (kbd "C-r") 'evil-redo)
  (define-key evil-insert-state-map (kbd "C-r") 'evil-redo)

  ;; Ensure new emacsclient frames start with scratch buffer and Evil active
  (defun morph/setup-new-frame (frame)
    "Set up new FRAME with scratch buffer and Evil normal state."
    (run-with-timer
     0.1 nil
     (lambda ()
       (with-selected-frame frame
         (switch-to-buffer "*scratch*")
         (org-mode)
         (evil-normal-state)
         (select-frame-set-input-focus frame)
         (force-mode-line-update t)
         (redraw-frame frame)))))
  (add-hook 'after-make-frame-functions #'morph/setup-new-frame)

  ;; Also handle server-visit for emacsclient
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (run-with-timer
               0.1 nil
               (lambda ()
                 (switch-to-buffer "*scratch*")
                 (org-mode)
                 (evil-normal-state)
                 (select-frame-set-input-focus (selected-frame))
                 (force-mode-line-update t)
                 (redraw-frame))))))

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

(use-package evil-org
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-commentary
  :demand t
  :config
  (evil-commentary-mode))

;;; --- macOS ---

(setq mac-option-modifier 'meta
      mac-command-modifier 'super)
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs)

;;; --- UI SETTINGS ---

(tooltip-mode -1)
(column-number-mode)
(setq confirm-kill-emacs nil
      inhibit-startup-message t
      shell-command-switch "-ic"
      counsel-find-file-at-point t
      ring-bell-function 'ignore)
(global-prettify-symbols-mode t)
(global-hl-line-mode)
(set-window-scroll-bars (minibuffer-window) nil nil)
(setq-default indent-tabs-mode nil)
(setq save-place-forget-unreadable-files nil)
(save-place-mode 1)
(global-visual-line-mode t)

;;; --- SEARCH ---

(use-package deadgrep
  :defer t
  :commands (deadgrep)
  :config
  (evil-define-key 'motion deadgrep-mode-map (kbd "C-p") 'project-find-file)
  (defun deadgrep--include-args (rg-args)
    (push "--hidden" rg-args)
    (push "--glob=!.git/" rg-args))
  (advice-add 'deadgrep--arguments :filter-return #'deadgrep--include-args))

;;; --- GIT ---

(use-package magit
  :hook (with-editor-mode . evil-insert-state)
  :bind ("C-x g" . magit-status)
  :config
  (use-package git-commit)
  (use-package magit-section)
  (use-package with-editor)
  (require 'git-rebase)
  (setq magit-push-always-verify nil
        git-commit-summary-max-length 50
        magit-commit-show-diff nil
        magit-branch-direct-configure nil
        magit-refresh-status-buffer nil
        magit-tramp-pipe-stty-settings 'pty))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

;;; --- SHELL/PATH ---

(use-package exec-path-from-shell
  :defer 1
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; --- TERMINAL ---

(use-package eat
  :config
  (eat-eshell-mode)
  (setq eshell-visual-commands '()))

(use-package vterm
  :bind* (:map vterm-mode-map
          ("C-x C-k" . vterm-copy-mode)
          :map vterm-copy-mode-map
          ("C-x C-k" . vterm-copy-mode))
  :config
  (setq vterm-max-scrollback 100000))

(use-package multi-vterm)

;;; --- EDITING HELPERS ---

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :demand t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode))

(use-package olivetti
  :defer t
  :commands (olivetti-mode))

(auto-image-file-mode 1)

;;; --- DIRED ---

(setq insert-directory-program "gls" dired-use-ls-dired t)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (evil-define-key 'normal dired-mode-map (kbd "o") 'dired-find-file-other-window)
  (setq-default dired-listing-switches "-alh --group-directories-first")
  (setq dired-clean-up-buffers-too t
        dired-dwim-target t
        dired-recursive-copies 'always
        delete-by-moving-to-trash t
        dired-recursive-deletes 'top
        global-auto-revert-non-file-buffers t))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "." 'dired-hide-dotfiles-mode))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package async
  :config
  (dired-async-mode 1))

;;; --- THEMES ---

(use-package autothemer)
(use-package gruvbox-theme)

(use-package circadian
  :config
  (setq calendar-latitude 40.0
        calendar-longitude -70.0
        circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

;;; --- NIX ---

(use-package nix-mode
  :mode "\\.nix\\'")

;;; --- ORG MODE ---

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (C . t)
   (shell . t)
   (python . t)
   (js . t)
   (emacs-lisp . t)))

(setq org-babel-python-command "python3"
      org-confirm-babel-evaluate nil
      org-startup-with-inline-images t)

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Org/zettelkasten"))
  :config
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;;; --- ORG AGENDA ---

(setq org-default-notes-file "~/Org/agenda/tasks.org")

(require 'org-agenda)

(setq org-return-follows-link t
      org-agenda-tags-column 75
      org-deadline-warning-days 30
      org-use-speed-commands t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))

(setq org-agenda-files
      '("~/Org/agenda/tasks.org"
        "~/Org/agenda/school.org"
        "~/Org/agenda/birthdays.org"
        "~/Org/agenda/habits.org"))

(setq initial-major-mode 'org-mode
      calendar-week-start-day 1
      org-startup-folded t)

;; Ensure scratch buffer uses org-mode and Evil starts in normal state
(setq initial-buffer-choice
      (lambda ()
        (let ((buf (get-buffer-create "*scratch*")))
          (with-current-buffer buf
            (org-mode)
            (when (fboundp 'evil-normal-state)
              (evil-normal-state)))
          buf)))

(defun my-org-ret-at-col-0 ()
  "In Org buffers, disable auto-indent on RET and insert newline at col 0."
  (electric-indent-local-mode -1)
  (setq-local org-adapt-indentation nil)
  (when (boundp 'evil-insert-state-map)
    (define-key evil-insert-state-local-map (kbd "RET") #'newline)))

(add-hook 'org-mode-hook #'my-org-ret-at-col-0)

;; iCloud syncing
(setq auto-save-default t
      auto-revert-use-notify nil
      auto-revert-verbose nil)
(global-auto-revert-mode 1)

;;; --- EMAIL (mu4e) ---

(use-package mu4e
  :if (executable-find "mu")  ; Only load if mu is installed
  :ensure nil  ; mu4e comes bundled with the mu package
  :commands (mu4e)
  :config
  (setq user-mail-address (or (getenv "EMAIL") user-mail-address))

  (setq mu4e-change-filenames-when-moving t
        mu4e-update-interval (* 5 60)
        mu4e-compose-format-flowed t
        mu4e-get-mail-command "mbsync --config ~/.mbsyncrc protonmail"
        mu4e-maildir "~/mail/protonmail"
        mu4e-index-cleanup nil
        mu4e-index-lazy-check t
        mu4e-headers-skip-duplicates t
        mu4e-headers-include-related nil
        mu4e-headers-results-limit 500
        mu4e-search-results-limit 500
        mu4e-attachment-dir "~/Downloads"
        mu4e-hide-index-messages t
        mu4e-update-mail-and-index-run-in-background t)

  (setq mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder "/Sent"
        mu4e-refile-folder "/All Mail"
        mu4e-trash-folder "/Trash")

  (setq mu4e-headers-auto-update t
        mu4e-view-show-images t
        mu4e-use-fancy-chars nil
        mu4e-compose-signature-auto-include nil
        mu4e-compose-reply-ignore-address `("no-?reply" ,user-mail-address)
        mu4e-index-update-in-background t
        mu4e-modeline-support t
        mu4e-view-prefer-html nil
        mu4e-view-show-addresses t)

  (setq message-signature "bgc")

  (setq mu4e-maildir-shortcuts
        '(("/INBOX"     . ?i)
          ("/Sent"      . ?s)
          ("/Trash"     . ?t)
          ("/Drafts"    . ?d)
          ("/All Mail"  . ?a)))

  (setq mu4e-bookmarks
        '((:name "Unread messages"
           :query "flag:unread and maildir:/INBOX"
           :key ?u)
          (:name "Today's messages"
           :query "date:today..now"
           :key ?t)
          (:name "Last 7 days"
           :query "date:7d..now"
           :key ?7)
          (:name "Messages with PDF"
           :query "mime:application/pdf"
           :key ?p)))

  (setq message-send-mail-function 'smtpmail-send-it
        auth-sources '("~/.authinfo.gpg")
        smtpmail-smtp-server "127.0.0.1"
        smtpmail-smtp-service 1025
        smtpmail-stream-type 'starttls)

  (defun my-mu4e-update-mail-async ()
    "Update mail asynchronously without blocking Emacs."
    (interactive)
    (let ((proc (start-process "mbsync" "*mbsync-output*" "mbsync"
                               "--config" (expand-file-name "~/.mbsyncrc") "protonmail")))
      (set-process-sentinel
       proc
       (lambda (process event)
         (cond
          ((string= event "finished\n")
           (message "Mail synced")
           (mu4e-update-index nil))
          (t
           (message "Sync failed - check *mbsync-output* buffer")))))
    (message "Syncing mail...")))

  (add-hook 'mu4e-index-updated-hook 'mu4e-headers-rerun-search)

  (when (executable-find "mu")
    (setq mu4e-mu-binary (executable-find "mu")))

  (when (boundp 'mu4e-main-mode-map)
    (define-key mu4e-main-mode-map (kbd "U") 'my-mu4e-update-mail-async))
  (when (boundp 'mu4e-headers-mode-map)
    (define-key mu4e-headers-mode-map (kbd "U") 'my-mu4e-update-mail-async)))

(use-package org-msg
  :after mu4e
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (require 'org-msg)
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-default-alternatives '((new          . (text html))
                                       (reply-to-html . (text html))
                                       (reply-to-text . (text)))
        org-msg-convert-citation t)
  (org-msg-mode))

;;; --- MARKDOWN ---

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode))
  :init
  (setq markdown-header-scaling nil
        markdown-fontify-code-blocks-natively nil))

;;; --- FINANCE ---

(use-package hledger-mode
  :mode ("\\.\\(h?ledger\\|journal\\|j\\)\\'" . hledger-mode)
  :commands (hledger-mode)
  :init
  (setq hledger-jfile "~/Documents/hledger/main.journal")
  :config
  (add-hook 'hledger-mode-hook
            (lambda ()
              (company-mode)
              (setq-local company-backends '(hledger-company)))))

(when (file-exists-p "~/Sync/projects/ai-financial-assistant/emacs/ai-financial-working.el")
  (load-file "~/Sync/projects/ai-financial-assistant/emacs/ai-financial-working.el")
  (add-hook 'hledger-mode-hook
            (lambda ()
              ;; C-c $ - Finance/money (local to hledger-mode)
              (local-set-key (kbd "C-c $ c") 'ai-financial-categorize-line)
              (local-set-key (kbd "C-c $ C") 'ai-financial-categorize-all)
              (local-set-key (kbd "C-c $ a") 'ai-financial-quick-analysis))))

;;; --- LISP ---

(use-package slime
  :config
  (setq slime-contribs '(slime-fancy)
        slime-net-coding-system 'utf-8-unix))

;;; --- CUSTOM (managed by Customize) ---

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378"
     "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" default))
 '(package-selected-packages
   '(async atomic-chrome circadian counsel deadgrep dired-hide-dotfiles eat
           evil-collection evil-commentary evil-org exec-path-from-shell flx
           gcmh git-commit gruvbox-theme hledger-mode magit-delta markdown-mode
           modus-themes multi-vterm nerd-icons-dired nix-mode olivetti org-msg
           org-roam-ui pdf-tools rainbow-delimiters slime undo-tree use-package
           which-key yasnippet))
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

;;; --- PERFORMANCE ---

(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil
      scroll-margin 0
      scroll-preserve-screen-position t
      read-process-output-max (* 1024 1024)
      idle-update-delay 1.0
      inhibit-compacting-font-caches t
      process-adaptive-read-buffering nil)

;;; --- TRAMP ---

(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t
      tramp-copy-size-limit (* 1024 1024)
      tramp-verbose 2)

(with-eval-after-load 'tramp
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook
                 #'tramp-compile-disable-ssh-controlmaster-options)))

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(defun $lsp-unless-remote ()
  "Start LSP only if not on a remote host."
  (if (file-remote-p buffer-file-name)
      (progn (eldoc-mode -1)
             (setq-local completion-at-point-functions nil))
    (lsp)))

;;; --- CHROME EMACS ---

(use-package atomic-chrome
  :demand t
  :config
  (setq atomic-chrome-extension-type-list '(atomic-chrome))
  (setq atomic-chrome-buffer-open-style 'frame)
  (setq atomic-chrome-default-major-mode 'markdown-mode)
  (setq atomic-chrome-auto-remove-file t)
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("gitlab\\.com" . gfm-mode)
          ("reddit\\.com" . markdown-mode)
          ("stackoverflow\\.com" . markdown-mode)
          ("us-east-2\\.console\\.aws\\.amazon\\.com" . yaml-ts-mode)
          ("leetcode\\.com" . typescript-ts-mode)
          ("" . markdown-mode)))

  ;; --- Frame transparency (50%) ---
  (defun atomic-chrome--apply-frame-settings (orig-fn buffer title)
    "Apply transparency to newly created atomic-chrome frame."
    (let ((frame (funcall orig-fn buffer title)))
      (when (framep frame)
        (set-frame-parameter frame 'alpha '(50 . 50)))
      frame))
  (advice-add 'atomic-chrome-show-edit-buffer
              :around #'atomic-chrome--apply-frame-settings)

  ;; --- Focus browser when done editing ---
  (defun atomic-chrome--focus-browser ()
    "Focus Brave Browser (falling back to Chrome/Safari)."
    (call-process "osascript" nil 0 nil
                  "-e" "tell application \"System Events\"
  set frontApp to name of first application process whose frontmost is true
end tell
if frontApp is \"Emacs\" then
  tell application \"Brave Browser\" to activate
end if"))

  ;; --- Proper close: send text, close buffer, restart server, focus browser ---
  (defun atomic-chrome--close-and-refocus ()
    "Send buffer to browser, close properly, ensure server, focus browser."
    (interactive)
    (when (bound-and-true-p atomic-chrome-edit-mode)
      (atomic-chrome-send-buffer-text)
      (let ((buf (current-buffer)))
        ;; Close the edit buffer through atomic-chrome's proper cleanup
        (atomic-chrome-close-current-buffer)
        ;; Ensure server is alive after close
        (run-with-timer 0.3 nil #'atomic-chrome--ensure-server)
        ;; Focus browser
        (run-with-timer 0.1 nil #'atomic-chrome--focus-browser))))

  ;; Hook into atomic-chrome's own done hook for any close path
  (add-hook 'atomic-chrome-edit-done-hook #'atomic-chrome--focus-browser)

  ;; --- Evil :wq / :q overrides (only in atomic-chrome buffers) ---
  (with-eval-after-load 'evil
    ;; Advice functions to handle atomic-chrome vs normal buffers
    (defun atomic-chrome--advice-evil-save-and-close (orig-fn &rest args)
      "Advice for evil-save-and-close to handle atomic-chrome buffers."
      (if (bound-and-true-p atomic-chrome-edit-mode)
          (atomic-chrome--close-and-refocus)
        (apply orig-fn args)))

    (defun atomic-chrome--advice-evil-quit (orig-fn &rest args)
      "Advice for evil-quit to handle atomic-chrome buffers."
      (if (bound-and-true-p atomic-chrome-edit-mode)
          (progn
            (atomic-chrome-close-current-buffer)
            (run-with-timer 0.3 nil #'atomic-chrome--ensure-server)
            (run-with-timer 0.1 nil #'atomic-chrome--focus-browser))
        (apply orig-fn args)))

    ;; Apply advice to Evil's save-and-close and quit functions
    (advice-add 'evil-save-and-close :around #'atomic-chrome--advice-evil-save-and-close)
    (advice-add 'evil-quit :around #'atomic-chrome--advice-evil-quit))

  ;; --- Robust server lifecycle ---
  (defun atomic-chrome--server-alive-p ()
    "Return non-nil if the atomic-chrome websocket server is alive."
    (and (boundp 'atomic-chrome-server-atomic-chrome)
         atomic-chrome-server-atomic-chrome
         (processp atomic-chrome-server-atomic-chrome)
         (process-live-p atomic-chrome-server-atomic-chrome)))

  (defun atomic-chrome--ensure-server (&rest _)
    "Restart the atomic-chrome server if it has died."
    (unless (atomic-chrome--server-alive-p)
      (ignore-errors
        ;; Clean up stale server state before restarting
        (when (boundp 'atomic-chrome-server-atomic-chrome)
          (setq atomic-chrome-server-atomic-chrome nil))
        (atomic-chrome-start-server)
        (message "atomic-chrome: server restarted"))))

  ;; Multiple safety nets (all lightweight - just a boundp + processp check)
  (add-hook 'delete-frame-functions
            (lambda (_frame)
              (run-with-timer 0.5 nil #'atomic-chrome--ensure-server)))
  (add-hook 'focus-in-hook #'atomic-chrome--ensure-server)
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (run-with-idle-timer 1 nil #'atomic-chrome--ensure-server)))

  ;; --- Keybindings ---
  (define-key atomic-chrome-edit-mode-map (kbd "C-c C-c") 'atomic-chrome--close-and-refocus)
  (define-key atomic-chrome-edit-mode-map (kbd "C-c C-k") 'atomic-chrome--close-and-refocus)
  (define-key atomic-chrome-edit-mode-map (kbd "C-x C-s") 'atomic-chrome-send-buffer-text)

  ;; Start the server
  (atomic-chrome-start-server))

;; C-c a - Applications prefix map
(defvar morph/apps-map (make-sparse-keymap) "C-c a - Application commands")
(define-key morph/apps-map (kbd "s")
            (lambda () (interactive)
              (require 'atomic-chrome)
              (ignore-errors (atomic-chrome-stop-server))
              (sit-for 0.3)
              (atomic-chrome-start-server)
              (message "atomic-chrome: server restarted on port 64292")))
(global-set-key (kbd "C-c a") morph/apps-map)

;;; --- FINAL KEYBINDING SETUP ---
;; Keybindings are consolidated in prefix keymaps above.
;; This section ensures the C-c prefix maps persist after all packages load.

(defun morph/ensure-keybindings ()
  "Re-bind all C-c prefix keymaps after packages load."
  (global-set-key (kbd "C-c e") morph/emacs-map)
  (global-set-key (kbd "C-c t") morph/terminal-map)
  (global-set-key (kbd "C-c g") morph/git-map)
  (global-set-key (kbd "C-c f") morph/file-map)
  (global-set-key (kbd "C-c d") morph/date-map)
  (global-set-key (kbd "C-c n") morph/notes-map)
  (global-set-key (kbd "C-c a") morph/apps-map))

;; Run after init completes
(add-hook 'emacs-startup-hook #'morph/ensure-keybindings 100)
;; Also run after a short delay to catch any deferred loading
(run-with-idle-timer 1 nil #'morph/ensure-keybindings)

;;; init.el ends here
