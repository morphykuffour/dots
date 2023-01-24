;; emacs os config for writing and productivity

;; use-package
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)
(setq package-check-signature nil)

(defun package--save-selected-packages (&rest opt) nil)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; (straight-use-package 'org)
(use-package org :straight (:type built-in))


(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory) user-emacs-directory)
        ((boundp 'user-init-directory) user-init-directory)
        (t "~/.emacs.d/")))

;; copy pasta
(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "font-resize.el")
(load-user-file "keymaps.el")
(load-user-file "utils.el")
;; (load-user-file "org-mode.el")

;; place custom-set-variables into its own file
;; (setq custom-file (concat user-emacs-directory "/custom.el"))
                                        ; (load-file custom-file)

;; sensible settings from hrs
(add-to-list  'load-path "~/.emacs.d/personal/sensible-defaults.el")
(require 'sensible-defaults)
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(use-package yasnippet
  :demand t
  :config
  (setq yas-indent-line 'auto)
  (yas-global-mode 1))

(use-package undo-fu)

(use-package evil
  :demand t

  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil
	evil-want-keybinding nil)

  :config
  (evil-mode 1)

  (evil-define-key '(normal insert) 'global (kbd "C-p") 'project-find-file)

  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-define-key 'insert org-mode-map (kbd "S-<right>") 'org-shiftright)
  (evil-define-key 'insert org-mode-map (kbd "S-<left>") 'org-shiftleft)

  (fset 'evil-visual-update-x-selection 'ignore))

;; (pdf-tools-install)

(use-package evil-collection
  :after evil
  :demand t

  :config
  (setq evil-collection-mode-list
        '(deadgrep
          dired
          elfeed
          eww
          ibuffer
          info
          magit
          mu4e
          package-menu
          pdf-view
          proced
          replace
          vterm
          which-key))

  (evil-collection-init))

(use-package evil-org
  :after org
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package vertico
  :config
  (vertico-mode))

;; ui tweaks
(tooltip-mode -1)
(column-number-mode)
(evil-commentary-mode)
(setq visible-bell nil)
(tool-bar-mode -1)
(set-fringe-mode 10)
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
;; (set-face-attribute 'mode-line nil :height 150)
;; (set-face-attribute 'mode-line-inactive nil :height 150)

;; hide minor modes
(use-package moody
  :demand t

  :custom
  (x-underline-at-descent-line t)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package battery
  :if (not (display-graphic-p))
  :config
  (when (and battery-status-function
             (not (string-match-p "unknown"
                                  (battery-format "%B" (funcall battery-status-function)))))
    (display-battery-mode 1)))

(use-package minions
  :config
  (setq minions-mode-line-lighter "?"
        minions-mode-line-delimiters (cons "" ""))
  (minions-mode 1))


(use-package deadgrep
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

;; page through history of a file
(use-package git-timemachine)

(use-package magit-delta
  ;; :ensure-system-package (delta . "cargo install git-delta")
  :hook (magit-mode . magit-delta-mode))

(getenv "SHELL")
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(require 'rainbow-delimiters)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; (use-package all-the-icons-ivy-rich
;;   :init
;;   (all-the-icons-ivy-rich-mode 1))

;; (use-package all-the-icons)

(use-package smex)

(use-package counsel
  :bind
  ;; ("M-x" . 'counsel-M-x)
  ("C-x b" . 'counsel-switch-buffer)
  ("C-x C-f" . 'counsel-find-file)
  ("C-s" . 'swiper)

  :config
  (use-package flx)

  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))

(require 'olivetti)
(auto-image-file-mode 1)


(use-package dired
  :straight nil
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
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
        dired-recursive-deletes 'top
        global-auto-revert-non-file-buffers t))

(use-package dired-single
  :commands (dired dired-jump))

;; (use-package dired-hide-dotfiles
;;   :hook (dired-mode . dired-hide-dotfiles-mode)
;;   :config
;;   (evil-collection-define-key 'normal 'dired-mode-map
;;     "." 'dired-hide-dotfiles-mode))

;; (use-package dired-open
;;   :config
;;   (setq dired-open-extensions
;;         '(("avi" . "mpv")
;;           ("cbr" . "zathura")
;;           ("doc" . "libreoffice")
;;           ("docx". "libreoffice")
;;           ("gif" . "ffplay")
;;           ("gnumeric" . "gnumeric")
;;           ("jpeg". "sxiv")
;;           ("jpg" . "sxiv")
;;           ("mkv" . "mpv")
;;           ("mov" . "mpv")
;;           ("mp3" . "mpv")
;;           ("mp4" . "mpv")
;;           ("pdf" . "zathura")
;;           ("png" . "s")
;;           ("webm" . "mpv")
;;           ("xls" . "gnumeric")
;;           ("xlsx" . "gnumeric"))))

;; perform dired actions asynchronously
(use-package async
  :config
  (dired-async-mode 1))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

;; sly
(setq inferior-lisp-program "sbcl")

(use-package circadian
  :ensure t
  :config
  (setq calendar-latitude 40.0)
  (setq calendar-longitude -70.0)
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

;; colemak dh
;; (use-package evil-colemak-basics
;;   :init
;;   (setq evil-colemak-basics-layout-mod 'mod-dh)
;;   :config
;;   (global-evil-colemak-basics-mode))

;; (use-package helpful
;;   :commands (helpful-callable helpful-variable helpful-command helpful-key)
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key] . helpful-key))




(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (C . t)
   (shell . t)
   (python . t)
   (js . t)
   (emacs-lisp . t)))

;; https://github.com/org-roam/org-roam/issues/397#issuecomment-611751481
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
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(setq org-default-notes-file "~/Org/agenda/tasks.org")
;; (load-user-file "agenda.el")

(require 'org-agenda)

(setq org-default-notes-file "~/Org/agenda/tasks.org")

;; define the custum capture templates
(setq org-capture-templates
      '(("t" "todo" entry (file org-default-notes-file)
	 "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
	("m" "Meeting" entry (file org-default-notes-file)
	 "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
	("d" "Diary" entry (file+datetree "~/Org/org/diary.org")
	 "* %?\n%U\n" :clock-in t :clock-resume t)
	("i" "Idea" entry (file org-default-notes-file)
	 "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
	("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	 "** NEXT %? \nDEADLINE: %t") ))

;; agenda files
(setq org-agenda-files
      '("~/Org/agenda/tasks.org"
	"~/Org/agenda/school.org"
	"~/Org/agenda/birthdays.org"
	"~/Org/agenda/habits.org"))

(setq initial-major-mode 'org-mode)
;; org-agenda setup
(setq calendar-week-start-day 1)

;; time grid
(setq org-agenda-time-leading-zero t)
(setq org-agenda-timegrid-use-ampm nil)
(setq org-agenda-use-time-grid t)
(setq org-agenda-show-current-time-in-grid t)
(setq org-agenda-current-time-string
      (concat "Now " (make-string 70 ?-)))
(setq org-agenda-time-grid
      '((daily today require-timed)
	(0600 0700 0800 0900 1000 1100
	      1200 1300 1400 1500 1600
	      1700 1800 1900 2000 2100)
	" ....." "-----------------"))

(setq org-agenda-custom-commands
      `(("A" "Daily agenda and top priority tasks"
         ((tags-todo "*"
                     ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                      (org-agenda-skip-function
                       `(org-agenda-skip-entry-if
                         'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                      (org-agenda-block-separator ?=)
                      (org-agenda-overriding-header "Important tasks without a date\n")))
          (agenda "" ((org-agenda-span 1)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator ?=)
                      (org-scheduled-past-days 0)
                      ;; We don't need the `org-agenda-date-today'
                      ;; highlight because that only has a practical
                      ;; utility in multi-day views.
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %-e %B %Y")
                      (org-agenda-overriding-header "\nToday's agenda\n")))
          (agenda "" ((org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+1d")
                      (org-agenda-span 3)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator ?=)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nNext three days\n")))
          (agenda "" ((org-agenda-time-grid nil)
                      (org-agenda-start-on-weekday nil)
                      ;; We don't want to replicate the previous section's
                      ;; three days, so we start counting from the day after.
                      (org-agenda-start-day "+4d")
                      (org-agenda-span 14)
                      (org-agenda-show-all-dates nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator ?=)
                      (org-agenda-entry-types '(:deadline))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))))
        ("P" "Plain text daily agenda and top priorities"
         ((tags-todo "*"
                     ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                      (org-agenda-skip-function
                       `(org-agenda-skip-entry-if
                         'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                      (org-agenda-block-separator ?=)
                      (org-agenda-overriding-header "Important tasks without a date\n")))
          (agenda "" ((org-agenda-span 1)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator ?=)
                      (org-scheduled-past-days 0)
                      ;; We don't need the `org-agenda-date-today'
                      ;; highlight because that only has a practical
                      ;; utility in multi-day views.
                      (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                      (org-agenda-format-date "%A %-e %B %Y")
                      (org-agenda-overriding-header "\nToday's agenda\n")))
          (agenda "" ((org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+1d")
                      (org-agenda-span 3)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator ?=)
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nNext three days\n")))
          (agenda "" ((org-agenda-time-grid nil)
                      (org-agenda-start-on-weekday nil)
                      ;; We don't want to replicate the previous section's
                      ;; three days, so we start counting from the day after.
                      (org-agenda-start-day "+4d")
                      (org-agenda-span 14)
                      (org-agenda-show-all-dates nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator ?=)
                      (org-agenda-entry-types '(:deadline))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n"))))
         ((org-agenda-with-colors nil)
          (org-agenda-prefix-format "%t %s")
          (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
          (org-agenda-fontify-priorities nil)
          (org-agenda-remove-tags t))
         ("agenda.txt"))))

;; org keymaps
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c d") 'insert-date)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c f") 'org-agenda-file-to-front)

;; org utility functions
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "<%Y-%m-%d %a>")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))

(defun reset-org-config ()
  "Reset all org-mode global variables to their default state."
  (interactive)
  (org-reset-all-org-global-variables))

; (setq org-gcal-client-id "YOUR_GOOGLE_CLIENT_ID_HERE"
;       org-gcal-client-secret "YOUR_GOOGLE_CLIENT_SECRET_HERE"
;       org-gcal-fetch-file-alist '(("your-email@example.com" . "~/Org/agenda/schedule.org")))

; (require 'oauth2-auto)
; (require 'plstore)
; (setq plstore-encrypt-to "your-email@example.com")
; (setq oauth2-auto-google-client-id "YOUR_GOOGLE_CLIENT_ID_HERE"
;       oauth2-auto-google-client-secret "YOUR_GOOGLE_CLIENT_SECRET_HERE")

; (defun org-gcal--get-access-token ()
;   (oauth2-auto-access-token-sync "your-email@example.com" 'google '(calendar email)))

; (use-package org-gcal
;   :straight (:host github :repo "kidd/org-gcal.el" :branch "master"))

; (require 'org-gcal)
; (setq plstore-cache-passphrase-for-symmetric-encryption t)

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 5
  :config
  (setq yas-snippet-dirs (list (expand-file-name "~/.emacs.d/snippets" )))
  (yas-global-mode 1)) ;; or M-x yas-reload-all if you've started YASnippet already.

;; Silences the warning when running a snippet with backticks (runs a command in the snippet)
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;; latex
;; Add this to .emacs.d/init.el:
(with-eval-after-load "tex"
  ;; enable synctex support for latex-mode
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  ;; add a new view program
  (add-to-list 'TeX-view-program-list
        '(;; arbitrary name for this view program
          "Zathura"
          (;; zathura command (may need an absolute path)
           ;; "zathura"
           "/etc/profiles/per-user/morp/bin/zathura"
           ;; %o expands to the name of the output file
           " %o"
           ;; insert page number if TeX-source-correlate-mode
           ;; is enabled
           (mode-io-correlate " --synctex-forward %n:0:%b"))))
  ;; use the view command named "Zathura" for pdf output
  (setcdr (assq 'output-pdf TeX-view-program-selection) '("Zathura")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Org/zettelkasten/20230115133855-s22week0.org" "/home/morp/iCloud/Org/Todo.org" "/home/morp/Org/agenda/tasks.org" "/home/morp/Org/agenda/school.org" "/home/morp/Org/agenda/birthdays.org" "/home/morp/Org/agenda/habits.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant italic)))))
