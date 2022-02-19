;;emacs os config

;;; PACKAGE LIST
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;;; USE-PACKAGE
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; PACKAGES
(use-package command-log-mode)
(use-package ivy
             :diminish
             :bind (("C-s" . swiper)
                    :map ivy-minibuffer-map
                    ("TAB" . ivy-alt-done)	
                    :map ivy-switch-buffer-map
                    ("C-d" . ivy-switch-buffer-kill)
                    :map ivy-reverse-i-search-map
                    ("C-k" . ivy-previous-line)
                    ("C-d" . ivy-reverse-i-search-kill))
             :config
             (ivy-mode 1))

;;; UNDO
(use-package undo-fu)

;;; Vim Bindings
(use-package evil
             :demand t
             :bind (("<escape>" . keyboard-escape-quit))
             :init
             (setq evil-search-module 'evil-search)
             (setq evil-want-keybinding nil)
             ;; no vim insert bindings
             (setq evil-undo-system 'undo-fu)
             :config
             (evil-mode 1))

;;; Vim Bindings Everywhere else
(use-package evil-collection
             :ensure t
             :after evil
             :config
             (setq evil-want-integration t)
             (evil-collection-init))

;; vertico
(use-package vertico
             :config
             (vertico-mode))

;; PERSONAL SETTINGS
(set-face-attribute 'mode-line nil  :height 150)
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font Mono" :height 100)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)        ; visible scrollbar
(tool-bar-mode -1)          ; the toolbar
(tooltip-mode -1)           ; tooltips
;;(set-fringe-mode 90)       ; space to left
(menu-bar-mode t)           ; the menu bar
(setq inhibit-startup-message t)
(evil-commentary-mode)
(column-number-mode)
(setq custom-file (concat user-emacs-directory "/custom.el"))
(setq shell-command-switch "-ic")
(setq counsel-find-file-at-point t)  

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "personal.el")
(load-user-file "hydra.el")
(load-user-file "utils.el")

;; TODO: test on linux
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(with-system darwin (custom-set-variables
                      '(markdown-command "/opt/homebrew/bin/pandoc")))

;; md mode
(use-package markdown-mode
             :ensure t
             :commands (markdown-mode gfm-mode)
             :mode (("README\\.md\\'" . gfm-mode)
                    ("\\.md\\'" . markdown-mode)
                    ("\\.rmd\\'" . markdown-mode)
                    ("\\.markdown\\'" . markdown-mode))
             :init (setq markdown-command "multimarkdown"))

;; Wrap line in markdown.
(add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))
(setq markdown-enable-math t)

;; org-roam
(require 'org)
(require 'org-roam)

(use-package org-roam
             :after org
             :ensure t
             :init
             (setq org-roam-v2-ack t)
             :custom
             (org-roam-directory (file-truename "~/Dropbox/Zettelkasten"))
             :bind (("C-c n l" . org-roam-buffer-toggle)
                    ("C-c n f" . org-roam-node-find)
                    ("C-c n g" . org-roam-ui-open)
                    ("C-c n i" . org-roam-node-insert)
                    ("C-c n c" . org-roam-capture)
                    ("C-c n a" . org-roam-alias-add)
                    :map org-mode-map
                    ("C-M-i" . completion-at-point)
                    ("C-c n j" . org-roam-dailies-capture-today)) ; Dailies
             :config
             (org-roam-setup)
             (org-roam-db-autosync-mode)
             (require 'org-roam-protocol)) ;; If using org-roam-protocol

(setq org-roam-graph-executable "dot")

;; md-roam
(setq org-roam-directory (file-truename "~/Dropbox/Zettelkasten"))
(setq org-roam-file-extensions '("org" "md"))
(add-to-list  'load-path "~/.emacs.d/elispfiles/md-roam")
(require 'md-roam)
(md-roam-mode 1)
(setq md-roam-file-extension "md")
(org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active

;; TODO add aliases and roam_refs
(add-to-list 'org-roam-capture-templates
             '("m" "Markdown" plain "" :target
               (file+head "%<%Y-%m-%dT%H%M%S>.md"
                          "---\ntitle: ${title}\nid: %<%Y-%m-%dT%H%M%S>\ncategory: \nroam_refs: \nroam_aliases: \n---\n")
               :unnarrowed t))

;; org-roam-ui
(use-package websocket
             :after org-roam)

(use-package org-roam-ui
             :after org-roam 
             :config
             (setq org-roam-ui-sync-theme t
                   org-roam-ui-follow t
                   org-roam-ui-update-on-save t
                   org-roam-ui-open-on-start t))
;; PDFs
(pdf-loader-install)

;; shell paths
(getenv "SHELL")
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(use-package rainbow-delimiters
             :hook (prog-mode . rainbow-delimiters-mode))


(use-package which-key
             :init (which-key-mode)
             :diminish which-key-mode
             :config
             (setq which-key-idle-delay 0.3))

(use-package counsel
             :bind (("M-x" . counsel-M-x)
                    ("C-x b" . counsel-ibuffer)
                    ("C-x C-f" . counsel-find-file)
                    :map minibuffer-local-map
                    ("C-r" . 'counsel-minibuffer-history))
             :config
             (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
             :init
             (ivy-rich-mode 1))

(use-package helpful
             :commands (helpful-callable helpful-variable helpful-command helpful-key)
             :custom
             (counsel-describe-function-function #'helpful-callable)
             (counsel-describe-variable-function #'helpful-variable)
             :bind
             ([remap describe-function] . counsel-describe-function)
             ([remap describe-command] . helpful-command)
             ([remap describe-variable] . counsel-describe-variable)
             ([remap describe-key] . helpful-key))


(use-package command-log-mode
             :commands command-log-mode)

(use-package doom-themes
             :init (load-theme 'doom-dracula t))

(global-hl-todo-mode)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))

(require 'dashboard)
(dashboard-setup-startup-hook)
;; Or if you use use-package
(use-package dashboard
             :ensure t
             :config
             (dashboard-setup-startup-hook)
             (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
             (setq dashboard-startup-banner 'nil) ; 'logo -> logo
             )

(use-package all-the-icons
             :if (display-graphic-p))

;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  (let* (
         (backupRootDir "~/Documents/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath ))         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
         )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
    )
  )
(setq make-backup-file-name-function 'my-backup-file-name)

(require 'olivetti)
(auto-image-file-mode 1)

;; R-markdown for pdfs
(require 'color)

(use-package ess
             :ensure t
             :init (require 'ess-site))

(use-package polymode
             :ensure t
             :config
             (use-package poly-R)
             (use-package poly-markdown)
             ;;; MARKDOWN
             (add-to-list 'auto-mode-alist '("\\.md\\'" . poly-markdown-mode))
             ;;; R modes
             (add-to-list 'auto-mode-alist '("\\.Snw\\'" . poly-noweb+r-mode))
             (add-to-list 'auto-mode-alist '("\\.Rnw\\'" . poly-noweb+r-mode))
             (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown+r-mode))
             (markdown-toggle-math t)
             (defun ess-rmarkdown ()
               "Compile R markdown (.Rmd). Should work for any output type."
               (interactive)
               ; Check if attached R-session
               (condition-case nil
                               (ess-get-process)
                               (error
                                 (ess-switch-process)))
               (let* ((rmd-buf (current-buffer)))
                 (save-excursion
                   (let* ((sprocess (ess-get-process ess-current-process-name))
                          (sbuffer (process-buffer sprocess))
                          (buf-coding (symbol-name buffer-file-coding-system))
                          (R-cmd
                            (format "library(rmarkdown); rmarkdown::render(\"%s\")"
                                    buffer-file-name)))
                     (message "Running rmarkdown on %s" buffer-file-name)
                     (ess-execute R-cmd 'buffer nil nil)
                     (switch-to-buffer rmd-buf)
                     (ess-show-buffer (buffer-name sbuffer) nil)))))
             )
;; (define-key polymode-mode-map "\M-ns" 'ess-rmarkdown)
(with-eval-after-load 'polymode
                      (define-key polymode-minor-mode-map (kbd "<f5>") 'ess-rmarkdown))

(require 'calendar)


(dired-recent-mode 1)
(use-package dired-recent
             :load-path "~/.emacs.d/contrib/dired-recent.el/"
             :config
             (require 'dired-recent)
             (dired-recent-mode 1)
             )
(defun my-dired-recent-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (let ((dir (ivy-read "Directory: "
                       dired-recent-directories
                       :re-builder #'ivy--regex
                       :sort nil
                       :initial-input nil)))
    (dired dir)))

(global-set-key (kbd "C-z") 'my-dired-recent-dirs)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(100))
(add-to-list 'default-frame-alist '(alpha . (100)))

;; Company
;; Autocomplete popups
(use-package company
             :ensure t
             :config
             (progn
               (setq company-idle-delay 0.2
                     ;; min prefix of 2 chars
                     company-minimum-prefix-length 2
                     company-selection-wrap-around t
                     company-show-numbers t
                     company-dabbrev-downcase nil
                     company-echo-delay 0
                     company-tooltip-limit 20
                     company-transformers '(company-sort-by-occurrence)
                     company-begin-commands '(self-insert-command)
                     )
               (global-company-mode))
             )
(add-hook 'after-init-hook 'global-company-mode)

;; Provides all the racket support
(use-package racket-mode
             :ensure t)

(add-hook 'racket-mode-hook
	  (lambda ()
	    (define-key racket-mode-map (kbd "<f5>") 'racket-run)))


(use-package magit
  :ensure t)

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode)

;; elfeed
(global-set-key (kbd "C-x w") 'elfeed)
(setq elfeed-feeds
      '(" https://jvns.ca/atom.xml"))
