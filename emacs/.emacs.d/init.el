;; emacs config

;;; PACKAGE LIST
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;;; BOOTSTRAP USE-PACKAGE
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
                    ("C-l" . ivy-alt-done)
                    ("C-j" . ivy-next-line)
                    ("C-k" . ivy-previous-line)
                    :map ivy-switch-buffer-map
                    ("C-k" . ivy-previous-line)
                    ("C-l" . ivy-done)
                    ("C-d" . ivy-switch-buffer-kill)
                    :map ivy-reverse-i-search-map
                    ("C-k" . ivy-previous-line)
                    ("C-d" . ivy-reverse-i-search-kill))
             :config
             (ivy-mode 1))

;;; UNDO
;; Vim style undo not needed for emacs 28
(use-package undo-fu)

;;; Vim Bindings
(use-package evil
             :demand t
             :bind (("<escape>" . keyboard-escape-quit))
             :init
             ;; allows for using cgn
             ;; (setq evil-search-module 'evil-search)
             (setq evil-want-keybinding nil)
             ;; no vim insert bindings
             (setq evil-undo-system 'undo-fu)
             :config
             (evil-mode 1))

;;; Vim Bindings Everywhere else
(use-package evil-collection
             :after evil
             :config
             (setq evil-want-integration t)
             (evil-collection-init))

;; vertico
(use-package vertico
             :config
             (vertico-mode))

;; PERSONAL SETTINGS
;; (load-theme 'adwaita)
(load-theme 'wombat)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(set-face-attribute 'mode-line nil  :height 200)
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font Mono" :height 100)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(eval-after-load "linum" 
                 '(set-face-attribute 'linum nil :height 100))
(setq inhibit-startup-message t)
(evil-commentary-mode)
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                 term-mode-hook
                 eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
 

;; TODO: test on linux
(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(with-system gnu/linux
             ;; zathura as pdf viewer
             ;; ~/.local/bin/zathura-sync.sh
             (setq TeX-view-program-list
                   '(("zathura" 
                      ("zathura" (mode-io-correlate "-sync.sh")
                       " "
                       (mode-io-correlate "%n:1:%t ")
                       "%o"))))

             (when (daemonp)
               (exec-path-from-shell-initialize))
             )

;; org-roam
(use-package org-roam
             :ensure t
             :init
             (setq org-roam-v2-ack t)
             :custom
             (org-roam-directory (file-truename "~/Dropbox/Zettelkasten"))
             :bind (("C-c n l" . org-roam-buffer-toggle)
                    ("C-c n f" . org-roam-node-find)
                    ("C-c n g" . org-roam-graph)
                    ("C-c n i" . org-roam-node-insert)
                    ("C-c n c" . org-roam-capture)
                    :map org-mode-map
                    ("C-M-i" . completion-at-point)
                    ;; Dailies
                    ("C-c n j" . org-roam-dailies-capture-today))
             :config
             (org-roam-setup)
             (org-roam-db-autosync-mode)
             (require 'org-roam-protocol)) ;; If using org-roam-protocol


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

;; md-roam
;; (require 'org-roam)
;; (setq org-roam-directory (file-truename "~/Dropbox/Zettelkasten"))
;; (setq org-roam-file-extensions '("org" "md"))
;; (add-to-list  'load-path "~/.emacs.d/elispfiles/md-roam.el")
;; (md-roam-mode 1)
;; (require 'md-roam)
;; (setq md-roam-file-extension "md")
;; (org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active

;; md-mode
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
             :ensure t
             :commands (markdown-mode gfm-mode)
             :mode (("README\\.md\\'" . gfm-mode)
                    ("\\.md\\'" . markdown-mode)
                    ("\\.markdown\\'" . markdown-mode))
             :init (setq markdown-command "multimarkdown"))

;; PDFs
(pdf-loader-install)

;; KEYMAPS

;; Org-roam
(define-key global-map (kbd "C-c n f") #'org-roam-node-find)
(define-key global-map (kbd "C-c n c") #'org-roam-capture)
(define-key global-map (kbd "C-c n i") #'org-roam-node-insert)
(define-key global-map (kbd "C-c n l") #'org-roam-buffer-toggle)

;; edit init.el TODO 
;; (define-key global-map (kbd "C-c e i") #'(find-file "~/dotfiles/emacs/.emacs.d/init.el"))

;; shell paths
(getenv "SHELL")
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

 
;; (use-package rainbow-delimiters
;;              :hook (prog-mode . rainbow-delimiters-mode))
;; 
;; 
;; (use-package which-key
;;              :init (which-key-mode)
;;              :diminish which-key-mode
;;              :config
;;              (setq which-key-idle-delay 0.3))
;; 
;; 
;; (use-package ivy-rich
;;              :init
;;              (ivy-rich-mode 1)
;;              :config
;;              (setq ivy-format-function #'ivy-format-function-line)
;;              (setq ivy-rich--display-transformers-list
;;                    (plist-put ivy-rich--display-transformers-list
;;                               'ivy-switch-buffer
;;                               '(:columns
;;                                  ((ivy-rich-candidate (:width 40))
;;                                   (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
;;                                   (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
;;                                   (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
;;                                   (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
;;                                  :predicate
;;                                  (lambda (cand)
;;                                    (if-let ((buffer (get-buffer cand)))
;;                                            ;; Don't mess with EXWM buffers
;;                                            (with-current-buffer buffer
;;                                                                 (not (derived-mode-p 'exwm-mode)))))))))
;; 
