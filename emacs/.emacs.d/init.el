;;emacs config

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

;; (setq evil-want-minibuffer t)

;; vertico
(use-package vertico
  :config
  (vertico-mode))

;; PERSONAL SETTINGS
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(set-face-attribute 'mode-line nil  :height 200)
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font Mono" :height 100)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)        ; visible scrollbar
(tool-bar-mode -1)          ; the toolbar
(tooltip-mode -1)           ; tooltips
(set-fringe-mode 90)        ; space to left
(menu-bar-mode 1)           ; the menu bar
(setq inhibit-startup-message t)
(evil-commentary-mode)
(column-number-mode)
(global-set-key [C-mouse-wheel-up-event]  'text-scale-increase)
(global-set-key  [C-mouse-wheel-down-event] 'text-scale-decrease)
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; (global-display-line-numbers-mode t)

;; Disable line numbers for some modes
;; (dolist (mode '(org-mode-hook term-mode-hook eshell-mode-hook shell-mode-hook))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))


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

(with-system darwin
  ;; multi-markdown error
  (custom-set-variables
    '(markdown-command "/opt/homebrew/bin/pandoc"))
  )

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

(setq org-roam-graph-executable "dot")

;; md-roam
(setq org-roam-v2-ack t)
(require 'org-roam)
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

;; KEYMAPS

;; Org-roam
(define-key global-map (kbd "C-c n f") #'org-roam-node-find)
(define-key global-map (kbd "C-c n c") #'org-roam-capture)
(define-key global-map (kbd "C-c n i") #'org-roam-node-insert)
(define-key global-map (kbd "C-c n l") #'org-roam-buffer-toggle)

;; edit init.el TODO 
(define-key global-map (kbd "C-c e i") #'(find-file "~/dotfiles/emacs/.emacs.d/init.el"))

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

;; TODO install tree-sitter
; (add-to-list 'load-path "$HOME/.emacs.d/elispfiles/elisp-tree-sitter/core")
; (add-to-list 'load-path "$HOME/.emacs.d/elispfiles/elisp-tree-sitter/lisp")
; (add-to-list 'load-path "$HOME/.emacs.d/elispfiles/elisp-tree-sitter/langs")

; (require 'tree-sitter)
; (require 'tree-sitter-hl)
; (require 'tree-sitter-langs)
; (require 'tree-sitter-debug)
; (require 'tree-sitter-query)

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

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

(use-package command-log-mode
  :commands command-log-mode)

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package all-the-icons)

(eval-after-load "linum" 
  '(set-face-attribute 'linum nil :height 100))

;; disbled
;; (use-package doom-modeline
;;   :init (doom-modeline-mode 0)
;;   :custom ((doom-modeline-height 15)))

(global-hl-todo-mode)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
	("FIXME"  . "#FF0000")
	("DEBUG"  . "#A020F0")
	("GOTCHA" . "#FF4500")
	("STUB"   . "#1E90FF")))

;; (company-mode)
;; (add-hook 'after-init-hook 'global-company-mode)
;; == irony-mode ==
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

;; == company-mode ==
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              nil
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	company-backends                '((company-irony company-gtags))
	)
  :bind ("C-;" . company-complete-common)
  )

(require 'dashboard)
(dashboard-setup-startup-hook)
;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner 'logo)
  )

(use-package all-the-icons
  :if (display-graphic-p))

;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
  If the new path's directories does not exist, create them."
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

(require 'org-transclusion)
(define-key global-map (kbd "<f12>") #'org-transclusion-add)
;; (define-key global-map (kbd "C-n t") #'org-transclusion-mode)


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
  ;; from https://gist.github.com/benmarwick/ee0f400b14af87a57e4a
  ;; compile rmarkdown to HTML or PDF with M-n s
  ;; use YAML in Rmd doc to specify the usual options
  ;; which can be seen at http://rmarkdown.rstudio.com/
  ;; thanks http://roughtheory.com/posts/ess-rmarkdown.html
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

(defun insert-current-date (&optional omit-day-of-week-p)
  "Insert today's date using the current locale.
With a prefix argument, the date is inserted without the day of
the week."
  (interactive "P*")
  (insert (calendar-date-string (calendar-current-date) nil
				omit-day-of-week-p)))
(global-set-key "\C-x\M-d" `insdate-insert-current-date)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(autoload 'fennel-mode (expand-file-name "~/.emacs.d/elispfiles/fennel-mode/fennel-mode") nil t)
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

(defun open-terminal ()
"Open default terminal emulator in the current directory."
  (interactive)
  (start-process "terminal" nil (getenv "TERMINAL")))

(defun open-terminal-in-project-root ()
"Open default terminal in the project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (open-terminal)))
;; (server-mode t)

(defun copy-filename()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))
