;;; keymaps.el --- Key Bindings -*- lexical-binding: t -*-

;; KEYMAPS

(global-set-key [C-mouse-wheel-up-event]   'text-scale-increase)
(global-set-key [C-mouse-wheel-down-event] 'text-scale-decrease)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; (global-set-key (kbd "C-c e i") (lambda () (interactive) (find-file "~/dotfiles/emacs/.emacs.d/init.el")))
(global-set-key (kbd "C-c e i") (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))
(global-set-key (kbd "C-c e p") 'package-install)
(global-set-key (kbd "C-c e t") 'counsel-load-theme)
(global-set-key (kbd "C-c e m") 'mu4e)
(global-set-key (kbd "C-c e o") 'olivetti-mode)
(global-set-key (kbd "C-c e r") 'org-babel-execute-src-block)
(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e e") 'eval-region)

;; (global-set-key (kbd "C-c d i") 'insdate-insert-current-date) ;TODO fixme
;; (global-set-key (kbd "C-x |") 'toggle-window-split);TODO fixme
;; (global-set-key (kbd "<space><space>") 'previous-buffer);TODO fixme

(global-set-key (kbd "C-x C-p") 'git-timemachine-show-previous-revision)
(global-set-key (kbd "C-x C-.") 'git-timemachine-show-current-revision)
(global-set-key (kbd "C-x C-n") 'git-timemachine-show-next-revision)
