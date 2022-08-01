;; KEYMAPS

(global-set-key (kbd "C-h f")   #'helpful-callable)
(global-set-key (kbd "C-h v")   #'helpful-variable)
(global-set-key (kbd "C-h k")   #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F")   #'helpful-function)
(global-set-key (kbd "C-h C")   #'helpful-command)
;; (global-set-key [C-mouse-wheel-up-event]   'text-scale-increase)
;; (global-set-key [C-mouse-wheel-down-event] 'text-scale-decrease)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c e i") (lambda () (interactive) (find-file "~/dotfiles/emacs/.emacs.d/init.el")))
(global-set-key (kbd "C-c e p") 'package-install)
(global-set-key (kbd "C-c e t") 'counsel-load-theme)
(global-set-key (kbd "C-c e m") 'mu4e)
(global-set-key (kbd "C-c e o") 'olivetti-mode)
(global-set-key (kbd "C-c e r") 'org-babel-execute-src-block)
(global-set-key (kbd "C-c d i") 'insdate-insert-current-date) ;TODO fixme
(global-set-key (kbd "<space><space>") 'previous-buffer)
