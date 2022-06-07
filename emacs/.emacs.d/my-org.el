;; org-mode  TODO: look at /home/morp/dotfiles/emacs/.emacs.d/org-mode.el for more goodies
(use-package org
  :config
  (require 'org-tempo)

  (add-hook 'org-mode-hook
            (lambda ()
              (setq mailcap-mime-data '())
              (mailcap-parse-mailcap "~/.mailcap")
              (setq org-file-apps
                    '((auto-mode . emacs)
                      ("mobi" . "fbreader %s")
                      ("\\.x?html?\\'" . mailcap)
                      ("pdf" . mailcap)
                      (system . mailcap)
                      (t . mailcap))))))

;; scratch buffer is in org-mode
(setq initial-major-mode 'org-mode)

;; org-mode ui
(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

(setq org-hide-emphasis-markers t)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

