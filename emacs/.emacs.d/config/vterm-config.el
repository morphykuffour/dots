;;; vterm-config.el --- Enhanced vterm configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This configuration replicates hexdump's vterm setup with:
;; - Convenient window splitting for vterm
;; - Command output copying to org-mode buffers
;; - Custom keybindings for terminal workflow

;;; Code:

;; Don't show certain buffers in new windows
;; (keeps the *commands-history* buffer hidden in the background)
(add-to-list 'display-buffer-alist
             '("*commands-history*"
               (display-buffer-no-window)
               (allow-no-window . t)))

(add-to-list 'display-buffer-alist
             '("*tmp-cmd*"
               (display-buffer-no-window)
               (allow-no-window . t)))

;; This function splits the window to create space for a little vterm
;; buffer at the bottom
(defun my/vterm-window-split ()
  "Split window and open vterm in the bottom split."
  (interactive)
  (split-window-below -15)
  (other-window 1)
  (vterm))

;; Define prompt patterns for command copying functionality
;; Customize these to match your shell prompts
(setq my/prompts '("\\[.+@.+ .*\\]\\$ "  ; bash/zsh with brackets
                   ".*@.*:.*\\$ "         ; standard user@host:path$
                   ".*@.*:.*# "           ; root prompt
                   "> "                   ; simple prompt
                   "% "                   ; zsh/fish prompt
                   ))

;; This function copies the output of the last command in vterm
;; and organizes it in an org-mode buffer (*commands-history*)
(defun my/vterm-copy-command ()
  "Copy the last vterm command output to *commands-history* buffer in org-mode format."
  (interactive)
  (vterm-copy-mode)
  (dolist (prompt my/prompts)
    (when (re-search-backward prompt nil 't 1)
      (setq my-max-point (point))
      (re-search-backward prompt nil nil 1)
      (setq my-min-point (point))

      (kill-new
       (let* ((string (buffer-substring my-min-point my-max-point))
              (len (length string))
              (current-buffer-name (buffer-name (window-buffer (minibuffer-selected-window)))))

         (switch-to-buffer "*tmp-cmd*" 't)

         (insert string)
         (backward-char len)
         (insert "*** ")
         (end-of-line)
         (insert "\n#+begin_example\n")
         (end-of-buffer)
         (insert "\n#+end_example\n")

         (let* ((cleaned_string (buffer-substring
                                 (progn
                                   (beginning-of-buffer)
                                   (point))
                                 (progn
                                   (end-of-buffer)
                                   (point))))
                (cmd_name (progn
                            (beginning-of-buffer)
                            (re-search-forward prompt)
                            (thing-at-point 'word' 'no-properties))))
           (switch-to-buffer "*commands-history*" 't)
           (org-mode)

           (if (search-forward (concat "** " cmd_name "\n") nil 't 1)
               (progn
                 (insert cleaned_string))
             (progn
               (insert (concat "** " cmd_name "\n"))
               (insert cleaned_string)))

           (kill-buffer "*tmp-cmd*")
           (beginning-of-buffer)
           (switch-to-buffer current-buffer-name)
           cleaned_string)))

      (vterm-copy-mode)
      (execute-kbd-macro (kbd "<return>"))
      (setq kill-ring (cdr kill-ring))
      (setq kill-ring-yank-pointer kill-ring)
      nil)))

;; Enhanced vterm keybindings
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c l") 'multi-vterm)
  (define-key vterm-mode-map (kbd "C-c v") 'my/vterm-window-split)
  (define-key vterm-mode-map (kbd "C-c o") 'my/vterm-copy-command))

;; Global keybindings for vterm (accessible from any buffer)
(global-set-key (kbd "C-c l") 'multi-vterm)
(global-set-key (kbd "C-c v") 'my/vterm-window-split)
(global-set-key (kbd "C-c o") 'my/vterm-copy-command)

(provide 'vterm-config)
;;; vterm-config.el ends here
