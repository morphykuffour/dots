;;; modeline.el --- Custom mode-line configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Lightweight, retina-friendly mode-line inspired by gonsie's design
;; https://www.gonsie.com/blorg/modeline.html

;;; Code:

;;; --- HELPER FUNCTIONS ---

(defun vc-branch ()
  "Get the current VC branch name without the prefix.
Returns the branch name like 'develop' instead of 'Git:develop'."
  (when (and vc-mode buffer-file-name)
    (let ((backend (vc-backend buffer-file-name)))
      (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))))

;;; --- MODE-LINE FORMAT ---

(setq-default mode-line-format
  (list
    ;; Timestamp - left side
    '(:eval (propertize (format-time-string " %b %d %H:%M ")
                       'face 'font-lock-builtin-face))

    ;; Git branch (if in version control)
    '(:eval (when-let ((branch (vc-branch)))
              (propertize (format "%s " branch)
                         'face 'font-lock-keyword-face)))

    ;; Buffer name - colored based on modified state
    '(:eval (propertize " %b "
                       'face (if (buffer-modified-p)
                                'font-lock-warning-face
                                'font-lock-type-face)
                       'help-echo (buffer-file-name)))

    ;; Position indicators - line and column
    " ("
    (propertize "%02l" 'face 'font-lock-constant-face)
    ","
    (propertize "%02c" 'face 'font-lock-constant-face)
    ") "

    ;; File position percentage and size
    "["
    (propertize "%p" 'face 'font-lock-string-face)
    "/"
    (propertize "%I" 'face 'font-lock-string-face)
    "] "

    ;; Right-aligned major mode name
    '(:eval (propertize
             " " 'display
             `((space :align-to (- (+ right right-fringe right-margin)
                                   ,(+ 3 (string-width mode-name)))))))
    (propertize " %m " 'face 'font-lock-comment-face)))

;;; --- MODE-LINE APPEARANCE (Retina-friendly, Modus-aware) ---

(defun morph/update-modeline-colors ()
  "Update mode-line colors to match current modus theme.
Automatically adapts to modus-operandi (light) and modus-vivendi (dark)."
  (let* ((bg (face-attribute 'default :background))
         (fg (face-attribute 'default :foreground))
         ;; Use slightly lighter/darker background for mode-line
         (ml-bg (if (eq (frame-parameter nil 'background-mode) 'dark)
                    ;; Dark theme: slightly lighter than background
                    (color-darken-name bg -5)
                  ;; Light theme: slightly darker than background
                  (color-darken-name bg 5))))

    ;; Active mode-line
    (set-face-attribute 'mode-line nil
      :background ml-bg
      :foreground fg
      :box `(:line-width 8 :color ,ml-bg)
      :overline nil
      :underline nil)

    ;; Inactive mode-line (more muted)
    (set-face-attribute 'mode-line-inactive nil
      :background (if (eq (frame-parameter nil 'background-mode) 'dark)
                      (color-darken-name bg -3)
                    (color-darken-name bg 3))
      :foreground (color-darken-name fg 20)
      :box `(:line-width 8 :color ,(if (eq (frame-parameter nil 'background-mode) 'dark)
                                       (color-darken-name bg -3)
                                     (color-darken-name bg 3)))
      :overline nil
      :underline nil)))

;; Apply colors immediately
(morph/update-modeline-colors)

;; Update colors when theme changes (works with circadian)
(advice-add 'load-theme :after
            (lambda (&rest _)
              (run-with-timer 0.1 nil #'morph/update-modeline-colors)))

;; Performance optimization: reduce mode-line updates
(setq-default mode-line-format-right-align nil)

(provide 'modeline)
;;; modeline.el ends here
