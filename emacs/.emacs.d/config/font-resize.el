;;; font-resize.el --- Font Configuration -*- lexical-binding: t -*-

(custom-set-faces
 '(italic ((t (:slant italic)))))

;; Font configuration
(setq hrs/default-fixed-font "JetBrainsMono Nerd Font Mono")
(setq hrs/default-fixed-font-size 120)
(setq hrs/current-fixed-font-size hrs/default-fixed-font-size)
(setq hrs/font-change-increment 1.1)

(defun hrs/font-spec ()
  "Return font spec string for current settings."
  (format "%s-%d" hrs/default-fixed-font (/ hrs/current-fixed-font-size 10)))

(defun hrs/apply-fonts (&optional frame)
  "Apply font to all faces. Works for both daemon and non-daemon."
  (let ((fr (or frame (selected-frame))))
    (when (display-graphic-p fr)
      (select-frame fr)
      ;; Set the frame font - controls the actual rendered default font
      (set-frame-font (hrs/font-spec) nil t)
      ;; Set base faces
      (dolist (face '(default fixed-pitch variable-pitch))
        (set-face-attribute face nil
                            :family hrs/default-fixed-font
                            :height hrs/current-fixed-font-size))
      ;; Set UI chrome faces and break inheritance so they scale independently
      (dolist (face '(tab-bar tab-bar-tab tab-bar-tab-inactive
                      mode-line mode-line-inactive))
        (set-face-attribute face nil
                            :family hrs/default-fixed-font
                            :height hrs/current-fixed-font-size
                            :inherit nil)))))

(defun hrs/set-font-size ()
  "Update all face sizes to current size."
  (when (display-graphic-p)
    (set-frame-font (hrs/font-spec) nil t))
  (dolist (face '(default fixed-pitch variable-pitch))
    (set-face-attribute face nil
                        :family hrs/default-fixed-font
                        :height hrs/current-fixed-font-size))
  (dolist (face '(tab-bar tab-bar-tab tab-bar-tab-inactive
                  mode-line mode-line-inactive))
    (set-face-attribute face nil
                        :family hrs/default-fixed-font
                        :height hrs/current-fixed-font-size
                        :inherit nil)))

(defun hrs/reset-font-size ()
  "Revert font sizes back to defaults."
  (interactive)
  (setq hrs/current-fixed-font-size hrs/default-fixed-font-size)
  (hrs/set-font-size))

(defun hrs/increase-font-size ()
  "Increase current font sizes by a factor of `hrs/font-change-increment'."
  (interactive)
  (setq hrs/current-fixed-font-size
        (ceiling (* hrs/current-fixed-font-size hrs/font-change-increment)))
  (hrs/set-font-size))

(defun hrs/decrease-font-size ()
  "Decrease current font sizes by a factor of `hrs/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq hrs/current-fixed-font-size
        (max 1
             (floor (/ hrs/current-fixed-font-size hrs/font-change-increment))))
  (hrs/set-font-size))

(define-key global-map (kbd "C-)") 'hrs/reset-font-size)
(define-key global-map (kbd "C-+") 'hrs/increase-font-size)
(define-key global-map (kbd "C-=") 'hrs/increase-font-size)
(define-key global-map (kbd "C-_") 'hrs/decrease-font-size)
(define-key global-map (kbd "C--") 'hrs/decrease-font-size)

;; Apply fonts to every new frame (critical for daemon mode)
(add-hook 'after-make-frame-functions #'hrs/apply-fonts)

;; Apply fonts on theme change
(add-hook 'enable-theme-functions (lambda (&rest _) (hrs/apply-fonts)))

;; Apply now for non-daemon startup
(unless (daemonp)
  (hrs/apply-fonts))
