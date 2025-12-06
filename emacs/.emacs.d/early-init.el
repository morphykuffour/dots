;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:
;; Performance-optimized early initialization

;;; Code:

;; Increase garbage collection threshold during startup (100 MB)
(setq gc-cons-threshold (* 100 1024 1024))
(setq gc-cons-percentage 0.6)

;; Reduce frequency of garbage collection by increasing the threshold
;; This will be reset after startup in init.el
(defvar morph--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disable unnecessary UI elements early
(push '(menu-bar-lines . 1) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(menu-bar-mode 1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Frame size settings
(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(width . (text-pixels . 1200)))
  (add-to-list var '(height . (text-pixels . 900))))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

;; Disable startup screens and messages
(setq use-dialog-box t
      use-file-dialog nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      inhibit-default-init t)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil
      package-quickstart nil)

;; Native compilation settings
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-deferred-compilation t
        native-comp-speed 2))

;; Prevent Emacs from loading site-start.el
(setq site-run-file nil)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
