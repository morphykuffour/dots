;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:
;; Performance-optimized early initialization with Kyure-A speedups
;; Backup of original config in: backup_20251219_*/early-init.el

;;; Code:

;;; --- CRITICAL STARTUP OPTIMIZATIONS ---

;; Increase garbage collection threshold during startup (512 MB)
;; This is the most important speedup - prevents GC during init
(setq gc-cons-threshold (* 512 1024 1024))
(setq gc-cons-percentage 0.6)

;; Disable file-name-handler-alist during startup
;; This prevents regex matching on every file load
(defvar morph--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disable package.el - packages managed by Nix
(setq package-enable-at-startup nil
      package-quickstart nil)

;; Prevent Emacs from loading site-start.el
(setq site-run-file nil)

;;; --- UI SUPPRESSION (must happen before frame creation) ---

;; Disable unnecessary UI elements before frame is created
;; This is faster than disabling them after
(push '(menu-bar-lines . 1) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Frame size settings
(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(width . (text-pixels . 1200)))
  (add-to-list var '(height . (text-pixels . 900)))
  (add-to-list var '(undecorated . t)))  ; Hide title bar like Kitty

;; Prevent frame resizing during startup (major speedup)
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

;; Actually disable modes early
(menu-bar-mode 1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; --- STARTUP SCREEN SUPPRESSION ---

(setq use-dialog-box t
      use-file-dialog nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      inhibit-default-init t)

;; Suppress startup messages
(setq inhibit-startup-message t
      initial-scratch-message nil)

;;; --- NATIVE COMPILATION SETTINGS ---

(when (native-comp-available-p)
  ;; Suppress native compilation warnings
  (setq native-comp-async-report-warnings-errors 'silent)
  ;; Enable deferred compilation
  (setq native-comp-deferred-compilation t)
  ;; Use 8 parallel jobs for native compilation
  (setq native-comp-async-jobs-number 8)
  ;; Maximum optimization level
  (setq native-comp-speed 3)
  ;; Always compile
  (setq native-comp-always-compile t))

;;; --- WARNING SUPPRESSION ---

;; Suppress warnings during startup
(setq display-warning-minimum-level :error)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp)))
(setq warning-suppress-types '((comp)))

;;; --- RENDERING OPTIMIZATIONS ---

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

;; Disable bidirectional text for performance (unless you need RTL languages)
(setq-default bidi-display-reordering nil)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Reduce redisplay frequency
(setq redisplay-skip-fontification-on-input t)

;;; early-init.el ends here
