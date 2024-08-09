;;; early-init.el --- qemacs early startup -*- lexical-binding: t; -*-

;;; Copyright:

;; Copyright (C) 2024 The Quo-Emacs Authors
;;
;; This library is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by the Free
;; Software Foundation; version 2.1.
;;
;; This library is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License along
;; with this library; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:

;; This file is for settings that need to be set, or are nice to be set, very early
;; in the startup process, before prompting the developer for questions and setting
;; up packages and general features to be supported.

;;; Code:

;; enforce latest Emacs version supported
(when (version< emacs-version "29.4")
  (error "Emacs 29.4 or above required"))

;; qemacs mode-line rigging
(defvar qemacs-initial-mode-line-format (copy-tree mode-line-format)
  "Set from init-settings-early.el, this is the stock `mode-line-format'.")
(setq mode-line-format nil)

;;
;; straight use-package support
;;

;; straight.el setup
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; setup custom user paths
(add-to-list 'load-path (concat user-emacs-directory "libs/debian"))
(add-to-list 'load-path (concat user-emacs-directory "libs/others"))
(add-to-list 'load-path (concat user-emacs-directory "libs/common"))
(add-to-list 'load-path (concat user-emacs-directory "libs/support"))
(add-to-list 'load-path (concat user-emacs-directory "libs/run-commands"))
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(require 'qemacs-common) ;; needed for startup and utility functions
(require 'qemacs-startup)
(qemacs-startup-begin)

;; conditionally set debug-on-error
(let ((qemacs-debug (getenv "QEMACS_DEBUG")))
  (when (string-is-true qemacs-debug)
    ;; (message "debugging on error")
    (setq debug-on-error t)))

;; conditionally start an emacs server
(let ((qemacs-server-start (getenv "QEMACS_SERVER_START"))
      (qemacs-server-name  (getenv "QEMACS_SERVER"))
      (qemacs-name         (getenv "QEMACS_NAME")))
  (when (string-is-true qemacs-server-start)
    (if (not (equal qemacs-name ""))
        (setq server-name qemacs-server-name))
    (server-start)
    ) ;; end when qemacs-server-start
  ;; default frame titles
  (let ((title-prefix (format "%%b - %s@%s" (getenv "USER") (system-name))))
    (if (or (not qemacs-server-name) (equal qemacs-name qemacs-server-name))
        (setq-default frame-title-format (format "%s (%s)" title-prefix qemacs-name))
      (setq-default frame-title-format (format "%s (%s@%s)"
                                               title-prefix qemacs-server-name qemacs-name))
      ) ;; end if equal qemacs-name qemacs-server-name
    ) ;; end let title-prefix
  ) ;; end let qemacs server variables


;; elisp internal settings

(setq max-lisp-eval-depth 1000) ;; default 500
(setq gc-cons-threshold 80000000) ; 100 x default

;; term-real-value is what would is expected from (getenv "TERM") but because
;; getenv mangles that value into "dummy", it's useless for anything actually
;; TERM related
(setq term-real-value (getenv-internal "TERM" initial-environment))

(require 'warnings) ;; suppress template insertion warnings
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;; setup unicode supports
(setq locale-coding-system 'utf-8
      default-input-method nil
      )
(prefer-coding-system         'utf-8)
(set-terminal-coding-system   'utf-8)
(set-keyboard-coding-system   'utf-8)
(set-selection-coding-system  'utf-8)
(set-language-environment     'utf-8)

;; backup handling
(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 backup-by-copying t
 create-lockfiles nil)
(setq backup-directory-alist
      `(("." . ,(user-emacs-path "transient" "auto-save.d"))))
(setq auto-save-directory-fallback
      (user-emacs-path "transient" "auto-save.d"))

;; mouse mode
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; unset tooltip-mode
(tooltip-mode -1)

;; unset use-dialog-box
(setq use-dialog-box nil)

;; remove abbrev-mode
(abbrev-mode -1)

;; truncate-lines by default
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; setup custom-vars
(setq custom-file (user-emacs-path "transient" "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; left/right side windows taller than top/bottom
(setq window-sides-vertical t)

;; cleanup auto-saves
(setf kill-buffer-delete-auto-save-files t)
;; select help window
(setq help-window-select t)
;; cleanup startup views
(setq inhibit-startup-buffer-menu t)

;; transient file locations
(setq auto-save-list-file-prefix nil
      auto-insert-directory           (user-emacs-path "templates")
      savehist-file                   (user-emacs-path "transient" "savehist")
      save-place-file                 (user-emacs-path "transient" "places")
      recentf-save-file               (user-emacs-path "transient" "recentf")
      project-list-file               (user-emacs-path "transient" "projects")
      lsp-session-file                (user-emacs-path "transient" "lsp-session-v1")
      ac-comphist-file                (user-emacs-path "transient" "ac-comphist.dat")
      nsm-settings-file               (user-emacs-path "transient" "network-security.data")
      tramp-persistency-file-name     (user-emacs-path "transient" "tramp")
      dape-default-breakpoints-file   (user-emacs-path "transient" "dape-breakpoints")
      auto-save-file-name-transforms  `((".*" ,(user-emacs-path "transient" "auto-save-list") t))
      ) ;; end set transient file locations

;; turn off recentf-mode as much as possible
(use-package recentf
  :demand t
  :straight (:type built-in)
  :init
  (setq recentf-max-menu-items 0
        recentf-max-saved-items 0
        recentf-list 'nil
        recentf-filter-changer-current 'nil
        ) ;; end settings
  :config
  ;; if the mode is loaded, immediately unload it
  (add-to-list 'recentf-load-hook (lambda () (recentf-mode -1)))
  ) ;; end use-package recentf

;; save places within files
(setq save-place-forget-unreadable-files nil)
(save-place-mode 1)

;; savehist
(setq savehist-additional-variables '(register-alist kill-ring search-ring regexp-search-ring))
(setq history-length 100)
(savehist-mode 1)
(add-hook 'after-init-hook 'savehist-mode)
(add-hook 'kill-emacs-hook 'qemacs-unpropertize-kill-ring)
(add-hook 'kill-emacs-hook 'savehist-save)

;; keep at most 50 of any type of history
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history    'history-length 50)
(put 'kill-ring          'history-length 50)

(global-display-line-numbers-mode 1) ;< line numbers in the margins

(tool-bar-mode -1)                  ; Disable the button bar atop screen
(setq visible-bell nil)             ; Disable annoying visual bell graphic
(setq ring-bell-function 'ignore)   ; Disable super annoying audio bell

(transient-mark-mode 1)   ;< highlight text selections
(delete-selection-mode 1) ;< delete selected text when typing
(global-font-lock-mode 1) ;< syntax colouring everywhere
(show-paren-mode 1)       ;< paren match highlighting
(column-number-mode 1)    ;< cursor column pos in status bar

(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 2)
(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 2)

(setq system-uses-terminfo nil)
(setq require-final-newline nil)
(setq split-height-threshold nil)
(setq split-width-threshold 180)

(random t)
(setq diff-switches "-u -w")
(setq load-prefer-newer t)
(setenv "PAGER" "cat")
(set-default 'indicate-empty-lines t)
(setq show-trailing-whitespace t)

;; indentation
(setq clean-aindent-is-simple-indent nil)
(setq clean-aindent-mode t)
(setq custom-buffer-indent 2)
(setq ecb-tree-indent 0)
(setq indent-tabs-mode nil)
(setq standard-indent 2)
(setq mail-indentation-spaces 3)

;;; early-init.el ends here
