;;; qemacs-init-theming.el --- qemacs theming support  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <kevin@krinke.ca>
;; Maintainer: Kevin C. Krinke <kevin@krinke.ca>
;; Keywords: quo-emacs
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

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

;;; Code:

(require 'qemacs-common)
(require 'qemacs-startup)

(when (display-graphic-p)
  (scroll-bar-mode -1))

(use-package rainbow-delimiters
  :demand t
  :straight t

  :preface
  (defvar rainbow-delimiters-omit-modes
    '(
      autoconf-mode
      makefile-mode
      makefile-gmake-mode
      makefile-imake-mode
      makefile-makepp-mode
      makefile-bsdmake-mode
      makefile-automake-mode
      )
    "List of modes to not use `rainbow-delimiters-mode' with.")

  (defun maybe-rainbow-delimiters ()
    "Conditionally use `rainbow-delimiters-mode'."
    (let ((proceed t))
      (catch 'break
        (dolist (this-mode rainbow-delimiters-omit-modes)
          (when (eq major-mode this-mode)
            (setq proceed nil)
            (throw 'break nil)
            ) ;; end when this-mode is major-mode
          ) ;; end dolist this-mode
        ) ;; end catch 'break
      (when proceed
        (rainbow-delimiters-mode))
      ) ;; end let proceed
    ) ;; end maybe-rainbow-delimiters

  :hook ((prog-mode . maybe-rainbow-delimiters))
  ) ;; end use-package rainbow-delimiters

(use-package qisses
  :demand t
  :config
  (when (qemacs-is-ide)
    ;; use the IDE banner
    (setq qisses-banner qisses-qide-banner))
  (qisses-setup)
  ) ;; end use-package qisses

(qemacs-startup-unless
 ;; condition
 (qemacs-startup-answer-was-yes "use-theming")
 ;; body
 `(if (equal qemacs-startup-theme-name 'qemacs-default)
      (progn
        (message-log "Keeping `qemacs-default' theme.")
        )
    (progn
      (message-log "Loading `qemacs-default' theme.")
      (qemacs-startup-restore-theme) ;; unload custom startup theme
      (load-theme 'qemacs-default t) ;; load default theme
      )
    )
 ) ;; end unless use-theming

(qemacs-startup-when
 ;; condition
 (qemacs-startup-answer-was-yes "use-theming")
 ;; body

 ;; `(use-package color-theme-sanityinc-tomorrow
 ;;   :demand t
 ;;   :straight t
 ;;
 ;;   :config
 ;;
 ;;   ;; terminal/graphical shared settings
 ;;   (setq-default line-spacing 0)
 ;;   (setq ansi-color-faces-vector [default default default italic underline success warning error])
 ;;
 ;;   (custom-set-faces '(web-mode-block-face ((t nil))))
 ;;   (custom-set-faces '(header-line ((t nil))))
 ;;   ;; (set-face-attribute 'header-line         t :background nil)
 ;;   ;; (set-face-attribute 'web-mode-block-face t :background nil)
 ;;   (set-face-attribute 'show-paren-match-expression nil :inherit 'show-paren-match)
 ;;   (set-face-attribute 'show-paren-mismatch nil :background "brightred" :foreground "white")
 ;;
 ;;   (qemacs-startup-restore-theme) ;; remove qemacs-default theme
 ;;   (if (display-graphic-p)
 ;;       ;; xemacs, qxemacs, qxide
 ;;       (progn ;; light theme setup
 ;;         (scroll-bar-mode -1)
 ;;         (if (find-font (font-spec :family "Liberation Mono"))
 ;;             (set-face-attribute 'default t :family "Liberation Mono"))
 ;;         (load-theme 'sanityinc-tomorrow-day t)
 ;;         (set-face-attribute 'show-paren-match nil :background "orange" :foreground "black")
 ;;         (set-face-attribute 'powerline-inactive0 nil :background "#efefef")
 ;;         (set-face-attribute 'powerline-inactive1 nil :background "#efefef")
 ;;         (set-face-attribute 'powerline-inactive2 nil :background "#efefef")
 ;;         (set-cursor-color "dark slate gray")
 ;;         ) ;; end light theme setup
 ;;     ;; emacs, qemacs, qide
 ;;     (progn ;; dark theme setup
 ;;       (load-theme 'sanityinc-tomorrow-night t)
 ;;       (set-face-attribute 'show-paren-match nil :background "black" :foreground "bright yellow")
 ;;       (set-face-attribute 'powerline-inactive0 nil :background "grey17")
 ;;       (set-face-attribute 'powerline-inactive1 nil :background "grey17")
 ;;       (set-face-attribute 'powerline-inactive2 nil :background "grey17")
 ;;       ) ;; end dark theme setup
 ;;     ) ;; end if display-graphic-p
 ;;
 ;;   ) ;; end use-package color-theme-sanityinc-tomorrow

 `(progn
    (qemacs-startup-restore-theme) ;; remove qemacs-startup theme

    (if (display-graphic-p)
        ;; graphic (xemacs, qxemacs, qxide)
        (use-package color-theme-sanityinc-tomorrow
          :demand t
          :straight t

          :config
          ;; terminal/graphical shared settings
          (setq-default line-spacing 0)
          (setq ansi-color-faces-vector [default default default italic underline success warning error])

          ;; (custom-set-faces '(header-line ((t nil))))
          ;; (custom-set-faces '(web-mode-block-face ((t nil))))
          (set-face-attribute 'header-line         t :background 'unspecified)
          (set-face-attribute 'web-mode-block-face t :background 'unspecified)
          ;; (set-face-attribute 'show-paren-match-expression nil :inherit 'show-paren-match)
          ;; (set-face-attribute 'show-paren-mismatch nil :background "brightred" :foreground "white")

          ;; xemacs, qxemacs, qxide
          (scroll-bar-mode -1)
          (if (find-font (font-spec :family "Liberation Mono"))
              (set-face-attribute 'default t :family "Liberation Mono"))
          (load-theme 'sanityinc-tomorrow-day t)
          (set-face-attribute 'show-paren-match nil :background "orange" :foreground "black")
          (set-face-attribute 'powerline-inactive0 nil :background "#efefef")
          (set-face-attribute 'powerline-inactive1 nil :background "#efefef")
          (set-face-attribute 'powerline-inactive2 nil :background "#efefef")
          (set-cursor-color "dark slate gray")
          (enable-theme 'sanityinc-tomorrow-day)

          ) ;; end use-package color-theme-sanityinc-tomorrow

      ;; terminal (emacs, qemacs, qide)
      (use-package modus-themes
        :demand t
        :straight t
        :config
        (load-theme 'modus-vivendi t)
        (custom-theme-set-faces
         'modus-vivendi
         '(font-lock-keyword-face       ((t :foreground "skyblue")))
         '(font-lock-function-name-face ((t :foreground "sandybrown")))
         '(font-lock-variable-name-face ((t :foreground "gold")))
         '(web-mode-block-face          ((t :background "#000000")))
         )
        (enable-theme 'modus-vivendi)
        ) ;; end use-package modus-themes
      ) ;; end if display-graphic-p
    ) ;; end progn

 ) ;; end when use-theming

(provide 'qemacs-init-theming)
;;; qemacs-init-theming.el ends here
