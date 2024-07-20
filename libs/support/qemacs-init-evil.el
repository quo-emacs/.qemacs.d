;;; qemacs-init-evil.el --- qemacs vi support  -*- lexical-binding: t; -*-

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


(use-package evil
  :demand t
  :straight t
  :diminish ""

  :config
  (defun qemacs-escape-evil (&optional count)
    "`qemacs-escape-hook' to execute :noh on <escape>."
    ;; TODO: figure out a better way to handle :noh/RET/TAB things
    (if (> (length evil-ex-active-highlights-alist) 0)
        ;; clear highlights with :noh
        (evil-ex-execute "noh")
      (progn ;; no active highlights
        (keyboard-quit)
        (evil-ret count)
        ) ;; end no active highlights
      ) ;; end if active highlights
    ) ;; end qemacs-escape-evil
  (add-hook 'qemacs-escape-hook 'qemacs-escape-evil)

  ;; evil mode initial states
  (setq evil-emacs-state-modes nil)
  (setq evil-normal-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-visual-state-modes nil)
  (setq evil-motion-state-modes nil)

  ;; evil-mode settings
  (setq evil-cross-lines t
        evil-move-cursor-back t
        evil-want-fine-undo t
        evil-want-C-u-scroll t
        evil-want-C-w-in-emacs-state nil
        evil-want-C-w-delete t
        evil-intercept-maps nil
        evil-overriding-maps nil
        evil-kill-on-visual-paste t
        evil-ex-search-persistent-highlight nil
        ) ;; end evil-mode settings

  ;; set initial state to normal
  (evil-set-initial-state 'nrepl-mode 'normal)

  ;; setup search
  (setq evil-search-module 'evil-search)
  (evil-select-search-module 'evil-search-module 'evil-search)

  ;;
  ;; additional evil-mode packages
  ;;

  (use-package evil-terminal-cursor-changer
    :demand t
    :straight t
    :diminish ""
    :config
    (unless (display-graphic-p)
      (evil-terminal-cursor-changer-activate)
      ) ;; end unless display-graphic-p
    ) ;; end use-package evil-terminal-cursor-changer

  (use-package undo-fu
    :demand t
    :straight t
    :diminish ""

    :config
    ;; evil mode undo-fu
    (setq evil-undo-system 'undo-fu)
    (setq undo-limit 67108864) ; 64mb
    (setq undo-strong-limit 100663296) ; 96mb
    (setq undo-outer-limit 1006632960) ; 960mb
    ) ;; end use-package undo-fu

  (use-package evil-leader
    :demand t
    :straight t
    :diminish ""
    :config
    (global-evil-leader-mode t)
    (evil-leader/set-leader "<SPC>")
    ) ;; end use-package evil-leader

  (use-package evil-surround
    :demand t
    :straight t
    :diminish ""
    :config (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :demand t
    :straight t
    :diminish ""
    )

  (use-package evil-escape
    :demand t
    :straight t
    :diminish ""

    :init
    (setq-default evil-escape-key-sequence "jk")
    (setq-default evil-escape-delay 0.2)

    ) ;; end use-package evil-escape

  (use-package evil-tutor
    :demand t
    :straight t
    :config
    (setq evil-tutor-working-directory (user-emacs-path "transient" "evil-tutor"))
    ) ;; evil-tutor

  ;; start evil-mode
  (evil-mode 1)
  (evil-escape-mode 1)

  ) ;; end use-package evil


(provide 'qemacs-init-evil)
;;; qemacs-init-evil.el ends here
