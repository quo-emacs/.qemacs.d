;;; quick-switch-evil.el --- quick (buffer) switch        -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <kevin@krinke.ca>
;; Maintainer: Kevin C. Krinke <kevin@krinke.ca>
;; Keywords: quo-emacs
;; Version: 0.1.0
;; Package-Requires: ((quick-switch "0.1.0") (evil))

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

;; Evil keybindings for `quick-switch'.

;;; Code:

(require 'evil)
(require 'quick-switch)

;;;###autoload
(defun quick-switch-evil/buffer-mode-hook ()
  "Evil mode support for `quick-switch-buffer-mode'."
  (dolist (this-mode (list 'normal 'insert 'visual 'motion))
    (evil-local-set-key this-mode (kbd "k") 'quick-switch-cycle-prev)
    (evil-local-set-key this-mode (kbd "j") 'quick-switch-cycle-next)
    (evil-local-set-key this-mode (kbd "C-^") 'quick-switch-cycle-prev)
    (evil-local-set-key this-mode (kbd "RET") 'push-button)
    (evil-local-set-key this-mode (kbd "C-RET") 'quick-switch-activate)
    (evil-local-set-key this-mode (kbd "C-g") 'quick-switch-hide)
    (evil-local-set-key this-mode (kbd "<escape>") 'quick-switch-hide)
    )
  )
(add-to-list 'quick-switch-buffer-mode-hook 'quick-switch-evil/buffer-mode-hook)

;;;###autoload
(defun quick-switch-evil/global-mode-hook ()
  "Evil mode support for `quick-switch-global-mode'."
  (dolist (this-mode (list 'normal 'insert 'visual 'motion))
    (evil-define-key this-mode quick-switch-global-map (kbd "C-@") 'quick-switch-cycle-next)
    (evil-define-key this-mode quick-switch-global-map (kbd "C-^") 'quick-switch-cycle-prev)
    (evil-define-key this-mode quick-switch-global-map (kbd "C-TAB") 'quick-switch-cycle-next)
    (evil-define-key this-mode quick-switch-global-map (kbd "C-<backtab>") 'quick-switch-cycle-prev)
    (evil-define-key this-mode quick-switch-global-map (kbd "C-<iso-lefttab>") 'quick-switch-cycle-prev)
    )
  )
(add-to-list 'quick-switch-global-mode-hook 'quick-switch-evil/global-mode-hook)

(provide 'quick-switch-evil)
;;; quick-switch-evil.el ends here
