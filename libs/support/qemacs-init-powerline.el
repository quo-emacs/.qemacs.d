;;; qemacs-init-powerline.el --- qemacs powerline support  -*- lexical-binding: t; -*-

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

(use-package powerline
  :demand t
  :straight t
  :diminish ""

  :config
  (use-package powerline-evil
    :demand t
    :straight t
    :diminish ""
    ) ;; end use-package powerline-evil

  ;; (use-package powerline-themes
  ;;   :demand t
  ;;   :diminish ""
  ;;   :config
  ;;   (powerline-center-evil-theme)
  ;;   ) ;; end use-package powerline-themes

  (use-package qemacs-powerline-themes
    :demand t
    :diminish ""
    :config
    (powerline-qemacs-theme)
    ;; (powerline-qemacs-all-modes-theme)
    ) ;; end qemacs-powerline-themes
  ) ;; end use-package powerline


(provide 'qemacs-init-powerline)
;;; qemacs-init-powerline.el ends here
