;;; qemacs-init-help.el --- qemacs user help supports  -*- lexical-binding: t; -*-

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

;; once upon a time, there was another using these configs and had a really
;; tough time adjusting to emacs+evil-mode, so just in case anyone else ends
;; up using these, <F1> should be helpful
(use-package cheatsheet
  :demand t
  :straight t
  :diminish ""
  :config
  ) ;; end use-package cheatsheet

(use-package qemacs-cheatsheet
  :demand t
  :diminish ""
  :after (cheatsheet)
  ) ;; end use-package qemacs-cheatsheet

(provide 'qemacs-init-help)
;;; qemacs-init-help.el ends here
