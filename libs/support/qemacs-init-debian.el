;;; qemacs-init-debian.el --- qemacs support for debian things  -*- lexical-binding: t; -*-

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

(use-package debian-el
  :demand t
  :straight t
  )

(use-package dpkg-dev-el
  :demand t
  :config
  (require 'debian-changelog-mode)
  (require 'debian-control-mode)
  (require 'debian-copyright)
  (require 'readme-debian)
  )

(provide 'qemacs-init-debian)
;;; qemacs-init-debian.el ends here
