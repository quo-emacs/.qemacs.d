;;; yasai-helm.el --- yasai helm support                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <kevin@krinke.ca>
;; Maintainer: Kevin C. Krinke <kevin@krinke.ca>
;; Keywords: quo-emacs
;; Version: 0.1.0
;; Package-Requires: ((yasai "0.1.0") (helm))

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

;; Adds `helm' support for `yasai'.

;;; Code:
(require 'helm)
(require 'yasai)

;;;###autoload
(defun yasai-helm/prompt (message choices)
  "`yasai/callback' completion handler using `helm'.

See: `yasai/custom-reader' for the meaning of MESSAGE and CHOICES."
  (helm
   :prompt message
   :helm "*helm yasai/prompt*"
   :sources (helm-build-sync-source "Available templates:"
              :must-match t
              :candidates choices)))

(setq yasai/custom-reader `yasai-helm/prompt)

(provide 'yasai-helm)
;;; yasai-helm.el ends here
