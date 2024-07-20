;;; dape-attach-helm.el --- attach dape to things              -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <https://github.com/kckrinke>
;; Maintainer: Kevin C. Krinke <https://github.com/kckrinke>
;; Keywords: quo-emacs
;; Version: 0.1.0
;; Package-Requires: ((helm) (dape-attach))

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

;; Helm support for `dape-attach'.

;;; Code:

(require 'dape-attach)
(require 'helm)

;;;###autoload
(defun dape-attach-helm/completion-handler (heading message choices default require-match)
  "`dape-attach' completion handler using `helm'.

See `dape-attach/prompt' for the meaning of HEADING, MESSAGE, CHOICES, DEFAULT
and REQUIRE-MATCH."
  (helm
   :prompt message
   :helm "*helm dape-attach/prompt*"
   :sources (helm-build-sync-source heading
              :must-match require-match
              :candidates choices))
  ) ;; end dape-attach-helm/completion-handler

(setq dape-attach/completion-handler `dape-attach-helm/completion-handler)

(provide 'dape-attach-helm)
;;; dape-attach-helm.el ends here
