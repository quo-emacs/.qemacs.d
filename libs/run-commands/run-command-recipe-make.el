;;; run-command-recipe-make.el --- Recipe for Makefile targets -*- lexical-binding: t -*-

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

;; Recipe for Makefile targets.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'helm-make)

(defun run-command-recipe-make ()
  "Provide commands to run Makefile targets.

Requires `helm-make' (https://github.com/abo-abo/helm-make) to
read Makefile targets, but does not require `helm' and can be
used with any of the selectors supported by `run-command'."

  (when (require 'helm-make nil t)
    (when-let* ((project-dir
                 (locate-dominating-file default-directory "Makefile"))
                (makefile (concat project-dir "Makefile"))
                (targets (helm--make-cached-targets makefile)))
      (seq-map
       (lambda (target)
         (list
          :command-name target
          :command-line (concat "make " target)
          :display target
          :working-dir project-dir))
       targets))))

(provide 'run-command-recipe-make)
;;; run-command-recipe-make.el ends here
