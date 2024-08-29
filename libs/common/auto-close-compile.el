;;; auto-close-compile.el --- auto-close compile buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <https://github.com/kckrinke>
;; Maintainer: Kevin C. Krinke <https://github.com/kckrinke>
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

;; `auto-close-compile' is a simple package for handling the auto-closing of the
;; Emacs compile buffer.
;;
;; This package is inspired by the following stack overflow threads:
;;
;;  https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close
;;  https://emacs.stackexchange.com/questions/74469/delete-or-hide-compilation-buffer-when-finished-compile-and-run-process

;;; Code:

(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'compile))

(defcustom auto-close-compile-always nil
  "Auto-close compile when non-nil, even when compilation failed."
  :group 'auto-close-compile
  :type 'boolean)

(defcustom auto-close-compile-delay 0
  "Number of seconds to delay auto-closing the *compile* buffer."
  :group 'auto-close-compile
  :type 'number)

;;;###autoload
(defun auto-close-compile/started (proc)
  "Internal function for handling start of compilation events.

SEE `compilation-start-hook' for meaning of PROC."
  (setq-local auto-close-compile/start-time (current-time))
  ) ;; end auto-close-compile/started

;;;###autoload
(defun auto-close-compile/finished (buffer text)
  "Internal function for handling end of compilation events.

See `compilation-finish-functions' for meaning of BUFFER and TEXT."

  (let* ((this-total-time (time-subtract nil auto-close-compile/start-time))
         (this-timestamp (format-time-string "%s.%3N" this-total-time)))

    (catch 'break

      (unless (string-equal text "finished\n")
        ;; one or more errors detected
        (message (string-trim text))
        (unless auto-close-compile-always
          (throw 'break nil))
        ) ;; end when one or more errors

      ;; made it this far, auto-close.
      (run-with-timer auto-close-compile-delay nil
                      'auto-close-compile/close buffer this-timestamp)

      ) ;; end catch 'break
    ) ;; end let variables
  ) ;; end auto-close-compile/finished

;;;###autoload
(defun auto-close-compile/close (buffer timestamp)
  "Internal function to perform the auto-close event for BUFFER.

TIMESTAMP is the duration of the compilation event."
  (delete-window (get-buffer-window buffer 'visible))
  (message "Compilation auto-closed. (%s seconds)" timestamp)
  ) ;; end auto-close-compile/close

;; add compilation event hooks
(add-hook 'compilation-start-hook       'auto-close-compile/started)
(add-hook 'compilation-finish-functions 'auto-close-compile/finished)

(provide 'auto-close-compile)
;;; auto-close-compile.el ends here
