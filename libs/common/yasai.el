;;; yasai.el --- yasnippet autoinsert                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <kevin@krinke.ca>
;; Maintainer: Kevin C. Krinke <kevin@krinke.ca>
;; Keywords: quo-emacs
;; Version: 0.1.1
;; Package-Requires: ((autoinsert) (yasnippet))

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

;; `autoinsert' using `yasnippet' templates.

;;; Changelog:

;; v0.1.1:
;;   * refactored `yasai/entry' to include snippet-desc in `auto-insert-alist'

;;; Code:
(require 'yasnippet)
(require 'autoinsert)

;;;###autoload
(defun yasai-add (file-pattern snippet-mode snippet-name snippet-desc)
  "Register an `auto-insert-alist' template.

Creates a new `yasai/entry' with FILE-PATTERN, SNIPPET-NAME, SNIPPET-DESC
and SNIPPET-MODE."
  (unless (yasai/add-table file-pattern snippet-mode snippet-name snippet-desc)
    (add-to-list
     'auto-insert-alist
     (cons
      (cons file-pattern snippet-desc)
      `(lambda () (yasai/callback ,file-pattern)))
     ) ;; end add-to auto-insert-alist
    ) ;; end yasai/add-table call
  ) ;; end yasai-add

(defclass yasai/entry ()
  ((file-pattern :initarg :file-pattern :type string :initform ""
                 :documentation "File name pattern for `autoinsert'.")
   (snippet-name :initarg :snippet-name :type string :initform ""
                 :documentation "Name of the `yasnippet' template.")
   (snippet-desc :initarg :snippet-desc :type string :initform ""
                 :documentation "Description of the `yasnippet' template.")
   (snippet-mode :initarg :snippet-mode   :type symbol :initform nil
                 :documentation "Major-mode used for `yas-lookup-snippet'."))
  :documentation
  "A registered `yasai' template entry.")

(defvar yasai/registry (make-hash-table :test #'equal)
  "Internal hash table for registered file pattern entries.")

;;;###autoload
(defun yasai/get-table (file-pattern)
  "Return a list of entries for the given FILE-PATTERN."
  (let ((these-entries (gethash file-pattern yasai/registry)))
    (if (and these-entries (> (length these-entries) 0))
        these-entries
      (list ))))

;;;###autoload
(defun yasai/add-table (file-pattern snippet-mode snippet-name snippet-desc)
  "Create a new `yasai/entry' instance and add it to the `yasai/registry'.

See `yasai-add' for details on FILE-PATTERN, SNIPPET-NAME, SNIPPET-DESC and
SNIPPET-MODE."
  (let* ((new-entry (make-instance
                     `yasai/entry
                     :file-pattern file-pattern
                     :snippet-name snippet-name
                     :snippet-desc snippet-desc
                     :snippet-mode snippet-mode))
         (existing (gethash file-pattern yasai/registry))
         (return-value (and existing (> (length existing) 0))))
    (add-to-list 'existing new-entry)
    (puthash file-pattern existing yasai/registry)
    return-value))

(defvar yasai/custom-reader `yasai/completing-reader
  "Custom user input reader.

Default is to use `yasai/completing-reader'.
Set this to a function which accepts two arguments, a message and the list of
choices to present.
Return value is required to be one of the choices given.")

;;;###autoload
(defun yasai/completing-reader (message choices)
  "User input reader for `yasai/callback'.

Uses `yasai/custom-reader' if set, otherwise uses `completing-read' with
MESSAGE and CHOICES."
  (completing-read message choices nil t))

;;;###autoload
(defun yasai/prompt (message choices)
  "Internal function to delegate user input reading.

See `completing-read' for the meaning of MESSAGE and CHOICES."
  (funcall yasai/custom-reader message choices))

;;;###autoload
(defun yasai/callback (file-pattern)
  "Internal `auto-insert-alist' hook function for looking up registered templates.

See `yasai-add' for details on FILE-PATTERN."
  (let ((these-entries (gethash file-pattern yasai/registry))
        (chose-snippet-name nil)
        (chose-snippet-mode nil))
    (when (> (length these-entries) 0)
      ;; one or more entries present
      (if (> (length these-entries) 1)
          ;; many entries present
          (let ((choices nil))
            (dolist (this-entry these-entries)
              (let ((this-name (slot-value this-entry :snippet-name))
                    (this-desc (slot-value this-entry :snippet-desc)))
                (add-to-list 'choices (cons this-name this-desc))))
            ;; prompt with choices
            (let ((answer (yasai/completing-reader "Select: " choices)))
              (dolist (this-entry these-entries)
                (let ((this-name (slot-value this-entry :snippet-name))
                      (this-mode (slot-value this-entry :snippet-mode)))
                  (when (string= this-name answer)
                    (setq chose-snippet-name this-name)
                    (setq chose-snippet-mode this-mode)
                    )
                  )
                )
              )
            )
        ;; just one entry present
        (let ((entry (car these-entries)))
          (setq chose-snippet-name (slot-value entry :snippet-name))
          (setq chose-snippet-mode (slot-value entry :snippet-mode))))
      ) ;; end when one or more these-entries
    (when (and chose-snippet-name chose-snippet-mode)
      (yas-expand-snippet
       (yas-lookup-snippet chose-snippet-name chose-snippet-mode))
      ) ;; end when chose-snippet
    ) ;; end let variables
  ) ;; end yasai/alist-binding

(provide 'yasai)
;;; yasai.el ends here
