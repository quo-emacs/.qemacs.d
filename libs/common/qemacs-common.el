;;; qemacs-common.el --- common emacs-lisp functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <https://github.com/kckrinke>
;; Maintainer: Kevin C. Krinke <https://github.com/kckrinke>
;; Keywords: quo-emacs
;; Version: 0.1.5

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

;; These functions are the cleaned up results of things found on the internet
;; that are not available within Emacs or any of the various ELPA and MELPA
;; package repositories.  These are such trivial things that their inspired
;; origins have been lost over time.

;; The following are the functions present within this package:
;;
;; `push-undo', 'pop-undo',
;; `indent-buffer', `indent-buffer-on-save',
;; `buffer-focused-p', `buffer-string*', `buffer-visible-p',
;; `dir-locals-el-path', `dir-locals-d', `message-log',
;; `qemacs-del-trail-space', `qemacs-del-trail-space-hook', `qemacs-escape',
;; `qemacs-is-ide', `qemacs-load', `qemacs-unpropertize-kill-ring',
;; `quit-current-buffer', `read-file-string', `sort-longest-to-shortest',
;; `string-present', `string-is-false', `string-is-true', `user-emacs-path', and
;; `write-file-string'.

;;; Changelog:

;; v0.1.5:
;;   * added `push-undo' and `pop-undo'
;;   * use push/pop-undo in `indent-buffer' on save
;;
;; v0.1.4:
;;   * added `indent-buffer', `indent-buffer-on-save' and `indent-buffer-on-save-modes-list'
;;
;; v0.1.3:
;;   * added `dir-locals-el-path' and `dir-locals-d' functions
;;
;; v0.1.2:
;;   * added `string-present' function

;;; Code:

;;;###autoload
(defun qemacs-del-trail-space-hook nil
  "`qemacs-del-trail-space' hooks run before `delete-trailing-whitespace'.")

;;;###autoload
(defun qemacs-del-trail-space ()
  "`delete-trailing-whitespace' wrapper function to include arbitrary hooks."
  (interactive)
  (run-hooks 'qemacs-del-trail-space-hook)
  (delete-trailing-whitespace))

(defvar qemacs-escape-hook nil
  "`qemacs-escape' hooks run before `keyboard-quit'.")

;;;###autoload
(defun qemacs-escape ()
  "Qemacs <escape> key handler."
  (interactive)
  (catch 'break
    (let* ((this-buffer (current-buffer))
           (this-name (buffer-name this-buffer)))
      ;; if current-buffer is *Help*, quit
      (when (equal this-name "*Help*")
        (quit-window)
        (throw 'break nil))
      (run-hooks 'qemacs-escape-hook)
      (keyboard-quit)
      ) ;; end let variables
    ) ;; end catch 'break
  ) ;; end qemacs-escape

;;;###autoload
(defun quit-current-buffer ()
  "Kill the current buffer and delete the window."
  (interactive)
  (kill-current-buffer)
  (delete-window))

;;;###autoload
(defun message-log (&rest argv)
  "Log ARGV with `message' to `*Messages*' and not the minibar."
  (let ((inhibit-message t))
    (message (apply 'format argv))))

;;;###autoload
(defmacro make-dd (named kebab verbose)
  "Expands to a debug function NAMED.

KEBAB is a prefix added to all *Messages*
VERBOSE sets the default verbosity."
  (if verbose
      `(defun ,named (&rest argv)
         "Output *Messages* with ARGV applied to `format'."
         (let ((inhibit-message t)
               (tag (format "%s" ,kebab)))
           (if (equal tag "")
               (message (apply 'format argv))
             (message (format "[%s] %s" tag (apply 'format argv)))
             ) ;; end empty tag check
           ) ;; end inhibit-message
         ) ;; end actual dd
    `(defun ,named (&rest argv)
       "Does nothing, ARGV ignored."
       ) ;; end empty dd
    ) ;; end if verbose
  ) ;; end defmacro make-dd

;;;###autoload
(defun write-file-string (file contents)
  "Overwrite the CONTENTS of FILE."
  (unless (file-exists-p file)
    (make-empty-file file))
  (write-region contents nil file)
  ) ;; end with file-string*

;;;###autoload
(defun read-file-string (file)
  "Return the contents of FILE."
  (let ((return-value ""))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (setq return-value (buffer-string))
          ) ;; end with-temp-buffer
      ) ;; end if file-exists-p
    return-value) ;; end let return-value
  ) ;; end with file-string*

;;;###autoload
(defun qemacs-is-ide ()
  "Return t if QEMACS_IDE is true."
  (let ((is-qide (getenv "QEMACS_IDE")))
    (if (and is-qide (string-is-true is-qide))
        t
      nil)
    ) ;; end let getenv
  ) ;; end qemacs-is-ide

;;;###autoload
(defun qemacs-load (file)
  "Convenience function for loading `.el' FILE in `user-emacs-directory'.

FILE must not start with a directory separator and can have the `.el' extension
omitted.

Examples:
 (qemacs-load \"init-thing\")
 (qemacs-load \"moar-thing.el\")
 (qemacs-load \"sub-directory/other-thing\")
 (qemacs-load \"sub-directory/another-thing.el\")"
  (if (string-match file "\.el$")
      ;; file already has extension
      (load (concat user-emacs-directory file))
    ;; file needs extension
    (load (concat user-emacs-directory (concat file ".el")))))

;;;###autoload
(defun qemacs-unpropertize-kill-ring ()
  "Remove properties from `kill-ring'."
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

;;;###autoload
(defun user-emacs-path (&rest argv)
  "Return `user-emacs-directory' suffixed with ARGV joined by slashes."
  (let ((return-value user-emacs-directory))
    (dolist (arg argv)
      (setq return-value (concat return-value "/" arg))
      ) ;; end dolist argv
    ;; collapse duplicated slashes
    (setq return-value (replace-regexp-in-string "/+" "/" return-value))
    return-value) ;; end let return-value
  ) ;; end user-emacs-path

;;;###autoload
(defun string-present (value)
  "Return t if VALUE is not empty and not nil."
  (cond
   ;; nil or empty is false
   ((or (not value) (equal value "")) nil)
   ;; present true
   (t t))
  ) ;; end string-present

;;;###autoload
(defun string-is-true (value)
  "Return t if VALUE is considered true."
  (cond
   ;; empty is false
   ((or (not value) (equal value "")) nil)
   ;; any of yes, on, true or t
   ((string-match "\\\`\s*\\(yes\\|on\\|true\\|t\\)\s*\\'" (downcase value)) t)
   ;; any positive value
   ((> (string-to-number value) 0) t)
   ;; not true
   (t nil))
  ) ;; ens string-is-true

;;;###autoload
(defun string-is-false (value)
  "Return t if VALUE is considered false."
  (cond
   ;; empty is false
   ((or (not value) (equal value "")) t)
   ;; any negative value, or zero
   ((<= (string-to-number value) 0) t)
   ;; any of no, off, false or f
   ((string-match "\\\`\s*\\(no\\|off\\|false\\|f\\)\s*\\'" (downcase value)) t)
   ;; not false
   (t nil))
  ) ;; ens string-is-false

;;;###autoload
(defun buffer-string* (buffer)
  "Return the contents of BUFFER."
  (with-current-buffer buffer
    (substring-no-properties (buffer-string))
    ) ;; end current buffer
  ) ;; end buffer-string*

;;;###autoload
(defun sort-longest-to-shortest (a b)
  "Predicate for determining if A is longer than B.

Returns t if the length of A is longer than B, or if both are of equal length,
returns the result of `string-lessp' and returns nil otherwise.

Use with the `sort' function."
  (let* ((a-len (length a))
         (b-len (length b)))
    (if (< a-len b-len)
        t ;; a is smaller than b
      (if (= a-len b-len)
          (string-lessp a b))
      ) ;; end if a < b
    ) ;; end let variables
  ) ;; end qemacs-common/longest-shortest-sorting

;;;###autoload
(defun buffer-visible-p (&optional this-buffer)
  "Return t if THIS-BUFFER is visible, nil otherwise.

If THIS-BUFFER is nil, uses `current-buffer'."
  (unless this-buffer
    (setq this-buffer (current-buffer)))
  (let ((this-window (get-buffer-window this-buffer)))
    (if this-window
        t
      nil)
    )
  ) ;; end buffer-visible-p

;;;###autoload
(defun buffer-focused-p (&optional this-buffer)
  "Return t if THIS-BUFFER is visible and focused, nil otherwise.

If THIS-BUFFER is nil, uses `current-buffer'."
  (unless this-buffer
    (setq this-buffer (current-buffer)))
  (if (eq this-buffer (window-buffer (selected-window)))
      t
    nil)
  ) ;; end buffer-focused-p

;;;###autoload
(defun dir-locals-el-path ()
  "Return the `.dir-locals.el' file directory.

See: https://stackoverflow.com/a/10248672 for details."
  (let ((variables-file (dir-locals-find-file (or (buffer-file-name) default-directory)))
        (dir-name nil))
    (cond
     ((stringp variables-file)
      (setq dir-name (file-name-directory variables-file)))
     ((consp variables-file)
      (setq dir-name (nth 0 variables-file))))
    dir-name)
  ) ;; end this-local-dir

;;;###autoload
(defun dir-locals-d ()
  "Return the `.dir-locals.d' directory."
  (let ((this-path (dir-locals-el-path))
        (return-value))
    (when (and (string-present this-path) (file-directory-p this-path))
      (let ((check-path (file-name-as-directory (concat (file-name-as-directory this-path) ".dir-locals.d"))))
        (if (file-directory-p check-path)
            (setq return-value check-path))
        )
      )
    return-value)
  ) ;; end dir-locals-d

(defvar pushpop-undo-stack '()
  "Backing list for push/pop-undo stack."
  )

;;;###autoload
(defun push-undo ()
  "Push current undo history to stack, clearing undo history."
  (interactive)
  (push buffer-undo-list pushpop-undo-stack)
  (setq buffer-undo-list nil)
  ) ;; end push-undo

;;;###autoload
(defun pop-undo ()
  "Push current undo history to stack, clearing undo history."
  (interactive)
  (setq-local this-list (pop pushpop-undo-stack))
  (setq buffer-undo-list this-list)
  ) ;; end pop-undo


;;;###autoload
(defun indent-buffer ()
  "Indent the current buffer, after deleting trailing whitespace."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  ) ;; end indent-buffer

(defvar indent-buffer-on-save-modes-list '()
  "Modes affected by calls to `indent-buffer-on-save' during `before-save-hook'.")

;;;###autoload
(defun indent-buffer-on-save ()
  "Call `indent-buffer' during `before-save-hook'."
  (push-undo)
  (catch 'break
    (dolist (this-mode indent-buffer-on-save-modes-list)
      (when (eq this-mode major-mode)
        (indent-buffer)
        (throw 'break nil))
      ))
  (pop-undo)
  ) ;; end indent-buffer-on-save

;; add indend-buffer to before-save-hook - does nothing unless
;; indent-buffer-on-save-modes-list is not nil
(add-hook 'before-save-hook 'indent-buffer-on-save nil nil)

(provide 'qemacs-common)
;;; qemacs-common.el ends here
