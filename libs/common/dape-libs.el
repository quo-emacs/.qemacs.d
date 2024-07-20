;;; dape-libs.el --- attach dape to things              -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <https://github.com/kckrinke>
;; Maintainer: Kevin C. Krinke <https://github.com/kckrinke>
;; Keywords: quo-emacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4") (dape "0.12.0") (f) (cl-lib))

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

;; Extra `dape'-related functions not included in the main package and required
;; by `dape-attach'.
;;
;; The following is a list of all the functions provided by `dape-libs':
;;
;; Interactive:
;; - `dape-maybe-kill-and-quit'
;; - `dape-maybe-disconnect-and-quit'
;; - `dape-breakpoint-load-current-buffer'
;;
;; Non-Interactive:
;; - `dape--find-saved-breakpoints'
;; - `dape--find-config-key'

;;; Code:

(require 'f)
(require 'cl-lib)
(require 'dape)

;;
;; Interactive
;;

;;;###autoload
(defun dape-maybe-kill-and-quit ()
  "Kill any running debugger session and quit dape."
  (interactive)
  (when dape-active-mode
    (call-interactively `dape-kill)
    (sleep-for 0.25))
  (dape--kill-buffers))

;;;###autoload
(defun dape-maybe-disconnect-and-quit ()
  "Disconnect any running debugger session and quit dape."
  (interactive)
  (if dape-active-mode
      (call-interactively `dape-disconnect-quit)
    (dape--kill-buffers)))

;;;###autoload
(defun dape-breakpoint-load-current-buffer (&optional breakpoints-file)
  "Load breakpoints for the `current-buffer'.
Will use `dape-default-breakpoints-file' if BREAKPOINTS-FILE is nil."
  (interactive (list (read-file-name "Load breakpoints from file: ")))
  (setq breakpoints-file (or breakpoints-file dape-default-breakpoints-file))
  (setq-local breakpoints-found (dape--find-saved-breakpoints buffer-file-name breakpoints-file))
  (when (> (length breakpoints-found) 0)
    (cl-loop
     with breakpoints = breakpoints-found
     for (point plist) in breakpoints
     do (ignore-errors
          (with-current-buffer (current-buffer)
            (save-excursion
              (goto-char point)
              (apply #'dape--breakpoint-place plist)))
          ) ;; end do ignore-errors
     ) ;; end cl-loop
    ) ;; end when one or more breakpoints
  ) ;; end dape-breakpoint-load-buffer

;;;###autoload
(defun dape-breakpoint-save-merged-default ()
  "Call `dape-breakpoint-save-merged' using `dape-default-breakpoint-file'."
  (interactive)
  (dape-breakpoint-save-merged))

;;;###autoload
(defun dape-breakpoint-save-merged (&optional breakpoints-file)
  "Update breakpoints in BREAKPOINTS-FILE.
Similar to `dape-breakpoint-save' without clobbering breakpoints in files that
have not been visited and yet are present in the BREAKPOINTS-FILE.

Essentially follows these steps:
- get a list of saved breakpoints, without loading them
- get a list of open file names
- for each open file, remove from saved breakpoints
- for each active breakpoint, add to saved breakpoints
- write the updated breakpoints to the BREAKPOINTS-FILE

Will use `dape-default-breakpoints-file' if BREAKPOINTS-FILE is nil."
  (interactive (list (read-file-name "Save breakpoints to file: ")))
  (setq breakpoints-file (or breakpoints-file dape-default-breakpoints-file))
  ;; get a list of all open files
  (setq-local open-files nil)
  (dolist (buf (buffer-list))
    (let ((file-name (buffer-file-name buf)))
      (if file-name
          (unless (string-match "^\s*\\*" file-name)
            (push file-name open-files)
            ) ;; end unless match file-name
        ) ;; end if file-name
      ) ;; end let file-name
    ) ;; end dolist buffer-list
  ;; for each open file, prune saved-breakpoints
  (setq-local saved-breakpoints (dape--find-all-saved-breakpoints))
  (setq-local final-breakpoints nil)
  (cl-loop
   ;; for each saved breapoint
   for b-p in saved-breakpoints
   for file = (nth 0 b-p)
   when file
   do (unless (catch 'break
                (dolist (file-name open-files)
                  (if (string= file-name file)
                      (throw 'break t)))
                nil)
        (push b-p final-breakpoints)
        )
   )
  ;; for each active breakpoint, add to final-breakpoints
  (cl-loop
   for ov in dape--breakpoints
   for file = (buffer-file-name (overlay-buffer ov))
   for point = (overlay-start ov)
   for args = (mapcar (apply-partially 'overlay-get ov) dape--breakpoint-args)
   when (and file point)
   do (push (append (list file point) args) final-breakpoints)
   ) ;; end cl-loop for each active breakpoint
  ;; build and write file-contents
  (setq-local file-contents
              (format "%s\n%s\n\n%s\n"
                      ";; Generated by `dape-breakpoint-save'"
                      ";; Load breakpoints with `dape-breakpoint-load'"
                      (format "%S" (reverse final-breakpoints))
                      ))
  (f-write-text file-contents 'utf-8 breakpoints-file)
  ) ;; end dape-breakpoint-save-merged


;;;###autoload
(defun dape-breakpoints-auto-save-merged ()
  "Registers hooks and advice for auto-saving (merged) breakpoints.

Using `dape-breakpoint-save-merged-default', will add to the `kill-emacs-hook'
and add advice to the following: `dape-breakpoint-log',
`dape-breakpoint-toggle', `dape-breakpoint-expression',
`dape-breakpoint-remove-all' and `dape-breakpoint-remove-at-point'."
  (interactive)
  ;; Save merged breakpoints on exiting emacs
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save-merged-default)
  ;; Save merged breakpoints on toggle/remove breakpoints
  (advice-add 'dape-breakpoint-log             :after #'dape-breakpoint-save-merged-default)
  (advice-add 'dape-breakpoint-toggle          :after #'dape-breakpoint-save-merged-default)
  (advice-add 'dape-breakpoint-expression      :after #'dape-breakpoint-save-merged-default)
  (advice-add 'dape-breakpoint-remove-all      :after #'dape-breakpoint-save-merged-default)
  (advice-add 'dape-breakpoint-remove-at-point :after #'dape-breakpoint-save-merged-default)
  ) ;; end dape-register-auto-save-breakpoints

;; When files are visited, include any saved breakpoints
;;;###autoload
(defun dape-breakpoints-auto-load-merged ()
  "Setup auto-loading saved breakpoints when visiting files.

Registers `dape-breakpoint-load-current-buffer' with the following hooks:
`find-file-hook', `lsp-after-open-hook' and `xref-after-jump-hook'."
  (interactive)
  (add-hook 'find-file-hook       'dape-breakpoint-load-current-buffer)
  (add-hook 'lsp-after-open-hook  'dape-breakpoint-load-current-buffer)
  (add-hook 'xref-after-jump-hook 'dape-breakpoint-load-current-buffer)
  ) ;; end dape-breakpoints-auto-load-current-buffer

;;
;; Non-Interactive
;;

;;;###autoload
(defun dape--find-config-key (named)
  "Return the NAMED config key to use, or nil if no config found."
  (setq-local
   this-key
   (catch 'break
     (dolist (this-cfg dape-configs)
       (setq-local this-cfg-name (car this-cfg))
       (if (string-equal (format "%s" named) (format "%s" this-cfg-name))
           (throw 'break this-cfg-name))))))

;;;###autoload
(defun dape--find-all-saved-breakpoints (&optional breakpoints-file)
  "Return all breakpoints saved.
Will use `dape-default-breakpoints-file' if BREAKPOINTS-FILE is nil."
  (setq breakpoints-file (or breakpoints-file dape-default-breakpoints-file))
  (when (file-exists-p breakpoints-file)
    (with-temp-buffer
      (insert-file-contents breakpoints-file)
      (goto-char (point-min))
      (nreverse (read (current-buffer))))
    ) ;; end when file-exists-p
  ) ;; end dape--find-all-saved-breakpoints

;;;###autoload
(defun dape--find-saved-breakpoints (file &optional breakpoints-file)
  "Return any breakpoints saved for the given FILE.
Will use `dape-default-breakpoints-file' if BREAKPOINTS-FILE is nil."
  (setq-local these-breakpoints ())
  (cl-loop
   with breakpoints = (dape--find-all-saved-breakpoints breakpoints-file)
   for (pfile point . args) in breakpoints
   for plist = (cl-mapcan 'list dape--breakpoint-args args)
   do (when (string= pfile file)
        (setq-local this-breakpoint (list point plist))
        (push this-breakpoint these-breakpoints)
        ) ;; end when file == buffer-file-name
   ) ;; end cl-loop
  (reverse these-breakpoints))

;;;###autoload
(defun dape--find-active-breakpoints (file)
  "Return all active `dape--breakpoints' for FILE."
  (setq-local found-breakpoints nil)
  (cl-loop
   for b-p in dape--breakpoints
   for pfile = (nth 0 b-p)
   for point = (nth 1 b-p)
   when (and pfile point)
   do (when (string= pfile file)
        (push b-p found-breakpoints)))
  found-breakpoints)

(provide 'dape-libs)
;;; dape-libs.el ends here
