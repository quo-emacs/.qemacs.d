;;; qemacs-startup.el --- emacs initialization features  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <https://github.com/kckrinke>
;; Maintainer: Kevin C. Krinke <https://github.com/kckrinke>
;; Keywords: quo-emacs
;; Version: 0.1.1
;; Package-Requires: ((qemacs-common "0.1.1"))

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

;; `qemacs-startup' requires `qemacs-common' and provides the following:
;;
;; `qemacs-startup-answer-unasked', `qemacs-startup-answer-was-no',
;; `qemacs-startup-answer-was-yes', `qemacs-startup-ask-yn',
;; `qemacs-startup-begin', `qemacs-startup-end', `qemacs-startup-install-theme',
;; `qemacs-startup-notice', `qemacs-startup-restore-theme',
;; `qemacs-startup-trace-use-package', `qemacs-startup/get-state',
;; `qemacs-startup/set-state', and `qemacs-startup/window-setup-callback'.

;;; Code:

(require 'qemacs-common)
(make-dd qemacs-startup/dd "qemacs-startup" nil)

(defvar qemacs-startup-theme-name 'qemacs-default
  "Theme to use when `qemacs-startup-begin' is invoked the first time.")

(defvar qemacs-startup-transient-file
  (user-emacs-path "transient" "qemacs-startup-answers")
  "Transient file for storing `qemacs-startup-ask-yn' values.")

(defvar qemacs-startup/has-started nil
  "Internal variable to track if startup has begun.

Automatically set on first call to `qemacs-startup-notice'.")

;;;###autoload
(defun qemacs-startup-begin ()
  "Begin `qemacs-startup'."
  (unless qemacs-startup/has-started
    (setq qemacs-startup/has-started t)
    (qemacs-startup-install-theme)
    ) ;; end unless has-started
  ) ;; end qemacs-startup-begin

;;;###autoload
(defun qemacs-startup-install-theme ()
  "Convenience function to `enable-theme' `qemacs-startup-theme-name'."
  (load-theme qemacs-startup-theme-name t t)
  (enable-theme qemacs-startup-theme-name)
  ) ;; end qemacs-startup-restore-theme

;;;###autoload
(defun qemacs-startup-restore-theme ()
  "Convenience function to `disable-theme' `qemacs-startup-theme-name'."
  (disable-theme qemacs-startup-theme-name)
  ) ;; end qemacs-startup-restore-theme

(defvar qemacs-startup-early-time nil
  "Startup timing variable (unix epoch).")

;;;###autoload
(defun qemacs-startup-end ()
  "Begin `qemacs-startup'.

Called automatically with `qemacs-startup-notice'."
  (let* ((before (time-convert before-init-time 'integer))
         (after  (time-convert after-init-time  'integer))
         (delta (- after before)))
    (cond
     ((< delta 60)
      (qemacs-startup-notice "(finished in %s)" (format-seconds "%S" delta)))
     ((< delta (* 60 60))
      (qemacs-startup-notice "(finished in %s)" (format-seconds "%M and %S" delta)))
     (t
      (qemacs-startup-notice "(finished in %s)" (format-seconds "%H, %M and %S" delta)))
     ) ;; end time format conditional
    ) ;; end let before after delta
  ) ;; end qemacs-startup-end
(add-to-list 'after-init-hook 'qemacs-startup-end)

;;;###autoload
(defun qemacs-startup-notice (&rest argv)
  "Startup notices, used when there is no UI while starting up.

ARGV is applied to `format'."
  (qemacs-startup-begin)
  (let ((prepared-text (apply 'format argv)))
    (message (format "Startup: %s" prepared-text))
    ) ;; end let prepared-text
  ) ;; end qemacs-startup-notice

;;;###autoload
(defun qemacs-startup-trace-use-package ()
  "Startup tracing of `use-package'."
  (advice-add
   'use-package :before
   `(lambda (&rest argv)
      (qemacs-startup-notice (format "%s" (car argv)))
      ) ;; end lambda
   ) ;; end advice-add use-package
  ) ;; end qemacs-startup-trace-use-package

;;;###autoload
(defun qemacs-startup-ask-yn (tag prompt)
  "Setup function for conditionally including Emacs features.

TAG is a kebab-cased name for the variable.
PROMPT is the text to prompt the user with."
  (qemacs-startup-begin)
  (let ((return-value nil)
        (value (qemacs-startup/get-state tag)))
    (cond
     ((= value 0)
      (if (y-or-n-p prompt)
          (progn ;; user says yes
            (qemacs-startup/set-state tag 1)
            (qemacs-startup/dd "Keeping qemacs setup: %s" tag)
            (setq return-value t)
            )
        (progn ;; user says nope
          (qemacs-startup/set-state tag -1)
          (qemacs-startup/dd "Skipping qemacs setup: %s" tag)
          )
        )
      )
     ((= value 1)
      ;; (qemacs-startup/dd "Including qemacs setup: %s" tag)
      (setq return-value t)
      )
     ;; ((= value -1)
     ;;  (qemacs-startup/dd "Excluding qemacs setup: %s" tag))
     ) ;; end conditions
    return-value) ;; end let state
  ) ;; end qemacs-startup-ask-yn

;;;###autoload
(defmacro qemacs-startup-when (condition body)
  "Macro that conditionally evaluate BODY when CONDITION is non-nil."
  (let ((pass (eval condition)))
    (when pass
      ;; (message (format "startup-when(%S): %S" pass body))
      (eval body))))

;;;###autoload
(defmacro qemacs-startup-unless (condition body)
  "Macro that conditionally evaluate BODY unless CONDITION is non-nil."
  (let ((pass (eval condition)))
    (unless pass
      ;; (message (format "startup-unless(%S): %S" pass body))
      (eval body))))

;;;###autoload
(defun qemacs-startup-answer-was-yes (tag)
  "Return t if TAG startup answer was y."
  (let ((answer (qemacs-startup/get-state tag)))
    (= answer 1))
  ) ;; end qemacs-startup-answer

;;;###autoload
(defun qemacs-startup-answer-was-no (tag)
  "Return t if TAG startup answer was n."
  (let ((answer (qemacs-startup/get-state tag)))
    (= answer -1))
  ) ;; end qemacs-startup-answer

;;;###autoload
(defun qemacs-startup-answer-unasked (tag)
  "Return t if TAG startup question was not asked yet."
  (let ((answer (qemacs-startup/get-state tag)))
    (= answer 0))
  ) ;; end qemacs-startup-answer

;;;###autoload
(defun qemacs-startup/get-state (tag)
  "Internal function to return the state of TAG."
  (let* ((return-value 0)
         (contents (read-file-string qemacs-startup-transient-file))
         (lines (split-string contents "\n")))
    (catch 'break
      (dolist (line lines)
        (qemacs-startup/dd "get-state(%s): checking line - %S" tag line)
        (when (string-match "^[ \t]*\\([^ \t]+\\)\t\\(-1\\|0\\|1\\)[ \t]*$" line)
          (qemacs-startup/dd "get-state(%s): string-match" tag)
          (let* ((key (match-string 1 line))
                 (val (match-string 2 line)))
            (when (equal tag key)
              (qemacs-startup/dd "get-state(%s): %S == %S" tag tag key)
              (setq return-value (string-to-number val))
              (throw 'break nil)
              )
            )
          )
        ) ;; end dolist lines
      ) ;; end catch 'break
    (qemacs-startup/dd "get-state(%s): return-value=%S" tag return-value)
    return-value)
  ) ;; end qemacs-startup/get-state

;;;###autoload
(defun qemacs-startup/set-state (tag state)
  "Internal function for setting a persistent STATE with TAG."
  (qemacs-startup/dd "set-state(%s): setting state=%S" tag state)
  (let* ((updated ())
         (need-append t)
         (contents (read-file-string qemacs-startup-transient-file))
         (lines (split-string contents "\n")))
    (dolist (line lines)
      (qemacs-startup/dd "\t* checking line - %S" line)
      (when (string-match "^[ \t]*\\([^ \t]+\\)\t\\(-1\\|0\\|1\\)[ \t]*$" line)
        (qemacs-startup/dd "\t\t* string-matched")
        (let* ((key (match-string 1 line))
               (val (match-string 2 line))
               (exist (string-to-number val)))
          (if (equal key tag)
              (progn
                (qemacs-startup/dd "\t\t\t* match - tag=%S, state=%S" tag state)
                (add-to-list 'updated (format "%s\t%d" tag state) t)
                (qemacs-startup/dd "\t\t\t* update=%S" updated)
                (setq need-append nil)
                )
            (progn
              (qemacs-startup/dd "\t\t\t* other - tag=%S, state=%S" key exist)
              (add-to-list 'updated (format "%s\t%d" key exist) t)
              (qemacs-startup/dd "\t\t\t* update=%S" updated)
              )
            ) ;; end equal key tag
          ) ;; end let key val exist
        ) ;; end match line
      ) ;; end dolist lines
    (when need-append
      (qemacs-startup/dd "\t\t* need-append - tag=%S, state=%S" tag state)
      (add-to-list 'updated (format "%s\t%d" tag state) t)
      (qemacs-startup/dd "\t\t* update=%S" updated)
      ) ;; end if need-append
    (qemacs-startup/dd "\t* updated=%S" updated)
    (write-file-string qemacs-startup-transient-file (string-join updated "\n"))
    ) ;; end let variables
  ) ;; end qemacs-startup/set-state

(defun qemacs-startup/window-setup-callback ()
  "`window-setup-hook' for qemacs.

Will switch to `*Warnings*' if present and always calls `delete-other-windows'."
  ;; switch to the warnings buffer if it exists
  (if (get-buffer "*Warnings*")
      (switch-to-buffer "*Warnings*"))
  ;; delete startup clutter
  (delete-other-windows)
  ) ;; end qemacs-startup-hook
(add-to-list 'window-setup-hook 'qemacs-startup/window-setup-callback)

(provide 'qemacs-startup)
;;; qemacs-startup.el ends here
