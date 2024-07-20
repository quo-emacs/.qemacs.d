;;; commentary.el --- code commenting utilities -*- lexical-binding: t; -*-

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

;; `commentary' enables `comment-line' and `comment-region' to do separate
;; things.
;;
;; When `commentary-mode' is active, `M-/' will comment the current line with
;; "// " and `M-?' will comment the current region with c-style block prefix
;; "/*" and suffix "*/".
;;
;; TODO: allow for per-mode customized comment-start/end/style settings

;;; Code:


(defvar-keymap commentary-mode-map
  :doc "Keymap for `commentary-mode'."
  "M-/" 'commentary-line
  "M-?" 'commentary-block)

(define-minor-mode commentary-mode
  "Minor-mode for managing `commentary' functionality."
  :lighter "#"
  :keymap commentary-mode-map
  ;; :after-hook (lambda ()
  ;;               (local-unset-key "M-?")
  ;;               (local-unset-key "M-/")
  ;;               (use-local-map 'commentary-mode-map))
  ) ;; end commentary-mode

;;;###autoload
(defun commentary-block (beg end &optional arg)
  "Toggle commenting the current block.

See `comment-region' for the meaning of BEG END and ARG."
  (interactive "*r\nP")
  (let ((orig-start comment-start)
        (orig-end   comment-end)
        (orig-style comment-style))
    ;; specify the start and end for line comments
    (setq-local comment-start "/*")
    (setq-local comment-end "*/")
    (setq-local comment-style 'multi-line)
    ;; comment the line
    (comment-or-uncomment-region beg end arg)
    ;; restore the original start and end settings
    (setq-local comment-start orig-start)
    (setq-local comment-end   orig-end)
    (setq-local comment-style orig-style)
    ) ;; end orig-start and orig-end declaration
  ) ;; end commentary-block

;;;###autoload
(defun commentary-line (n)
  "Toggle commenting the current line.

See `comment-line' for the meaning of N."
  (interactive "p")
  (commentary/line-handler 'comment-line n)
  ) ;; end commentary-line

;;
;; Internal Functions
;;

;;;###autoload
(defun commentary/line-handler (orig-fn &rest argv)
  "Internal function for `commentary-line' and advice wrapping `comment-line'.

ORIG-FN is the original function to wrap.
ARGV is the variable argument list for the ORIG-FN."
  (let ((orig-start comment-start)
        (orig-end   comment-end)
        (orig-style comment-style))
    ;; specify the start and end for line comments
    (setq-local comment-start "// ")
    (setq-local comment-end   "")
    (setq-local comment-style 'indent)
    ;; comment the line
    (apply orig-fn argv)
    ;; restore the original start and end settings
    (setq-local comment-start orig-start)
    (setq-local comment-end   orig-end)
    (setq-local comment-style orig-style)
    ) ;; end orig-start and orig-end declaration
  ) ;; end commentary-line

;;;###autoload
(defun commentary/shutdown ()
  "Internal function for shutting down `commentary-mode'."
  (advice-remove 'comment-line 'commentary/line-handler)
  (setq
   emulation-mode-map-alists
   (remove
    emulation-mode-map-alists
    `((commentary-mode . ,commentary-mode-map)))
   )
  )

;;;###autoload
(defun commentary/startup ()
  "Internal function for starting up `commentary-mode'."
  (advice-add 'comment-line :around 'commentary/line-handler)
  (add-to-list
   'emulation-mode-map-alists
   `((commentary-mode . ,commentary-mode-map))
   )
  )

;;;###autoload
(defun commentary/toggle ()
  "Internal function to manage toggling commentary mode."
  (if commentary-mode-hook
      ;; already present, go away
      (commentary/shutdown)
    ;; not present, show up
    (commentary/startup)
    ) ;; end mode check
  ) ;; end commentary/toggle-mode

;; register startup/shutdown hook
(add-hook 'commentary-mode-hook 'commentary/toggle)

(provide 'commentary)
;;; commentary.el ends here
