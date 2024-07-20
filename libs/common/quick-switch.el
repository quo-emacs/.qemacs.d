;;; quick-switch.el --- quick (buffer) switch        -*- lexical-binding: t; -*-

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

;; Yet another quick buffer switching system.
;;
;; Use `Control+Backtick' to display a buffer listing and select the next
;; buffer from the current, or `Control+Tilde' to select the previous.
;; Use `Control+Enter' to switch to the selected buffer.

;;; Code:

;; TODO: implement pagination of some sort

(eval-and-compile
  (require 'cl-lib))

;;
;; Settings
;;

(defvar quick-switch-list-sorter nil
  "Function to use with `cl-sort' on the list of buffers.

Value must be either nil or a predicate accepting two string arguments and
returning non-nil if the first is considered less than the second.

Use nil to specify no sorting of the list returned by `buffer-list'.

Use `string-lessp' to sort the list alphabetically for example.")

(defvar quick-switch-list-hidden t
  "Set to t to include a section for buffers starting with an asterisk.

Examples: `*scratch*', `*Messages*' and `*Warnings*' are considered hidden.")

(defvar quick-switch-list-all-hidden nil
  "Set to t to include buffers starting with one or more spaces and an asterisk.

Requires `quick-switch-list-hidden' to also be set to t.")

(defvar quick-switch-list-ignore
  (list "^\s*[*]")
  "List of regular expression patterns used to determine if a buffer is ignored.")

(defvar quick-switch-list-include
  (list "^\s*[*]\\(splash\\|scratch\\|Messages\\|Warnings\\)[*]\s*$")
  "List of regular expression patterns used to include buffers otherwise ignored.")

(defface quick-switch-selected-face
  '((t :foreground "orange" :weight bold))
  "`quick-switch' selected buffer name style."
  :group 'quick-switch)

(defface quick-switch-selected-modified-face
  '((t :inherit quick-switch-selected-face :italic t))
  "`quick-switch' modified buffer name style."
  :group 'quick-switch)

(defface quick-switch-default-face
  '((t :inherit t))
  "`quick-switch' default buffer name style."
  :group 'quick-switch)

(defface quick-switch-default-modified-face
  '((t :inherit quick-switch-default-face :italic t))
  "`quick-switch' modified buffer name style."
  :group 'quick-switch)

(defvar quick-switch-side 'right
  "Specify where to place the quick-switch side window.

Must be one of: `left' `bottom' `right' or `top'.")

(add-variable-watcher
 'quick-switch-side
 (lambda (symbol newval operation where)
   (cond
    ((equal newval 'top))
    ((equal newval 'left))
    ((equal newval 'bottom))
    ((equal newval 'right))
    (t (user-error (format "%S is not a valid `quick-switch-side' value." newval)))
    )))

;;
;; Internal Variables
;;

(defvar quick-switch/buffer nil
  "Internal variable to track an active quick-switch view.")

(defvar quick-switch/selected nil
  "Internal variable to track the currently-selected buffer in the list.")

(defvar quick-switch/cache (list)
  "Internal variable to track the current list of buffers.")

;;
;; Keymaps and Modes
;;

(defvar-keymap quick-switch-global-map
  :doc "Keymap for `quick-switch-global-mode'."
  "C-c C-t t"       'quick-switch-toggle
  "C-c C-t n"       'quick-switch-cycle-next
  "C-c C-t p"       'quick-switch-cycle-prev
  "C-`"             'quick-switch-cycle-next ;; Control+Backtick
  "C-@"             'quick-switch-cycle-next ;; Control+Backtick
  "C-^"             'quick-switch-cycle-prev ;; Control+Tilde
  "C-<tab>"         'quick-switch-cycle-next
  "C-<backtab>"     'quick-switch-cycle-prev
  "C-<iso-lefttab>" 'quick-switch-cycle-prev
  )

(define-minor-mode quick-switch-global-mode
  "Global minor-mode providing `quick-switch' functionality."
  :require 'quick-switch
  :group 'quick-switch
  :global t
  :lighter nil
  :init-value nil
  :keymap quick-switch-global-map
  ) ;; end quick-switch-global-mode

(defvar-keymap quick-switch-buffer-map
  :doc "Keymap for `quick-switch-buffer-mode'."
  :parent quick-switch-global-map
  "<escape>"      'quick-switch-hide
  "C-RET"         'quick-switch-activate
  )

(define-derived-mode quick-switch-buffer-mode fundamental-mode
  "Quick Switch"
  "Specific major-mode for interacting with the `quick-switch' buffers list."
  :global nil
  :lighter nil
  :init-value nil
  :keymap quick-switch-buffer-map
  (setq-local header-line-format " *Quick Switch*")
  ;; (display-line-numbers-mode -1)
  ) ;; end quick-switch-buffer-mode

;;
;; Command Functions
;;

;;;###autoload
(defun quick-switch-show ()
  "Show the `quick-switch' side window."
  (interactive)
  (quick-switch/setup)
  (quick-switch/render)
  ) ;; end quick-switch-cycle-next

;;;###autoload
(defun quick-switch-hide ()
  "Hide the `quick-switch' side window."
  (interactive)
  (quick-switch/cleanup)
  (setq quick-switch/selected nil)
  ) ;; end quick-switch-cycle-next

;;;###autoload
(defun quick-switch-toggle ()
  "Toggle the `quick-switch' side window."
  (interactive)
  (if quick-switch/buffer
      (quick-switch-hide)
    (quick-switch-show))
  ) ;; end quick-switch-toggle

;;;###autoload
(defun quick-switch-cycle-next ()
  "Cycle to the next `quick-switch' buffer."
  (interactive)
  (quick-switch/setup)
  (quick-switch/select-next)
  (quick-switch/render)
  ) ;; end quick-switch-cycle-next

;;;###autoload
(defun quick-switch-cycle-prev ()
  "Cycle to the prev `quick-switch' buffer."
  (interactive)
  (quick-switch/setup)
  (quick-switch/select-prev)
  (quick-switch/render)
  ) ;; end quick-switch-cycle-prev

;;;###autoload
(defun quick-switch-activate ()
  "Switch to the currently selected buffer and close the side window."
  (interactive)
  (quick-switch/setup)
  (quick-switch/cleanup)
  (switch-to-buffer quick-switch/selected)
  (setq quick-switch/selected nil)
  ) ;; end quick-switch-cycle-next

;;
;; Internal Functions
;;

;;;###autoload
(defun quick-switch/ignored (this-name)
  "Return t if THIS-NAME is ignored."
  (let* ((return-value nil))
    (catch 'break
      (dolist (pattern quick-switch-list-ignore)
        (when (string-match pattern this-name)
          ;; this-name is to be ignored
          (let ((include-this nil))
            (catch 'inner-break
              (dolist (include-pattern quick-switch-list-include)
                (when (string-match include-pattern this-name)
                  (setq include-this t)
                  (throw 'inner-break nil)
                  )
                )
              ) ;; end catch inner-break
            (unless include-this
              (setq return-value t)
              (throw 'break nil)
              ) ;; end include-this even though ignored
            )
          ) ;; end this-name is ignored
        ) ;; end dolist quick-switch-list-ignore
      ) ;; end catch break
    return-value)
  ) ;; end quick-switch/ignored

;;;###autoload
(defun quick-switch/refresh ()
  "Internal function to refresh buffer lists."
  (let* ((cache-list (list)))
    (dolist (this-buf (buffer-list))
      (let ((this-name (buffer-name this-buf)))
        (unless (equal this-name " *quick-switch*")
          (unless (quick-switch/ignored this-name)

            (cond
             ;; special hidden (help, completions, etc)
             ((string-match "^\s+" this-name)
              (if quick-switch-list-hidden
                  (if quick-switch-list-all-hidden
                      (setq cache-list (append cache-list `(,this-name))))
                ))
             ;; normal hidden (scratch, warnings, splash)
             ((string-match "^\\*" this-name)
              (if quick-switch-list-hidden
                  (setq cache-list (append cache-list `(,this-name)))))
             ;; everything else
             (t (setq cache-list (append cache-list `(,this-name))))
             ) ;; end conditional

            ) ;; end unless ignored

          ) ;; end unless *quick-switch*
        ) ;; end let this-name
      ) ;; end dolist buffer-list

    (if quick-switch-list-sorter
        (setq quick-switch/cache (cl-sort cache-list quick-switch-list-sorter))
      (setq quick-switch/cache cache-list)
      ) ;; end if sorter

    ) ;; end let normal-list

  ) ;; end quick-switch/refresh

;;;###autoload
(defun quick-switch/select-next ()
  "Internal function to select the next buffer in the list."
  (let* ((is-next nil)
         (current-name quick-switch/selected))
    (setq quick-switch/selected nil)
    (catch 'break
      (dolist (this-name quick-switch/cache)
        (if is-next
            (progn
              (setq quick-switch/selected this-name)
              (throw 'break nil))
          (if (equal this-name current-name)
              (setq is-next t))
          )
        )
      )
    (unless quick-switch/selected
      ;; nothing selected yet, current buffer should always be selected
      (setq quick-switch/selected (nth 0 quick-switch/cache)))
    )
  ) ;; end quick-switch/select-next

;;;###autoload
(defun quick-switch/select-prev ()
  "Internal function to select the prev buffer in the list."
  (let* ((prev-name nil)
         (current-name quick-switch/selected))
    (setq quick-switch/selected nil)
    (catch 'break
      (dolist (this-name quick-switch/cache)
        (if (equal this-name current-name)
            (progn
              (setq quick-switch/selected prev-name)
              (throw 'break nil))
          (setq prev-name this-name)
          )
        )
      )
    (unless quick-switch/selected
      (setq quick-switch/selected (nth 0 (last quick-switch/cache))))
    )
  ) ;; end quick-switch/select-prev

;;;###autoload
(defun quick-switch/setup ()
  "Internal function to setup a quick-switch side window if not already present."
  (quick-switch/refresh) ;; always refresh
  (unless quick-switch/buffer
    ;; get a reference to the current buffer
    (setq quick-switch/selected (buffer-name (current-buffer)))
    ;; get a new reference to a quick-switch buffer
    (setq quick-switch/buffer (get-buffer-create " *quick-switch*"))
    ;; set the mode
    (with-current-buffer quick-switch/buffer
      ;; setup major-mode
      (quick-switch-buffer-mode))
    ;; display the buffer in a side window
    (display-buffer-in-side-window
     quick-switch/buffer
     (list `(side . ,quick-switch-side))
     ) ;; end side window display
    ) ;; end unless quick-switch/buffer
  ) ;; end quick-switch/setup

;;;###autoload
(defun quick-switch/cleanup ()
  "Internal function to cleanup the quick-switch side window when present."
  (when quick-switch/buffer
    ;; remove the buffer
    (kill-buffer quick-switch/buffer)
    ;; let go of the reference
    (setq quick-switch/buffer nil))
  ) ;; end quick-switch/cleanup

;;;###autoload
(defun quick-switch/render-cache ()
  "Internal function for rendering a section of the list of buffers."
  (let ((selected-pos -1) (counter 0))

    (catch 'break
      (dolist (buf-name quick-switch/cache)
        ;; (if (> counter 9)
        ;;     (throw 'break nil))
        (setq counter (+ counter 1))
        (let* ((this-face 'quick-switch-default-face)
               (is-modified (buffer-modified-p (get-buffer buf-name)))
               (is-selected (equal quick-switch/selected buf-name)))

          ;; handle selected buffer styles and cursor position
          (cond
           ((and is-selected is-modified)
            (setq this-face 'quick-switch-selected-modified-face)
            (setq selected-pos (point))
            )
           ((and is-selected (not is-modified))
            (setq this-face 'quick-switch-selected-face)
            (setq selected-pos (point))
            )
           ((and (not is-selected) is-modified)
            (setq this-face 'quick-switch-default-modified-face))
           (t
            (setq this-face 'quick-switch-default-face))
           ) ;; end condition

          ;; add the button field
          (insert-button
           buf-name
           'buffer-name buf-name
           'face this-face
           'action (lambda (button)
                     (setq quick-switch/selected (button-get button 'buffer-name))
                     (quick-switch-activate)))

          ;; handle vertical vs horizontal spacing
          (cond
           ((equal quick-switch-side 'top)    (insert "\t"))
           ((equal quick-switch-side 'left)   (insert "\n"))
           ((equal quick-switch-side 'bottom) (insert "\t"))
           ((equal quick-switch-side 'right)  (insert "\n"))
           (t (error "not equal?")))

          )
        ) ;; end buffer-list loop
      ) ;; end catch break

    (if (> selected-pos -1)
        ;; buffer selected
        (goto-char selected-pos))
    ) ;; end buffer listing

  ) ;; end quick-switch/render-section

;;;###autoload
(defun quick-switch/render ()
  "Internal function to display/update the quick-switch view."
  ;; ensure the cursor is correctly placed
  (select-window (get-buffer-window quick-switch/buffer))
  ;; operate on the buffer
  (with-current-buffer quick-switch/buffer
    ;; make buffer read-write
    (read-only-mode -1)
    ;; allow wrapping of long lines
    (setq truncate-lines nil)
    ;; reset the buffer contents
    (erase-buffer)
    (goto-char (point-min))
    ;; render the actual listing
    (quick-switch/render-cache)
    ;; make buffer read-only again
    (read-only-mode 1)
    ) ;; end buffer operations
  ) ;; end quick-switch/render

(provide 'quick-switch)
;;; quick-switch.el ends here
