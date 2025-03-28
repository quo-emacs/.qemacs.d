;;; storydown.el --- storydown mode helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kevin C. Krinke

;; Author: Kevin C. Krinke <kevin@krinke.ca>
;; Version: 0.0.1

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

;;; Code:

;; (defvar-keymap storydown-mode-map
;;   :doc "Keymap for `storydown-mode'."
;;   "C-c s d e" 'storydown-emote-line
;;   ) ;; end storydown-mode-map
;;
;; (define-minor-mode storydown-mode
;;   "Minor-mode for managing `storydown' functionality."
;;   :lighter "#"
;;   :keymap storydown-mode-map
;;   ) ;; end storydown-mode

;;;###autoload
(defun storydown-emote-line ()
  "Toggle emoting the current line."
  (interactive)
  ;; get buffer, cursor pos, get multiline of text
  (if (use-region-p)
      ;; true case
      (let* (
             (start-pos (region-beginning))
             (end-pos (region-end))
             (point-pos (point))
             )
        (storydown/toggle-emote-line point-pos start-pos end-pos)
        ) ;; end let
    ;; false case
    (let* (
           (start-pos (line-beginning-position))
           (end-pos (line-end-position))
           (point-pos (point))
           )
      (storydown/toggle-emote-line point-pos start-pos end-pos)
      ) ;; end let
    ) ;; end if
  ) ;; end commentary-line

;;
;; Internal Functions
;;

;;;###autoload
(defun storydown/toggle-emote-line (point-pos start-pos end-pos)
  "Toggle the emotive text state.

Text region is START-POS to END-POS
Returns cursor to POINT-POS."
  (let* (
         (this-lede (buffer-substring-no-properties start-pos (+ start-pos 3)))
         (this-tail (buffer-substring-no-properties end-pos (- end-pos 3)))
         (is-present (and (string-equal this-lede "\\**") (string-equal this-tail "*\\*")))
         )
    (when is-present
      ;; (message "un-emote %S %d|%d-%d: %s (%s|%s)" is-present point-pos start-pos end-pos this-text this-lede this-tail)
      (delete-region end-pos (- end-pos 3))
      (delete-region start-pos (+ start-pos 3))
      )
    (when (not is-present)
      ;; (message "emoting %S %d|%d-%d: %s (%s|%s)" is-present point-pos start-pos end-pos this-text this-lede this-tail)
      (goto-char end-pos)
      (insert "*\\*")
      (goto-char start-pos)
      (insert "\\**")
      (goto-char point-pos)
      )
    ) ;; end let
  ) ;; end storydown/toggle-emote-line

(provide 'storydown)
;;; storydown.el ends here