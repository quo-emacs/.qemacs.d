;;; qemacs-cheatsheet.el --- qemacs shortcuts help                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <https://github.com/kckrinke>
;; Maintainer: Kevin C. Krinke <https://github.com/kckrinke>
;; Keywords: quo-emacs
;; Version: 0.1.0
;; Package-Requires: ((cheatsheet "1.0"))

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

;; Cheatsheet setup for qemacs.

;;; Code:

(require 'cheatsheet)

;; override cheatsheet C-q binding
(define-key cheatsheet-mode-map (kbd "q") 'qemacs-cheatsheet-close)
(define-key cheatsheet-mode-map (kbd "C-q") 'qemacs-cheatsheet-close)
(define-key cheatsheet-mode-map (kbd "<escape>") 'qemacs-cheatsheet-close)

(defvar qemacs-cheatsheet/previous-layout nil
  "Internal variable for tracking the previous window layout.")

;;;###autoload
(defun qemacs-cheatsheet/save-layout ()
  "Save the current window layout."
  (unless (window-configuration-p qemacs-cheatsheet/previous-layout)
    (setq qemacs-cheatsheet/previous-layout (current-window-configuration))
    ) ;; end unless
  ) ;; end qemacs-cheatsheet/save-layout

;;;###autoload
(defun qemacs-cheatsheet/restore-layout()
  "Restore the previous window layout."
  (when (window-configuration-p qemacs-cheatsheet/previous-layout)
    (set-window-configuration qemacs-cheatsheet/previous-layout))
  (setq qemacs-cheatsheet/previous-layout nil)
  ) ;; end qemacs-cheatsheet/restore-layout

;;;###autoload
(defun qemacs-cheatsheet-open ()
  "Display cheatsheet in full view."
  (interactive)
  (unless (get-buffer "*cheatsheet*")
    ;; remember the current-window-configuration
    (qemacs-cheatsheet/save-layout)
    ;; display the cheatsheet
    (cheatsheet-show)
    ;; switch to the cheatsheet buffer
    (switch-to-buffer "*cheatsheet*")
    ;; scroll to the top of the buffer
    (goto-char (point-min))
    ;; clear all other windows
    (delete-other-windows)
    ;; setup evil bindings if present
    (when (fboundp 'evil-local-set-key)
      (dolist (this-mode (list 'normal 'motion))
        (evil-local-set-key this-mode (kbd "q")        'qemacs-cheatsheet-close)
        (evil-local-set-key this-mode (kbd "C-q")      'qemacs-cheatsheet-close)
        (evil-local-set-key this-mode (kbd "<escape>") 'qemacs-cheatsheet-close)
        )
      )
    )
  ) ;; end qemacs-cheatsheet-open

;;;###autoload
(defun qemacs-cheatsheet-close ()
  "Remove cheatsheet from view."
  (interactive)
  (let* ((cheatsheet-buffer (get-buffer "*cheatsheet*")))
    (when cheatsheet-buffer
      (qemacs-cheatsheet/restore-layout)
      (kill-buffer cheatsheet-buffer)))
  ) ;; end qemacs-cheatsheet-close

;;;###autoload
(defun qemacs-cheatsheet-toggle ()
  "Toggle displaying the `qemacs-cheatsheet'."
  (interactive)
  (let* ((existing-buffer (get-buffer "*cheatsheet*")))
    (if existing-buffer
        (qemacs-cheatsheet-close)
      (qemacs-cheatsheet-open)))
  ) ;; end qemacs-cheatsheet-toggle

(cheatsheet-add-group
 "Cheatsheet Legend"
 '(:key "<backtick>" :description "grave accent   (`)")
 '(:key "<tilde>"    :description "squiggly line  (~)")
 '(:key "C-"         :description "control key    (CTRL)")
 '(:key "S-"         :description "shift key      (SHIFT)")
 '(:key "M-"         :description "alt key        (ALT)")
 '(:key "TAB"        :description "tab key        (\t)")
 '(:key "SPC"        :description "space key      ( )")
 '(:key "RET"        :description "return key     (Enter)")
 '(:key "ESC"        :description "escape key     (ESC)")
 )

(cheatsheet-add-group
 'Help
 '(:key " C-h w"       :description "show emacs help for which key does what")
 '(:key " C-h v"       :description "show emacs help for variable")
 '(:key " C-h f"       :description "show emacs help for function")
 '(:key " C-h C-h"     :description "emacs help")
 '(:key " M-x evil-tutor-start" :description "vimtutor for emacs")
 '(:key " <f1>"        :description "toggle this cheatsheet")
 ) ;; end Help

(cheatsheet-add-group
 'Common
 '(:key "C-x f"        :description "find files (helm)")
 '(:key "<f3>"         :description "record macro")
 '(:key "<f4>"         :description "end-record or run macro")
 '(:key "S-<f2>"       :description "go to next spellcheck error")
 '(:key "C-<f2>"       :description "spellcheck entire buffer")
 '(:key "<f2>"         :description "spell correct at point")
 '(:key "M-<backtick>" :description "emacs menu bar")
 '(:key "C-x C-c"      :description "quit emacs")
 '(:key "C-x s"        :description "save some files")
 '(:key "C-x C-s"      :description "save this file")
 '(:key "C-c q b"      :description "quit current buffer")
 ) ;; end Common

(cheatsheet-add-group
 'Editing
 '(:key "M-_"    :description "undo")
 '(:key "C-_"    :description "redo")
 '(:key "ESC"    :description "evil-escape (quit action)")
 '(:key "M-?"    :description "toggle commentary block")
 '(:key "M-/"    :description "toggle commentary line")
 '(:key "M-'"    :description "expand snippet text")
 '(:key "M-;"    :description "completion at point")
 )

(cheatsheet-add-group
 "Quick Switching"
 '(:key "C-c C-t p"        :description "switch to previous buffer")
 '(:key "C-c C-t n"        :description "switch to next buffer")
 '(:key "C-c C-t t"        :description "toggle quick switch buffer list")
 '(:key "C-<tilde>"        :description "switch to previous buffer")
 '(:key "C-<backtick>"     :description "switch to next buffer")
 '(:key "C-S-<tab>"        :description "switch to previous buffer")
 '(:key "C-<tab>"          :description "switch to next buffer")
 '(:key "<f12>"            :description "quick-switch between windows")
 ) ;; end Quick Switch

(cheatsheet-add-group
 "Quick Switch (Buffer Mode)"
 '(:key "C-RET"    :description "activate selected buffer (highlighted)")
 '(:key "RET"      :description "activate buffer at point")
 '(:key "S-TAB"    :description "select previous buffer")
 '(:key "TAB"      :description "select next buffer")
 '(:key "ESC"      :description "hide quick switch buffer list")
 ) ;; end Quick Switch

(cheatsheet-add-group
 'Neotree
 '(:key "C-<f12>"      :description "open to project directory")
 '(:key "S-<f12>"      :description "sidebar toggle show/hide")
 '(:key "n" :description "next line")
 '(:key "p" :description "previous line")
 '(:key "U" :description "Go up a directory")
 '(:key "g" :description "Refresh")
 '(:key "A" :description "Maximize/Minimize the NeoTree Window")
 '(:key "H" :description "Toggle display hidden files")
 '(:key "O" :description "Recursively open a directory")
 '(:key "C-c C-n" :description "Create a file or create a directory if filename ends with a ‘/’")
 '(:key "C-c C-d" :description "Delete a file or a directory.")
 '(:key "C-c C-r" :description "Rename a file or a directory.")
 '(:key "C-c C-c" :description "Change the root directory.")
 '(:key "C-c C-p" :description "Copy a file or a directory.")
 '(:key "RET" :description "Open current item if it is a file. Fold/Unfold current item if it is a directory.")
 '(:key "SPC" :description "(same as RET)")
 '(:key "TAB" :description "(same as TAB)")
 )

(cheatsheet-add-group
 'Snippets
 '(:key "M-k"     :description "previous field during expand snippet")
 '(:key "M-j"     :description "next field or maybe expand snippet")
 '(:key "M-'"     :description "expand snippet")
 )

(cheatsheet-add-group
 'Debugging
 '(:key "C-x a b" :description "dape-breakpoint-toggle")
 '(:key "C-x a B" :description "dape-breakpoint-remove-all")
 '(:key "<f7>"    :description "dape-step-in")
 '(:key "S-<f7>"  :description "dape-step-out")
 '(:key "<f8>"    :description "dape-next")
 '(:key "<f9>"    :description "dape-continue")
 '(:key "C-<f10>" :description "dape-kill-quit")
 '(:key "<f10>"   :description "dape-disconnect-quit")
 '(:key "<f11>"   :description "dape-attach (dlv)")
 '(:key "C-<f11>" :description "dape-attach-port (:2345)")
 '(:key "S-<f11>" :description "dape-attach")
 )

(cheatsheet-add-group
 'Languages
 '(:key "S-<f6>"   :description "lsp rename thing")
 '(:key "<f6>"     :description "lsp find references")
 '(:key "M-,"      :description "lsp describe thing")
 '(:key "M-."      :description "lsp find definition")
 '(:key "M->"      :description "xref go back")
 '(:key "M-}"      :description "go to next flycheck error")
 '(:key "M-{"      :description "go to previous flycheck error")
 '(:key "M-n"      :description "go to last change")
 '(:key "M-N"      :description "go to last change (reverse)")
 '(:key "C-c d g"  :description "lsp doc glance at point")
 )

;; (cheatsheet-add-group
;;  'Web
;;  '(:key "C-c t w i" :description "toggle web indentation (2<>4)")
;;  )

(provide 'qemacs-cheatsheet)
;;; qemacs-cheatsheet.el ends here
