;;; qemacs-init-snippets.el --- qemacs snippet support  -*- lexical-binding: t; -*-

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

;;; Code:

(use-package autoinsert
  :demand t
  :straight t
  :diminish ""
  :config
  (setq auto-insert t
        ;;auto-insert-query nil ;; uncomment to disable auto-insert prompt
        auto-insert-alist nil   ;; comment to restore default templates
        ) ;; end settings
  (auto-insert-mode 1)
  ;; run auto-insert on find-file-hook
  (add-hook 'find-file-hook 'auto-insert)
  ) ;; end use-package autoinsert

(use-package yasnippet-snippets
  :demand t
  :straight t
  :diminish ""
  )

(use-package yasnippet
  :demand t
  :straight t
  :diminish yas-minor-mode
  :after (yasnippet-snippets autoinsert)
  :preface
  (defun qemacs-yas-dir-locals ()
    "Utility function for adding .dir-locals.d/snippets to `yas-snippet-dirs'."
    (let ((this-path (dir-locals-d)))
      (when (and this-path (file-directory-p this-path))
        (let ((these-snippets (file-name-as-directory (concat this-path "snippets"))))
          (when (file-directory-p these-snippets)
            (add-to-list 'yas-snippet-dirs these-snippets)
            (yas-reload-all)
            )
          )
        )
      )
    ) ;; end qemacs-yas-dir-locals
  :config
  (yas-global-mode 1)
  ) ;; end use-package yasnippet

(use-package yast
  :demand t
  :diminish ""
  ) ;; end use-package yast

(use-package yasai
  :demand t
  :diminish ""
  :after (yasnippet yast)
  ) ;; end use-package yasai

;; (use-package yasai-helm
;;   :demand t
;;   :diminish ""
;;   :after (yasai)
;;   ) ;; end use-package yasai-helm

(provide 'qemacs-init-snippets)
;;; qemacs-init-snippets.el ends here
