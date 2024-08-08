;;; qemacs-init-modes-nots.el --- qemacs ide support without tree-sitter  -*- lexical-binding: t; -*-

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

;;
;; Go
;;

(use-package go-mode
  :demand t
  :straight t
  :diminish ""
  :after (flycheck helm)

  :init

  ;; append -tags=all (for Go-Enjin projects)
  (let ((flags (getenv "GOFLAGS"))
        (extra "-tags=all"))
    (if (equal flags "")
        ;; no flags set
        (setenv "GOFLAGS" extra)
      ;; keep existing and add the extra
      (setenv "GOFLAGS" (concat flags " " extra))
      )
    ) ;; end let go flags

  :preface
  ;; Set up before-save hooks to format buffer and add/delete imports.
  (defun qemacs-lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    ;; (add-hook 'before-save-hook 'gofmt-before-save))
    )

  :config

  (let ((this-mode-hook 'go-mode-hook)
        (this-mode 'go-mode))
    (unless (qemacs-startup-answer-was-yes "use-go-mode")
      (setq this-mode 'go-ts-mode)
      (setq this-mode-hook 'go-ts-mode-hook)
      )
    (add-to-list 'auto-mode-alist `("\\.go\\'" . ,this-mode))
    (add-hook this-mode-hook 'yas-minor-mode)
    (when (fboundp 'lsp-mode)
      (add-hook this-mode-hook 'lsp-deferred)
      (add-hook this-mode-hook 'qemacs-lsp-go-install-save-hooks)
      )
    )

  ;; go things require final newlines
  (setq mode-require-final-newline t)

  ;; go-mode flycheck settings
  (setq flycheck-go-build-tags              '("all")
        flycheck-go-vet-executable          "go vet -tags=all"
        flycheck-go-test-executable         "go test -tags=all"
        flycheck-go-gofmt-executable        "~/.emacs.d/bin/goimports.sh"
        flycheck-go-errcheck-executable     "errcheck -tags=all"
        flycheck-go-unconvert-executable    "unconvert -tags=all"
        flycheck-go-staticcheck-executable  "staticcheck -tags=all"
        ) ;; end settings

  ;; (setq tab-width 4
  ;; indent-tabs-mode t
  ;; gofmt-command (user-emacs-path "bin" "goimports.sh"))
  ;; (auto-complete-mode 1)
  (setq tab-width 2)
  (setq indent-tabs-mode 1)

  ;; configure lsp things
  (when (fboundp 'lsp-mode)
    (lsp-register-custom-settings
     '(
       ("gopls.staticcheck"         t t)
       ("gopls.verboseOutput"       t t)
       ("gopls.usePlaceholders"     t t)
       ("gopls.completeUnimported"  t t)
       ) ;; end gopls settings list
     ) ;; end lsp-register-custom-settings
    )

  ;; go helm support
  (use-package helm-go-package
    :demand t
    :straight t
    :diminish ""
    :config
    (eval-after-load 'go-mode
      '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map))
    ) ;; end use-package helm-go-package

  (use-package go-eldoc
    :demand t
    :straight t
    :diminish ""
    ) ;; end use-package go-eldoc

  (use-package go-errcheck
    :demand t
    :straight t
    :diminish ""
    ) ;; end use-package go-errcheck

  ) ;; end use-package go-mode



(provide 'qemacs-init-modes-nots)
;;; qemacs-init-modes-nots.el ends here
