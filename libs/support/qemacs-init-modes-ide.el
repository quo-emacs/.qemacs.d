;;; qemacs-init-modes-ide.el --- qemacs IDE mode supports  -*- lexical-binding: t; -*-

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

(require 'qemacs-common)
(require 'qemacs-startup)
(use-package f :demand t :straight t)
(use-package dape-libs :demand t)
(use-package dape-attach :demand t)
(use-package dape-attach-helm :demand t)

;;
;; auto-fmt
;;

(use-package apheleia
  :demand t
  :straight t
  :diminish ""
  :config
  (apheleia-global-mode +1)
  ) ;; end use-package apheleia


;;
;; dape
;;

(use-package dape
  :demand t

  :config

  ;; (setq dape-key-prefix "\C-x\C-a") ;; same as gud

  ;; Save breakpoints on quit (from dape-libs)
  (dape-breakpoints-auto-save-merged)
  (dape-breakpoints-auto-load-merged)

  ;; To use window configuration like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)

  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

  ;; Projectile users
  ;; (setq dape-cwd-fn 'projectile-project-root)

  ) ;; end use-package dape

;;
;; lsp
;;

(use-package lsp-mode
  :demand t
  :straight t
  :diminish ""
  :after (jsonrpc)

  :init
  ;; (setq lsp-use-plists t) ;; this breaks gopls -- do not use

  :hook (
         (lsp-mode               . lsp-diagnostics-mode)
         (bash-ts-mode           . lsp-deferred)
         (tsx-ts-mode            . lsp-deferred)

         ;;; BUG: json-ls warnings on load:
         ;;; `Warning (lsp-mode): Unknown request method: workspace/diagnostic/refresh`
         ;; (json-mode              . lsp-deferred)
         ;; (json-ts-mode           . lsp-deferred)

         ;;; TODO: check for py3.10+ before starting
         ;; (python-mode         . lsp-deferred)
         ;; (python-ts-mode      . lsp-deferred)

         ;; (javascript-ts-mode  . lsp-deferred)
         )

  :config
  ;; additional lsp packages
  (use-package external-completion :demand t :straight t :diminish "")
  (use-package lsp-ui
    :demand t
    :straight t
    :diminish ""
    :commands (lsp-ui-doc-show lsp-ui-doc-glance)
    :after    (lsp-mode evil)
    :config
    (setq lsp-ui-doc-enable t
          evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
          lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
          lsp-ui-doc-include-signature t       ; Show signature
          lsp-ui-doc-position 'at-point)
    ) ;; end use-package lsp-ui

  :custom
  (lsp-prefer-flymake nil)                            ; not using flymake
  (lsp-completion-provider :none)                     ; using company as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-log-io nil)                                    ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)                      ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                                ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                                 ; Use xref to find references
  (lsp-auto-configure t)                              ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)                          ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)                   ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)                            ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)                        ; I use prettier
  (lsp-enable-links nil)                              ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)                 ; Prettier handles this
  (lsp-enable-suggest-server-download t)              ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)                  ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)                ; This is Treesitter's job
  ;; (lsp-ui-sideline-enable nil)                     ; KCK: uncomment to stop inline documentation
  (lsp-ui-sideline-show-hover nil)                    ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20)           ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable nil)
  (lsp-completion-enable-additional-text-edit t)      ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                              ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                        ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)                ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil)  ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-signature-doc-lines 1)                         ; Don't raise the echo area
  (lsp-modeline-diagnostics-enable nil)               ; Already supported through `flycheck'
  (lsp-modeline-code-actions-enable nil)              ; Modeline should be relatively clean
  (lsp-modeline-workspace-status-enable nil)          ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-ui-doc-enable nil)                             ; KCK: uncomment to stop show docs on hover
  (lsp-ui-doc-use-childframe t)                       ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)                          ; would be nice if it could respect `lsp-signature-doc-lines'
  ;; lens
  (lsp-lens-enable nil)                               ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)                    ; Related to highlighting, using treesitter instead
  ;; multiple languages supported by iph, give it a transient home
  (lsp-intelephense-storage-path        (user-emacs-path "transient" "intelephense"))
  (lsp-intelephense-global-storage-path (user-emacs-path "transient" "intelephense"))

  ) ;; end use-package lsp-mode

;;
;; CCLS
;;

(use-package ccls
  :demand t
  :straight t
  :diminish ""
  :after (lsp flycheck)
  :config
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c-ts-mode c++-mode cpp-ts-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  ) ;; end use-package ccls

;;
;; Go (with lsp and other fancy)
;;

(use-package go-mode
  :demand t
  :straight t
  :diminish ""
  :after (flycheck helm treesit-auto)

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
      (add-to-list 'auto-mode-alist '("go\\.mod\\'"  . go-mod-ts-mode))
      (add-to-list 'auto-mode-alist '("go\\.work\\'" . go-work-ts-mode))
      ) ;; end unless use-go-mode
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
    (if (qemacs-startup-answer-was-yes "use-go-mode")
        (eval-after-load 'go-mode
          '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map))
      (eval-after-load 'go-ts-mode
        '(substitute-key-definition 'go-import-add 'helm-go-package go-ts-mode-map))
      )
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


(provide 'qemacs-init-modes-ide)
;;; qemacs-init-modes-ide.el ends here
