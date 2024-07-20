;;; qemacs-init-completion.el --- qemacs code completion support  -*- lexical-binding: t; -*-

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
;; Code Completion
;;

(use-package auto-complete
  :demand t
  :straight t
  :diminish ""
  :config
  (require 'auto-complete-config)
  ) ;; end use-package auto-complete

(use-package company
  :demand t
  :straight t
  :diminish ""
  :after (auto-complete)
  :diminish (global-company-mode . "")
  :config

  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-files)
  (company-tng-configure-default)

  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-align-annotations t ;; aligns annotation to the right hand side
        company-files-exclusions '(".git/" ".DS_Store")
        ) ;; end setq
  ) ;; end use-package company

(use-package company-statistics
  :demand t
  :straight t
  :diminish ""
  :init
  (setq company-statistics-file
        (user-emacs-path "transient" "company-statistics-cache.el"))
  :config
  (company-statistics-mode)
  ) ;; end use-package company-statistics

(use-package company-lsp
  :demand t
  :straight t
  :diminish ""
  :after (company lsp)
  :commands company-lsp
  :config (add-to-list 'company-backends 'company-lsp)
  ) ;; end use-package company-lsp

(use-package company-web
  :demand t
  :straight t
  :diminish ""
  :after (company)
  :commands company-web-html
  :config (add-to-list 'company-backends 'company-web-html)
  ) ;; end use-package company-web

(use-package company-shell
  :demand t
  :straight t
  :diminish ""
  :after (company)
  :commands company-shell
  :config (add-to-list 'company-backends 'company-shell)
  ) ;; end use-package company-shell

(use-package company-plisp
  :demand t
  :straight t
  :diminish ""
  :after (company)
  :commands company-plisp
  :config (add-to-list 'company-backends 'company-plisp)
  ) ;; end use-package company-plisp

(provide 'qemacs-init-completion)
;;; qemacs-init-completion.el ends here
