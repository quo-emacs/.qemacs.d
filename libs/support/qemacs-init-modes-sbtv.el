;;; qemacs-init-modes-sbtv.el --- qemacs slightly better than vim support  -*- lexical-binding: t; -*-

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

;;
;; cc-mode is considered "sbtv" because these settings are used by many other
;; modes for indentation and other default values
;;

(use-package cc-mode
  :demand t
  :config
  (setq
   c-default-style                      "bsd"
   c-backslash-column                   'set-from-style
   c-doc-comment-style                  'javadoc
   c-backslash-max-column               'set-from-style
   c-block-comment-prefix               'set-from-style
   c-syntactic-indentation              t
   c-guess-offset-threshold             10
   c-comment-only-line-offset           0
   c-label-minimum-indentation          2
   c-objc-method-parameter-offset       2
   c-echo-syntactic-information-p       nil
   c-objc-method-arg-unfinished-offset  2
   c-cleanup-list
   '(empty-defun-braces one-liner-defun scope-operator compact-empty-funcall)
   ) ;; end settings
  ) ;; end use-package cc-mode

;;
;; code folding
;;

(use-package yafolding
  :demand t
  :straight t
  :diminish ""
  :config
  (add-hook 'qemacs-del-trail-space 'yafolding-show-all)
  ) ;; end use-package yafolding

;;
;; apache configuration
;;

(use-package apache-mode
  :demand t
  :straight t
  :diminish ""
  :mode
  (("\\\`/etc/apache.*\\.\\(conf\\|load\\)\\'" . apache-mode))
  ) ;; end use-package apache-mode

;;
;; Makefile
;;

(use-package make-mode
  :demand t
  :straight t
  :diminish ""
  
  :mode (("Makefile\\'" . makefile-mode)
         ("\\.mk\\'"    . makefile-mode))

  ;; :config
  ;; BUG: autotools-language-server requires python 3.10+
  ;; (add-hook 'after-init-hook
  ;;           `(lambda ()
  ;;              (if (fboundp 'lsp-mode)
  ;;                  (add-to-list
  ;;                   'lsp-language-id-configuration
  ;;                   '("Makefile\\'" . makefile-gmake-mode)
  ;;                   )
  ;;                )
  ;;              ))

  ) ;; end use-package make-mode

(use-package makefile-executor
  :demand t
  :straight t
  :diminish ""
  :hook ((makefile-mode . makefile-executor-mode))
  ) ;; end use-package makefile-executor

;;
;; run-command (with make support)
;;

(use-package run-command
  :demand t
  :straight t
  :diminish ""
  :config
  (setq run-command-default-runner 'run-command-runner-compile)
  (setq run-command-selector nil)
  ) ;; end use-package run-command

(use-package run-command-recipe-make
  :demand t
  :diminish ""
  :config
  (add-to-list 'run-command-recipes 'run-command-recipe-make)
  ) ;; end use-package run-command-recipe-make
;;
;; commentary
;;

(setq comment-empty-lines t)
(use-package commentary
  :demand t
  :diminish ""
  )

;;
;; navigation
;;

(use-package quick-switch
  :demand t
  :diminish ""
  :config
  (use-package quick-switch-evil :demand t :diminish "")
  (quick-switch-global-mode)
  ) ;; end use-package quick-switch

(use-package goto-chg :demand t :diminish "")

;; combobulate is so much better but does not support many languages, and yet
;; combobulate is not so good at go-mode (only on development branch and there
;; are strange rendering issues with the current checkout)
;;
;; move-text isn't "bad", maybe just need a "move-func-up/down" thing
(use-package move-text
  :demand t
  :straight t
  :diminish ""
  )

;;
;; windowing
;;

(use-package ace-window
  :demand t
  :straight t
  :diminish ""
  ) ;; end use-package ace-window

;;
;; markdown
;;

(use-package markdown-mode
  :demand t
  :straight t
  :diminish ""
  :mode
  (
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode)
   )
  ) ;; end use-package markdown-mode

;;
;; org
;;

(use-package org
  :demand t
  :straight t
  :diminish ""

  :mode
  (("\\.org\\'" . org-mode))

  :hook
  ((org-mode . evil-org-mode))

  :config
  (setq org-pretty-entities t
        ;; Fontify the whole line for headings (with a background color).
        org-fontify-whole-heading-line t
        org-persist-directory (user-emacs-path "transient" "org-persist")
        )

  (use-package evil-org
    :demand t
    :straight t
    :diminish ""
    :config
    (evil-org-set-key-theme
	   '(textobjects insert navigation additional shift todo heading))
    ) ;; end use-package evil-org
  ) ;; end use-package org


;;
;; Perl
;;

(use-package multi-mode
  ;; multi-mode is used for integrating pod-cperl-mode
  :demand t
  :straight t
  :diminish ""

  :init
  ;; use cperl-mode instead of perl-mode
  (setq auto-mode-alist        (rassq-delete-all 'perl-mode auto-mode-alist))
  (setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))

  :mode
  (
   ("\\.pl$"  . cperl-mode)
   ("\\.pm$"  . cperl-mode)
   ("\\.pod$" . pod-cperl-mode)
   )

  :config

  (require 'pod-mode) ;; libs/other
  (add-hook 'pod-mode-hook 'font-lock-mode)

  (use-package cperl-mode
    :demand t
    :straight t
    :diminish ""
    :config
    (cperl-toggle-abbrev)
    (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
    (setq cperl-hairy nil
          cperl-electric-parens nil
          cperl-electric-keywords nil
          cperl-electric-linefeed nil
          cperl-electric-lbrace-space nil
          cperl-electric-parens-string nil
          cperl-electric-backspace-untabify nil
          )
    ) ;; end use-package cperl-mode

  (use-package pod-cperl-mode
    :demand t
    :straight (pod-cperl-mode
               :type git
               :host github
               :repo "renormalist/emacs-pod-cperl-mode")
    :diminish ""
    :config
    (use-package cperl-mode :demand t :straight t :diminish "")
    ) ;; use-package pod-cperl-mode

  ) ;; end use-package multi-mode

;;
;; front-matter
;;

(use-package json-mode
  :demand t
  :straight t
  :diminish ""
  )
(use-package yaml-mode
  :demand t
  :straight t
  :diminish ""
  )
(use-package conf-mode
  :demand t
  :diminish ""
  )

(use-package front-matter
  :demand t
  :diminish ""

  :mode (("\\.njn\\'"        . json-mode)
         ("\\.njn\\.tmpl\\'" . json-mode))

  :hook ((org-mode           . front-matter-mode)
         (web-mode           . front-matter-mode)
         (json-mode          . front-matter-mode)
         (json-ts-mode       . front-matter-mode)
         (text-mode          . front-matter-mode)
         (markdown-mode      . front-matter-mode))

  :config
  ;; limit front-matter to within "content" parent paths
  (add-to-list 'front-matter-allow-patterns "/content/")
  (add-to-list 'front-matter-allow-patterns "/emails/")
  ;; deny front-matter within any README file
  (add-to-list 'front-matter-deny-patterns "README\\.[a-zA-Z0-9]+\\'")
  ) ;; end use-package front-matter

;;
;; Additional packages
;;

;; generate UUID strings, required for `yast' package
(use-package uuidgen
  :demand t
  :straight (uuidgen
             :type git
             :host github
             :repo "kanru/uuidgen-el"))

;; auto close the *compile* buffer
(use-package auto-close-compile
  :demand t
  :config
  (setq auto-close-compile-always nil)
  (setq auto-close-compile-delay    0)
  )

(provide 'qemacs-init-modes-sbtv)
;;; qemacs-init-modes-sbtv.el ends here
