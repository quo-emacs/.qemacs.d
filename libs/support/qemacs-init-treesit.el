;;; qemacs-init-treesit.el --- qemacs tree-sitter support  -*- lexical-binding: t; -*-

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

(use-package treesit-auto
  :demand t
  :straight t
  :diminish global-treesit-auto-mode

  :init
  (setq qemacs/treesit-language-sources
        (list
         '(bash        . ("https://github.com/tree-sitter/tree-sitter-bash"))
         '(c           . ("https://github.com/tree-sitter/tree-sitter-c"))
         '(cpp         . ("https://github.com/tree-sitter/tree-sitter-cpp"))
         '(objc        . ("https://github.com/jiyee/tree-sitter-objc"))
         '(c-sharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
         ;; '(cmake       . ("https://github.com/uyha/tree-sitter-cmake"))
         '(css         . ("https://github.com/tree-sitter/tree-sitter-css"))
         '(scss        . ("https://github.com/serenadeai/tree-sitter-scss"))
         '(dockerfile  . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
         '(elisp       . ("https://github.com/Wilfred/tree-sitter-elisp"))
         '(javascript  . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
         '(jsdoc       . ("https://github.com/tree-sitter/tree-sitter-jsdoc" "master" "src"))
         '(json        . ("https://github.com/tree-sitter/tree-sitter-json"))
         '(lua         . ("https://github.com/Azganoth/tree-sitter-lua"))
         ;; '(make        . ("https://github.com/alemuller/tree-sitter-make"))
         '(markdown    . ("https://github.com/ikatyang/tree-sitter-markdown"))
         '(python      . ("https://github.com/tree-sitter/tree-sitter-python"))
         '(ruby        . ("https://github.com/tree-sitter/tree-sitter-ruby"))
         '(rust        . ("https://github.com/tree-sitter/tree-sitter-rust"))
         '(toml        . ("https://github.com/tree-sitter/tree-sitter-toml"))
         '(tsx         . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
         '(typescript  . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
         '(yaml        . ("https://github.com/ikatyang/tree-sitter-yaml"))
         )
        ) ;; end setq qemacs/treesit-language-sources


  :mode
  (
   ("\\.tsx\\'"         . tsx-ts-mode)
   ("\\.js\\'"          . typescript-ts-mode)
   ("\\.mjs\\'"         . typescript-ts-mode)
   ("\\.mts\\'"         . typescript-ts-mode)
   ("\\.cjs\\'"         . typescript-ts-mode)
   ("\\.ts\\'"          . typescript-ts-mode)
   ("\\.jsx\\'"         . tsx-ts-mode)
   ("\\.json\\'"        . json-ts-mode)
   ("\\.Dockerfile\\'"  . dockerfile-ts-mode)
   ("\\.sh\\'"          . sh-mode)
   ("\\.yml\\'"         . yaml-ts-mode)
   ("\\.toml\\'"        . toml-ts-mode)
   ("\\.conf\\'"        . toml-ts-mode)
   ("\\.c\\'"           . c-ts-mode)
   ("\\.h\\'"           . c-ts-mode)
   ("\\.cpp\\'"         . cpp-ts-mode)
   ("\\.m\\'"           . objc-ts-mode)
   ("\\.cs\\'"          . c-sharp-ts-mode)
   ("\\.sql\\'"         . sql-mode)
   ("\\.py\\'"          . python-mode)
   )

  :custom
  (treesit-auto-install nil)

  :config
  (unless (qemacs-startup-answer-was-yes "use-go-mode")
    (add-to-list 'qemacs/treesit-language-sources '(go     . ("https://github.com/tree-sitter/tree-sitter-go")) t)
    (add-to-list 'qemacs/treesit-language-sources '(gomod  . ("https://github.com/camdencheek/tree-sitter-go-mod")) t)
    (add-to-list 'qemacs/treesit-language-sources '(gowork . ("https://github.com/omertuc/tree-sitter-go-work")) t)
    ) ;; end unless use-go-mode

  (unless (qemacs-startup-answer-was-yes "use-web-mode")
    ;; php-web requires html-ts-mode and php-ts-mode
    (add-to-list
     'qemacs/treesit-language-sources
     (cons `html '("https://github.com/tree-sitter/tree-sitter-html"))
     )
    (add-to-list
     'qemacs/treesit-language-sources
     (cons `php '("https://github.com/tree-sitter/tree-sitter-php" "master" "php/src"))
     t)
    ) ;; end unless use-web-mode

  ;; (global-treesit-auto-mode)
  (dolist (grammar qemacs/treesit-language-sources)
    ;; (message "tree-sitter grammar (check): %S" (car grammar))
    (add-to-list 'treesit-language-source-alist grammar)
    ;; Only install `grammar' if we don't already have it
    ;; installed. However, if you want to *update* a grammar then
    ;; this obviously prevents that from happening.
    (unless (treesit-language-available-p (car grammar))
      (let ((inhibit-message t))
        (message "tree-sitter grammar (install): %S" (car grammar)))
      (treesit-install-language-grammar (car grammar)))
    ) ;; end dolist grammar

  (let ((remapping-mode-list
         '(
           (bash-mode        . bash-ts-mode)
           ;; (php-mode         . php-ts-mode)
           (css-mode         . css-ts-mode)
           (js2-mode         . javascript-ts-mode)
           (json-mode        . json-ts-mode)
           (js-json-mode     . json-ts-mode)
           ;; (python-mode      . python-ts-mode)
           (typescript-mode  . typescript-ts-mode)
           (javascript-mode  . javascript-ts-mode)
           (yaml-mode        . yaml-ts-mode)
           (conf-toml-mode   . toml-ts-mode)
           )))
    (unless (qemacs-startup-answer-was-yes "use-go-mode")
      (add-to-list 'remapping-mode-list '(go-mode . go-ts-mode)))
    (dolist (mapping remapping-mode-list)
      (add-to-list 'major-mode-remap-alist mapping))
    ) ;; end let remapping

  ;; fix js vs javascript
  (setq treesit-load-name-override-list
        '((js "libtree-sitter-js" "tree_sitter_javascript")))

  ) ;; end use-package treesit-auto


(provide 'qemacs-init-treesit)
;;; qemacs-init-treesit.el ends here
