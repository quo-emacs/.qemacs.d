;;; php-web.el --- emacs initialization features  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <https://github.com/kckrinke>
;; Maintainer: Kevin C. Krinke <https://github.com/kckrinke>
;; Keywords: quo-emacs
;; Version: 0.1.0
;; Package-Requires: ((polymode) (php-ts-mode) (css-mode) (html-ts-mode))

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

;; This is an experiment in providing a `web-mode'-like experience but using
;; tree-sitter things and `polymode' to integrate multiple major modes when editing
;; complicated stuff like PHP files which can include PHP code along with virtually
;; any other file format surrounding the PHP.

;; The biggest problems with this package is that the syntax highlighting system
;; is not cooperating well and so while editing various regions of a source
;; file, the highlighting can do some very strange things like treating the PHP
;; regions as if they were CSS, or just not highlighting PHP at all.

;; There are also issues with the `TAB' key functionality, resulting in the `TAB'
;; key not indenting, nor doing anything else.

;;; Code:

(require 'polymode)
(require 'polymode-core)
(require 'php-ts-mode)
(require 'css-mode)
(require 'html-ts-mode)
;; (require 'web-mode)
;; (require 'javascript-ts-mode)

(defface php-function-call-traditional
  '((t :inherit t))
  "Missing face from `php-ts-mode'."
  :group 'php-web
  )

(define-polymode php-web-mode
                 :hostmode 'php-web/html-hostmode
                 :innermodes '(
                               php-web/php-innermode
                               php-web/css-innermode
                               php-web/js-innermode
                               )
                 ) ;; end php-web-mode

(define-hostmode
 php-web/html-hostmode
 :mode 'html-ts-mode
 ;; :mode 'web-mode
 ) ;; end hostmode php-web

(define-innermode php-web/php-innermode
                  :mode 'php-ts-mode
                  :protect-font-lock t
                  :head-matcher "<[?]php"
                  :tail-matcher "[?]>"
                  :head-mode 'host
                  :tail-mode 'host
                  :body-indent-offset 2)

(define-innermode php-web/css-innermode
                  :mode 'css-mode
                  :protect-font-lock t
                  :head-matcher "<[[:space:]]*style[[:space:]]*>"
                  :tail-matcher "</[[:space:]]*style[[:space:]]*>"
                  :head-mode 'host
                  :tail-mode 'host
                  :body-indent-offset 2)

(define-innermode php-web/js-innermode
                  :mode 'js-ts-mode
                  :protect-font-lock t
                  :head-matcher "<[[:space:]]*script[[:space:]]*>"
                  :tail-matcher "</[[:space:]]*script[[:space:]]*>"
                  :head-mode 'host
                  :tail-mode 'host
                  :body-indent-offset 2)

(provide 'php-web)
;;; php-web.el ends here
