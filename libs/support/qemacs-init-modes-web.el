;;; qemacs-init-modes-web.el --- qemacs support for web-mode  -*- lexical-binding: t; -*-

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

(defun qemacs-stop-apheleia ()
  "Internal function to remove apheleia from specific modes."
  (interactive)
  (when (fboundp 'apheleia-mode)
    (apheleia-mode -1)))

;;
;; css mode (with lsp)
;;

(use-package css-mode
  :demand t
  :straight t
  :diminish ""
  :mode
  (("\\.s?css" . css-mode))
  :config
  (when (fboundp 'lsp-mode)
    (add-hook 'css-mode-hook     'lsp-deferred)
    (add-hook 'css-ts-mode-hook  'lsp-deferred)
    )
  ) ;; end use-package css-mode

;;
;; php-web mode
;;

(qemacs-startup-when
 (and (qemacs-startup-answer-was-yes "use-ts-mode")
      (not (qemacs-startup-answer-was-yes "use-web-mode")))
 ;; requires treesit html and php modes

 `(progn

    (use-package html-ts-mode
      :demand t
      :straight (html-ts-mode
                 :type git
                 :repo "https://github.com/mickeynp/html-ts-mode.git")
      :diminish ""
      ) ;; end use-package html-ts-mode

    (use-package php-ts-mode
      :demand t
      :straight (php-ts-mode
                 :type git
                 :repo "https://github.com/emacs-php/php-ts-mode.git")
      :diminish ""
      :hook ((php-ts-mode . lsp-deferred))
      :config
      (setq php-ts-mode-name "PHP[ts]")
      ) ;; end use-package php-ts-mode

    ;; php-web-mode is an experiment in using polymode to blend together
    ;; lsp-aware blocks of code within .php files, with html-ts-mode as
    ;; the hostmode and php-ts-mode, css-ts-mode and javascript-ts-mode
    ;; as the innermodes.  Overall it does work with one glaring failure,
    ;; the syntax highlighting for <?php ... ?> blocks is either not present
    ;; or mangled with whatever other mode was last active before moving the
    ;; cursor into the php block region.
    ;;
    ;; web-mode with lsp-mode seems to be good enough.

    (use-package polymode
      :demand t
      :straight (polymode
                 :type git
                 :host github
                 :repo "polymode/polymode")
      :diminish ""
      :config
      (require 'polymode-core)
      ) ;; end use-package polymode

    (use-package php-web
      :demand t
      :diminish ""
      :after (polymode html-ts-mode php-ts-mode css-mode)

      :mode
      (
       ("\\.inc\\'"         . php-ts-mode)
       ("\\.php\\'"         . php-web-mode)
       ) ;; end :mode

      :hook
      (
       (php-web-mode . qemacs-stop-apheleia)
       (javascript-ts-mode  . lsp-deferred)
       )

      );; end use-package php-web

    ) ;; end progn

 ) ;; end unless use-web-mode

;;
;; web mode
;;

(qemacs-startup-when
 (or (not (qemacs-startup-answer-was-yes "use-ts-mode"))
     (qemacs-startup-answer-was-yes "use-web-mode"))
 ;; php-web requires both not-use-web-mode and use-ts-mode
 ;; so if not-use-ts-mode, normal web-mode is the fallback

 `(progn

    (use-package web-mode
      :demand t
      :straight t
      :diminish ""

      :preface

      :mode
      (
       ("\\.[agj]sp\\'" . web-mode)
       ("\\.as[cp]x\\'" . web-mode)
       ("\\.erb\\'" . web-mode)
       ("\\.mustache\\'" . web-mode)
       ("\\.djhtml\\'" . web-mode)
       ("\\.phtml\\'" . web-mode)
       ("\\.tpl\\.php\\'" . web-mode)
       ("\\.php\\'" . web-mode)
       ("\\.html?\\'" . web-mode)
       ("\\.xml\\'" . web-mode)
       ("\\.info\\'" . web-mode)
       ("\\.module\\'" . web-mode)
       ("\\.inc\\'" . web-mode)
       )

      :config
      (when (fboundp 'lsp-mode)
        (add-hook 'web-mode-hook 'lsp-deferred)
        (add-hook 'web-mode-hook 'qemacs-stop-apheleia)
        )
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-enable-part-face t)
      (setq web-mode-enable-block-face t)
      (setq web-mode-enable-auto-pairing t)
      (setq web-mode-enable-css-colorization t)
      (setq web-mode-enable-comment-keywords t)
      (setq web-mode-enable-heredoc-fontification t)
      (setq web-mode-enable-current-element-highlight t)
      (setq web-mode-enable-current-column-highlight t)

      ) ;; end use-package web-mode

    ) ;; end progn

 ) ;; end when use-web-mode

(provide 'qemacs-init-modes-web)
;;; qemacs-init-modes-web.el ends here
