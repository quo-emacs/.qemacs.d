;;; qemacs-init-spelling.el --- qemacs spell checking support  -*- lexical-binding: t; -*-

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

(use-package ispell
  :demand t
  :straight t
  :diminish ""
  :after (qemacs-common)
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--camel-case" "--run-together"))
  (setq ispell-personal-dictionary (user-emacs-path "transient" "custom-dictionary"))
  (setq ispell-local-dictionary "en")
  (setq ispell-dictionary "en")
  (setq ispell-local-dictionary-alist
        '(("en" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en") nil utf-8)))
  ) ;; end use-package ispell

(use-package flyspell
  :demand t
  :straight t
  :diminish ""
  :after (ispell)
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  ) ;; end use-package flyspell

(use-package flyspell-correct-helm
  :demand t
  :straight t
  :diminish ""
  :after (ispell flyspell helm)
  ) ;; end use-package flyspell-correct-helm


(provide 'qemacs-init-spelling)
;;; qemacs-init-spelling.el ends here
