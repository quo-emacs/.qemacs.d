;;; qemacs-init-magit.el --- qemacs git support  -*- lexical-binding: t; -*-

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

(use-package magit
  :demand t
  :straight t
  :diminish ""
  :config

  (setq magit-refresh-status-buffer nil)

  (use-package diff-hl
    :demand t
    :straight t
    :diminish ""
    :config
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    ) ;; end use-package diff-hl

  (use-package git-gutter
    :demand t
    :straight t
    :diminish ""
    :config
    (setq git-gutter:update-interval 1)
    (setq git-gutter:visual-line    t)
    (setq git-gutter:diff-option    "-w")
    (setq git-gutter:modified-sign  " ")
    (setq git-gutter:added-sign     " ")
    (setq git-gutter:deleted-sign   " ")
    (setq git-gutter:unchanged-sign " ")

    (if (display-graphic-p)
        (progn ;; light theme gutters
          (set-face-attribute 'git-gutter:added nil :background "lightgreen")
          (set-face-attribute 'git-gutter:deleted nil :background "brightred")
          (set-face-attribute 'git-gutter:modified nil :background "orange")
          (set-face-attribute 'git-gutter:unchanged nil :background "gray80")
          ) ;; end light theme gutters
      (progn ;; dark theme gutters
        (set-face-attribute 'git-gutter:added nil :background "darkgreen")
        (set-face-attribute 'git-gutter:deleted nil :background "brightred")
        (set-face-attribute 'git-gutter:modified nil :background "orange")
        (set-face-attribute 'git-gutter:unchanged nil :background "gray10")
        );; end dark theme gutters
      ) ;; end if display-graphic-p

    (global-git-gutter-mode +1)
    ) ;; end use-package git-gutter

  ) ;; end use-package magit

(provide 'qemacs-init-magit)
;;; qemacs-init-magit.el ends here
