;;; qemacs-init-neotree.el --- qemacs neotree support  -*- lexical-binding: t; -*-

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

(use-package neotree
  :demand t
  :straight t
  :diminish ""

  :config

  (setq neo-smart-open t
        neo-theme 'arrow
        neo-cwd-line-style 'button
        neo-hide-cursor t
        neo-vc-integration '(face char)
        neo-window-fixed-size nil
        )

  ;; icons are a terminal nightmare because they need installation that is
  ;; beyond the scope of anything emacs, and then there's configuring emacs to
  ;; use that
  ;;(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  ) ;; end use-package neotree

(use-package find-file-in-project
  :demand t
  :straight t
  :diminish ""
  :after (neotree)
  :config
  (defun qemacs-neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (ffip-project-root))
          (file-name (buffer-file-name)))
      (if project-dir
          (progn
            (neotree-dir project-dir)
            (neotree-find file-name))
        (message "Could not find git project root."))))
  ) ;; end use-package find-file-in-project

(provide 'qemacs-init-neotree)
;;; qemacs-init-neotree.el ends here
