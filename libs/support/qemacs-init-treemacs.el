;;; qemacs-init-treemacs.el --- qemacs treemacs support  -*- lexical-binding: t; -*-

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

(use-package treemacs
  :demand t
  :straight t

  :preface
  (defun qemacs-treemacs-startup-workspace ()
    "Startup with workspace named in `QEMACS_WORKSPACE'."
    (when (qemacs-is-ide)
      (let ((workspace-name (getenv "QEMACS_WORKSPACE")))
        (when (and (string-present workspace-name) (not (equal workspace-name "default")))
          (treemacs-do-switch-workspace workspace-name)
          ) ;; end when string-present
        ) ;; end let workspace-name
      ) ;; end when qemacs-is-ide
    ) ;; end qemacs-is-ide

  ;; :bind
  ;; (:map global-map
  ;;       ("M-0"       . treemacs-select-window)
  ;;       ("C-x t 1"   . treemacs-delete-other-windows)
  ;;       ("C-x t t"   . treemacs)
  ;;       ("C-x t d"   . treemacs-select-directory)
  ;;       ("C-x t B"   . treemacs-bookmark)
  ;;       ("C-x t C-t" . treemacs-find-file)
  ;;       ("C-x t M-t" . treemacs-find-tag)
  ;;       )

  :config
  (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay        0.5
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-eldoc-display                   'simple
        treemacs-file-event-delay                2000
        treemacs-file-extension-regex            treemacs-last-period-regex-value
        treemacs-file-follow-delay               0.2
        treemacs-file-name-transformer           #'identity
        treemacs-follow-after-init               t
        treemacs-expand-after-init               t
        treemacs-find-workspace-method           'find-for-file-or-pick-first
        treemacs-git-command-pipe                ""
        treemacs-goto-tag-strategy               'refetch-index
        treemacs-header-scroll-indicators        '(nil . "^^^^^^")
        treemacs-hide-dot-git-directory          t
        treemacs-indentation                     2
        treemacs-indentation-string              " "
        treemacs-is-never-other-window           nil
        treemacs-max-git-entries                 5000
        treemacs-missing-project-action          'ask
        treemacs-move-files-by-mouse-dragging    t
        treemacs-move-forward-on-expand          nil
        treemacs-no-png-images                   nil
        treemacs-no-delete-other-windows         t
        treemacs-project-follow-cleanup          nil
        treemacs-persist-file                    (expand-file-name
                                                  "transient/treemacs-persist"
                                                  user-emacs-directory)
        treemacs-position                        'left
        treemacs-read-string-input               'from-child-frame
        treemacs-recenter-distance               0.1
        treemacs-recenter-after-file-follow      nil
        treemacs-recenter-after-tag-follow       nil
        treemacs-recenter-after-project-jump     'always
        treemacs-recenter-after-project-expand   'on-distance
        treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
        treemacs-project-follow-into-home        nil
        treemacs-show-cursor                     t
        treemacs-show-hidden-files               nil
        treemacs-silent-filewatch                nil
        treemacs-silent-refresh                  nil
        treemacs-sorting                         'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes        t
        treemacs-tag-follow-cleanup              t
        treemacs-tag-follow-delay                1.5
        treemacs-text-scale                      nil
        treemacs-user-mode-line-format           nil
        treemacs-user-header-line-format         nil
        treemacs-wide-toggle-width               70
        treemacs-width                           35
        treemacs-width-increment                 1
        treemacs-width-is-initially-locked       nil
        treemacs-workspace-switch-cleanup        nil)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)

  (treemacs-follow-mode nil)
  (treemacs-filewatch-mode nil)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode nil)

  (setq aw-ignored-buffers (delete 'treemacs-mode aw-ignored-buffers))

  (use-package treemacs-evil
    :after (evil)
    :demand t
    :straight t)

  (use-package treemacs-projectile
    :after (projectile)
    :demand t
    :straight t)

  (use-package treemacs-icons-dired
    :hook (dired-mode . treemacs-icons-dired-enable-once)
    :demand t
    :straight t)

  (use-package treemacs-magit
    :after (magit)
    :demand t
    :straight t)

  ;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  ;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  ;;   :demand t
  ;;   :straight t
  ;;   :config (treemacs-set-scope-type 'Perspectives))

  (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
    ;; :after (treemacs)
    :demand t
    :straight t
    ;; :config (treemacs-set-scope-type 'Tabs)
    )

  (when (fboundp 'lsp)
    (use-package lsp-treemacs
      :demand t
      :straight t
      ;;     :config
      ;;     (lsp-treemacs-sync-mode 1)
      )
    ) ;; end when lsp

  (when (= (length command-line-args) 1)
    ;; display treemacs if no files given
    (treemacs-start-on-boot))

  ;; handle startup workspace switching
  (add-hook 'after-init-hook 'qemacs-treemacs-startup-workspace)
  ) ;; end use-package treemacs


(provide 'qemacs-init-treemacs)
;;; qemacs-init-treemacs.el ends here
