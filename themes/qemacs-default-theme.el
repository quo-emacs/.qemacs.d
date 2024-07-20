;;; qemacs-default-theme.el --- qemacs default theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kevin C. Krinke

;; Author: Kevin C. Krinke <kevin@krinke.ca>
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; TODO:

;; * evil visual selection has no background
;; * consider selecting a very simple theme to use instead

;;; Code:

(deftheme qemacs-default
  "Slightly better than default.")

(defvar qemacs-default--general--region--bg              "#343434")
(defvar qemacs-default--graphic--region--bg              "#fffacd")
(defvar qemacs-default--general--prompt--fg              "#ffffff")
(defvar qemacs-default--graphic--prompt--fg              "#000000")
(defvar qemacs-default--general--mode-line--bg           "grey17")
(defvar qemacs-default--general--show-paren-match--bg    "black")
(defvar qemacs-default--general--show-paren-match--fg    "bright yellow")
(defvar qemacs-default--general--powerline-active--bg    "grey17")
(defvar qemacs-default--general--powerline-inactive--bg  "grey40")

(let ((prompt-fg qemacs-default--general--prompt--fg)
      (region-bg qemacs-default--general--region--bg))
  (when (display-graphic-p)
    (setq prompt-fg qemacs-default--graphic--prompt--fg)
    (setq region-bg qemacs-default--graphic--region--bg)
    )
  (custom-theme-set-faces
   'qemacs-default
   `(region              ((t :background ,region-bg)))
   '(default             ((t nil)))
   `(mode-line           ((t :background ,qemacs-default--general--mode-line--bg)))
   '(header-line         ((t nil)))
   `(minibuffer-prompt   ((t :foreground ,prompt-fg)))
   '(web-mode-block-face ((t nil)))
   `(show-paren-match    ((t :background ,qemacs-default--general--show-paren-match--bg
                             :foreground ,qemacs-default--general--show-paren-match--fg)))
   `(powerline-active0   ((t :background ,qemacs-default--general--powerline-active--bg)))
   `(powerline-active1   ((t :background ,qemacs-default--general--powerline-active--bg)))
   `(powerline-active2   ((t :background ,qemacs-default--general--powerline-active--bg)))
   `(powerline-inactive0 ((t :background ,qemacs-default--general--powerline-inactive--bg)))
   `(powerline-inactive1 ((t :background ,qemacs-default--general--powerline-inactive--bg)))
   `(powerline-inactive2 ((t :background ,qemacs-default--general--powerline-inactive--bg)))
   ) ;; end custom-theme-set-faces
  ) ;; end let variables

(provide 'qemacs-default-theme)
;;; qemacs-default-theme.el ends here
