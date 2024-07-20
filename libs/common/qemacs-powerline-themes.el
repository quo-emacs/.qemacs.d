;;; qemacs-powerline-themes.el --- Kevin's Emacs Evil themes for Powerline

;; Copyright (C) 2014 Chris Johnson
;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <https://github.com/kckrinke>
;; Maintainer: Kevin C. Krinke <https://github.com/kckrinke>
;; Keywords: quo-emacs
;; Version: 0.1.0
;; Package-Requires: ((powerline))

;; This file is not part of GNU Emacs.

;; This file provides modified versions themes provided here:
;; https://github.com/johnson-christopher/powerline-evil

;; These modified themes are provided under the same license as the original
;; versions.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, version 3.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Qemacs Evil powerline themes.
;;
;; Included themes: powerline-qemacs-theme, powerline-qemacs-all-modes-theme.
;;
;; These are modified versions of the themes provided here:
;; https://github.com/johnson-christopher/powerline-evil

;;; Code:

(require 'powerline)

;;;###autoload (autoload 'qemacs-powerline-process "powerline")
(defpowerline
 qemacs-powerline-process
 (cond
  ((not mode-line-process) nil)
  ((symbolp mode-line-process)
   (symbol-value mode-line-process))
  ((listp mode-line-process)
   (format-mode-line mode-line-process))
  ((stringp mode-line-process)
   ;; truncate mode-line-process strings when >10
   (let ((total-length (length mode-line-process)))
     (if (> total-length 10)
         (let* ((trimmed (substring mode-line-process 0 10)))
           (format-mode-line trimmed))
       (format-mode-line mode-line-process))))
  (t mode-line-process)))

;;;###autoload
(defun powerline-qemacs-theme ()
  "Powerline-Evil's center-evil theme with the evil state in color, not centered."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode (powerline-raw (powerline-evil-tag) evil-face)))
                                     (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-size nil 'l)
                                     (powerline-buffer-id nil 'l)
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (powerline-narrow face1 'l)
                                     (powerline-vc face1)))
                          (rhs (list (powerline-raw global-mode-string face1 'r)
                                     (powerline-raw "%4l" face1 'r)
                                     (powerline-raw ":" face1)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1)))
                          (center (append (list (powerline-raw " " face1)
                                                (funcall separator-left face1 face2)
                                                (when (boundp 'erc-modified-channels-object)
                                                  (powerline-raw erc-modified-channels-object face2 'l))
                                                (powerline-major-mode face2 'l)
                                                (qemacs-powerline-process face2)
                                                (powerline-raw " " face2))
                                          )))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

;;;###autoload
(defun powerline-qemacs-all-modes-theme ()
  "Powerline-Evil's center-evil them with the evil state in color, not centered."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode (powerline-raw (powerline-evil-tag) evil-face)))
                                     (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-size nil 'l)
                                     (powerline-buffer-id nil 'l)
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (powerline-narrow face1 'l)
                                     (powerline-vc face1)))
                          (rhs (list (powerline-raw global-mode-string face1 'r)
                                     (powerline-raw "%4l" face1 'r)
                                     (powerline-raw ":" face1)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1)))
                          (center (append (list (powerline-raw " " face1)
                                                (funcall separator-left face1 face2)
                                                (when (boundp 'erc-modified-channels-object)
                                                  (powerline-raw erc-modified-channels-object face2 'l))
                                                (powerline-major-mode face2 'l)
                                                (powerline-process face2)
                                                (powerline-raw " " face2))
                                          (if (split-string (format-mode-line minor-mode-alist))
                                              (append (list (powerline-minor-modes face2 'l)
                                                            (powerline-raw " " face2)
                                                            (funcall separator-right face2 face1))))
                                          )))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

(provide 'qemacs-powerline-themes)
;;; qemacs-powerline-themes.el ends here
