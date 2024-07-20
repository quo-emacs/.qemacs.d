;;; qisses.el --- qemacs splash screen -*- lexical-binding: t; -*-

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

;; This is the qemacs splash screen.

;; Inspired by https://github.com/jsilve24/kisses

;;; Code:

;; prevent normal startup screen and messaging
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

(defface qemacs-splash-term-screen
  '((t :foreground "grey36" :weight bold))
  "Qemacs splash picture styles."
  :group 'qemacs-faces)

(defface qemacs-splash-true-color
  '((t :foreground "steelblue" :weight bold))
  "Qemacs splash picture styles."
  :group 'qemacs-faces)

(defconst qisses-qemacs-banner
  "   QQQQQQ    QQQQQQQQQQ QQQQ  QQQQQ    QQQQ       QQQQQQQ    QQQQQQQQ
  QQQQQQQQ   QQQQQQQQQQ QQQQQ QQQQQ   QQQQQQ     QQQQQQQQ   QQQQQQQQQ
 QQQQ  QQQQ  QQQ        QQQQQ QQQQQ   QQQQQQ    QQQQ       QQQQ
 QQQ    QQQ  QQQQQQQQQ  QQQQQQQQQQQ  QQQQQQQQ   QQQQ        QQQQQQ
 QQQ    QQQ  QQQQQQQQQ  QQQQQQQ QQQ  QQQ  QQQ   QQQQ         QQQQQQQQ
 QQQ    QQQ  QQQ        QQQ QQQ QQQ  QQQQQQQQ   QQQQ            QQQQQ
 QQQQ  QQQQ  QQQ        QQQ     QQQ QQQQQQQQQQ  QQQQQ      QQ     QQQ
  QQQQQQQQ   QQQQQQQQQQ QQQ     QQQ QQQ    QQQ   QQQQQQQQ  QQQQQQQQQQ
   QQQQQQ    QQQQQQQQQQ QQQ     QQQQQQQ    QQQQ    QQQQQQ   QQQQQQQQ
       QQQ                                                            "
  "QEMACS banner.")

(defconst qisses-qide-banner
  "    QQQQQQQQQQQ     QQQQQQQQQQQQQ    QQQQQQQQQQQ       QQQQQQQQQQQQQ
   QQQQQQQQQQQQQ    QQQQQQQQQQQQQ    QQQQQQQQQQQQQ    QQQQQQQQQQQQQQ
  QQQQQ    QQQQQQ       QQQQQ        QQQQ    QQQQQQ   QQQQQ
  QQQQQ     QQQQQ       QQQQQ        QQQQ     QQQQQ   QQQQQQQQQQQQQ
  QQQQQ     QQQQQ       QQQQQ        QQQQ     QQQQQQ  QQQQQQQQQQQQQ
  QQQQQ     QQQQQ       QQQQQ        QQQQ     QQQQQ   QQQQQ
  QQQQQQ   QQQQQQ       QQQQQ        QQQQ    QQQQQQ   QQQQQ
   QQQQQQQQQQQQQ    QQQQQQQQQQQQQ    QQQQQQQQQQQQQ    QQQQQQQQQQQQQQ
     QQQQQQQQQQ     QQQQQQQQQQQQQ    QQQQQQQQQQ        QQQQQQQQQQQQQ
           QQQQQ                                                      "
  "QIDE banner.")

(defvar qisses-banner
  qisses-qemacs-banner
  "Banner text to display.")

(defvar qisses-auto-quit t
  "Automatically quit qisses when `qisses-auto-quit' is non-nil.

When non-nil, qisses runs a timer to periodically check the `buffer-list' for
any buffers that do not start with zero-or-more spaces and an asterisk.
When these non-asterisk buffers exist, `qisses-quit' is called to quietly
have qisses leave the Emacs session.")

(defvar qisses-display-user t
  "On a line appended to `qisses-banner', add the USER name.")

(defvar qisses-display-host t
  "On a line appended to `qisses-banner', add the hostname.")

(defvar qisses-align-status 'center
  "Configure the alignment of the status line.

`qisses-align-status' can be one of the following: \\'left, \\'center or
\\'right.
Any other value than those results in the default \\'center alignment.")

(defvar qisses/buffer nil
  "The internal qisses display buffer (replaces *splash*).")

(defvar qisses/quit-timer nil
  "The internal qisses quit timer instance.")

;;;###autoload
(defun qisses-setup (&optional auto-quit display-user display-host)
  "Setup qisses to maybe open on startup.

AUTO-QUIT sets the `qisses-auto-quit' variable.
DISPLAY-USER sets the `qisses-display-user' variable.
DISPLAY-HOST sets the `qisses-display-host' variable.

Use t to set, \\'f to unset, and nil to do nothing."
  (cond
   ((eql auto-quit t) (setq qisses-auto-quit t))
   ((eql auto-quit 'f) (setq qisses-auto-quit nil)))
  (cond
   ((eql display-user t) (setq qisses-display-user t))
   ((eql display-user 'f) (setq qisses-display-user nil)))
  (cond
   ((eql display-host t) (setq qisses-display-host t))
   ((eql display-host 'f) (setq qisses-display-host nil)))
  (add-hook 'window-setup-hook 'qisses/maybe-splash-screen))

;;;###autoload
(defun qisses-start ()
  "Splash qisses."
  (interactive)
  (qisses/render-display))

;;;###autoload
(defun qisses-quit ()
  "Close the *splash* screen."
  (interactive)
  (if (get-buffer "*splash*")
      (kill-buffer "*splash*"))
  (remove-hook 'window-size-change-functions 'qisses/window-size-changed)
  (setq qisses/quit-timer nil)
  (setq qisses/buffer nil))

(define-derived-mode qisses/major-mode
  fundamental-mode "QISSES"
  "Major mode for displaying the qisses splash screen (`C-q' to quit)."
  (qisses/set-local-vars)
  (local-set-key (kbd "C-q") 'qisses-quit)
  (add-hook 'window-size-change-functions 'qisses/window-size-changed))

;;;###autoload
(defun qisses/visible ()
  "Return non-nil if the qisses buffer is visible."
  (cond ((eq qisses/buffer (window-buffer (selected-window))) t)
        ((get-buffer-window qisses/buffer) t)
        (t nil)))

;;;###autoload
(defun qisses/detect-buffers ()
  "Detect when there are one or more buffers that do not start with an asterisk."
  (let* ((found-buf 0))
    (dolist (buf (buffer-list))
      (let ((buf-name (buffer-name buf)))
        (unless (string-match "^[ *]" buf-name)
          (setq found-buf +1))))
    (if (> found-buf 0) t nil)))

;;;###autoload
(defun qisses/term-is-screen ()
  "Return nil if not running in a screen session."
  (if (string-match "^screen" (getenv-internal "TERM" initial-environment))
      t
    nil))

;;;###autoload
(defun qisses/set-local-vars ()
  "Internal function used to set all the local variables for the mode."
  (internal-show-cursor (selected-window) nil)
  (display-line-numbers-mode 0)
  (visual-line-mode -1)
  (setq-local auto-hscroll-mode nil)
  (setq-local hscroll-margin 0)
  (setq left-fringe-width 0)
  (setq right-fringe-width 0)
  (set-display-table-slot standard-display-table 'truncation 32)
  (set-window-buffer (selected-window) (get-buffer "*splash*"))
  (goto-char (point-min))
  (setq visible-cursor nil)
  (setq cursor-type nil)
  (internal-show-cursor (selected-window) nil)
  (setq truncate-lines t)
  (face-remap-add-relative 'region '(:inherit default))
  (if (fboundp 'evil-mode)
      (setq-local evil-normal-state-cursor nil)
    (setq-local evil-emacs-state-cursor nil)
    (setq-local cursor-type nil)))

;;;###autoload
(defun qisses/render-display ()
  "Setup the display and redraw the PREPARED-BANNER."
  (unless qisses/buffer (setq qisses/buffer (get-buffer-create "*splash*")))
  (switch-to-buffer qisses/buffer)
  (qisses/render-banner)
  (qisses/major-mode)
  (if qisses-auto-quit (qisses/quit-timer-start)))

;;;###autoload
(defun qisses/longest-line (banner-lines)
  "Return the length of the longest line in the `qisses-banner' string.

BANNER-LINES is the `qisses-banner' split into a sequence of lines."
  ;; find the longest line length
  (let ((banner-width 0))
    (dolist (line banner-lines)
      (let ((line-length (length line)))
        (if (> line-length banner-width)
            (setq banner-width line-length))))
    banner-width))

;;;###autoload
(defun qisses/format-status (width)
  "Format the optional status line.

WIDTH is the length of the longest `qisses-banner' line."
  (let ((return-value nil)) ;; default no status
    (cond
     ;; both user and host
     ((and qisses-display-user qisses-display-host)
      (setq return-value (format "%s@%s" (getenv "USER") (system-name))))
     ;; just user
     (qisses-display-user
      (setq return-value (format "%s" (getenv "USER"))))
     ;; just host
     (qisses-display-host
      (setq return-value (format "%s" (system-name)))))
    (when return-value
      (let ((size (length return-value)))
        ;; if status line is smaller than banner width, add left padding
        (if (< size width)
            (let ((padding-text ""))
              (cond
               ((eql qisses-align-status 'left))
               ((eql qisses-align-status 'right)
                (setq padding-text (make-string (- width size) ? )))
               (t ;; default 'center
                (setq padding-text (make-string (/ (- width size) 2) ? ))))
              ;; apply the padding-text prefix
              (setq return-value (concat padding-text return-value)))
          )
        )
      )
    ;; return the formatted status line
    return-value))

;;;###autoload
(defun qisses/render-banner ()
  "Internal function to redraw the PREPARED-BANNER to the current buffer."
  (read-only-mode -1)
  (erase-buffer)
  (goto-char (point-min))
  ;; prepare dimensions
  (let* ((total-width  (window-total-width))
         (total-height (window-total-height))
         (banner-lines (split-string qisses-banner "\n"))
         (banner-width (qisses/longest-line banner-lines))
         (status-line (qisses/format-status banner-width))
         (banner-height (length banner-lines))
         (top-padding (/ (- total-height banner-height) 2))
         (left-padding (/ (- total-width banner-width) 2)))
    (when (> left-padding 1)
      (setq left-padding (- left-padding 1)))
    (if status-line
        ;; add the trailing status line
        (setq banner-lines (append banner-lines (list status-line))))
    ;; apply top-padding
    (insert (make-string top-padding ?\n))
    ;; insert styled lines with left-padding
    (dolist (line banner-lines)
      (insert (make-string left-padding ? ))
      (if (qisses/term-is-screen)
          (put-text-property 0 (length line) 'face 'qemacs-splash-term-screen line)
        (put-text-property 0 (length line) 'face 'qemacs-splash-true-color line))
      (insert line)
      (insert "\n"))
    ;; move cursor to top-left
    (goto-char (point-min)))
  ;; restore read-only mode
  (read-only-mode 1))

;;;###autoload
(defun qisses/quit-timer-start ()
  "Internal function to start a quit-timer."
  (setq qisses/quit-timer
        (run-with-timer
         "0.25 sec"
         "1 sec"
         'qisses/quit-timer-fn)))

;;;###autoload
(defun qisses/quit-timer-stop ()
  "Internal function to stop a quit-timer."
  (if qisses/quit-timer (cancel-timer qisses/quit-timer)))

;;;###autoload
(defun qisses/quit-timer-fn ()
  "Internal timer callback used to quit qisses when no longer visible."
  (qisses/quit-timer-stop)
  (if qisses-auto-quit
      ;; only when auto-quit is non-nil
      (if (and (qisses/visible) (not (qisses/detect-buffers)))
          ;; setup timer again
          (qisses/quit-timer-start)
        ;; not visible or buffers detected
        (qisses-quit))))

;;;###autoload
(defun qisses/window-size-changed (this-frame)
  "Hook function for redrawing the banner on window size changed.

Accepts one parameter: THIS-FRAME."
  (let* ((this-buffer-list (frame-parameter this-frame 'buffer-list))
         (first-buffer (car this-buffer-list)))
    (when first-buffer
      (when (string= (buffer-name first-buffer) "*splash*")
        (qisses/render-banner)))))

;;;###autoload
(defun qisses/maybe-splash-screen ()
  "When all buffers start with a *, call `qissess/render-display'."
  (if (qisses/detect-buffers)
      ;; cleanup any previous splash
      (qisses-quit)
    ;; splash the qisses
    (qisses/render-display)))

(provide 'qisses)
;;; qisses.el ends here
