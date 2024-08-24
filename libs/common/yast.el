;;; yast.el --- yasnippet template utilities             -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <kevin@krinke.ca>
;; Maintainer: Kevin C. Krinke <kevin@krinke.ca>
;; Keywords: quo-emacs
;; Version: 0.1.1
;; Package-Requires: ((yasnippet) (uuidgen))

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

;; `yasnippet' template functions.

;;; Changelog:

;; - v0.1.1:
;;   - added yast/datestamp
;;   - added yast/timedate
;;   - added yast/uuid (requires uuidgen)

;;; Code:

(require 'yasnippet)
(require 'uuidgen)

;;;###autoload
(defun yast/find-file (yas-name mode)
  "Expand a `yasnippet' using YAS-NAME and MODE for new files."
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet yas-name mode)))

;;;###autoload
(defun yast/this-basename ()
  "Return the `current-buffer' basename (without extension)."
  (file-name-nondirectory
   (file-name-sans-extension
    (buffer-file-name)))
  ) ;; end yast/buffer-basename

;;;###autoload
(defun yast/this-file-name ()
  "Return the current buffer file name."
  (buffer-file-name (current-buffer)))

;;;###autoload
(defun yast/directory-files (directory &optional full match nosort)
  "Like `directory-files', but excluding \".\" and \"..\".

See `directory-files' for the meaning of DIRECTORY, FULL, MATCH, and NOSORT."
  (let* ((files (cons nil (directory-files directory full match nosort)))
         (parent files)
         (current (cdr files))
         (exclude (list "." ".."))
         (file nil))
    (while (and current exclude)
      (setq file (car current))
      (if (not (member file exclude))
          (setq parent current)
        (setcdr parent (cdr current))
        (setq exclude (delete file exclude)))
      (setq current (cdr current)))
    (cdr files)))

;;;###autoload
(defun yast/read-file (file)
  "Return the contents of FILE as a string."
  (let ((return-value ""))
    (with-temp-buffer
      (insert-file-contents file)
      (setq return-value (buffer-string)))
    return-value))

;;;###autoload
(defun yast/grep-go-package-names (dir)
  "Return the package names used in all *.go files within DIR.

Requires the `grep' unix command line program."
  (let* ((return-value nil)
         (grep-command
          (format
           (concat
            ;; grep package lines from DIR/*.go
            "grep -h -E \"^package \" \"%s\"/*.go 2> /dev/null"
            ;; extract just the second column
            " | awk '{print $2}'"
            ;; natural sorted unique values only
            " | sort -u -V")
           dir))
         (grep-outputs (shell-command-to-string grep-command))
         (grep-results (split-string grep-outputs "\n")))
    (let* ((grep-count (length grep-results))
           (grep-first (nth 0 grep-results)))
      (if (and (= grep-count 1) (equal grep-first ""))
          (setq return-value nil)
        (setq return-value grep-results)
        )
      )
    return-value))

;;;###autoload
(defun yast/this-go-pkg-name ()
  "Return a Go package name derived from the directory name."
  ;; TODO: figure out how to get users to select from list
  (let ((return-value nil))
    (let* ((names-found (yast/grep-go-package-names "."))
           (is-go-test  (string-match "_test\\.go$" (buffer-file-name))))
      (when (> (length names-found) 0)
        (catch 'break
          (dolist (this-name names-found)
            (if is-go-test
                (when (string-match "_test$" this-name)
                  (setq return-value this-name)
                  (throw 'break nil)
                  )
              (unless (string-match "_test$" this-name)
                (setq return-value this-name)
                (throw 'break nil)
                )
              )
            )
          ) ;; end catch break
        (unless return-value
          (setq return-value (car names-found))
          )
        )
      )
    ;; default action of the directory name's last dash-segment
    (unless return-value
      (let* ((this-file (yast/this-file-name))
             (this-path (file-name-directory this-file))
             (this-dir (directory-file-name this-path))
             (this-split (string-split this-dir "-"))
             (this-last (last this-split)))
        (setq return-value (nth 0 this-last))
        ) ;; end let variables
      ) ;; end unless return-value
    return-value)
  ) ;; end yast/this-go-pkg-name

(defun yast/datestamp (&optional time zone)
  "Return a datestamp in the format of YYYYMMDD-HHmm.

See `format-time-string` for the meaning of TIME and ZONE."
  (format-time-string "%Y%m%d-%H%M" time zone))

(defun yast/timedate (&optional time zone)
  "Return a datestamp in the format of YYYY-MM-DDTHH:mm:SSZ.

See `format-time-string` for the meaning of TIME and ZONE."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time zone))

(defun yast/uuid ()
  "Return a new UUID string in V4 format."
  (uuidgen-4))

(provide 'yast)
;;; yast.el ends here
