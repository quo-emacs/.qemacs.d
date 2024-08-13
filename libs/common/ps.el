;;; ps.el --- list running process              -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <https://github.com/kckrinke>
;; Maintainer: Kevin C. Krinke <https://github.com/kckrinke>
;; Keywords: quo-emacs
;; Version: 0.1.1

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

;; `ps' is a package for listing the running processes on the local machine.
;;
;; Use `ps/list' to run a "ps -ax ..." sub-command and have the results parsed
;; into a list of `ps/proc' instances, optionally filtered by one or more
;; filtering functions.
;;
;; A filtering function accepts a single `ps/proc' argument and is expected to
;; return nil if the given proc should be excluded and to return non-nil to
;; allow.
;;
;; When more than one filtering function is present, they all must return
;; non-nil for any given process to be present in the return values of the
;; `ps/list' call.
;;
;; The `ps' package uses "which ps" to determine if the command is present on
;; package load.
;;
;; Only tested on Debian.  YMMV.

;;; Changelog:

;; v0.1.1:
;;   * refactored to not use `setq-local'
;;   * bugfix for truncated "exe" column of ps output
;;   * eval-and-compile require 's

;;; Code:

(eval-and-compile (require 'eieio))
(eval-and-compile (require 's))

(when (string="" (shell-command-to-string "which ps 2> /dev/null"))
  (error "The ps command was not found"))

(defclass ps/proc ()
  ((pid :initarg :pid :initform  0 :type number :documentation "Process ID")
   (cpu :initarg :cpu :initform  0 :type number :documentation "CPU Usage")
   (mem :initarg :mem :initform  0 :type number :documentation "MEM Usage")
   (uid :initarg :uid :initform  0 :type number :documentation "User ID")
   (gid :initarg :gid :initform  0 :type number :documentation "Group ID")
   (exe :initarg :exe :initform "" :type string :documentation "EXE path")
   (cmd :initarg :cmd :initform "" :type string :documentation "Command"))
  :documentation
  "A snapshot of an individual process.")

;;;###autoload
(defun ps/list (&rest filters)
  "Return a list of all running processes.

FILTERS is a list of functions accepting a single `ps/proc' instance and is
expected to return nil to exclude or non-nil to include this instance from the
returned list of processes.  All filters must return non-nil to have the
process included.

Example:
 (ps/list (lambda (proc) (> (slot-value proc :pid) 10)))
 ;; returned list should not include processes where their pid is 10 or less"
  (let* ((ps-list (shell-command-to-string "ps -ax -o 'pid=,uid=,gid=,pcpu=,pmem=,command='"))
         (these-lines (split-string ps-list "\n"))
         (this-list))
    (dolist (this-line these-lines this-list)
      (unless (string="" this-line)
        (let* ((line-parts (split-string this-line))
               (this-pid (string-to-number (pop line-parts)))
               (this-uid (string-to-number (pop line-parts)))
               (this-gid (string-to-number (pop line-parts)))
               (this-cpu (string-to-number (pop line-parts)))
               (this-mem (string-to-number (pop line-parts)))
               (this-exe (shell-command-to-string (format "ps -p %d -o 'exe='" this-pid)))
               (this-cmd (s-join " " line-parts))
               (this-proc (make-instance
                           `ps/proc
                           :pid this-pid
                           :uid this-uid
                           :gid this-gid
                           :cpu this-cpu
                           :mem this-mem
                           :exe this-exe
                           :cmd this-cmd))
               (omit-this nil))
          (dolist (this-filter filters)
            (unless (funcall this-filter this-proc)
              (setq omit-this t)))
          (unless omit-this (push this-proc this-list))
          ) ;; end let variables
        ) ;; end unless this-line
      ) ;; end dolist this-line
    ;;(setq this-list (reverse this-list))
    ) ;; end this-list
  ) ;; end ps/list

(provide 'ps)
;;; ps.el ends here
