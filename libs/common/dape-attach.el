;;; dape-attach.el --- attach dape to things              -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <https://github.com/kckrinke>
;; Maintainer: Kevin C. Krinke <https://github.com/kckrinke>
;; Keywords: quo-emacs
;; Version: 0.1.0
;; Package-Requires: ((dape "0.12.0") (dape-libs "0.1.0") (ps "0.1.0"))

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

;; Dape debugging facilities for attaching to a process ID selected by
;; the user from a list of running programs that have debug info present.
;;
;; Example binding of dlv-specific dape-attach call:
;;
;;  (make-dape-attach-with dape-attach-dlv-to "dlv")
;;  (global-set-key 'dape-attach-dlv-to)

;;; Code:

(when (version< emacs-version "29.4")
  (error "Emacs 29.4 or above required"))

(require 'ps)
(require 'dape)
(require 'helm)
(require 'dape-libs)

;;
;; User Settings
;;

;;;###autoload
(defun dape-attach/filter-files-only (proc)
  "PROC :cmd must be an absolute or relative path."
  (if (string-match "^\\(\\./\\|/\\)" (slot-value proc :cmd)) 1 nil))

;;;###autoload
(defun dape-attach/filter-debug-info (proc)
  "PROC :exe binary file command output must contain \"with debug_info\"."
  (setq-local this-out (shell-command-to-string (concat "file " (slot-value proc :exe))))
  (if (string-match "with debug_info" this-out) 1 nil))

;;;###autoload
(defun dape-attach/filter-omit-emacs (proc)
  "PROC :exe binary must not be special things like gopls."
  ;; TODO: figure out a better way of filtering out language servers
  (if (not (string-match "/\\(gopls\\)\\b" (slot-value proc :exe))) 1 nil))

(defvar dape-attach/ps-filters
  `(
    dape-attach/filter-omit-emacs
    dape-attach/filter-files-only
    dape-attach/filter-debug-info
    )
  "Sequence of functions used to filter dape-attach process listings.

All functions must return 1 for the process to be included.

Functions receive one argument, a `ps/proc' instance and must return
either non-nil to accept or nil to reject.

The following filters are included by default:

- `dape-attach/filter-omit-emacs'
- `dape-attach/filter-files-only'
- `dape-attach/filter-debug-info'")

;;
;; Attach To PID
;;

(defmacro make-dape-attach-to-pid-with (func named)
  "Create a FUNC for a `dape-attach-to-pid-with' call with the NAMED config."
  `(defun ,func ()
     "Call `dape-attach-to-pid' with a preset config."
     (interactive) (dape-attach-to-pid-with ,named)))

;;;###autoload
(defun dape-attach-to-pid (&rest filters)
  "Attach dape to a running process.

Prompts for a dape configuration name to use and then calls
`dape-attach-to-pid-with' with the selected name and any given FILTERS."
  (interactive)
  (if dape-active-mode (error (format "Dape is already connected.")))
  (let (choices) ;; prepare config selection choices
    (dolist (cfg dape-configs choices)
      (push (cons (format "%s" (car cfg)) (car cfg)) choices))
    (let ((name (dape-attach/prompt "Available dape-configs:" "Select config: " choices)))
      (if (string="" name) (error (format "No action taken.")))
      (apply 'dape-attach-to-pid-with name filters)
      ) ;; end let dape-attach/prompt
    ) ;; end choices
  ) ;; end dape-attach

;;;###autoload
(defun dape-attach-to-pid-with (named &rest filters)
  "Attach dape to a running process with NAMED config.

All FILTERS given must return non-nil in order for any given `ps/proc' to be
included in the return value.

If no filters are given, the `dape-attach/ps-filters' list is used and if that
is also empty, no filtering is done and all running local processes will be
displayed."
  (interactive)
  (if dape-active-mode (error (format "Dape is already connected.")))
  (setq-local ps-filters dape-attach/ps-filters)
  (if filters (setq-local ps-filters filters))
  (setq-local ps-list (apply 'ps/list ps-filters))
  (unless (> (length ps-list) 0)
    (error (format "No matching processes found.")))
  (let (choices) ;; prepare the user selection choices
    (dolist (this-proc ps-list choices)
      (setq this-display (format "%10d\t%3d%%\t%3d%%\t%d\t%d\t%s"
                                 (slot-value this-proc :pid)
                                 (slot-value this-proc :cpu)
                                 (slot-value this-proc :mem)
                                 (slot-value this-proc :uid)
                                 (slot-value this-proc :gid)
                                 (slot-value this-proc :cmd)))
      (push (cons this-display (slot-value this-proc :pid)) choices))
    (let ((answer
           (dape-attach/prompt
            "Attach     PID\t CPU\t MEM\t UID\t GID\tCOMMAND"
            (format "Attach %s to PID: " named)
            choices)))
      (unless answer (error (format "No action taken.")))
      ;; the answer is the entire "pid command" line, get the pid as a number
      (if (numberp answer)
          (dape-attach/connect-named-pid named answer)
        (let ((pid (string-to-number (nth 0 (split-string answer)))))
          ;; call dape with a suitable configuration
          (dape-attach/connect-named-pid named pid)
          ) ;; end pid attach
        ) ;; end numberp answer
      ) ;; end let completing-read
    ) ;; end choices
  ) ;; end dape-attach-to-pid-with

;;;###autoload
(defun dape-attach/connect-named-pid (named pid)
  "Call dape with NAMED config and request attach to local PID.

The NAMED argument can be a symbol or a string."
  (setq-local this-argv (list :request "attach" :mode "local" :processId pid))
  (setq-local this-key (dape--find-config-key named))
  (if this-key
      (dape (dape--config-eval this-key this-argv))
    (error (format "%s config not found." name))))

;;
;; Attach To Host:Port
;;

(defmacro make-dape-attach-to-port (func host)
  "Create a FUNC for a `dape-attach-to-port' call with the given HOST."
  `(defun ,func ()
     "Call `dape-attach-to-port' with a preset host."
     (interactive) (dape-attach-to-port ,host)))

(defmacro make-dape-attach-to-host-port (func host port)
  "Create a FUNC for a `dape-attach/connect-host-port' call with the given HOST and PORT."
  `(defun ,func ()
     "Call `dape-attach/connect-host-port' with a preset host and port."
     (interactive) (dape-attach/connect-host-port ,host ,port)))

(defvar dape-attach/host-list
  `(("127.0.0.1 (localhost)" . "127.0.0.1"))
  "List of preset hosts to select from during `dape-attach-to-host-port'.

List is of (DISPLAY . REAL) strings.")

(defvar dape-attach/port-list
  `(("2345 (dlv go-enjin)" . "2345"))
  "List of default ports to select from during `dape-attach-to-host-port'.

List is of (DISPLAY . REAL) strings.")

;;;###autoload
(defun dape-attach-to-host-port ()
  "Prompt for a host and call `dape-attach-to-port'.

The host must match the regular expression: ^[a-zA-Z0-9][-._a-zA-Z0-9]*$"
  (interactive)
  (if dape-active-mode (error (format "Dape is already connected.")))
  (let ((host-value
         (dape-attach/prompt
          "Attach to host"
          "Select host: "
          dape-attach/host-list
          "" "ignore"
          )))
    (if (or (not host-value) (string="" host-value))
        (error (format "No action taken."))
      (if (not (string-match "^[a-zA-Z0-9][-._a-zA-Z0-9]*$" host-value))
          (error (format "Invalid host: \"%s\"" host-value))
        (dape-attach-to-port host-value)
        ) ;; end host-value validation check
      ) ;; end host-value empty check
    ) ;; end host-value prompt
  ) ;; end dape-attach-to-host-port-with

;;;###autoload
(defun dape-attach-to-port (host)
  "Prompt for a port on HOST, and attach dape to the remote DAP instance.

The default HOST is \"127.0.0.1\".
The port must be a number within the range of 1-65534.
The HOST must satisfy the regular expression: ^[a-zA-Z0-9][-._a-zA-Z0-9]*$"
  (interactive)
  (if dape-active-mode (error (format "Dape is already connected.")))
  (setq host-value host)
  (unless host (setq host-value "127.0.0.1"))
  (if (not (string-match "^[a-zA-Z0-9][-._a-zA-Z0-9]*$" host-value))
      (error (format "Invalid host: \"%s\"" host-value)))
  (let ((port-value
         (dape-attach/prompt
          (format "Attach to port on %s" host-value)
          (format "Select %s port: " host-value)
          dape-attach/port-list
          "" "ignore"
          )))
    (if (or (not port-value) (string="" port-value))
        (error (format "No action taken."))
      (if (not (string-match "^[0-9]+$" port-value))
          (error (format "Invalid port: \"%s\"" port-value))
        (let ((port-real (string-to-number port-value)))
          (if (not (and (>= port-real 1) (<= port-real 65534)))
              (error (format "Invalid port range: \"%d\" [1-65534]" port-real))
            (dape-attach/connect-host-port host-value port-real)
            ) ;; end port-real range check
          ) ;; end port-real conversion
        ) ;; end port-value validation check
      ) ;; end port-value empty check
    ) ;; end port-value prompt
  ) ;; end dape-attach-to-port

;;;###autoload
(defun dape-attach/connect-host-port (host port)
  "Attach dape to HOST on PORT."
  (dape (dape--config-eval
         'attach
         (list
          :request "attach"
          :mode "remote"
          'host host
          'port port))))

;;
;; General Functions
;;

(defvar dape-attach/completion-handler nil
  "Override `completing-read' for the dape-attach package.")

;;;###autoload
(defun dape-attach/default-completion-handler (heading message choices default require-match)
  "Default completion handler using `completing-read'.

See `dape-attach/prompt' for the meaning of HEADING, MESSAGE, DEFAULT, CHOICES
and REQUIRE-MATCH."
  (completing-read message choices nil require-match nil nil default))

(setq dape-attach/completion-handler `dape-attach/default-completion-handler)

;;;###autoload
(defun dape-attach/prompt (heading message choices &optional default require-match)
  "Prompt using `dape-attach/completion-handler'.

HEADING is the completion area heading (ignored by `completing-read').
MESSAGE is the prompt text to use.
DEFAULT is the default value to start with.
CHOICES is the list of completion options.

REQUIRE-MATCH specifies the response must be one of the given choices if
non-nil, and allows any input when nil."
  (funcall dape-attach/completion-handler
           heading message choices default require-match)
  ) ;; end dape-attach/prompt

(provide 'dape-attach)
;;; dape-attach.el ends here
