;;; front-matter.el --- front-matter content support  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 The Quo-Emacs Authors

;; Author: Kevin C. Krinke <https://github.com/kckrinke>
;; Maintainer: Kevin C. Krinke <https://github.com/kckrinke>
;; Keywords: quo-emacs
;; Version: 0.1.0
;; Package-Requires: ((qemacs-common "0.1.1"))

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

;;  `front-matter-mode'
;;  `front-matter-map'
;;  `front-matter-toggle-on-off'
;;  `front-matter-toggle-show-hide'
;;  M-x customize-group RET front-matter RET

;;; Code:

(require 'eieio)
(require 'qemacs-common)
(require 'json-mode)
(require 'yaml-mode)
(require 'conf-mode)

;; package customization group
(defgroup front-matter nil
  "Customization group for `front-matter' support settings."
  :group 'files
  :prefix 'front-matter
  :version "0.1.0")

;;
;; Internal variables
;;

(defvar front-matter/debug-internal nil
  "Internal variable to setup (messages) debugging.")

(defvar-local front-matter/started nil
  "Internal variable to indicate `front-matter' is setup for `current-buffer'.")

(defvar-local front-matter/displayed nil
  "Internal variable to track when the `front-matter' side window is present.")

;;
;; Supported Stanza Types Configuration
;;

(defclass front-matter-stanza-type nil
  (
   (key   :initarg :key
          :type string :initform ""
          :documentation "Key for this stanza type (kebab-case).")
   (name   :initarg :name
           :type string :initform ""
           :documentation "Name of this stanza type.")
   (mode   :initarg :mode
           :type symbol :initform nil
           :documentation "Major-mode for editing this stanza type.")
   (opener :initarg :opener
           :type string :initform ""
           :documentation "Opening line marker for this stanza type.")
   (closer :initarg :closer
           :type string :initform ""
           :documentation "Closing line marker for this stanza type.")
   (start  :initarg :start
           :type number :initform 0
           :documentation "Starting offset for narrowing buffers.")
   (end    :initarg :end
           :type number :initform 0
           :documentation "Ending offset for narrowing buffers. Can be negative.")
   )
  :documentation
  "Stanza type definition for `front-matter' support.")

(defvar front-matter-stanza-types-supported
  (list
   (make-instance 'front-matter-stanza-type
                  :key "toml" :name "TOML" :mode 'conf-toml-mode
                  :opener "+++" :closer "+++"
                  :start 5      :end 3)
   (make-instance 'front-matter-stanza-type
                  :key "yaml" :name "YAML" :mode 'yaml-mode
                  :opener "---" :closer "---"
                  :start 5      :end 3)
   (make-instance 'front-matter-stanza-type
                  :key "json" :name "JSON" :mode 'json-mode
                  :opener "{{{" :closer "}}}"
                  :start 3      :end 2)
   (make-instance 'front-matter-stanza-type
                  :key "json-hugo" :name "JSON (Hugo)" :mode 'json-mode
                  :opener "{" :closer "}"
                  :start 0      :end -1)
   )
  "List of `front-matter-stanza-type' instances.")

;;
;; User Settings
;;

(defcustom front-matter-auto-detect t
  "Specify `front-matter-mode' auto startup if the buffer has an existing stanza."
  :type 'boolean)

(defcustom front-matter-default-type "toml"
  "Default front-matter type to use when creating new `front-matter'.

See `front-matter-stanza-types-supported' for the list of options."
  :type (append '(choice)
                (mapcar
                 (lambda (stanza-type)
                   `(const
                     :tag ,(slot-value stanza-type :name)
                     ,(slot-value stanza-type :key)))
                 front-matter-stanza-types-supported)
                ))

(defcustom front-matter-allow-patterns nil
  "List of regular expression pattern strings.

When set, must be a `list' of regular expression pattern strings, each matched
against the absolute file path of the buffer being inspected for `front-matter'
support.
If any buffer's path does not match one of the list, that buffer is excluding
`front-matter-mode'."
  :type '(choice (const :tag "Unset" nil)
                 (repeat :tag "Pattern List" string))
  )

(defcustom front-matter-deny-patterns nil
  "List of regular expression pattern strings.

When set, must be a `list' of regular expression pattern strings, each matched
against the absolute file path of the buffer being inspected for `front-matter'
support.
If any buffer's path matches one of the list, that buffer is excluded by
`front-matter-mode'."
  :type '(choice (const :tag "Unset" nil)
                 (repeat :tag "Pattern List" string))
  )

(defcustom front-matter-extensions
  (list
   ".html.tmpl"
   ".text.tmpl"
   ".json.tmpl"
   ".njn.tmpl"
   ".org.tmpl"
   ".bb.tmpl"
   ".md.tmpl"
   ".html"
   ".text"
   ".tmpl"
   ".json"
   ".njn"
   ".org"
   ".bb"
   ".md"
   )
  "List of content file extensions to provide `front-matter' support.

The default list is of all the standard Go-Enjin content page types."
  :type '(repeat string)
  )

;;
;; Keymaps and Minor-Mode
;;

(defvar-keymap front-matter-map
  :doc "Keymap for `front-matter-mode'."
  "C-c f m o" 'front-matter-toggle-on-off
  "C-c f m t" 'front-matter-toggle-show-hide)

(define-minor-mode front-matter-mode
  "Minor mode for `front-matter' content support."
  :global nil
  :init-value nil
  :lighter nil
  :keymap front-matter-map
  ) ;; end front-matter-mode

;;
;; Interactive Functions
;;

;;;###autoload
(defun front-matter-toggle-on-off ()
  "Toggle the `front-matter' support, on or off."
  (interactive)
  (let* ((this-name (front-matter/get-buffer-name (current-buffer)))
         (this-buffer (get-buffer this-name)))
    (front-matter/toggle-on-off this-buffer)
    )
  ) ;; end front-matter-toggle-on-off

;;;###autoload
(defun front-matter-toggle-show-hide ()
  "Show/hide the `front-matter' side window buffer."
  (interactive)
  (let* ((this-name (front-matter/get-buffer-name (current-buffer)))
         (this-buffer (get-buffer this-name)))
    (front-matter/toggle-show-hide this-buffer)
    )
  ) ;; end front-matter-toggle-show-hide

;;
;; Internals
;;

(defclass front-matter/type ()
  ((parent :initarg :parent
           :type (or null buffer) :initform nil
           :documentation "Parent buffer instance for this front-matter.")
   (name   :initarg :name
           :type string :initform ""
           :documentation "Indirect buffer name for this front-matter.")
   (stanza :initarg :stanza
           :type (or null front-matter-stanza-type) :initform nil
           :documentation "Stanza type for this front-matter.")
   (end    :initarg :end
           :type number :iniform -1
           :documentation "End point of this front-matter stanza.")
   (buffer :initarg :buffer
           :type (or null buffer) :initform nil
           :documentation "Indirect side window buffer instance for this front-matter."))
  :documentation
  "Internal type returned by `front-matter/parse'.")

;;;###autoload
(defun front-matter/auto-cleanup-on-kill ()
  "Internal function to cleanup front-matter buffers when actual buffer is killed."
  (let ((this-name (buffer-name)))
    (unless (string-match ":front-matter\\*$" this-name)
      (when front-matter/started
        (front-matter/shutdown (current-buffer))
        )
      ) ;; end unless front-matter
    ) ;; end let this-name
  ) ;; end front-matter/auto-cleanup-on-kill

;;;###autoload
(defun front-matter/auto-cleanup-side-window (&rest ignored)
  "Internal function to cleanup front-matter buffers when actual buffer is buried.

Note: IGNORED is ignored."
  ;; for each buffer in the list
  (dolist (this-buffer (buffer-list))
    ;; if it's not a front-matter buffer
    (unless (buffer-visible-p this-buffer)
      (let ((this-name (buffer-name this-buffer)))
        (unless (string-match ":front-matter\\*$" this-name)
          ;; check if there is a front-matter buffer for this one
          (with-current-buffer this-buffer

            (when front-matter/displayed
              (front-matter/debug "auto-cleanup-side-window: %S" this-name)
              (front-matter/hide this-buffer))

            ) ;; end with this-buffer
          ) ;; end unless front-matter
        ) ;; end unless visible
      ) ;; end let this-name
    ) ;; end buffer-list loop
  ) ;; end front-matter/auto-cleanup-on-kill

;;;###autoload
(defun front-matter/auto-detection ()
  "Internal function for `front-matter-mode' startup auto-detection."
  (when front-matter-auto-detect
    (let ((this-name (buffer-name))
          (file-name (buffer-file-name)))
      (unless (string-match ":front-matter\\*$" this-name)
        (let ((file-error (front-matter/file-supported file-name)))
          (unless file-error
            (unless front-matter/started
              (front-matter/debug "auto-detection: %S" this-name)
              (front-matter/startup (current-buffer))
              )
            )
          )
        ) ;; end unless front-matter buffer-name
      ) ;; end let this-name
    ) ;; end when auto-detect
  ) ;; end front-matter/auto-detection

;;;###autoload
(defun front-matter/toggle-on-off (buffer)
  "Toggle the `front-matter' support for the (actual) BUFFER."
  (let* ((this-name (buffer-name buffer))
         (file-name (buffer-file-name buffer))
         (file-error (front-matter/file-supported file-name)))
    (if file-error
        (user-error file-error)
      (progn
        (front-matter/debug "toggle-on-off: %S" this-name)
        ;; is this startup or shutdown?
        (if front-matter/started
            (front-matter/shutdown buffer)
          (front-matter/startup buffer)
          )
        )
      )
    ) ;; end let variables
  ) ;; end front-matter/toggle

;;;###autoload
(defun front-matter/toggle-show-hide (buffer)
  "Toggle the `front-matter' side window for (actual) BUFFER."
  (let* ((this-name (buffer-name buffer))
         (file-name (buffer-file-name buffer))
         (file-error (front-matter/file-supported file-name)))
    (if file-error
        (user-error file-error)
      (progn
        (front-matter/debug "toggle-show-hide: %S" this-name)
        ;; is this show or hide?
        (if front-matter/displayed
            (front-matter/hide buffer)
          (front-matter/show buffer)
          )
        )
      )
    ) ;; end let variables
  ) ;; end front-matter/toggle

;;;###autoload
(defun front-matter/startup (buffer)
  "Parse (actual) BUFFER `front-matter', narrow content and setup indirect buffer."
  (front-matter/debug "startup: %S" (buffer-name buffer))
  (let* ((front-matter (front-matter/parse buffer)))
    (when front-matter
      (with-current-buffer (slot-value front-matter :buffer)
        (setq-local front-matter/started t))
      (with-current-buffer (slot-value front-matter :parent)
        (setq-local front-matter/started t))
      (front-matter/narrow-buffers front-matter)
      ) ;; end when front-matter
    ) ;; end let variables
  ) ;; end front-matter/startup

;;;###autoload
(defun front-matter/shutdown (buffer)
  "Internal function to remove the `front-matter' and restore BUFFER."
  (front-matter/debug "shutdown: %S" (buffer-name buffer))
  (let* ((front-matter (front-matter/parse buffer)))
    (when front-matter
      (with-current-buffer (slot-value front-matter :buffer)
        (setq-local front-matter/started nil))
      (with-current-buffer (slot-value front-matter :parent)
        (setq-local front-matter/started nil))
      (front-matter/restore-buffers front-matter)
      )
    ) ;; end let variables
  ) ;; end front-matter/shutdown

;;;###autoload
(defun front-matter/show (buffer)
  "Internal function for showing the side window for the BUFFER front-matter."
  (front-matter/debug "show: %S" (buffer-name buffer))
  (let* ((front-matter (front-matter/parse buffer)))
    (unless front-matter
      (setq front-matter (front-matter/create buffer))
      ;; (unless front-matter
      ;;   (message "No front-matter added."))
      )
    (when front-matter
      (let ((this-buffer (slot-value front-matter :buffer))
            (this-parent (slot-value front-matter :parent)))
        (front-matter/narrow-buffers front-matter)
        (display-buffer-in-side-window
         this-buffer
         (list `(side . top) `(dedicated . nil)))
        (select-window (get-buffer-window this-buffer))
        (with-current-buffer this-buffer
          (setq-local front-matter/displayed t)
          ;; no sane and obvious way to refresh the syntax highlighting
          ;; (unless font-lock-mode
          ;;   (font-lock-mode 1))
          (font-lock-fontify-buffer))
        (with-current-buffer this-parent
          (setq-local front-matter/displayed t)
          ;; no sane and obvious way to refresh the syntax highlighting
          ;; (font-lock-fontify-buffer)
          )
        )
      )
    ) ;; end let variables
  ) ;; end front-matter/show

;;;###autoload
(defun front-matter/hide (buffer)
  "Internal function for hiding the side window for the BUFFER front-matter."
  (front-matter/debug "hide: %S" (buffer-name buffer))
  (let* ((front-matter (front-matter/parse buffer)))
    (unless front-matter
      (error (format "front-matter: failed to parse buffer contents"))
      )
    (when front-matter
      (let* ((this-buffer (slot-value front-matter :buffer))
             (this-parent (slot-value front-matter :parent)))
        (when (and front-matter this-buffer)
          (front-matter/narrow-buffers front-matter)
          (replace-buffer-in-windows this-buffer)
          (with-current-buffer this-buffer
            (setq-local front-matter/displayed nil))
          (with-current-buffer this-parent
            (setq-local front-matter/displayed nil))
          )
        )
      )
    ) ;; end let variables
  ) ;; end front-matter/hide

;;;###autoload
(defun front-matter/create (buffer)
  "Prompt the user and add a new front-matter section to BUFFER."
  (front-matter/debug "create: %S" (buffer-name buffer))
  (let* ((return-value nil)
         (choices (append (list)
                          (mapcar
                           (lambda (stanza-type)
                             (list (slot-value stanza-type :key)))
                           front-matter-stanza-types-supported)
                          ))
         (answer (completing-read
                  (format "Add front-matter (select type):")
                  choices nil t nil nil front-matter-default-type)))
    (when answer
      (with-current-buffer buffer
        (let ((orig-pos (make-marker)))
          (set-marker orig-pos (point))
          (goto-char (point-min))

          (catch 'break
            (dolist (stanza front-matter-stanza-types-supported)
              (when (equal answer (slot-value stanza :key))
                (insert (slot-value stanza :opener)) (insert "\n")
                (insert (slot-value stanza :closer)) (insert "\n")
                (throw 'break nil)
                )
              ))

          (goto-char orig-pos)
          (set-marker orig-pos nil)
          )
        (setq return-value (front-matter/parse buffer))
        )
      )
    return-value)
  )

;;;###autoload
(defun front-matter/parse-stanza (buffer stanza)
  "Internal function to parse the given STANZA in BUFFER.

Returns a `front-matter/type' instance if present, nil otherwise."
  (with-current-buffer buffer
    (front-matter/debug "parse-stanza (widen): %S" (buffer-name buffer))
    (widen)) ;; widen before parsing
  (let* ((return-value nil)
         (this-name (buffer-name buffer))
         (content (buffer-string* buffer))
         (stanza-key    (slot-value stanza :key))
         (stanza-opener (slot-value stanza :opener))
         (stanza-closer (slot-value stanza :closer))
         (empty-stanza-pattern (concat
                                "\\\`" ;; start of buffer contents
                                "\\(" stanza-opener "\\)" "\n"
                                "\\(" stanza-closer "\\)" "\n"
                                ))
         (found-stanza-pattern (concat
                                "\\\`" ;; start of buffer contents
                                "\\(" stanza-opener "\\)" "\n"
                                "\\(.*?\\(?:\n.*\\)*?\n\\)"
                                "\\(" stanza-closer "\\)" "\n"
                                ))
         (stanza-closer-first-char (substring-no-properties stanza-closer 0 1))
         (error-stanza-pattern (concat
                                "\\\`" ;; start of buffer contents
                                "\\(" stanza-opener "\\)" "\n"
                                "\\(.*?\\(?:\n.*\\)*?[^" stanza-closer-first-char "]\\)"
                                "\\(" stanza-closer "\\)" "\n"
                                ))
         (empty-stanza-matched (string-match empty-stanza-pattern content))
         (found-stanza-matched (string-match found-stanza-pattern content))
         (error-stanza-matched (string-match error-stanza-pattern content))
         (this-name (format " *%s:front-matter*" (buffer-name buffer)))
         (this-opener "")
         (this-stanza "")
         (this-closer "")
         (this-matched nil))

    (front-matter/debug "parse-stanza (%S): %S" stanza-key this-name)
    (front-matter/debug "parse-stanza (%S) [checks]: err=%S; zero=%S; yes=%S; open=%S; close=%S"
                        stanza-key
                        error-stanza-matched empty-stanza-matched found-stanza-matched
                        this-opener this-closer)

    ;; detect missing newline before stanza closer
    (when (and (not empty-stanza-matched) (not found-stanza-matched) error-stanza-matched)
      ;; (error "Missing newline before stanza closer")
      (setq this-matched t)
      (setq this-opener (match-string 1 content))
      (setq this-stanza (match-string 2 content))
      (setq this-closer (match-string 3 content))
      (with-current-buffer buffer
        (when (not (string-match "\n\\'" this-stanza))
          ;; ensure stanza has trailing newline
          (let* ((this-point (point))
                 (open-len (length this-opener))
                 (text-len (length this-stanza))
                 )
            (goto-char (+ open-len text-len 1 1)) ;; opener newline + added newline
            (insert "\n")
            (setq this-stanza (concat this-stanza "\n"))
            (goto-char this-point)
            )
          )
        )
      )

    ;; detect empty front-matter stanza
    (when (and (not this-matched) empty-stanza-matched)
      (setq this-matched t)
      (setq this-opener (match-string 1 content))
      (setq this-closer (match-string 2 content))
      ) ;; end detect empty stanza

    ;; if not empty, detect populated front-matter stanza
    (when (and (not this-matched) (string-match found-stanza-pattern content))
      (setq this-matched t)
      (setq this-opener (match-string 1 content))
      (setq this-stanza (match-string 2 content))
      (setq this-closer (match-string 3 content))
      ) ;; end detect non-empty stanza

    (when this-matched ;; either empty or found
      (let ((this-end (+ (length this-stanza)   ;; content size
                         (length this-opener) 1 ;; plus newline
                         (length this-closer) 1 ;; plus newline
                         )))

        (front-matter/debug "parse-stanza (matched): %S" this-stanza)

        ;; validate opener and closer semantics
        (if (not (and (equal this-opener stanza-opener)
                      (equal this-closer stanza-closer)))
            (front-matter/debug "parse-stanza (opener-closer err): %S - %S" stanza-key this-name)
          (progn
            ;; validated opener and closer semantics
            (let* ((this-buffer (get-buffer this-name)))
              (front-matter/debug "parse-stanza (typed): %S - %S" stanza-key this-name)
              (unless this-buffer
                (front-matter/debug "parse-stanza (make-indirect): %S" this-name)
                (setq this-buffer (make-indirect-buffer buffer this-name nil t )))
              (setq     ;; set the return-value instance
               return-value
               (make-instance
                `front-matter/type
                :parent buffer
                :stanza stanza
                :name   this-name
                :end    this-end
                :buffer this-buffer
                )
               ) ;; end set the return-value instance
              ) ;; end let this-buffer
            ) ;; end if true opener closer
          ) ;; end when valid opener and closer semantics

        ) ;; end let this-end
      ) ;; end when this-matched

    return-value) ;; end let stanza variables
  )

;;;###autoload
(defun front-matter/parse (buffer)
  "Internal function to parse the `front-matter' section of BUFFER."
  (let ((return-value nil))
    (setq return-value
          (catch 'break
            (dolist (stanza front-matter-stanza-types-supported)
              (let ((return-value (front-matter/parse-stanza buffer stanza)))
                (if return-value
                    (throw 'break return-value)))
              )))
    return-value)
  ) ;; end front-matter/parse

;;;###autoload
(defun front-matter/file-supported (file)
  "Return an error message if the FILE is not accepted for `front-matter'.

 * FILE extension must be in the `front-matter-extensions' list.
 * When the following lists are non-nil:
   * FILE path must match at least one of `front-matter-allow-patterns'.
   * FILE path must not match any of the `front-matter-deny-patterns'."
  (when file
    (front-matter/debug "file-supported (checking): %S" file)
    (let ((return-value nil)
          (valid-check nil)
          (extensions (copy-sequence front-matter-extensions))
          )
      ;; validate extensions, these are always required
      (sort extensions 'sort-longest-to-shortest)
      (setq valid-check
            (catch 'break
              (dolist (this-extn extensions)
                (if (string-suffix-p this-extn file)
                    (throw 'break t))
                ) ;; end dolist extensions
              ) ;; end catch break
            ) ;; end set return-value
      (if (not valid-check)
          (setq return-value "Unsupported `front-matter' file extension")
        (progn
          (front-matter/debug "file-supported (valid extn): %S %S" file return-value)
          (when front-matter-allow-patterns
            (setq valid-check
                  (catch 'break
                    (dolist (this-pattern front-matter-allow-patterns)
                      (if (string-match this-pattern file)
                          (throw 'break t))
                      ) ;; end dolist allow-patterns
                    ) ;; end catch break
                  ) ;; end update return-value
            (if valid-check
                (front-matter/debug "file-supported (is-included): %S %S" file return-value)
              (setq return-value "Excluding `front-matter' file path")
              ) ;; end unless valid-check
            ) ;; end when allow-patterns
          (when (and valid-check front-matter-deny-patterns)
            (setq valid-check
                  (catch 'break
                    (dolist (this-pattern front-matter-deny-patterns)
                      (if (string-match this-pattern file)
                          (throw 'break nil))
                      ) ;; end dolist allow-patterns
                    t) ;; end catch break
                  ) ;; end update return-value
            (if valid-check
                (front-matter/debug "file-supported (not-excluded): %S %S" file return-value)
              (setq return-value "Excluded `front-matter' file path")
              ) ;; end unless valid-check
            ) ;; end when valid-check and deny-patterns
          )
        ) ;; end if not valid-check
      return-value) ;; end let variables
    ) ;; end when file
  ) ;; end front-matter/file-supported

;;;###autoload
(defun front-matter/get-buffer-name (buffer)
  "Return the actual buffer name, even if BUFFER is front-matter."
  (let ((return-value nil)
        (this-name (buffer-name buffer)))
    (if (string-match "^ \\*\\(.+?\\):front-matter\\*$" this-name)
        ;; is a front-matter buffer
        (setq return-value (match-string 1 this-name))
      ;; not a front-matter buffer
      (setq return-value this-name))
    return-value)
  ) ;; end front-matter/get-buffer-name

;;;###autoload
(defun front-matter/narrow-buffers (front-matter)
  "Initialize a FRONT-MATTER indirect buffer instance."

  (let* ((this-buffer (slot-value front-matter :buffer))
         (this-parent (slot-value front-matter :parent))
         (this-end    (slot-value front-matter :end))
         (this-stanza (slot-value front-matter :stanza))
         (stanza-mode  (slot-value this-stanza :mode))
         (stanza-start (slot-value this-stanza :start))
         (stanza-end   (slot-value this-stanza :end)))

    (if (= stanza-start 0)
        (setq stanza-start 1))

    (when this-buffer
      (with-current-buffer this-buffer
        (widen)
        (narrow-to-region stanza-start (- this-end stanza-end))
        (if stanza-mode (funcall stanza-mode))
        (font-lock-flush (point-min) (point-max))
        (front-matter-mode)
        (goto-char (point-min))
        ) ;; end with front-matter indirect buffer
      ) ;; end when this-buffer

    (when this-parent
      (with-current-buffer this-parent
        (widen)
        (narrow-to-region (+ this-end 1) (point-max))
        (font-lock-flush (point-min) (point-max))
        ) ;; end with front-matter parent buffer
      ) ;; end when this-parent

    ) ;; end let variables
  ) ;; end front-matter/narrow-buffers

;;;###autoload
(defun front-matter/restore-buffers (front-matter)
  "Internal function to restore the FRONT-MATTER parent contents.

Will also kill any existing indirect buffer."
  (let ((this-buffer (slot-value front-matter :buffer))
        (this-parent (slot-value front-matter :parent)))
    (when this-buffer
      (with-current-buffer this-buffer
        (kill-buffer)))
    (when this-parent
      (with-current-buffer this-parent
        (widen)))
    ) ;; end let variables
  ) ;; end front-matter/restore-buffers

;;;###autoload
(defun front-matter/ensure-newline ()
  "Ensure front-matter always ends with a newline with a local `before-save-hook'."
  (add-hook
   'before-save-hook
   (lambda() (front-matter/parse (current-buffer)))
   nil ;; no depth
   t   ;; local hook
   ) ;; end add-hook
  ) ;; end front-matter/ensure-newline

;;;###autoload
(defun front-matter/debug (&rest argv)
  "Internal debug logging function when `front-matter/debug-internal' is non-nil.

ARGV are passed to `message' and `format'."
  (when front-matter/debug-internal
    (let ((argc (length argv)))
      (cond
       ((> argc 1)
        (message (apply 'format argv)))
       ((= argc 1)
        (message (car argv)))
       )
      )
    )
  )

;;
;; Register Package Hooks
;;

(add-hook 'front-matter-mode-hook         'front-matter/auto-detection)
(add-hook 'front-matter-mode-hook         'front-matter/ensure-newline)
(add-hook 'kill-buffer-hook               'front-matter/auto-cleanup-on-kill)
(add-hook 'window-buffer-change-functions 'front-matter/auto-cleanup-side-window)

(provide 'front-matter)
;;; front-matter.el ends here
