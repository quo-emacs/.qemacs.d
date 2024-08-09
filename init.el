;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-

;;; Copyright:

;; Copyright (C) 2024 The Quo-Emacs Authors
;;
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

;; This is the main configuration file for Quo-Emacs installations.
;;
;; See: `qemacs-startup' for more information about working with the startup
;; system.

;;; Code:

;;
;; Startup Questions
;;

(require 'qemacs-common)
(require 'qemacs-startup)
(qemacs-startup-notice "Starting...")
(qemacs-startup-trace-use-package) ;; comment this out for less noisy *Messages*

;;
;; startup questions, answer caching
;;

;; Uncomment the following example to force Quo-Emacs to always prompt for
;; theming:
;;
;; (qemacs-startup/set-state "use-theming" 0)

(unless (= (qemacs-startup/get-state "startup-support-asked") 1)
  (let* ((read-answer-short t)
         (answer (read-answer "Select startup support: "
                              '(("sbtv" ?v "setup a slightly bettern than vim")
                                ("sink" ?s "setup an integrated development env")
                                ("ask"  ?a "ask startup questions (this time)")))))
    (cond
     ((string= answer "sbtv")
      (qemacs-startup/set-state "use-ts-mode"  -1)
      (qemacs-startup/set-state "use-lsp-mode" -1)
      (qemacs-startup/set-state "use-go-mode"   1)
      (qemacs-startup/set-state "use-web-mode"  1)
      (qemacs-startup/set-state "startup-support-asked" 1)
      )
     ((string= answer "sink")
      (qemacs-startup/set-state "use-ts-mode"   1)
      (qemacs-startup/set-state "use-lsp-mode"  1)
      (qemacs-startup/set-state "use-go-mode"   1)
      (qemacs-startup/set-state "use-web-mode"  1)
      (qemacs-startup/set-state "startup-support-asked" 1)
      )
     ((string= answer "ask")
      (qemacs-startup/set-state "startup-support-asked" 1)
      )
     )
    )
  )

(qemacs-startup-ask-yn
 "support-debian"
 "Support debian (y) or (n)? ")
(qemacs-startup-ask-yn
 "use-theming"
 "Use theming (y) or default (n)? ")
(qemacs-startup-ask-yn
 "use-lsp-mode"
 "Use IDE language servers? ")
(when (qemacs-startup-ask-yn
       "use-ts-mode"
       "Use tree-sitter (y) or keep it simple (n)? ")
  ;; `go-ts-mode' is only relevant when use-ts-mode
  (qemacs-startup-ask-yn
   "use-go-mode"
   "Use `go-mode' (y) or experimental `go-ts-mode' (n)? ")
  )
(qemacs-startup-ask-yn
 "use-web-mode"
 "Use `web-mode' (y) or experimental `php-web-mode' (n)? ")

(unless (qemacs-startup-answer-was-yes "use-ts-mode")
  ;; purge libtree-sitter-go.so when use-go-mode because the ts mode overrides
  ;; the go-mode which is actually requested
  (let ((lib-ts-dir (user-emacs-path "tree-sitter")))
    (when (file-directory-p lib-ts-dir)
      (delete-directory lib-ts-dir t nil))))

(when (qemacs-startup-answer-was-yes "use-go-mode")
  ;; purge libtree-sitter-go.so when use-go-mode because the ts mode overrides
  ;; the go-mode which is actually requested
  (let ((lib-ts-go-file     (user-emacs-path "tree-sitter" "libtree-sitter-go.so"))
        (lib-ts-gomod-file  (user-emacs-path "tree-sitter" "libtree-sitter-gomod.so"))
        (lib-ts-gowork-file (user-emacs-path "tree-sitter" "libtree-sitter-gowork.so")))
    (dolist (lib-ts-file (list lib-ts-go-file lib-ts-gomod-file lib-ts-gowork-file))
      (when (file-exists-p lib-ts-file) (delete-file lib-ts-file))
      )
    )
  ) ;; end when use-go-mode

;;
;; actually start setting up support systems
;;

(use-package diminish
  :demand t
  :straight t
  :config
  (diminish 'auto-revert-mode)
  (diminish 'auto-complete-mode)
  ) ;; end use-package diminish

(use-package jsonrpc :demand t :diminish "")
(use-package eglot :demand t :diminish "")
(use-package eldoc :demand t :diminish "")
(use-package project :demand t :straight t :diminish "")

;;
;; qemacs init
;;

(use-package qemacs-init-help :demand t)
(use-package qemacs-init-spelling :demand t)
(use-package qemacs-init-powerline :demand t)
(use-package qemacs-init-terminal :demand t)
(use-package qemacs-init-evil :demand t)
(use-package qemacs-init-helm :demand t)
(use-package qemacs-init-magit :demand t)
(qemacs-startup-when
 (qemacs-startup-answer-was-yes "use-ts-mode")
 `(use-package qemacs-init-treesit :demand t)
 )
(qemacs-startup-when
 (qemacs-startup-answer-was-yes "use-lsp-mode")
 `(use-package qemacs-init-modes-ide :demand t)
 )
(qemacs-startup-unless
 (qemacs-startup-answer-was-yes "use-lsp-mode")
 `(use-package qemacs-init-modes-nots :demand t)
 )
(use-package qemacs-init-modes-sbtv :demand t)
(use-package qemacs-init-modes-web :demand t)
(use-package qemacs-init-snippets :demand t)
(use-package qemacs-init-completion :demand t)
(use-package qemacs-init-checkers :demand t)
(qemacs-startup-unless
 (qemacs-is-ide)
 `(use-package qemacs-init-neotree :demand t)
 ) ;; end unless qemacs-is-ide
(qemacs-startup-when
 (qemacs-is-ide)
 `(use-package qemacs-init-treemacs :demand t)
 ) ;; end when qemacs-is-ide
(use-package qemacs-init-theming :demand t)
(qemacs-startup-when
 (qemacs-startup-answer-was-yes "support-debian")
 `(use-package qemacs-init-debian :demand t)
 )

;;
;; snippet customization
;;

;; go mode
(let ((this-mode 'go-mode))
  (unless (qemacs-startup-answer-was-yes "use-go-mode")
    (setq this-mode 'go-ts-mode))
  (yasai-add "\\.go$"          this-mode "new.go"              "new go source")
  (yasai-add "main_test\\.go$" this-mode "main_test.go"        "main package test func")
  (yasai-add "main\\.go$"      this-mode "main.go"             "main package func")
  (yasai-add "_test\\.go$"     this-mode "_test.go"            "plain go test file")
  (yasai-add "_test\\.go$"     this-mode "_test.go (. convey)" "local convey")
  (yasai-add "_test\\.go$"     this-mode "_test.go (c convey)" "alias convey")
  )

;; org mode
(yasai-add "\\.org$" 'org-mode "new.org" "new org file")

;; elisp mode
(yasai-add "\\.el$" 'emacs-lisp-mode "new-package.el" "new elisp package")

;;
;; keybindings
;;

(qemacs-startup-notice "keybindings")

;; TODO: figure out a keybinds file format to use, or otherwise abstract the
;;       keybinding process to make this stage more convenient.

;; disable help prefix so F1 can be used for other things
(setq help-char nil)
(unless (display-graphic-p)
  ;; prevent <backspace> from sending C-h on terminals
  ;; this allows the C-h help system to function correctly on terminals
  (setq normal-erase-is-backspace t))

;; general movement keys
(evil-define-key '(normal) 'global
  (kbd "<remap> <evil-next-line>")     'evil-next-visual-line
  (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line
  (kbd "<remap> <evil-next-line>")     'evil-next-visual-line
  (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line
  )
(evil-define-key '(insert normal) 'global
  (kbd "M-h") 'backward-char
  (kbd "M-j") 'next-line
  (kbd "M-k") 'previous-line
  (kbd "M-l") 'forward-char
  )
(evil-define-key '(normal motion) 'global
  (kbd "H") 'backward-char
  (kbd "J") 'next-line
  (kbd "K") 'previous-line
  (kbd "L") 'forward-char
  )

(when (fboundp 'front-matter-mode)
  (define-key front-matter-map (kbd "M-RET") 'front-matter-toggle-show-hide)
  (evil-define-key '(normal insert visual motion) 'front-matter-map
    (kbd "M-RET") 'front-matter-toggle-show-hide)
  )

;; move-text bindings
(global-set-key (kbd "C-<up>") 'move-text-up)
(global-set-key (kbd "C-<down>") 'move-text-down)

;; convenience bindings
(evil-leader/set-key "r f"      'font-lock-fontify-buffer)
(global-set-key (kbd "C-c r f") 'font-lock-fontify-buffer)

;; minibuffer escape
(define-key minibuffer-local-map            [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)

;; general escape
(global-set-key [escape] 'qemacs-escape)
;; evil escape
(evil-define-key '(normal visual motion) 'global [escape]    'qemacs-escape)
(evil-define-key '(normal visual motion) 'global (kbd "RET") 'qemacs-escape)

;; undo/redo
(evil-define-key '(normal visual motion) 'global "U" 'undo-fu-only-redo)
(global-set-key (kbd "M-_") 'undo-fu-only-undo)
(global-set-key (kbd "C-_") 'undo-fu-only-redo)

;; general key bindings
(global-set-key (kbd "<escape>") 'evil-escape)
(global-set-key (kbd "C-c F") 'yafolding-toggle-all)
(global-set-key (kbd "C-c f") 'yafolding-toggle-element)

(evil-leader/set-key "d t r" 'qemacs-del-trail-space)
(global-set-key (kbd "C-c d t r") 'qemacs-del-trail-space)
(global-set-key (kbd "M-<tab>") nil)
(global-set-key (kbd "M-v") 'evil-paste-after)

(global-set-key (kbd "M-{") 'previous-error)
(global-set-key (kbd "M-}") 'next-error)

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x f") 'helm-find-files)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key emacs-lisp-mode-map (kbd "C-c C-f") 'helm-find-files)

;; C-/ is blocked by VT100 terminals, nothing within emacs can be done, always
;; results in a C-_ event
(global-set-key (kbd "M-;") 'company-complete-common)
(evil-define-key '(emacs normal insert visual motion)
  'global
  (kbd "M-;") 'company-complete-common)

(when (fboundp 'dape)
  (message "keybinding dape-mode")
  ;; add some debugging shortcuts
  (global-set-key (kbd "C-x a b")  'dape-breakpoint-toggle)
  (global-set-key (kbd "C-x a B")  'dape-breakpoint-remove-all)
  (global-set-key (kbd "<f7>")     'dape-step-in)
  (global-set-key (kbd "S-<f7>")   'dape-step-out)
  (global-set-key (kbd "<f8>")     'dape-next)
  (global-set-key (kbd "<f9>")     'dape-continue)
  (global-set-key (kbd "C-<f10>")  'dape-maybe-kill-and-quit)
  (global-set-key (kbd "<f10>")    'dape-maybe-disconnect-and-quit)
  (global-set-key (kbd "S-<f11>")  'dape-attach-to-pid)
  (make-dape-attach-to-pid-with     dape-attach-dlv-to-pid "dlv")
  (global-set-key (kbd "<f11>")    'dape-attach-dlv-to-pid)
  (make-dape-attach-to-host-port    dape-attach-to-enjin   "127.0.0.1" 2345)
  (global-set-key (kbd "C-<f11>")  'dape-attach-to-enjin)
  )

(global-set-key (kbd "<f12>") 'ace-window)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<right>") 'windmove-right)

(global-set-key (kbd "M-$") 'flyspell-correct-at-point)
(global-set-key (kbd "<f2>") 'flyspell-correct-at-point)
(global-set-key (kbd "S-<f2>") 'flyspell-goto-next-error)
(global-set-key (kbd "C-<f2>") 'flyspell-buffer)

;; restore CTRL+z
(global-set-key (kbd "C-z") nil)
(dolist (state '(emacs visual insert normal motion replace operator))
	(evil-global-set-key state (kbd "C-z") 'suspend-frame))

;; remove evil-record-macro bindings
(evil-define-key
  '(emacs visual insert normal motion replace operator)
  'global
  (kbd "q") nil)

(global-set-key (kbd "M-?") 'comment-region)
(global-set-key (kbd "M-/") 'comment-line)
(if (fboundp 'go-ts-mode)
    (add-hook 'go-ts-mode-hook    'commentary-mode)
  (add-hook 'go-mode-hook    'commentary-mode)
  )

(global-set-key (kbd "C-c c l")   'comment-line)
(global-set-key (kbd "C-c c r")   'comment-region)
(global-set-key (kbd "C-c u c r") 'uncomment-region)

;; cheatsheet help
(global-set-key (kbd "<f1>") 'qemacs-cheatsheet-toggle)

;; these bindings end up permanently set after using `M-X customize...' once,
;; which is not what is needed.  Need to bind TAB and RET for evil normal
;; state such that when not on a help or customization screen, the bindings are
;; not present
;; (evil-define-key 'normal 'custom-mode-map
;;   (kbd "TAB")    'widget-forward
;;   (kbd "RET")    'evil-ret
;;   )

;; neotree things
(when (fboundp 'neotree-mode)
  (global-set-key [(shift   f12)] 'qemacs-neotree-project-dir)
  (global-set-key [(control f12)] 'neotree-toggle)
  (evil-define-key '(normal visual motion) neotree-mode-map
    (kbd "TAB")  'neotree-enter
    (kbd "SPC")  'neotree-quick-look
    (kbd "q")    'neotree-hide
    (kbd "RET")  'neotree-enter
    (kbd "g")    'neotree-refresh
    (kbd "n")    'neotree-next-line
    (kbd "p")    'neotree-previous-line
    (kbd "A")    'neotree-stretch-toggle
    (kbd "H")    'neotree-hidden-file-toggle
    )
  ) ;; end when neotree

;; treemacs things
(when (fboundp 'treemacs-mode)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (global-set-key (kbd "C-<f12>")   'treemacs)
  (global-set-key (kbd "S-<f12>")   'treemacs)
  (global-set-key (kbd "M-0")       'treemacs-select-window)
  ;; (global-set-key (kbd "C-x t 1")   'treemacs-delete-other-windows)
  ;; (global-set-key (kbd "C-x t t")   'treemacs)
  ;; (global-set-key (kbd "C-x t d")   'treemacs-select-directory)
  ;; (global-set-key (kbd "C-x t B")   'treemacs-bookmark)
  ;; (global-set-key (kbd "C-x t C-t") 'treemacs-find-file)
  ;; (global-set-key (kbd "C-x t M-t") 'treemacs-find-tag)
  (evil-leader/set-key
    "t t"   'treemacs
    "t 0"   'treemacs-select-window
    "t B"   'treemacs-bookmark
    "t f f" 'treemacs-find-file
    "t f t" 'treemacs-find-tag
    "t s w" 'treemacs-switch-workspace
    )
  ) ;; end when treemacs

;; yasnippet things
(define-key yas-minor-mode-map (kbd "M-'") 'yas-expand)
(define-key yas-keymap (kbd "M-j") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "M-k") 'yas-prev-field)

(when (fboundp 'lsp-mode)
  ;; lsp-mode things
  (define-key lsp-mode-map (kbd "M->") 'xref-go-back)
  (define-key lsp-mode-map (kbd "<f6>") 'lsp-find-references)
  (define-key lsp-mode-map (kbd "S-<f6>") 'lsp-rename)
  (define-key lsp-mode-map (kbd "M-.")     'lsp-find-definition)
  (define-key lsp-mode-map (kbd "M-,")     'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "C-c d t") 'lsp-ui-doc-toggle)
  (define-key lsp-mode-map (kbd "C-c d s") 'lsp-ui-doc-show)
  (define-key lsp-mode-map (kbd "C-c d g") 'lsp-ui-doc-glance)
  (evil-leader/set-key "d t" 'lsp-ui-doc-toggle)
  (evil-leader/set-key "d s" 'lsp-ui-doc-show)
  (evil-leader/set-key "d g" 'lsp-ui-doc-glance)
  (evil-define-key '(normal insert) 'global
    (kbd "M->")    'xref-go-back
    (kbd "<f6>")   'lsp-find-references
    (kbd "S-<f6>") 'lsp-rename
    (kbd "M-.")    'lsp-find-definition
    (kbd "M-,")    'lsp-describe-thing-at-point
    )
  ) ;; end when lsp-mode

;; goto-chg
(global-set-key (kbd "M-n") 'goto-last-change)
(global-set-key (kbd "M-N") 'goto-last-change-reverse)

;; window/frame cleanup helpers
(global-set-key (kbd "C-c q b") 'quit-current-buffer)

;;; init.el ends here
