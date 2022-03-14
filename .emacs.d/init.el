;; straight
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(setq comp-deferred-compilation t)

(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  rdefine
                                  obsolete
                                  noruntime
                                  cl-functions
                                  interactive-only))

(setq-default default-frame-alist
              (append (list
                       '(font . "Monospace:size=28")
                       '(internal-border-width . 0)
                       '(left-fringe    . 0)
                       '(right-fringe   . 0)
                       '(tool-bar-lines . 0)
                       '(menu-bar-lines . 0)
                       '(vertical-scroll-bars . nil))))
(setq mode-line-format nil)
(setq-default mode-line-format nil)

(setq visible-bell nil
      ring-bell-function #'ignore)

(setq-default auto-save-default nil
              auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
              backup-by-copying t
              backup-directory-alist `((".*" . ,temporary-file-directory))
              confirm-nonexistent-file-or-buffer nil
              delete-old-versions t
              dired-recursive-copies 'always
              dired-recursive-deletes 'always
              dired-clean-confirm-killing-deleted-buffers nil
              fill-column 100
              undo-limit 500
              native-comp-async-report-warnings-errors nil
              gc-cons-threshold 100000000
              inhibit-compacting-font-caches t
              inhibit-startup-echo-area-message t
              make-backup-files nil
              auto-save-default nil
              jit-lock-defer-time 0
              make-backup-files nil
              read-process-output-max (* 1024 1024)
              scroll-conservatively 101
              scroll-preserve-screen-position t
              tab-width 4
              org-enforce-todo-dependencies t
              truncate-lines t
              split-width-threshold nil
              inhibit-startup-screen t
              initial-scratch-message nil
              vc-follow-symlinks t)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)

(defalias 'yes-or-no-p 'y-or-n-p)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)

(global-auto-revert-mode 1)
(display-time-mode 1)
(global-hl-line-mode 1)
(global-font-lock-mode 1)
(column-number-mode 1)
(winner-mode 1)
(recentf-mode 1)
(menu-bar-mode -1) 
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-eldoc-mode -1)
(blink-cursor-mode -1)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(put 'dired-find-alternate-file 'disabled nil)

(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.gb\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.ch8\\'" . hexl-mode))

;; sailfish
(add-to-list 'auto-mode-alist '("\\.stpl\\'" . mhtml-mode))

(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; bash
(add-hook 'sh-mode-hook 'flycheck-mode)

;; utf-8 
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-locale-environment "en_NZ.UTF-8")
(setq-default buffer-file-coding-system 'utf-8)
(when (boundp 'default-buffer-file-coding-system)
  (setq default-buffer-file-coding-system 'utf-8))
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; xclip for terminal
(custom-set-variables '(x-select-enable-clipboard t))

;; emacsclient
(load "server")
(unless (server-running-p) (server-start))

;; window divider
;; (setq window-divider-default-bottom-width 1)
;; (window-divider-mode t)

;; toggle terminals
(use-package term-toggle
  :straight (:host github :repo "knarkzel/emacs-term-toggle"))

;; theme
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-flatwhite t))

;; superior keybindings
(use-package xah-fly-keys
  :straight t
  :init
  (require 'xah-fly-keys)
  (xah-fly-keys-set-layout "colemak")
  (global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate)
  (xah-fly-keys))

;; keybindings command
(define-key xah-fly-command-map (kbd "k") 'consult-line)
(define-key xah-fly-command-map (kbd "K") 'eldoc-doc-buffer)
(define-key xah-fly-command-map (kbd "A") 'eglot-code-actions)
(define-key xah-fly-command-map (kbd "R") 'eglot-rename)
(define-key xah-fly-command-map (kbd "F") 'flymake-show-buffer-diagnostics)
(define-key xah-fly-command-map (kbd "P") 'projectile-find-file)
(define-key xah-fly-command-map (kbd "E") 'eshell-toggle)
(define-key xah-fly-command-map (kbd "T") 'term-toggle-term)
(define-key xah-fly-command-map (kbd "U") 'winner-undo)
(define-key xah-fly-command-map (kbd "G") 'magit)
(define-key xah-fly-command-map (kbd "V") 'rectangle-mark-mode)

;; keybindings leader
(define-key xah-fly-leader-key-map (kbd ":") 'eval-expression)
(define-key xah-fly-leader-key-map (kbd "t") 'consult-buffer)

;; lol
(rectangle-mark-mode)
(rectangle-mark-mode)
(define-key rectangle-mark-mode-map (kbd "I") 'string-insert-rectangle)

;; dired
(use-package dired
  :init
  (define-key dired-mode-map (kbd "i") 'wdired-change-to-wdired-mode)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . dired-omit-mode))
  :custom
  (dired-omit-files "^\\.")
  (dired-listing-switches "--group-directories-first --dereference -Al"))

(use-package all-the-icons-dired
  :straight t
  :after (all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))
  
;; eglot
(use-package eglot
  :straight t)
  
;; rust
(use-package rust-mode
  :straight t
  :hook (rust-mode . eglot-ensure)
  :custom
  (rust-format-on-save t)
  :init
  (require 'rust-mode)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

;; electric pair
(use-package electric-pair
  :hook (prog-mode . electric-pair-mode))

(use-package consult
  :straight t
  :custom
  (consult-buffer-sources '(consult--source-buffer)))

(use-package vertico
  :straight t
  :custom
  (vertico-count-format '("" . ""))
  :init
  (vertico-mode t))

(use-package vertico-posframe
  :straight t
  :init
  (vertico-posframe-mode t)
  :custom
  (vertico-posframe-border-width 1)
  (vertico-posframe-width 100))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (selectrum-highlight-candidates-function #'orderless-highlight-matches))

(use-package marginalia
  :straight t
  :init (marginalia-mode t))

(use-package all-the-icons
  :straight t
  :init
  (when (display-graphic-p)
    (require 'all-the-icons)))

(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode t))

(use-package which-key
  :straight t
  :custom (which-key-idle-delay 0.5)
  :config (which-key-mode t))

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.20)
  (corfu-count 5)
  :init (corfu-global-mode))

(use-package magit
  :straight t
  :custom (magit-refresh-status-buffer nil))

(use-package projectile
  :straight t
  :custom (projectile-indexing-method 'hybrid)
  :config (projectile-mode t))

;; snippets
(use-package yasnippet
  :straight t
  :init
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand))

;; eshell
(use-package eshell
  :init
  (defun odd/clear ()
    (interactive)
    (let ((input (eshell-get-old-input)))
      (eshell/clear 1)
      (eshell-emit-prompt)
      (insert input)))
  (defun eshell/vim (file)
    (find-file file))
  (add-to-list 'load-path "~/.emacs.d/packages")
  (require 'eshell-toggle)
  :custom
  (eshell-ls-use-colors t)
  (eshell-cmpl-cycle-completions nil)
  (eshell-history-size (* 1024 8))
  (eshell-hist-ignoredups t)
  (eshell-destroy-buffer-when-process-dies t))

;; org
(use-package org
  :hook (org-mode . org-indent-mode)
  :straight t
  :custom
  (org-hidden-keywords nil)
  (org-hide-emphasis-markers t)
  (org-image-actual-width (list 300))
  (org-return-follows-link t)
  (org-file-apps
    (quote
        ((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . "/usr/bin/chromium %s")
        ("\\.pdf\\'" . "/usr/bin/chromium %s"))))
  :config
  (set-face-attribute 'org-document-info-keyword nil
                      :foreground "#9d8f7c")
  (set-face-attribute 'org-document-info nil
                      :foreground "#9d8f7c")
  (set-face-attribute 'org-document-title nil
                      :foreground "#9d8f7c" :bold nil))

;; org-roam
(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t
        org-roam-directory "~/org-roam"
        org-agenda-files (directory-files-recursively "~/org-roam" "\\.org$")
        org-roam-completion-everywhere t)
  :config (org-roam-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :straight t)

;; vlang
(use-package v-mode
  :straight t
  :init
  (defun replace-alist-mode (alist oldmode newmode)
    (dolist (aitem alist)
      (if (eq (cdr aitem) oldmode)
          (setcdr aitem newmode))))
  (defun odd/v-format-buffer ()
    "Format the current buffer using the 'v fmt -w'."
    (interactive)
    (when (eq major-mode 'v-mode)
      (save-window-excursion
        (shell-command (concat  "v fmt -w " (buffer-file-name))))
      (revert-buffer
       :ignore-auto
       :noconfirm)))
  (replace-alist-mode auto-mode-alist 'verilog-mode 'v-mode)
  (add-hook 'v-mode-hook
            (lambda () (setq after-save-hook '(odd/v-format-buffer)))))

;; tree sitter
(use-package tree-sitter
  :straight t
  :init
  (use-package tree-sitter-langs
    :straight t)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-mode-hook 'tree-sitter-hl-mode))

;; eldoc 
(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil)
  (add-function :before-until (local 'eldoc-documentation-function) #'prog-mode-eldoc-function))

;; serve directory
(use-package simple-httpd
  :straight t)

;; org-agenda
(use-package org-agenda
  :custom
  (org-agenda-start-on-weekday nil))

;; org-download
(use-package org-download
  :straight t
  :init
  (add-hook 'dired-mode-hook 'org-download-enable)
  (add-hook 'org-mode-hook 'org-download-enable))

;; lisp
(add-hook 'elisp-lisp-mode-hook
          (lambda () (add-hook 'local-write-file-hooks 'check-parens)))

(use-package adjust-parens
  :straight t
  :init
  (add-hook 'emacs-lisp-mode-hook 'adjust-parens-mode))

;; haskell
(use-package haskell-mode
  :hook ((haskell-mode . eglot-ensure)
         (haskell-iterate-mode . eglot-ensure))
  :straight t)

(use-package sudo-edit
  :straight t)

;; auto inserts
(defun odd/org-mode-template ()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "template" 'org-mode)))

(use-package autoinsert
  :custom
  (auto-insert-query nil)
  (auto-insert-alist nil)
  :init
  (auto-insert-mode t)
  (add-hook 'find-file-hook 'auto-insert)
  (add-to-list 'auto-insert-alist '("\\.org$" . [odd/org-mode-template])))

(use-package rainbow-delimiters
  :straight t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; python
  (add-hook 'python-mode-hook 'eglot-ensure)
