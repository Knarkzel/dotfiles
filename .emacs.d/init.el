(let ((file-name-handler-alist nil))
  (setq gc-cons-threshold 100000000)

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
  (global-set-key (kbd "C-x k") 'kill-buffer-and-window)

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

  ;; toggle terminals
  (use-package term-toggle
    :straight (:host github :repo "knarkzel/emacs-term-toggle"))

  (use-package markdown-mode
    :straight t)
  
  ;; theme
  (use-package doom-themes
    :straight t
    :config
    (load-theme 'doom-flatwhite t)
    ;; fix color for lsp-ui-doc
    (require 'markdown-mode)
    (set-face-background 'markdown-code-face "#f1ece4"))

  ;; superior keybindings
  (use-package xah-fly-keys
    :straight t
    :init
    (require 'xah-fly-keys)
    (xah-fly-keys-set-layout "colemak")
    (global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate)
    (xah-fly-keys)
    (add-hook 'xah-fly-command-mode-activate-hook (lambda () (interactive) (abort-recursive-edit))))

  ;; line
  (define-key xah-fly-command-map (kbd "k") 'consult-line)

  ;; coding
  (define-key xah-fly-command-map (kbd "E") 'eshell-toggle)
  (define-key xah-fly-command-map (kbd "U") 'winner-undo)
  (define-key xah-fly-command-map (kbd "G") 'magit)
  (define-key xah-fly-command-map (kbd "R") 'consult-ripgrep)
  (define-key xah-fly-command-map (kbd "F") 'consult-find)

  ;; keybindings leader
  (define-key xah-fly-leader-key-map (kbd ":") 'eval-expression)
  (define-key xah-fly-leader-key-map (kbd "t") 'consult-buffer)

  ;; dired
  (use-package dired
    :init
    (define-key dired-mode-map (kbd "i") 'wdired-change-to-wdired-mode)
    (define-key dired-mode-map (kbd ".") 'dired-omit-mode)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    :custom
    (dired-omit-files "^\\.")
    (dired-dwim-target t)
    (dired-listing-switches "--group-directories-first --dereference -Al"))

  ;; lsp
  (use-package lsp-mode
    :straight t
    :init
    (use-package flycheck
      :straight t)
    (use-package lsp-ui
      :straight t
      :custom
      (lsp-ui-doc-max-width 45)
      (lsp-ui-doc-max-height 20)
      (lsp-ui-doc-show-with-cursor t)
      (lsp-ui-doc-show-with-mouse nil)
      (lsp-ui-doc-delay 0.25))
    (define-key lsp-mode-map (kbd "C-c e") 'flycheck-next-error)
    (define-key lsp-mode-map (kbd "C-c f") 'lsp-find-definition)
    (define-key lsp-mode-map (kbd "C-c n") 'lsp-rename)
    (define-key lsp-mode-map (kbd "C-c a") 'lsp-execute-code-action)
    (define-key lsp-mode-map (kbd "C-c r") 'lsp-find-references)
    :custom
    (lsp-keymap-prefix "C-c l")
    (lsp-idle-delay 0.500)
    (lsp-log-io nil)
    (lsp-headerline-breadcrumb-enable nil)
    (lsp-lens-enable nil)
    (lsp-signature-auto-activate nil))

  ;; rust
  (use-package rust-mode
    :straight t
    :custom
    (rust-format-on-save nil)
    :init
    (add-hook 'rust-mode-hook 'lsp-deferred))

  ;; electric pair
  (add-hook 'prog-mode-hook 'electric-pair-mode)

  (use-package consult
    :straight t
    :custom
    (consult-preview-key nil)
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
    (set-face-background 'vertico-posframe-border "white")
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

  (use-package which-key
    :straight t
    :custom (which-key-idle-delay 0.5)
    :config (which-key-mode t))

  (use-package corfu
    :straight t
    :custom
    (corfu-cycle t)
    (corfu-auto t)
    (corfu-quit-no-match 'separator)
    (corfu-auto-prefix 1)
    (corfu-auto-delay 0.5)
    (corfu-count 5)
    :init (corfu-global-mode))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))

  (use-package magit
    :straight t
    :custom (magit-refresh-status-buffer nil))

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
    (defun eshell/nano (file)
      (find-file file))
    (add-to-list 'load-path "~/.emacs.d/packages")
    (require 'eshell-toggle)
    (define-key eshell-mode-map (kbd "C-l") 'odd/clear)
    :custom
    (eshell-ls-use-colors t)
    (eshell-cmpl-cycle-completions nil)
    (eshell-history-size (* 1024 8))
    (eshell-hist-ignoredups t)
    (eshell-banner-message "")
    (eshell-destroy-buffer-when-process-dies t))

  ;; org
  (use-package org
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
       ("\\.x?html?\\'" . "/usr/bin/firefox %s")
       ("\\.pdf\\'" . "/usr/bin/firefox %s"))))
    :config
    (add-hook 'org-mode-hook 'org-indent-mode)
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
    :straight t
    :custom
    (add-hook 'org-mode-hook 'org-bullets-mode))

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
    (eldoc-echo-area-display-truncation-message nil))

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

  ;; sudo
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
  (add-hook 'python-mode-hook 'lsp-deferred)

  (use-package php-mode
    :straight t)

  (use-package flymake-diagnostic-at-point
    :straight (:host github :repo "knarkzel/flymake-diagnostic-at-point")
    :after flymake
    :custom
    (flymake-diagnostic-at-point-error-prefix "")
    (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer)
    :config
    (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

  ;; tramp
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-verbose 1)

  (use-package markdown-mode
    :straight t)

  (use-package zig-mode
    :straight t
    :init
    (add-hook 'zig-mode-hook 'lsp-deferred))

  (use-package format-all
    :straight t
    :init
    (add-hook 'prog-mode-hook 'format-all-mode))

  ;; rustdesk
  (add-to-list 'auto-mode-alist '("\\.tis\\'" . javascript-mode))

  (use-package rainbow-mode
    :straight t)) 
