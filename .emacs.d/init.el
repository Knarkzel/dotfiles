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
(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)
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
              native-comp-async-report-warnings-errors nil
              gc-cons-threshold 100000000
              display-line-numbers-type 'relative
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
              truncate-lines t
              split-width-threshold nil
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
(global-display-line-numbers-mode 1)
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

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; theme
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-solarized-light t))

;; superior keybindings
(use-package xah-fly-keys
  :straight t
  :init
  (require 'xah-fly-keys)
  (xah-fly-keys-set-layout "colemak")
  (global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate)
  (xah-fly-keys))

;; keybindings
(define-key xah-fly-command-map (kbd "k") 'loccur-isearch)
(define-key xah-fly-command-map (kbd "E") 'eshell-toggle)
(define-key xah-fly-command-map (kbd "F") 'grep)

;; dired
(use-package dired
  :init
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . dired-omit-mode))
  :custom
  (dired-omit-files "^\\.")
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
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 1.0)
  (lsp-log-io nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  (lsp-signature-auto-activate nil))

;; rust
(use-package rustic
  :straight t
  :hook (rustic-mode . lsp-deferred))

;; electric pair
(use-package electric-pair
  :hook (prog-mode . electric-pair-mode))

;; elfeed
(use-package elfeed
  :straight t
  :custom
  (elfeed-feeds '(("https://feeds.fireside.fm/coder/rss")
	                 ("https://lobste.rs/rss")
	                 ("https://videos.lukesmith.xyz/feeds/videos.xml")
                     ("https://unixsheikh.com/feed.rss")
	                 ("https://buttondown.email/j2kun/rss")
	                 ("https://www.tedinski.com/feed.xml")
	                 ("https://this-week-in-rust.org/rss.xml")))
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 100))

(use-package selectrum
  :straight t
  :init (selectrum-mode t))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless))
  (orderless-skip-highlighting (lambda () selectrum-is-active))
  (selectrum-highlight-candidates-function #'orderless-highlight-matches))

(use-package marginalia
  :straight t
  :init (marginalia-mode t))

(use-package which-key
  :straight t
  :custom (which-key-idle-delay 1.0)
  :config (which-key-mode t))

(use-package company
  :straight t
  :hook (prog-mode . global-company-mode)
  :custom
  (company-idle-delay 1.0)
  (company-minimum-prefix-length 1)
  (company-icon-size 0)
  (company-icon-margin 1))

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
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
  (yas-global-mode t))

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
  (add-hook 'eshell-mode-hook (lambda () (interactive) (company-mode -1)))
  :custom
  (eshell-ls-use-colors t)
  (eshell-cmpl-cycle-completions nil)
  (eshell-history-size (* 1024 8))
  (eshell-hist-ignoredups t)
  (eshell-destroy-buffer-when-process-dies t))

;; org-roam
(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t
        org-roam-directory "~/org-roam"
        org-agenda-files (directory-files-recursively "~/org-roam" "\\.org$")
        org-roam-completion-everywhere t)
  :config (org-roam-setup))

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

;; css-mode
(use-package css-mode
  :hook (css-mode . rainbow-mode))

;; org-agenda
(use-package org-agenda
  :custom
  (org-agenda-start-on-weekday nil))

;; org-download
(use-package org-download
  :straight t
  :init
  (setq-default org-download-image-dir "~/downloads/images")
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
  :hook ((haskell-mode . lsp-deferred)
         (haskell-iterate-mode . lsp-deferred))
  :straight t)

(use-package lsp-haskell
  :straight t
  :config
  (setq lsp-haskell-server-path "haskell-language-server"))

(use-package loccur
  :straight t)

(use-package sudo-edit
  :straight t)

;; bash
(add-hook 'sh-mode-hook 'flycheck-mode)

;; utf-8 in terminal
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8) ;; also see `my-frame-config'
(set-keyboard-coding-system 'utf-8)
(set-locale-environment "en_NZ.UTF-8")
(setq-default buffer-file-coding-system 'utf-8)
(when (boundp 'default-buffer-file-coding-system) ;; obsolete since 23.2
  (setq default-buffer-file-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; xclip for terminal
(custom-set-variables '(x-select-enable-clipboard t))

;; emacsclient
(load "server")
(unless (server-running-p) (server-start))
