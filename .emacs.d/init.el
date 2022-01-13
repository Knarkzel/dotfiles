(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

;; compile for everything
(setq comp-deferred-compilation t)

;; ignore byte-compile warnings
(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  redefine
                                  obsolete
                                  noruntime
                                  cl-functions
                                  interactive-only))

;; get rid of bell
(setq visible-bell nil
      ring-bell-function #'ignore)

;; sweet defaults
(setq-default auto-save-default nil
              auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
              backup-by-copying t
              backup-directory-alist `((".*" . ,temporary-file-directory))
              confirm-nonexistent-file-or-buffer nil
              delete-old-versions t
              dired-recursive-copies 'always
              dired-recursive-deletes 'always
              dired-clean-confirm-killing-deleted-buffers nil
              fill-column 80
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
	          use-package-always-ensure t
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

(use-package general)

(use-package evil 
  :init
  (setq evil-want-C-u-scroll t
   	    evil-want-integration t
  	    evil-want-keybinding nil)
  :config (evil-mode t))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode t))

;; dired
(setq dired-omit-files "^\\.+"
      dired-listing-switches "-AlghX")
(use-package dired-ranger)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook 'dired-omit-mode)
(evil-define-key 'normal dired-mode-map
  (kbd ".") 'dired-omit-mode
  (kbd "J") 'dired-find-file
  (kbd "K") 'dired-up-directory
  (kbd "c") 'dired-ranger-copy
  (kbd "p") 'dired-ranger-paste
  (kbd "P") 'dired-ranger-move)

(define-key evil-normal-state-map
  (kbd "g h") (lambda () (interactive) (dired "~/")))

(define-key evil-normal-state-map
  (kbd "-") (lambda () (interactive) (dired ".")))

;; lsp
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-idle-delay 0.500
        lsp-log-io nil
        lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable nil
        lsp-signature-auto-activate nil)
  :general
  (:keymaps 'prog-mode-map :states 'normal
            "g r" 'lsp-rename
            "C-a" 'lsp-execute-code-action))
(use-package flycheck)
(use-package lsp-ui
  :init
  (setq lsp-ui-doc-max-width 45
        lsp-ui-doc-max-height 20
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-delay 0.25))

;; rust
(use-package rustic
  :hook (rustic-mode . lsp-deferred))

;; electric pair
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; winner mode undo/redo
(define-key evil-normal-state-map (kbd "U") 'winner-undo)

;; elfeed
(use-package elfeed)
(evil-define-key 'normal elfeed-search-mode-map (kbd "g r") 'elfeed-update)
(setq elfeed-feeds '(("https://feeds.fireside.fm/coder/rss")
	                 ("https://lobste.rs/rss")
	                 ("https://videos.lukesmith.xyz/feeds/videos.xml")
	                 ("https://buttondown.email/j2kun/rss")
	                 ("https://www.tedinski.com/feed.xml")
	                 ("https://this-week-in-rust.org/rss.xml")))
(setq-default elfeed-search-title-max-width 100)
(setq-default elfeed-search-title-min-width 100)

(use-package selectrum
  :init (selectrum-mode +1))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (orderless-skip-highlighting (lambda () selectrum-is-active))
  (selectrum-highlight-candidates-function #'orderless-highlight-matches))

(use-package marginalia
  :init (marginalia-mode t))

(use-package which-key
  :custom (which-key-idle-delay 0.5)
  :config (which-key-mode t))

(use-package company
  :hook (prog-mode . global-company-mode)
  :custom
  (company-idle-delay 0.25)
  (company-minimum-prefix-length 1)
  (company-icon-size 0)
  (company-icon-margin 1))

(use-package magit
  :general
  (:keymaps 'magit-mode-map :states 'normal
            "-" (lambda () (interactive) (dired ".")))
  :custom (magit-refresh-status-buffer nil))

(use-package projectile
  :custom (projectile-indexing-method 'hybrid)
  :config (projectile-mode t))

;; snippets
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :init
  (use-package yasnippet-snippets)
  :general 
  (:keymaps 'yas-minor-mode-map :states 'insert
            "C-o" 'yas-expand))

;; clear eshell
(defun odd/clear ()
  (interactive)
  (let ((input (eshell-get-old-input)))
      (eshell/clear 1)
      (eshell-emit-prompt)
      (insert input)))

;; good ol' habit
(defun eshell/vim (file)
  (find-file file))

;; eshell
(use-package eshell
  :ensure nil
  :hook (eshell-mode . (lambda ()
                         (company-mode -1)))
  :init
  (setq eshell-ls-use-colors t
      eshell-cmpl-cycle-completions nil
      eshell-history-size (* 1024 8)
      eshell-hist-ignoredups t
      eshell-destroy-buffer-when-process-dies t)
  (add-to-list 'load-path "~/.emacs.d/packages")
  (require 'eshell-toggle)
  :general
  (:keymaps 'eshell-mode-map :states 'insert
            "C-l" 'odd/clear))

;; org-roam
(use-package org-roam
  :init
  (setq org-roam-v2-ack t
        org-roam-directory "~/org-roam"
        org-agenda-files (directory-files-recursively "~/org-roam" "\\.org$")
        org-roam-completion-everywhere t)
  :general
  ("C-c n l" 'org-roam-buffer-toggle)
  ("C-c n f" 'org-roam-node-find)
  ("C-c n i" 'org-roam-node-insert)
  (:keymaps 'org-mode-map :states 'normal
            "C-i" 'completion-at-point)
  :config (org-roam-setup))

;; vlang
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

(use-package v-mode
  :init
  (replace-alist-mode auto-mode-alist 'verilog-mode 'v-mode)
  (add-hook 'v-mode-hook
            (lambda () (setq after-save-hook '(odd/v-format-buffer)))))

;; theme
(use-package doom-themes
  :config
  (load-theme 'doom-material t)
  (set-face-attribute 'default nil :family "Monospace" :height 200))

;; tree sitter
(use-package tree-sitter
  :init
  (use-package tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-mode-hook 'tree-sitter-hl-mode))

;; kill this buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; paste with alt-v
(defun odd/paste ()
  (interactive)
  (evil-paste-before 1)
  (evil-forward-char 1))
(global-set-key (kbd "M-v") 'odd/paste)

;; focus
(use-package focus)

;; writeroom
(use-package writeroom-mode)

;; bind <f11> to comfy-mode
(defun comfy-mode ()
  (interactive)
  (writeroom-mode 'toggle))
(global-set-key (kbd "<f11>") 'comfy-mode)

;; eldoc 
(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-echo-area-display-truncation-message nil)
(add-function :before-until (local 'eldoc-documentation-function)         
 #'prog-mode-eldoc-function)

;; serve directory
(use-package simple-httpd)

;; vterm
(setq vterm-always-compile-module t)
(use-package vterm)

;; leader bindigns
(general-create-definer global-definer
  :keymaps 'override
  :states  '(normal)
  :prefix  "SPC")

(global-definer
  "a" 'org-agenda
  "b" 'switch-to-buffer
  "e" 'eshell-toggle
  "f" 'find-file
  "g" 'magit
  "o" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "p" 'projectile-command-map
  "x" 'execute-extended-command
  "n" 'org-roam-node-find
  "v" 'vterm)

;; C-u should be more like shell
(general-define-key
 :states 'insert
 "C-u" (lambda ()
         (interactive)
         (kill-line 0)
         (indent-according-to-mode)))

;; Binaries should be opened with hexl
(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.gb\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.ch8\\'" . hexl-mode))

;; ^ > 0
(general-define-key
 :states 'normal
 "0" 'evil-first-non-blank)

;; devkitpro
(setenv "DEVKITPRO" "/opt/devkitpro")
(setenv "DEVKITARM" "/opt/devkitpro/devkitARM")
(setenv "DEVKITPPC" "/opt/devkitpro/devkitPPC")
(setenv "CLANG_VERSION" "13.0.0")
(setenv "PATH" (concat "/opt/devkitpro/tools/bin" (getenv "PATH")))

;; css-mode
(add-hook 'css-mode-hook 'rainbow-mode)

;; move those pesky backups
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; org-agenda
(setq org-agenda-start-on-weekday nil)
