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

;; compile for everything
(setq comp-deferred-compilation t)

;; ignore byte-compile warnings
(setq byte-compile-warnings '(not nresolved
                                  free-vars
                                  callargs
                                  rdefine
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

;; theme
(use-package doom-themes
  :config
  (load-theme 'doom-solarized-light t))

;; clean up the frame
(use-package frame
  :config
  (setq-default default-frame-alist
                (append (list
                '(font . "Monospace:size=20")
                '(internal-border-width . 0)
                '(left-fringe    . 0)
                '(right-fringe   . 0)
                '(tool-bar-lines . 0)
                '(menu-bar-lines . 0)
                '(vertical-scroll-bars . nil))))
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t))

;; get rid of the mode-line
(add-hook 'prog-mode-hook
          (lambda ()
              (setq mode-line-format nil)
              (setq-default mode-line-format nil)))

;; general
(use-package general
  :straight t)

;; evil
(use-package evil 
  :straight t
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :init
  (evil-mode t)
  (use-package evil-collection
    :straight t
    :config (evil-collection-init))
  (use-package evil-commentary
    :straight t
    :config (evil-commentary-mode t))
  (defun odd/paste ()
    (interactive)
    (evil-paste-before 1)
    (evil-forward-char 1))
  (global-set-key (kbd "M-v") 'odd/paste)
  :general
  ("0" 'evil-first-non-blank))

;; dired
(use-package dired
  :after evil
  :init
  (use-package dired-ranger
    :straight t)
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . dired-omit-mode))
  :custom
  (dired-omit-files "^\\.+")
  (dired-listing-switches "--group-directories-first --dereference -Al")
  :config
  (evil-define-key 'normal dired-mode-map
    (kbd ".") 'dired-omit-mode
    (kbd "J") 'dired-find-file
    (kbd "K") 'dired-up-directory
    (kbd "c") 'dired-ranger-copy
    (kbd "p") 'dired-ranger-paste
    (kbd "P") 'dired-ranger-move)
  :general
  (:keymaps 'global :states 'normal
            "g h" (lambda () (interactive) (dired "~/"))
            "-" (lambda () (interactive) (dired "."))))

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
  (lsp-idle-delay 0.500)
  (lsp-log-io nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  (lsp-signature-auto-activate nil)
  :general
  (:keymaps 'prog-mode-map :states 'normal
            "g r" 'lsp-rename
            "C-a" 'lsp-execute-code-action))

;; rust
(use-package rustic
  :straight t
  :hook (rustic-mode . lsp-deferred))

;; electric pair
(use-package electric-pair
  :hook (prog-mode . electric-pair-mode))

;; winner mode undo/redo
(use-package winner-mode
  :init (winner-mode t)
  :general
  (:keymaps 'global :states 'normal
            "U" 'winner-undo))

;; elfeed
(use-package elfeed
  :straight t
  :custom
  (elfeed-feeds '(("https://feeds.fireside.fm/coder/rss")
	                 ("https://lobste.rs/rss")
	                 ("https://videos.lukesmith.xyz/feeds/videos.xml")
	                 ("https://buttondown.email/j2kun/rss")
	                 ("https://www.tedinski.com/feed.xml")
	                 ("https://this-week-in-rust.org/rss.xml")))
  (elfeed-search-title-max-width 100)
  (elfeed-search-title-min-width 100)
  :general
  (:keymaps 'elfeed-search-mode-map :states 'normal
            "g r" 'elfeed-update))

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
  :custom (which-key-idle-delay 0.5)
  :config (which-key-mode t))

(use-package company
  :straight t
  :hook (prog-mode . global-company-mode)
  :custom
  (company-idle-delay 0.25)
  (company-minimum-prefix-length 1)
  (company-icon-size 0)
  (company-icon-margin 1))

(use-package magit
  :straight t
  :general
  (:keymaps 'magit-mode-map :states 'normal
            "-" (lambda () (interactive) (dired ".")))
  :custom (magit-refresh-status-buffer nil))

(use-package projectile
  :straight t
  :custom (projectile-indexing-method 'hybrid)
  :config (projectile-mode t))

;; snippets
(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-minor-mode)
  :init
  (use-package yasnippet-snippets
    :straight t)
  :general 
  (:keymaps 'yas-minor-mode-map :states 'insert
            "C-o" 'yas-expand))

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
  :hook (eshell-mode . (lambda () (company-mode -1)))
  :custom
  (eshell-ls-use-colors t)
  (eshell-cmpl-cycle-completions nil)
  (eshell-history-size (* 1024 8))
  (eshell-hist-ignoredups t)
  (eshell-destroy-buffer-when-process-dies t)
  :general
  (:keymaps 'eshell-mode-map :states 'insert
            "C-l" 'odd/clear))

;; org-roam
(use-package org-roam
  :straight t
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

;; leader bindings
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
  "x" 'execute-extended-command)
