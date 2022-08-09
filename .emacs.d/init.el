;;; config.el -*- lexical-binding: t ; eval: (view-mode -1) -*-

(use-package xah-fly-keys
  :straight t
  :init
  (require 'xah-fly-keys)
  (xah-fly-keys-set-layout "colemak")
  (global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate)
  (xah-fly-keys)
  (add-hook 'xah-fly-command-mode-activate-hook (lambda () (interactive) (corfu-quit) (abort-recursive-edit)))

  ;; keybindings
  (define-key xah-fly-command-map (kbd "k") 'consult-line)
  (define-key xah-fly-command-map (kbd "E") 'odd/open-vterm)
  (define-key xah-fly-command-map (kbd "U") 'winner-undo)
  (define-key xah-fly-command-map (kbd "G") 'magit)
  (define-key xah-fly-command-map (kbd "R") 'consult-ripgrep)
  (define-key xah-fly-command-map (kbd "F") 'consult-find)

  ;; keybindings leader
  (define-key xah-fly-leader-key-map (kbd ":") 'eval-expression)
  (define-key xah-fly-leader-key-map (kbd "t") 'consult-buffer))

(use-package markdown-mode
  :straight t)

(use-package zig-mode
  :straight t)

(use-package rainbow-mode
  :straight t)

(use-package markdown-mode
  :straight t)

(use-package flycheck
  :straight t)

(use-package sudo-edit
  :straight t)

(use-package php-mode
  :straight t)

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-flatwhite t)
  ;; fix color for lsp-ui-doc
  (require 'markdown-mode)
  (set-face-background 'markdown-code-face "#f1ece4"))

(use-package dired
  :init
  (define-key dired-mode-map (kbd "i") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd ".") 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)
  (define-key global-map [mouse-3] 'dired-jump)
  :custom
  (dired-omit-files "^\\.")
  (dired-dwim-target t)
  (dired-listing-switches "--group-directories-first --dereference -Alh"))

(use-package lsp-ui
  :straight t
  :custom
  (lsp-ui-doc-max-width 45)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-delay 0.25))

(use-package lsp-mode
  :straight t
  :init
  (define-key lsp-mode-map (kbd "C-c e") 'flycheck-next-error)
  (define-key lsp-mode-map (kbd "C-c f") 'lsp-find-definition)
  (define-key lsp-mode-map (kbd "C-c n") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c a") 'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c r") 'lsp-find-references)
  (add-hook 'python-mode-hook 'lsp-deferred)
  (add-hook 'rust-mode-hook 'lsp-deferred)
  (add-hook 'zig-mode-hook 'lsp-deferred)
  (add-hook 'nix-mode-hook 'lsp-deferred)
  (add-hook 'typescript-mode-hook 'lsp-deferred)
  (add-hook 'haskell-mode-hook 'lsp-deferred)
  (add-hook 'haskell-literate-mode-hook 'lsp-deferred)
  (add-hook 'latex-mode-hook 'lsp-deferred)
  (add-hook 'scala-mode-hook 'lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 0.500)
  (lsp-log-io nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"]))

(use-package rust-mode
  :straight t
  :custom
  (rust-format-on-save nil))

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
  :init
  (marginalia-mode t))

(use-package which-key
  :straight t
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode t))

;; auto-complete
(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  (corfu-count 5)
  :config
  (global-corfu-mode))

;; more completion backends
(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package embark
  :straight t
  :init
  (global-set-key (kbd "C-.") 'embark-dwim)
  (global-set-key (kbd "M-.") 'embark-act))

(use-package avy
  :straight t
  :init
  (global-set-key (kbd "C-,") 'avy-goto-word-1)
  (global-set-key (kbd "M-,") 'avy-goto-char))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; git magic
(use-package magit
  :straight t
  :custom (magit-refresh-status-buffer nil))

(use-package magit-todos
  :straight t
  :init
  (magit-todos-mode t))

;; snippets
(use-package yasnippet
  :straight t
  :init
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand))

;; org
(use-package org
  :custom
  (org-hidden-keywords nil)
  (org-hide-emphasis-markers t)
  (org-image-actual-width (list 250))
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

;; tree sitter
(use-package tree-sitter
  :straight t
  :init
  (use-package tree-sitter-langs
    :straight t)
  (use-package tree-sitter-indent
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

(use-package rainbow-delimiters
  :straight t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package csharp-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

;; https://github.com/akermu/emacs-libvterm#vterm-enable-manipulate-selection-data-by-osc52
(use-package vterm
  :straight t
  :custom
  (vterm-always-compile-module t)
  (vterm-buffer-name-string "vterm %s")
  (vterm-timer-delay 0.01)
  :config
  (defun vterm-directory-sync (&rest _)
    "Synchronize current working directory."
    (interactive)
    (when (and vterm--process (equal major-mode 'vterm-mode))
      (let* ((pid (process-id vterm--process))
             (dir (file-truename (format "/proc/%d/cwd/" pid))))
        (setq default-directory dir))))
  (advice-add #'dired-jump :before #'vterm-directory-sync)
  (define-key vterm-mode-map (kbd "C-v") 'vterm-yank)
  (define-key vterm-mode-map (kbd "C-u") 'vterm-send-C-u))

(use-package vterm-toggle
  :straight t
  :custom
  (vterm-toggle-scope 'all)
  :config
  (define-key vterm-mode-map (kbd "<escape>") 'xah-fly-command-mode-activate)
  :hook
  (vterm-toggle-show . xah-fly-insert-mode-activate))

(defun odd/open-vterm ()
  (interactive)
  ;; if current buffer is vterm, delete its window, otherwise
  ;; find vterm buffer that matches current directory, otherwise
  ;; open new vterm buffer
  (require 'vterm-toggle)
  (if (string-match-p "vterm" (buffer-name))
      (delete-window)
    (let* ((dir (expand-file-name default-directory))
           (vterm (format "vterm %s" dir))
           (buffers (seq-filter (lambda (it) (string-match-p "vterm" (buffer-name it))) (buffer-list))))
      (while (and buffers (not (string= vterm (format "%s/" (buffer-name (car buffers))))))
        (setq buffers (cdr buffers)))
      (if (> (length buffers) 0)
          (switch-to-buffer-other-window (car buffers))
        (vterm-toggle-show)))))

(use-package dart-mode
  :straight t)

(use-package typescript-mode
  :straight t)

(use-package nix-mode
  :straight t
  :after lsp
  :init
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(use-package yaml-mode
  :straight t)

(use-package haskell-mode
  :straight t)

(use-package lsp-haskell
  :straight t)

(use-package lsp-latex
  :straight t)

(use-package lsp-python-ms
  :straight t
  :hook (python-mode . (lambda () (require 'lsp-python-ms)))
  :init
  (setq lsp-python-ms-executable (executable-find "python-language-server")))

(use-package slime
  :straight t
  :custom
  (inferior-lisp-program "sbcl"))

(use-package envrc
  :straight t
  :init
  (envrc-global-mode))

(use-package lsp-metals
  :straight t
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off")))

(provide 'init)
