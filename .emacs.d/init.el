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
  (define-key xah-fly-command-map (kbd "A") 'org-roam-node-find)
  (define-key xah-fly-command-map (kbd "E") 'eshell-toggle)
  (define-key xah-fly-command-map (kbd "U") 'winner-undo)
  (define-key xah-fly-command-map (kbd "G") 'magit)
  (define-key xah-fly-command-map (kbd "R") 'consult-ripgrep)
  (define-key xah-fly-command-map (kbd "F") 'consult-find)
  (define-key xah-fly-command-map (kbd "P") 'projectile-find-file)

  ;; keybindings leader
  (define-key xah-fly-leader-key-map (kbd ":") 'eval-expression)
  (define-key xah-fly-leader-key-map (kbd "t") 'consult-buffer))

(use-package markdown-mode
  :straight t)

(use-package zig-mode
  :straight t)

(use-package rainbow-mode
  :straight t)

(use-package term-toggle
  :straight (:host github :repo "knarkzel/emacs-term-toggle"))

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
  :custom
  (dired-omit-files "^\\.")
  (dired-dwim-target t)
  (dired-listing-switches "--group-directories-first --dereference -Al"))

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
  :init
  (corfu-global-mode))

;; more completion backends
(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package embark
  :straight t
  :init
  (define-key global-map (kbd "C-.") 'embark-dwim)
  (define-key global-map (kbd "M-.") 'embark-act))

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

(use-package forge
  :straight t
  :after magit
  :custom
  (auth-sources '("~/.authinfo"))
  (markdown-max-image-size '(800 . 800)))

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
    (find-filefile))
  (add-to-list 'load-path "~/.emacs.d/packages")
  (require 'eshell-toggle)
  (define-key eshell-mode-map (kbd "C-l") 'odd/clear)
  :custom
  (eshell-ls-use-colors t)
  (eshell-cmpl-cycle-completions nil)
  (eshell-history-size (* 1024 8))
  (eshell-hist-ignoredups t)
  (eshell-banner-message "")
  (eshell-destroy-buffer-when-process-dies t)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode))))

(use-package esh-autosuggest
  :straight t
  :init
  (add-hook 'eshell-mode-hook 'esh-autosuggest-mode))

(use-package alert
  :straight t
  :init
  (alert-add-rule :status '(buried)
		          :mode 'eshell-mode
		          :style 'notifications)
  (add-hook 'eshell-kill-hook (lambda (process status) (interactive) (alert "Command finished" :title "Eshell"))))

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

;; org-download
(use-package org-download
  :straight t
  :init
  (add-hook 'dired-mode-hook 'org-download-enable)
  (add-hook 'org-mode-hook 'org-download-enable))

(use-package adjust-parens
  :straight t
  :init
  (add-hook 'emacs-lisp-mode-hook 'adjust-parens-mode))

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

(use-package flymake-diagnostic-at-point
  :straight (:host github :repo "knarkzel/flymake-diagnostic-at-point")
  :after flymake
  :custom
  (flymake-diagnostic-at-point-error-prefix "")
  (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-minibuffer)
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

(use-package format-all
  :straight t
  :init
  (add-hook 'prog-mode-hook 'format-all-mode))

(use-package undo-fu
  :straight t
  :init
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z") 'undo-fu-only-undo)
  (define-key xah-fly-command-map (kbd "j") 'undo-fu-only-undo)
  (define-key xah-fly-command-map (kbd "J") 'undo-fu-only-redo))

(use-package undo-fu-session
  :straight t
  :init
  (global-undo-fu-session-mode t))

(use-package csharp-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))
