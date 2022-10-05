;;; config.el -*- lexical-binding: t -*-

;; Remove that disgusting system bar
(set-frame-parameter nil 'undecorated t)

(use-package xah-fly-keys
  :straight t
  :init
  (require 'xah-fly-keys)
  (xah-fly-keys-set-layout "colemak")
  (global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate)
  (xah-fly-keys)
  (add-hook 'xah-fly-command-mode-activate-hook (lambda () (interactive) (corfu-quit)))

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

(use-package zig-mode
  :straight t)

(use-package sudo-edit
  :straight t)

(use-package markdown-mode
  :straight t)

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-flatwhite t))

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
  (dired-free-space nil)
  (dired-listing-switches "--group-directories-first --dereference -Alh"))

(use-package eglot
  :straight t
  :config
  (define-key eglot-mode-map (kbd "C-c e") 'flymake-goto-next-error)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c n") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c r") 'xref-find-references))

(use-package rust-mode
  :straight t)

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
  (vertico-posframe-width 80))

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

;; git magic
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

;; org-agenda
(use-package org-agenda
  :custom
  (org-agenda-start-on-weekday nil))

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

(use-package rainbow-delimiters
  :straight t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package csharp-mode
  :straight t
  :init
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  (add-to-list 'auto-mode-alist '("\\.cshtml\\'" . mhtml-mode)))

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

(use-package envrc
  :straight t
  :init
  (envrc-global-mode))

(use-package lsp-metals
  :straight t
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off")))

(defun sort-lines-by-length (reverse beg end)
  "Sort lines by length."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))))))

(use-package wat-mode
  :straight '(:type git :repo "https://github.com/knarkzel/wat-mode"))

;; wasm2wat
(add-hook 'find-file-hook 'wasm2wat-hook)
(defun wasm2wat-hook ()
  (when (string= (file-name-extension buffer-file-name) "wasm")
    (let ((file (make-temp-file "wasm2wat")))
      (write-region (point-min) (point-max) file)
      (delete-region (point-min) (point-max))
      (insert (shell-command-to-string (concat "wasm2wat" " " file)))
      (beginning-of-buffer)
      (wat-mode)
      (message "")
      (set-buffer-modified-p nil)
      (read-only-mode))))

(use-package olivetti
  :straight t)

(use-package ccls
  :straight t)

(provide 'init)
