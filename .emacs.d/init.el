;;; config.el -*- lexical-binding: t -*-

(use-package xah-fly-keys
  :straight t
  :init
  (require 'xah-fly-keys)
  (xah-fly-keys-set-layout "colemak")
  (global-set-key (kbd "<escape>") 'xah-fly-command-mode-activate)
  (xah-fly-keys)
  (add-hook 'xah-fly-command-mode-activate-hook (lambda () (interactive) (corfu-quit)))

  ;; keybindings
  (define-key xah-fly-command-map (kbd "A") 'org-agenda)
  (define-key xah-fly-command-map (kbd "E") 'odd/open-vterm)
  (define-key xah-fly-command-map (kbd "U") 'winner-undo)
  (define-key xah-fly-command-map (kbd "G") 'magit)
  (define-key xah-fly-command-map (kbd "R") 'consult-ripgrep)
  (define-key xah-fly-command-map (kbd "F") 'consult-find)
  (define-key xah-fly-command-map (kbd "C") 'org-capture)
  (define-key xah-fly-command-map (kbd "N") 'notmuch)
  (define-key xah-fly-command-map (kbd "k") 'consult-line)
  (define-key xah-fly-command-map (kbd "P") 'project-find-file)
  (define-key xah-fly-command-map (kbd ":") 'eval-expression)
  (define-key xah-fly-command-map (kbd "5") 'split-window-right)
  
  ;; kill buffer
  (define-key global-map (kbd "C-x k") 'kill-this-buffer)  

  ;; cycle org-agenda-files
  (define-key global-map (kbd "C-'") 'org-cycle-agenda-files)

  ;; moving windows
  (define-key global-map (kbd "M-<up>") 'windmove-swap-states-up)
  (define-key global-map (kbd "M-<down>") 'windmove-swap-states-down)
  (define-key global-map (kbd "M-<left>") 'windmove-swap-states-left)
  (define-key global-map (kbd "M-<right>") 'windmove-swap-states-right)

  ;; remove bad bindings
  (global-unset-key (kbd "C-w"))
  
  ;; keybindings leader
  (define-key xah-fly-leader-key-map (kbd "t") 'consult-buffer))

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-flatwhite t))

(use-package dired
  :defer t
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . dired-omit-mode))
  :init
  (define-key dired-mode-map (kbd "i") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd ".") 'dired-omit-mode)
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

(use-package tree-sitter-langs
  :straight t)

(use-package tree-sitter-indent
  :straight t)

(use-package tree-sitter
  :straight t
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-mode-hook 'tree-sitter-hl-mode))

(use-package zig-mode
  :straight t
  :hook ((zig-mode . eglot-ensure)
         (zig-mode . (lambda () (setq-local devdocs-current-docs '("zig"))))))

(use-package rust-mode
  :straight t
  :hook ((rust-mode . eglot-ensure)
         (rust-mode . (lambda () (setq-local devdocs-current-docs '("rust"))))))

(use-package markdown-mode
  :straight t)

(use-package nix-mode
  :straight t
  :hook (nix-mode . eglot-ensure))

(use-package csharp-mode
  :straight t
  :mode (("\\.cs\\'" . csharp-mode)
         ("\\.cshtml\\'" . mhtml-mode)))

(use-package typescript-mode
  :straight t
  :hook (typescript-mode . (lambda ()
                             (electric-indent-mode -1)
                             (setq-local indent-width 4)))
  :mode ("\\.tsx\\'" . typescript-mode))

(use-package cc-mode
  :init
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure))

(use-package wat-mode
  :straight '(:type git :repo "https://github.com/knarkzel/wat-mode"))

(defun wasm2wat ()
  (interactive)
  (let ((file (make-temp-file "wasm2wat")))
    (write-region (point-min) (point-max) file)
    (delete-region (point-min) (point-max))
    (insert (shell-command-to-string (concat "wasm2wat " file)))
    (beginning-of-buffer)
    (message "")
    (set-buffer-modified-p nil)
    (read-only-mode)
    (wat-mode)))

(use-package sudo-edit
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

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-count 5)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.5)
  (corfu-quit-no-match t)
  :config
  (global-corfu-mode))

(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package magit
  :straight t
  :custom (magit-refresh-status-buffer nil))

(use-package yasnippet
  :straight t
  :init
  (yas-global-mode t)
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand))

(use-package org
  :custom
  (org-hidden-keywords nil)
  (org-hide-emphasis-markers t)
  (org-image-actual-width (list 250))
  (org-return-follows-link t)
  (org-edit-src-content-indentation 0)
  (org-html-validation-link t)
  (org-html-head-include-scripts nil)
  (org-html-head-include-default-style nil)
  (org-html-html5-fancy t)
  (org-html-doctype "html5")
  (org-html-htmlize-output-type 'inline)
  (org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "brave %s")
     ("\\.pdf\\'" . "brave %s"))))
  :config
  (set-face-attribute 'org-document-info-keyword nil
                      :foreground "#9d8f7c")
  (set-face-attribute 'org-document-info nil
                      :foreground "#9d8f7c")
  (set-face-attribute 'org-document-title nil
                      :foreground "#9d8f7c" :bold nil))

(use-package org-agenda
  :custom
  (org-agenda-start-on-weekday nil)
  (org-agenda-files '("~/source/org/work")))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

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

(use-package envrc
  :straight t
  :init
  (envrc-global-mode))

(use-package ccls
  :straight t)

(use-package emms
  :straight t
  :custom
  (emms-source-file-default-directory "~/source/quran")
  :config
  (emms-all)
  (emms-default-players))

(use-package hyperbole
  :straight t
  :config
  (hyperbole-mode))

(use-package prolog-mode
  :mode ("\\.pl\\'" . prolog-mode)
  :hook (prolog-mode . eglot-ensure))

(use-package elm-mode
  :straight t
  :hook ((elm-mode . eglot-ensure)
         (elm-mode . (lambda () (electric-indent-mode -1)))))

(use-package prettify-symbols-mode
  :hook (elisp-mode . prettify-symbols-mode)
  :config
  (defconst lisp--prettify-symbols-alist
    '(("lambda"  . ?λ))))

(use-package slime
  :straight t
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package paredit
  :hook (lisp-mode . paredit-mode)
  :straight t)

(use-package just-mode
  :straight t)

(use-package yaml-mode
  :straight t)

(use-package rainbow-mode
  :straight t)

(use-package dashboard
  :straight t
  :custom
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-items '((agenda)))
  (dashboard-week-agenda t)
  (dashboard-set-footer nil)
  (dashboard-set-init-info nil)
  (dashboard-banner-logo-title nil)
  :config
  (dashboard-setup-startup-hook))

(provide 'init)
