;;; defaults.el --- provide sensible defaults  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Odd-Harald Myhren

;; Author: Odd-Harald Myhren <knarkzel@gmail.com>

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf-keywords-init)))

(setq-default auto-save-default nil
              auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
              backup-by-copying t
              backup-directory-alist `((".*" . ,temporary-file-directory))
              delete-old-versions t
              make-backup-files nil)

(setq-default inhibit-compacting-font-caches t
              inhibit-startup-echo-area-message t)

(setq-default scroll-conservatively 101
              scroll-preserve-screen-position t)

(setq-default confirm-nonexistent-file-or-buffer nil)

(setq-default fill-column 80)

(setq-default jit-lock-defer-time 0)

(setq-default read-process-output-max (* 1024 1024))

(setq-default truncate-lines t)

(setq comint-move-point-for-output nil
      comint-scroll-show-maximum-output nil)

(setq confirm-kill-emacs nil
      confirm-kill-processes nil)

(setq comp-async-report-warnings-errors nil)

(setq warning-minimum-level ':error)

(setq display-line-numbers-type 'relative)

(setq fast-but-imprecise-scrolling t)

(setq show-paren-delay 0)

(setq split-height-threshold nil
      split-width-threshold 0)

(setq tab-width 4)

(setq indent-tabs-mode nil)

(setq vc-follow-symlinks t)

(setq mouse-wheel-scroll-amount '(5))

(setq debug-on-quit t)

(global-auto-revert-mode t)
(display-time-mode t)
(global-display-line-numbers-mode t)
(global-hl-line-mode t)
(global-font-lock-mode t)
(global-eldoc-mode t)
(column-number-mode t)
(show-paren-mode t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(fset 'yes-or-no-p 'y-or-n-p)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))
