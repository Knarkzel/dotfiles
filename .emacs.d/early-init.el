;;; -*- lexical-binding: t; -*-

;; Speed
(defvar comp-deferred-compliation)
(setq comp-deferred-compilation t)

(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)

;; remove ugly early
(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; max memory available for gc on startup
(defvar me/gc-cons-threshold 16777216)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold me/gc-cons-threshold
                  gc-cons-percentage 0.1)))

;; max memory available for gc when opening minibuffer
(defun me/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun me/restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold me/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'me/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'me/restore-garbage-collection-h)
(setq garbage-collection-messages t)

;; file name handler disable
(defvar me/-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist me/-file-name-handler-alist)))

;; more speed
(setq site-run-file nil)
(setq inhibit-compacting-font-caches t)
(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

;; straight
(setq straight-cache-autoloads t
      straight-vc-git-default-clone-depth 1
      vc-follow-symlinks t)

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
(setq vc-follow-symlinks 'ask) ; restore default
(require 'straight-x)
(straight-use-package 'use-package)

(use-package esup
  :straight t
  :demand t
  :commands esup)

(use-package benchmark-init
  :demand t
  :straight (:host github :repo "kekeimiku/benchmark-init-el")
  :hook (after-init . benchmark-init/deactivate))

(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs ready in %s with %d garbage collections."
            (format
             "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time)))
            gcs-done)))

(use-package gcmh
  :straight t
  :demand t
  :config
  (gcmh-mode 1))

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
              inhibit-startup-echo-area-message t
              make-backup-files nil
              auto-save-default nil
              jit-lock-defer-time 0
              fast-but-imprecise-scrolling t
              make-backup-files nil
              scroll-conservatively 101
              scroll-preserve-screen-position t
              tab-width 4
              org-enforce-todo-dependencies t
              truncate-lines t
              split-width-threshold nil
              inhibit-startup-screen t
              initial-scratch-message nil)

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

;; electric pair
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; lisp
(add-hook 'elisp-lisp-mode-hook
          (lambda () (add-hook 'local-write-file-hooks 'check-parens)))

;; rustdesk
(add-to-list 'auto-mode-alist '("\\.tis\\'" . javascript-mode))

;; tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq tramp-inline-compress-start-size 1000)
(setq tramp-copy-size-limit 10000)
(setq vc-handled-backends '(Git))
(setq tramp-default-method "scp")
(setq tramp-use-ssh-controlmaster-options nil)
(setq tramp-verbose 1)

(provide 'early-init)
