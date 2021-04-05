;;; colors.el --- theming of my emacs                -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Odd-Harald Myhren

;; Author: Odd-Harald Myhren <knarkzel@gmail.com>

(add-to-list 'load-path "~/.emacs.d/nano-emacs")
(require 'nano)

(defun set-style (face &rest args)
  "Shorter version of (set-face-attribute)"
  (interactive)
  (apply #'set-face-attribute face nil args))

;; nano-color-foreground       "#ECEFF4"
;; nano-color-background       "#2E3440"
;; nano-color-highlight        "#3B4252"
;; nano-color-critical         "#A3BE8C"
;; nano-color-salient          "#81A1C1"
;; nano-color-strong           "#ECEFF4"
;; nano-color-popout           "#88C0D0"
;; nano-color-subtle           "#434C5E"
;; nano-color-faded            "#677691"
;; nano-color-popout-lighter   "#5E81AC"
;; nano-color-critical-lighter "#8FBCBB"

;; show paren
(set-style 'show-paren-match :foreground nano-color-foreground)

;; line numbers
(set-style 'line-number-current-line :foreground nano-color-popout)

;; rainbow delimiters
(add-hook 'rainbow-delimiters-mode-hook
          (lambda () (interactive)
            (set-style 'rainbow-delimiters-depth-1-face :foreground nano-color-popout)
            (set-style 'rainbow-delimiters-depth-2-face :foreground nano-color-critical)
            (set-style 'rainbow-delimiters-depth-3-face :foreground nano-color-popout-lighter)
            (set-style 'rainbow-delimiters-depth-4-face :foreground nano-color-critical-lighter)))

;; dired
(set-style 'dired-mark :foreground nano-color-popout)
(set-style 'dired-header :foreground nano-color-popout)
(set-style 'dired-directory :foreground nano-color-critical)
(set-style 'dired-symlink :foreground nano-color-popout-lighter)
(set-style 'dired-broken-symlink :foreground nano-color-warning :background nano-color-background :underline t)
(set-style 'dired-marked :foreground nano-color-background :background nano-color-foreground :bold t :underline t)

;; eshell
(add-hook 'eshell-mode-hook
          (lambda () (interactive)
            (set-style 'eshell-prompt :foreground nano-color-popout)
            (set-style 'eshell-ls-directory :foreground nano-color-critical)
            (set-style 'eshell-ls-symlink :foreground nano-color-popout-lighter)))
