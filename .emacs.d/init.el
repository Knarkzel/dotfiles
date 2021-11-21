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

;; sweet defaults
(setq-default auto-save-default nil
              auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
              backup-by-copying t
              backup-directory-alist `((".*" . ,temporary-file-directory))
              confirm-nonexistent-file-or-buffer nil
              delete-old-versions t
              dired-recursive-copies 'always
              dired-recursive-deletes 'always
              fill-column 80
              gc-cons-threshold 100000000
              display-line-numbers-type 'relative
              inhibit-compacting-font-caches t
              inhibit-startup-echo-area-message t
              make-backup-files nil
              visible-bell t
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

(setq split-width-threshold 0
      split-height-threshold nil)

(use-package evil 
  :init
  (setq evil-want-C-u-scroll t
   	evil-want-integration t
  	evil-want-keybinding nil)
  :config
  (evil-mode t))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode t))

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "b" 'switch-to-buffer
    "e" 'eshell
    "f" 'find-file
    "g" 'magit
    "o" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    "p" 'projectile-command-map
    "x" 'counsel-M-x))

(use-package ivy 
      :init
      (setq ivy-use-virtual-buffers t
      	    enable-recursive-minibuffers t
            ivy-extra-directories nil)
      :config
      (ivy-mode t))

(use-package counsel 
  :init
  ;; File names beginning with # or ., and ending with # or ~
  (setq counsel-find-file-ignore-regexp
        (concat "\\(?:\\`[#.]\\)"
                "\\|\\(?:\\`.+?[#~]\\'\\)"))
  :config  
  (counsel-mode t))

(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  :config  
  (which-key-mode t))

(use-package company
  :init
  (setq company-idle-delay 0.25
        company-minimum-prefix-length 1)
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package zoom
  :init
  (setq zoom-size '(0.618 . 0.618))
  :config
  (zoom-mode t))

(use-package beacon
  :config
  (beacon-mode t))

(use-package magit
  :config
  (setq magit-refresh-status-buffer nil))

;; theme
(use-package doom-themes
  :config
  (load-theme 'doom-vibrant t))
(set-face-attribute 'default nil :height 180)

;; dired
(setq dired-omit-files "^\\.+"
      dired-listing-switches "-AlghX")
(use-package dired-ranger)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook 'dired-omit-mode)
(evil-define-key 'normal dired-mode-map
	(kbd "J") 'dired-find-file
	(kbd "K") 'dired-up-directory
	(kbd ".") 'dired-omit-mode
	(kbd "c") 'dired-ranger-copy
	(kbd "p") 'dired-ranger-paste
	(kbd "P") 'dired-ranger-move
	(kbd "-") (lambda () (interactive) (dired "~/")))

;; electric pair
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; elfeed
(evil-define-key 'normal elfeed-search-mode-map (kbd "g r") 'elfeed-update)
(setq elfeed-feeds '(("https://feeds.fireside.fm/coder/rss")
	       ("https://lobste.rs/rss")
	       ("https://videos.lukesmith.xyz/feeds/videos.xml")
	       ("https://buttondown.email/j2kun/rss")
	       ("https://www.tedinski.com/feed.xml")
	       ("https://this-week-in-rust.org/rss.xml")))
(setq-default elfeed-search-title-max-width 100)
(setq-default elfeed-search-title-min-width 100)
