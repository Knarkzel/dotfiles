;;; defuns.el --- useful functions                   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Odd-Harald Myhren

;; Author: Odd-Harald Myhren <knarkzel@gmail.com>

(defun odd/kill-buffer-window ()
  (interactive)
  (kill-buffer (current-buffer))
  (when (not (one-window-p))
    (delete-window)))

(defun odd/cargo-doc ()
  (interactive)
  (compile "cargo doc --open"))

(defun odd/cargo-build ()
  (interactive)
  (compile "cargo build"))

(defun odd/cargo-run ()
  (interactive)
  (compile "cargo run"))

(defun odd/eshell-clear ()
  (interactive)
  (let ((input (eshell-get-old-input)))
    (eshell/clear-scrollback)
    (eshell-emit-prompt)
    (insert input)))

(defun odd/org-mode-to-docx ()
  (interactive)
  (org-latex-export-to-latex)
  (let ((file (file-name-sans-extension buffer-file-name)))
    (shell-command (concat "pandoc -s --filter pandoc-crossref -o"
                           file ".docx "
                           "--bibliography=sources.bib "
                           ;; "--reference-doc=~/source/useful/style.docx "
                           file ".tex"))
    (async-shell-command (concat "libreoffice " file ".docx"))))

(defun odd/m-x ()
  (interactive)
  (execute-extended-command ""))

(defun odd/add-hooks (hook defuns)
  (interactive)
  (dolist (defun defuns)
    (add-hook hook defun)))

(defun odd/go-back-window-only ()
  (interactive)
  (previous-window-any-frame)
  (delete-other-windows))

(defun odd/open-config-folder ()
  (interactive)
  (find-file "~/.emacs.d/packages/packages.el"))

(defun odd/create-project ()
  (interactive)
  (let ((input (read-string "Enter file name: ")))
    (make-directory input)
    (make-directory (concat input "/images"))
    (make-empty-file (concat input "/sources.bib"))
    (make-empty-file (concat input "/" input ".org"))
    (dired (concat default-directory input "/" input ".org"))))

(defun odd/elfeed-search-eww-open (&optional use-generic-p)
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-search-browse-url use-generic-p)))

(defvar elfeed-mpv-patterns
  '("youtu\\.?be" "videos\.lukesmith")
  "List of regexp to match against elfeed entry link to know
whether to use mpv to visit the link.")

(defun elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (message "Opening %s with mpv..." (elfeed-entry-link entry))
    (start-process "elfeed-mpv" nil "mpv" (elfeed-entry-link entry))))

(defun elfeed-visit-or-play-with-mpv ()
  "Play in mpv if entry link matches `elfeed-mpv-patterns', visit otherwise.
See `elfeed-play-with-mpv'."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (patterns elfeed-mpv-patterns))
    (while (and patterns (not (string-match (car patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (if patterns
        (elfeed-play-with-mpv)
      (odd/elfeed-search-eww-open))))
