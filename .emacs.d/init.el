;;; init.el --- initial loading                -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Odd-Harald Myhren

;; Author: Odd-Harald Myhren <knarkzel@gmail.com>

(let ((base "~/.emacs.d/packages/")
      (packages '("defaults.el"
                  "defuns.el"
                  "packages.el"
		          "exwm.el"
                  "colors.el")))
  (dolist (package packages)
    (load-file (concat base package))))

(setq debug-on-error nil
      debug-on-quit nil
      inhibit-message nil
      file-name-handler-alist file-name-handler-alist-original)

(defun display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time)
