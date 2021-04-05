;;; early-init.el --- make loading look much better                -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Odd-Harald Myhren

;; Author: Odd-Harald Myhren <knarkzel@gmail.com>

;; startup optimizations
(setq inhibit-message t
      gc-cons-threshold (* 1024 1024 100)
      gc-cons-threshold-original gc-cons-threshold
      file-name-handler-alist-original file-name-handler-alist
      file-name-handler-alist nil)
