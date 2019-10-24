;; -*- lexical-binding: t -*-

;; Run test for scihub.el

(require 'buttercup)
(require 'async)

;; This is a massive hack, but using async-let here seems the emacs
;; init process to finish before we start the tests. If we execute the
;; tests synchronously during init emacs will freeze
(async-let ((x (sleep-for 2)))
  (let ((buf  (get-buffer-create "*Buttercup*")))
    (display-buffer buf)
    (delete-other-windows (get-buffer-window buf t))
    (buttercup-run-discover)))
