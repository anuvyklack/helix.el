;;; helix-helpful.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Integration Helix with Helpful package.
;;
;;; Code:
(require 'helix-core)
(require 'helpful)

(helix-set-initial-state 'helpful-mode 'normal)
(helix-inhibit-insert-state helpful-mode-map)

(put 'helpful-at-point 'multiple-cursors 'false)

(dolist (keymap (list emacs-lisp-mode-map lisp-data-mode-map))
  (helix-keymap-set keymap :state 'normal
    ;; Vim uses `K' but it is occupied in Helix. `M' is near `K' and it is free.
    "M" #'helpful-at-point))

;; Open links to functions, variables and symbols in helpful buffer
;; in the same window.
(add-to-list 'display-buffer-alist
             '((derived-mode . helpful-mode)
               (display-buffer-reuse-mode-window display-buffer-pop-up-window)
               (mode . helpful-mode)
               (body-function . select-window)))

(provide 'helix-helpful)
;;; helix-helpful.el ends here
