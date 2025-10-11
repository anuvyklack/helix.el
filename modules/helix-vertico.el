;;; helix-vertico.el -*- lexical-binding: t; -*-
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
;; Integration Helix with Vertico package.
;;
;;; Code:

(require 'helix-core)
(require 'vertico)

(helix-keymap-set vertico-map :state 'normal
  "y"   #'vertico-save ; Copy current candidate to kill ring
  "j"   #'vertico-next
  "k"   #'vertico-previous
  "g g" #'vertico-first
  "G"   #'vertico-last
  "C-l" #'vertico-insert)

(helix-keymap-set vertico-map :state 'insert
  "C-l" #'vertico-insert)

(helix-keymap-set vertico-map
  "M-j" #'next-history-element
  "M-k" #'previous-history-element
  ;; Rebind forward/backward paragraphs keys
  "<remap> <helix-mark-paragraph-forward>"  #'vertico-next-group
  "<remap> <helix-mark-paragraph-backward>" #'vertico-previous-group
  ;; Rebind scrolling keys
  "<remap> <helix-smooth-scroll-down>"      #'vertico-scroll-up
  "<remap> <helix-smooth-scroll-up>"        #'vertico-scroll-down
  "<remap> <helix-smooth-scroll-page-down>" #'vertico-scroll-up
  "<remap> <helix-smooth-scroll-page-up>"   #'vertico-scroll-down)

(with-eval-after-load 'vertico-directory
  (dolist (state '(normal insert))
    (helix-keymap-set vertico-map :state state
      "C-h" 'vertico-directory-up)))

(provide 'helix-vertico)
;;; helix-vertico.el ends here
