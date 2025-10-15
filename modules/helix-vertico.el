;;; helix-vertico.el -*- lexical-binding: t; -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/anuvyklack/helix.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Keybindings for Vertico package.
;;
;;; Code:
(require 'helix-core)
(require 'vertico)

(helix-keymap-set vertico-map :state 'normal
  "y"     #'vertico-save ; Copy current candidate to kill ring.
  "j"     #'vertico-next
  "k"     #'vertico-previous
  "g g"   #'vertico-first
  "G"     #'vertico-last)

(helix-keymap-set vertico-map
  "M-j"   #'next-history-element
  "M-k"   #'previous-history-element

  "C-l"   #'vertico-insert
  "C-h"   #'vertico-directory-up
  "C-n"   #'vertico-next-group
  "C-S-n" #'vertico-previous-group

  ;; Rebind } / { or ]p / [p keys
  "<remap> <helix-mark-paragraph-forward>"  #'vertico-next-group
  "<remap> <helix-mark-paragraph-backward>" #'vertico-previous-group

  ;; Rebind scrolling keys
  "<remap> <helix-smooth-scroll-down>"      #'vertico-scroll-up
  "<remap> <helix-smooth-scroll-up>"        #'vertico-scroll-down
  "<remap> <helix-smooth-scroll-page-down>" #'vertico-scroll-up
  "<remap> <helix-smooth-scroll-page-up>"   #'vertico-scroll-down)

(with-eval-after-load 'vertico-directory
  (helix-keymap-set vertico-directory-map
    "C-h" 'vertico-directory-up))

(provide 'helix-vertico)
;;; helix-vertico.el ends here
