;;; helix-consult.el -*- lexical-binding: t; -*-
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
;; Integration Helix with Consult package.
;;
;;; Code:

(require 'helix-macros)
(require 'helix-common)
(require 'helix-core)

(dolist (state '(normal motion))
  (helix-keymap-global-set :state state
    "C-/" 'consult-line       ; "/" is bound to search
    "C-?" 'consult-line-multi ; "C-S-/"
    "g i" 'consult-imenu
    "g I" 'consult-imenu-multi
    "g o" 'consult-outline
    "g m" 'consult-mark
    "g M" 'consult-global-mark))

(provide 'helix-consult)
;;; helix-consult.el ends here
