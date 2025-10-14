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

(helix-cache-input consult--read)

(helix-keymap-global-set :state '(normal motion)
  "C-/" 'consult-line       ; `/' is bound to search
  "C-?" 'consult-line-multi ; `C-S-/'
  "g i" 'consult-imenu
  "g I" 'consult-imenu-multi
  "g o" 'consult-outline
  "g m" 'consult-mark
  "g M" 'consult-global-mark)

(put 'consult-yank-pop 'multiple-cursors t) ; Execute for all cursors.

(dolist (cmd '(consult-line
               consult-mark
               consult-global-mark
               consult-imenu
               consult-outline
               consult-grep
               consult-git-grep
               consult-ripgrep))
  (helix-advice-add cmd :before #'helix-deactivate-mark-a))

(provide 'helix-consult)
;;; helix-consult.el ends here
