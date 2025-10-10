;;; helix-info.el -*- lexical-binding: t; -*-
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
;; Keybindings for Info package.
;;
;;; Code:
(require 'helix-core)
(require 'info)

(helix-set-initial-state 'Info-mode 'normal)
(helix-inhibit-insert-state Info-mode-map)

(helix-keymap-set Info-mode-map :state 'normal
  "C-j"   'Info-next
  "C-k"   'Info-prev
  "z j"   'Info-forward-node
  "z k"   'Info-backward-node
  "z u"   'Info-up
  "z d"   'Info-directory

  "z h"   'Info-history
  "u"     'Info-history-back
  "U"     'Info-history-forward
  "C-<i>" 'Info-history-forward
  "C-o"   'Info-history-back

  "z i"   'Info-index
  "z I"   'Info-virtual-index
  "z a"   'info-apropos

  "g t"   'Info-toc
  "M-h"   'Info-help

  "] ]"   'Info-next-reference
  "[ ["   'Info-prev-reference)

(helix-advice-add 'Info-next-reference :before #'helix-deactivate-mark-a-a)
(helix-advice-add 'Info-prev-reference :before #'helix-deactivate-mark-a-a)

(provide 'helix-info)
;;; helix-info.el ends here
