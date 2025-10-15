;;; helix-custom.el -*- lexical-binding: t; -*-
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
;; Keybindings for Customize interface.
;;
;;; Code:
(require 'helix-core)
(require 'cus-edit)

(helix-set-initial-state 'Custom-mode 'normal)

(helix-keymap-set custom-mode-map :state 'normal
  "z j" #'widget-forward
  "z k" #'widget-backward
  "z u" #'Custom-goto-parent
  "q"   #'Custom-buffer-done)

(provide 'helix-custom)
;;; helix-custom.el ends here
