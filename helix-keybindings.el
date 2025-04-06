;;; helix-keybindings.el --- Helix keybindings -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Helix default keybindins.
;;
;;; Code:

(require 'helix-commands)
(require 'helix-states)
(require 'keypad)

;;; Normal state

(keymap-set helix-normal-state-map "SPC" #'keypad)

(keymap-set helix-normal-state-map "h" #'helix-backward-char)
(keymap-set helix-normal-state-map "j" #'helix-next-line)
(keymap-set helix-normal-state-map "k" #'helix-previous-line)
(keymap-set helix-normal-state-map "l" #'helix-forward-char)

(keymap-set helix-normal-state-map "i" #'helix-insert)
(keymap-set helix-normal-state-map "a" #'helix-append)

(keymap-set helix-normal-state-map "w" #'helix-forward-word-start)
(keymap-set helix-normal-state-map "b" #'helix-backward-word-start)
(keymap-set helix-normal-state-map "e" #'helix-forward-word-end)
(keymap-set helix-normal-state-map "W" #'helix-forward-WORD-start)
(keymap-set helix-normal-state-map "B" #'helix-backward-WORD-start)
(keymap-set helix-normal-state-map "e" #'helix-forward-WORD-end)
(keymap-set helix-normal-state-map "x" #'helix-select-or-extend-line)

(keymap-set helix-normal-state-map "d" #'helix-delete-selection)
(keymap-set helix-normal-state-map "v" #'helix-extend-selection)
(keymap-set helix-normal-state-map ";" #'helix-collapse-selection)
(keymap-set helix-normal-state-map "<escape>" #'helix-normal-state-escape)

(keymap-set helix-normal-state-map "u" #'helix-undo)
(keymap-set helix-normal-state-map "U" #'undo-redo)

;;; Insert state

(keymap-set helix-insert-state-map "<escape>" #'helix-normal-state)

;;; Motion state


(provide 'helix-keybindings)
;;; helix-keybindings.el ends here
