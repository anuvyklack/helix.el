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
(require 'helix-multiple-cursors)

;;; Normal state

(keymap-set helix-normal-state-map "<backspace>" #'execute-extended-command)
(keymap-set helix-normal-state-map "SPC" #'keypad)

(keymap-set helix-normal-state-map "h" #'helix-backward-char)
(keymap-set helix-normal-state-map "j" #'helix-next-line)
(keymap-set helix-normal-state-map "k" #'helix-previous-line)
(keymap-set helix-normal-state-map "l" #'helix-forward-char)

(keymap-set helix-normal-state-map "w" #'helix-forward-word-start)
(keymap-set helix-normal-state-map "b" #'helix-backward-word-start)
(keymap-set helix-normal-state-map "e" #'helix-forward-word-end)
(keymap-set helix-normal-state-map "W" #'helix-forward-WORD-start)
(keymap-set helix-normal-state-map "B" #'helix-backward-WORD-start)
(keymap-set helix-normal-state-map "E" #'helix-forward-WORD-end)
(keymap-set helix-normal-state-map "x" #'helix-line)

(keymap-set helix-normal-state-map "v" #'helix-extend-selection)
(keymap-set helix-normal-state-map ";" #'helix-collapse-selection)
(keymap-set helix-normal-state-map "<escape>" #'helix-normal-state-escape)

(keymap-set helix-normal-state-map "u" #'helix-undo)
(keymap-set helix-normal-state-map "U" #'undo-redo)

(defvar-keymap helix-window-map
  "C-h" #'helix-window-left
  "C-j" #'helix-window-down
  "C-k" #'helix-window-up
  "C-l" #'helix-window-right
  "h"   #'helix-window-left
  "j"   #'helix-window-down
  "k"   #'helix-window-up
  "l"   #'helix-window-right
  "H"   #'helix-move-window-left
  "J"   #'helix-move-window-down
  "K"   #'helix-move-window-up
  "L"   #'helix-move-window-right)

(keymap-set helix-normal-state-map "C-w" helix-window-map)

;;;; Changes

(keymap-set helix-normal-state-map "i" #'helix-insert)
(keymap-set helix-normal-state-map "a" #'helix-append)
(keymap-set helix-normal-state-map "c" #'helix-change)
(keymap-set helix-normal-state-map "d" #'helix-delete)

;;;; Selections

(keymap-set helix-normal-state-map "," #'helix-keep-primary-selection)

(global-unset-key (kbd "M-<down-mouse-1>"))
(keymap-set helix-normal-state-map "M-<mouse-1>" #'helix-toggle-cursor-on-click)

;;; Insert state

(keymap-set helix-insert-state-map "<escape>" #'helix-normal-state)

;;; Motion state


(provide 'helix-keybindings)
;;; helix-keybindings.el ends here
