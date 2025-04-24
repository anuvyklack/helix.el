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
(require 'helix-scrolling)
(require 'helix-states)
(require 'keypad)
(require 'helix-multiple-cursors)

;;; Normal state

(keymap-set helix-normal-state-map "C-h k" #'keypad-describe-key)

(keymap-set helix-normal-state-map "0" #'digit-argument)
(keymap-set helix-normal-state-map "1" #'digit-argument)
(keymap-set helix-normal-state-map "2" #'digit-argument)
(keymap-set helix-normal-state-map "3" #'digit-argument)
(keymap-set helix-normal-state-map "4" #'digit-argument)
(keymap-set helix-normal-state-map "5" #'digit-argument)
(keymap-set helix-normal-state-map "6" #'digit-argument)
(keymap-set helix-normal-state-map "7" #'digit-argument)
(keymap-set helix-normal-state-map "8" #'digit-argument)
(keymap-set helix-normal-state-map "9" #'digit-argument)

(keymap-set helix-normal-state-map "<backspace>" #'execute-extended-command)
(keymap-set helix-normal-state-map "SPC" #'keypad)

(keymap-set helix-normal-state-map "h"   #'helix-backward-char)
(keymap-set helix-normal-state-map "j"   #'helix-next-line)
(keymap-set helix-normal-state-map "k"   #'helix-previous-line)
(keymap-set helix-normal-state-map "l"   #'helix-forward-char)
(keymap-set helix-normal-state-map "w"   #'helix-forward-word-start)
(keymap-set helix-normal-state-map "W"   #'helix-forward-WORD-start)
(keymap-set helix-normal-state-map "b"   #'helix-backward-word-start)
(keymap-set helix-normal-state-map "B"   #'helix-backward-WORD-start)
(keymap-set helix-normal-state-map "e"   #'helix-forward-word-end)
(keymap-set helix-normal-state-map "E"   #'helix-forward-WORD-end)
(keymap-set helix-normal-state-map "g s" #'helix-beginning-of-line)
(keymap-set helix-normal-state-map "g h" #'helix-first-non-blank)
(keymap-set helix-normal-state-map "g l" #'helix-end-of-line)
(keymap-set helix-normal-state-map "g g" #'helix-goto-first-line)
(keymap-set helix-normal-state-map "G"   #'helix-goto-last-line)

;;;; Changes

(keymap-set helix-normal-state-map "i" #'helix-insert)
(keymap-set helix-normal-state-map "a" #'helix-append)
(keymap-set helix-normal-state-map "c" #'helix-change)
(keymap-set helix-normal-state-map "d" #'helix-delete)
(keymap-set helix-normal-state-map "u" #'helix-undo)
(keymap-set helix-normal-state-map "U" #'undo-redo)

;;;; Selections

(keymap-set helix-normal-state-map "v" #'helix-extend-selection)
(keymap-set helix-normal-state-map "x" #'helix-select-line)
(keymap-set helix-normal-state-map "X" #'helix-select-line-upward)
(keymap-set helix-normal-state-map "," #'helix-keep-primary-selection)
(keymap-set helix-normal-state-map ";" #'helix-collapse-selection)
(keymap-set helix-normal-state-map "M-;" #'exchange-point-and-mark)
(keymap-set helix-normal-state-map "g o" #'exchange-point-and-mark)
(keymap-set helix-normal-state-map "<escape>" #'helix-normal-state-escape)

(global-unset-key (kbd "M-<down-mouse-1>"))
(keymap-set helix-normal-state-map "M-<mouse-1>" #'helix-toggle-cursor-on-click)

(define-prefix-command 'helix-match-map)
(keymap-set helix-match-map "0"   #'helix-match-map-digit-argument)
(keymap-set helix-match-map "1"   #'helix-match-map-digit-argument)
(keymap-set helix-match-map "2"   #'helix-match-map-digit-argument)
(keymap-set helix-match-map "3"   #'helix-match-map-digit-argument)
(keymap-set helix-match-map "4"   #'helix-match-map-digit-argument)
(keymap-set helix-match-map "5"   #'helix-match-map-digit-argument)
(keymap-set helix-match-map "6"   #'helix-match-map-digit-argument)
(keymap-set helix-match-map "7"   #'helix-match-map-digit-argument)
(keymap-set helix-match-map "8"   #'helix-match-map-digit-argument)
(keymap-set helix-match-map "9"   #'helix-match-map-digit-argument)
(keymap-set helix-match-map "i p" #'helix-mark-inner-paragraph)
(keymap-set helix-match-map "p"   #'helix-mark-inner-paragraph)
(keymap-set helix-match-map "i w" #'helix-mark-inner-word)
(keymap-set helix-match-map "w"   #'helix-mark-inner-word)
(keymap-set helix-match-map "i W" #'helix-mark-inner-WORD)
(keymap-set helix-match-map "W"   #'helix-mark-inner-WORD)

;;;; Scrolling

(keymap-set helix-normal-state-map "C-b" #'helix-smooth-scroll-page-up)
(keymap-set helix-normal-state-map "C-f" #'helix-smooth-scroll-page-down)
(keymap-set helix-normal-state-map "C-d" #'helix-smooth-scroll-down)
(keymap-set helix-normal-state-map "C-u" #'helix-smooth-scroll-up)
(keymap-set helix-normal-state-map "C-e" #'helix-mix-scroll-line-down)
(keymap-set helix-normal-state-map "C-y" #'helix-mix-scroll-line-up)
;; (keymap-set helix-normal-state-map "z z" #'helix-smooth-scroll-line-to-center)
(keymap-set helix-normal-state-map "z z" #'helix-smooth-scroll-line-not-to-very-top)
(keymap-set helix-normal-state-map "z t" #'helix-smooth-scroll-line-to-top)
(keymap-set helix-normal-state-map "z b" #'helix-smooth-scroll-line-to-bottom)

;;;; Windows

(define-prefix-command 'helix-window-map)
(keymap-set helix-window-map "s"   #'helix-window-split)
(keymap-set helix-window-map "v"   #'helix-window-vsplit)
(keymap-set helix-window-map "d"   #'helix-window-delete)
(keymap-set helix-window-map "c"   #'helix-window-delete)
(keymap-set helix-window-map "h"   #'helix-window-left)
(keymap-set helix-window-map "j"   #'helix-window-down)
(keymap-set helix-window-map "k"   #'helix-window-up)
(keymap-set helix-window-map "l"   #'helix-window-right)
(keymap-set helix-window-map "C-h" #'helix-window-left)
(keymap-set helix-window-map "C-j" #'helix-window-down)
(keymap-set helix-window-map "C-k" #'helix-window-up)
(keymap-set helix-window-map "C-l" #'helix-window-right)
(keymap-set helix-window-map "H"   #'helix-move-window-left)
(keymap-set helix-window-map "J"   #'helix-move-window-down)
(keymap-set helix-window-map "K"   #'helix-move-window-up)
(keymap-set helix-window-map "L"   #'helix-move-window-right)
(keymap-set helix-normal-state-map "C-w" 'helix-window-map)

;;; Insert state

(keymap-set helix-insert-state-map "<escape>" #'helix-normal-state)

;;; Motion state


(provide 'helix-keybindings)
;;; helix-keybindings.el ends here
