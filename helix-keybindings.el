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
;;  Helix default keybindings.
;;
;;; Code:

(require 'helix-commands)
(require 'helix-scrolling)
(require 'helix-core)
(require 'helix-states)
(require 'keypad)
(require 'helix-multiple-cursors)

;;; Normal state

(helix-keymap-set nil 'normal
  "SPC" #'keypad
  "C-h k" #'keypad-describe-key
  "<backspace>" #'execute-extended-command)

(helix-keymap-set nil 'normal
  ":" #'execute-extended-command

  "0" #'digit-argument
  "1" #'digit-argument
  "2" #'digit-argument
  "3" #'digit-argument
  "4" #'digit-argument
  "5" #'digit-argument
  "6" #'digit-argument
  "7" #'digit-argument
  "8" #'digit-argument
  "9" #'digit-argument

  ;; Motions
  "h"   #'helix-backward-char
  "j"   #'helix-next-line
  "k"   #'helix-previous-line
  "l"   #'helix-forward-char
  "w"   #'helix-forward-word-start
  "W"   #'helix-forward-WORD-start
  "b"   #'helix-backward-word-start
  "B"   #'helix-backward-WORD-start
  "e"   #'helix-forward-word-end
  "E"   #'helix-forward-WORD-end
  "g s" #'helix-beginning-of-line
  "g h" #'helix-first-non-blank
  "g l" #'helix-end-of-line
  "g g" #'helix-goto-first-line
  "G"   #'helix-goto-last-line
  "] p" #'helix-forward-paragraph
  "[ p" #'helix-backward-paragraph
  "}"   #'helix-forward-paragraph
  "{"   #'helix-backward-paragraph

  ;; Easymotion / Avy
  "g w" #'helix-avy-word-forward
  "g b" #'helix-avy-word-backward
  "g W" #'helix-avy-WORD-forward
  "g B" #'helix-avy-WORD-backward
  "g j" #'helix-avy-next-line
  "g k" #'helix-avy-previous-line

  ;; Changes
  "i"   #'helix-insert
  "a"   #'helix-append
  "c"   #'helix-change
  "d"   #'helix-delete
  "u"   #'helix-undo
  "U"   #'helix-redo
  "J"   #'helix-join-line
  "`"   #'helix-downcase
  "M-`" #'helix-upcase
  "~"   #'helix-invert-case
  "g u" #'helix-downcase
  "g U" #'helix-upcase

  ;; Selections
  "<escape>" #'helix-normal-state-escape
  "v"   #'helix-extend-selection
  "x"   #'helix-mark-line
  "X"   #'helix-mark-line-upward
  "%"   #'helix-select-all
  "C"   #'helix-copy-selection
  "M-c" #'helix-copy-selection-up
  "("   #'helix-rotate-selections-backward
  ")"   #'helix-rotate-selections-forward
  "M-(" #'helix-rotate-selections-content-backward
  "M-)" #'helix-rotate-selections-content-forward
  "s"   #'helix-select-regex
  "S"   #'helix-split-region
  "M-s" #'helix-split-region-on-newline
  "K"   #'helix-keep-selections
  "M-K" #'helix-remove-selections
  ","   #'helix-disable-multiple-cursors-mode
  "M-," #'helix-delete-main-cursor
  "M--" #'helix-merge-selections
  ";"   #'helix-collapse-selection
  "M-;" #'exchange-point-and-mark
  "g o" #'exchange-point-and-mark
  "_"   #'helix-trim-whitespaces-from-selection
  "&"   #'helix-align-selections
  "/"   #'helix-search-forward
  "?"   #'helix-search-backward
  "*"   #'helix-construct-search-pattern
  "M-*" #'helix-construct-search-pattern-no-bounds
  "n"   #'helix-search-next
  "N"   #'helix-search-previous

  ;; Scrolling
  "C-b" #'helix-smooth-scroll-page-up
  "C-f" #'helix-smooth-scroll-page-down
  "C-d" #'helix-smooth-scroll-down
  "C-u" #'helix-smooth-scroll-up
  "C-e" #'helix-mix-scroll-line-down
  "C-y" #'helix-mix-scroll-line-up
  ;; "z z" #'helix-smooth-scroll-line-to-center
  "z z" #'helix-smooth-scroll-line-not-to-very-top
  "z t" #'helix-smooth-scroll-line-to-top
  "z b" #'helix-smooth-scroll-line-to-bottom

  ;;; Misc
  "g f" #'find-file-at-point
  "g x" #'browse-url-at-point)

(keymap-global-unset "M-<down-mouse-1>")
(helix-keymap-set nil 'normal "M-<mouse-1>" #'helix-toggle-cursor-on-click)

;;;; Match

(helix-keymap-set nil 'normal "m" 'helix-match-map)
(define-prefix-command 'helix-match-map)
(helix-keymap-set helix-match-map nil
  "0"   #'helix-match-map-digit-argument
  "1"   #'helix-match-map-digit-argument
  "2"   #'helix-match-map-digit-argument
  "3"   #'helix-match-map-digit-argument
  "4"   #'helix-match-map-digit-argument
  "5"   #'helix-match-map-digit-argument
  "6"   #'helix-match-map-digit-argument
  "7"   #'helix-match-map-digit-argument
  "8"   #'helix-match-map-digit-argument
  "9"   #'helix-match-map-digit-argument

  "m"   #'helix-jump-to-match-item

  "i p" #'helix-mark-inner-paragraph
  "p"   #'helix-mark-inner-paragraph
  "a p" #'helix-mark-a-paragraph
  "i w" #'helix-mark-inner-word
  "w"   #'helix-mark-inner-word
  "i W" #'helix-mark-inner-WORD
  "W"   #'helix-mark-inner-WORD
  "\""  #'helix-mark-inner-double-quoted
  "i \"" #'helix-mark-inner-double-quoted
  "a \"" #'helix-mark-a-double-quoted
  "'"   #'helix-mark-inner-single-quoted
  "i '" #'helix-mark-inner-single-quoted
  "a '" #'helix-mark-a-single-quoted
  "`"   #'helix-mark-inner-back-quoted
  "i `" #'helix-mark-inner-back-quoted
  "a `" #'helix-mark-a-back-quoted

  "("   #'helix-mark-inner-paren
  ")"   #'helix-mark-inner-paren
  "i (" #'helix-mark-inner-paren
  "i )" #'helix-mark-inner-paren
  "a (" #'helix-mark-a-paren
  "a )" #'helix-mark-a-paren

  "["   #'helix-mark-inner-bracket
  "]"   #'helix-mark-inner-bracket
  "i [" #'helix-mark-inner-bracket
  "i ]" #'helix-mark-inner-bracket
  "a [" #'helix-mark-a-bracket
  "a ]" #'helix-mark-a-bracket

  "{"   #'helix-mark-inner-curly
  "}"   #'helix-mark-inner-curly
  "i {" #'helix-mark-inner-curly
  "i }" #'helix-mark-inner-curly
  "a {" #'helix-mark-a-curly
  "a }" #'helix-mark-a-curly

  "<"   #'helix-mark-inner-angle
  ">"   #'helix-mark-inner-angle
  "i <" #'helix-mark-inner-angle
  "i >" #'helix-mark-inner-angle
  "a <" #'helix-mark-an-angle
  "a >" #'helix-mark-an-angle

  "!" #'helix-mark-inner-surround
  "@" #'helix-mark-inner-surround
  "#" #'helix-mark-inner-surround
  "$" #'helix-mark-inner-surround
  "%" #'helix-mark-inner-surround
  "^" #'helix-mark-inner-surround
  "&" #'helix-mark-inner-surround
  "*" #'helix-mark-inner-surround
  "~" #'helix-mark-inner-surround
  "=" #'helix-mark-inner-surround
  "_" #'helix-mark-inner-surround

  "i !" #'helix-mark-inner-surround
  "i @" #'helix-mark-inner-surround
  "i #" #'helix-mark-inner-surround
  "i $" #'helix-mark-inner-surround
  "i %" #'helix-mark-inner-surround
  "i ^" #'helix-mark-inner-surround
  "i &" #'helix-mark-inner-surround
  "i *" #'helix-mark-inner-surround
  "i ~" #'helix-mark-inner-surround
  "i =" #'helix-mark-inner-surround
  "i _" #'helix-mark-inner-surround

  "a !" #'helix-mark-a-surround
  "a @" #'helix-mark-a-surround
  "a #" #'helix-mark-a-surround
  "a $" #'helix-mark-a-surround
  "a %" #'helix-mark-a-surround
  "a ^" #'helix-mark-a-surround
  "a &" #'helix-mark-a-surround
  "a *" #'helix-mark-a-surround
  "a ~" #'helix-mark-a-surround
  "a =" #'helix-mark-a-surround
  "a _" #'helix-mark-a-surround

  ;; Surround
  "s" #'helix-surround
  "d" #'helix-surround-delete
  "r" #'helix-surround-change ; Helix original key
  "c" #'helix-surround-change)

;;;; Windows

(helix-keymap-set nil 'normal "C-w" 'helix-window-map)
(define-prefix-command 'helix-window-map)
(helix-keymap-set helix-window-map nil
  "s"   #'helix-window-split
  "v"   #'helix-window-vsplit
  "d"   #'helix-window-delete
  "c"   #'helix-window-delete
  "h"   #'helix-window-left
  "j"   #'helix-window-down
  "k"   #'helix-window-up
  "l"   #'helix-window-right
  "C-h" #'helix-window-left
  "C-j" #'helix-window-down
  "C-k" #'helix-window-up
  "C-l" #'helix-window-right
  "H"   #'helix-move-window-left
  "J"   #'helix-move-window-down
  "K"   #'helix-move-window-up
  "L"   #'helix-move-window-right)

;; Ingored

(helix-keymap-set nil 'normal
  "q"  #'ignore
  "\"" #'ignore
  "'"  #'ignore
  ;; "n" #'ignore
  ;; "p" #'ignore
  ;; "f" #'ignore
  ;; "t" #'ignore
  ;; "o" #'ignore
  )

;; (defun helix-general-o-fun ()
;;   (interactive)
;;   (message "General o function"))
;;
;; (keymap-set helix-normal-state-map "o" #'helix-general-o-fun)
;;
;; (defun helix-elisp-mode-o-fun ()
;;   (interactive)
;;   (message "Emacs-Lisp mode local o function"))
;;
;; (helix-keymap-set emacs-lisp-mode-map 'normal
;;   "o" #'helix-elisp-mode-o-fun)

;;; Multiple cursors

(keymap-set helix-multiple-cursors-map "C-g" #'helix-disable-multiple-cursors-mode)
;; (keymap-set helix-multiple-cursors-map "C-:" #'helix-mc-repeat-command)
(when (fboundp 'phi-search)
  (keymap-set helix-multiple-cursors-map "C-s" #'phi-search))
(when (fboundp 'phi-search-backward)
  (keymap-set helix-multiple-cursors-map "C-r" #'phi-search-backward))

;;; Insert state

(helix-keymap-set nil 'insert "<escape>" #'helix-normal-state)

;;; Motion state


(provide 'helix-keybindings)
;;; helix-keybindings.el ends here
