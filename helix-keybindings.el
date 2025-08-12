;;; helix-keybindings.el --- Helix keybindings -*- lexical-binding: t; -*-
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
;;  Helix default keybindings.
;;
;;; Code:

(require 'helix-commands)
(require 'helix-scrolling)
(require 'helix-core)
(require 'keypad)

;;; Universal argument

;; Bind `universal-argument' to `M-u' since `C-u' is used for scrolling.
;; By default `M-u' is binded to `upcase-word'.
(keymap-global-set "M-u" #'universal-argument)
(keymap-set universal-argument-map "M-u" #'universal-argument-more)
;; (helix-keymap-set nil 'normal "M-u" #'universal-argument)
;; (helix-keymap-set nil 'motion "M-u" #'universal-argument)

;;; Normal state

(keymap-global-unset "M-<down-mouse-1>")
(helix-keymap-set nil 'normal "M-<mouse-1>" #'helix-toggle-cursor-on-click)

(helix-keymap-set nil 'normal
  "<backspace>" #'execute-extended-command
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
  "<left>"  #'helix-backward-char
  "<down>"  #'helix-next-line
  "<up>"    #'helix-previous-line
  "<right>" #'helix-forward-char
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
  "f"   #'helix-find-char-forward
  "F"   #'helix-find-char-backward
  "t"   #'helix-till-char-forward
  "T"   #'helix-till-char-backward
  "g s" #'helix-beginning-of-line-command
  "g h" #'helix-first-non-blank
  "g l" #'helix-end-of-line-command
  "g g" #'helix-goto-first-line
  "G"   #'helix-goto-last-line
  "] p" #'helix-forward-paragraph
  "[ p" #'helix-backward-paragraph
  "}"   #'helix-forward-paragraph
  "{"   #'helix-backward-paragraph
  "] f" #'helix-forward-function
  "[ f" #'helix-backward-function
  "] s" #'helix-forward-sentence
  "[ s" #'helix-backward-sentence
  "] ." #'helix-forward-sentence
  "[ ." #'helix-backward-sentence

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
  "I"   #'helix-insert-line
  "A"   #'helix-append-line
  "o"   #'helix-open-below
  "O"   #'helix-open-above
  "c"   #'helix-change
  "d"   #'helix-cut
  "D"   #'helix-delete
  "u"   #'helix-undo
  "U"   #'helix-redo
  "y"   #'helix-copy
  "p"   #'helix-paste-after
  "P"   #'helix-paste-before
  "R"   #'helix-replace-with-kill-ring
  "C-p" #'helix-paste-pop ;; yank-pop
  "C-n" #'helix-paste-undo-pop
  "J"   #'helix-join-line
  "`"   #'helix-downcase
  "M-`" #'helix-upcase
  "~"   #'helix-invert-case
  "g u" #'helix-downcase
  "g U" #'helix-upcase
  "="   #'indent-region
  "<"   #'indent-rigidly-left
  ">"   #'indent-rigidly-right

  ;; Selections
  "<escape>" #'helix-normal-state-escape
  "v"   #'helix-extend-selection
  "x"   #'helix-expand-line-selection
  "X"   #'helix-contract-line-selection
  "%"   #'mark-whole-buffer
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
  ","   #'helix-delete-all-fake-cursors
  "M-," #'helix-remove-main-cursor
  "M--" #'helix-merge-selections
  ";"   #'helix-collapse-selection
  "M-;" #'exchange-point-and-mark
  "g ;" #'exchange-point-and-mark
  "_"   #'helix-trim-whitespaces-from-selection
  "&"   #'helix-align-selections

  ;; Search
  "/"   #'helix-search-forward
  "?"   #'helix-search-backward
  "*"   #'helix-construct-search-pattern
  "M-*" #'helix-construct-search-pattern-no-bounds
  "n"   #'helix-search-next
  "N"   #'helix-search-previous

  "m" (helix-define-keymap-with-digit-argument helix-mark-map
        ;; Surround
        "m" #'helix-jump-to-match-item
        "s" #'helix-surround
        "d" #'helix-surround-delete
        "r" #'helix-surround-change

        ;; Mark
        "w"   #'helix-mark-inner-word
        "i w" #'helix-mark-inner-word
        "a w" #'helix-mark-a-word
        "W"   #'helix-mark-inner-WORD
        "i W" #'helix-mark-inner-WORD
        "a W" #'helix-mark-a-WORD
        "i s" #'helix-mark-inner-sentence
        "a s" #'helix-mark-a-sentence
        "f"   #'helix-mark-inner-function
        "i f" #'helix-mark-inner-function
        "a f" #'helix-mark-a-function
        "p"   #'helix-mark-inner-paragraph
        "i p" #'helix-mark-inner-paragraph
        "a p" #'helix-mark-a-paragraph

        "\""   #'helix-mark-inner-double-quoted
        "i \"" #'helix-mark-inner-double-quoted
        "a \"" #'helix-mark-a-double-quoted
        "'"    #'helix-mark-inner-single-quoted
        "i '"  #'helix-mark-inner-single-quoted
        "a '"  #'helix-mark-a-single-quoted
        "`"    #'helix-mark-inner-back-quoted
        "i `"  #'helix-mark-inner-back-quoted
        "a `"  #'helix-mark-a-back-quoted

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

        "!"   #'helix-mark-inner-surround
        "@"   #'helix-mark-inner-surround
        "#"   #'helix-mark-inner-surround
        "$"   #'helix-mark-inner-surround
        "%"   #'helix-mark-inner-surround
        "^"   #'helix-mark-inner-surround
        "&"   #'helix-mark-inner-surround
        "*"   #'helix-mark-inner-surround
        "~"   #'helix-mark-inner-surround
        "="   #'helix-mark-inner-surround
        "_"   #'helix-mark-inner-surround

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
        "a _" #'helix-mark-a-surround)

  ;; Misc
  "C-o"   #'pop-to-mark-command
  "g c"   #'comment-dwim
  "g f"   #'find-file-at-point
  "g x"   #'browse-url-at-point
  "g q"   #'fill-region
  "] SPC" #'helix-add-blank-line-below
  "[ SPC" #'helix-add-blank-line-above
  "z n"   #'narrow-to-region
  "z w"   #'widen

  ;; Xref
  "g d"     #'xref-find-definitions
  "C-w g d" #'xref-find-definitions-other-window
  "g D"     #'xref-find-references
  "[ x"     #'xref-go-back
  "] x"     #'xref-go-forward)

;;;; Windows

(helix-keymap-set nil 'normal
  "C-w" 'helix-window-map)

(define-prefix-command 'helix-window-map)
(helix-keymap-set helix-window-map nil
  "s"   #'helix-window-split
  "v"   #'helix-window-vsplit
  "d"   #'helix-window-delete
  "c"   #'helix-window-delete
  "o"   #'delete-other-windows
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

;;; Keybindings common between Normal and Motion states

(dolist (state '(normal motion))
  (helix-keymap-set nil state
    "C-w" 'helix-window-map
    "<backspace>" #'execute-extended-command
    "] b" #'next-buffer
    "[ b" #'previous-buffer

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
    "z b" #'helix-smooth-scroll-line-to-bottom))

;;; Insert state

(helix-keymap-set nil 'insert
  "<escape>" #'helix-normal-state)

(provide 'helix-keybindings)
;;; helix-keybindings.el ends here
