;;; helix-keybindings.el --- Helix keybindings -*- lexical-binding: t; -*-
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
;;  Helix default keybindings.
;;
;;; Code:

(require 'helix-commands)
(require 'helix-scrolling)
(require 'helix-core)

;;; Universal argument

;; Bind `universal-argument' to `M-u' since `C-u' is used for scrolling.
;; By default `M-u' is binded to `upcase-word'.
(keymap-global-set "M-u" #'universal-argument)
(keymap-set universal-argument-map "M-u" #'universal-argument-more)
;; (helix-keymap-global-set 'normal "M-u" #'universal-argument)
;; (helix-keymap-global-set 'motion "M-u" #'universal-argument)

;;; Normal state

(keymap-global-unset "M-<down-mouse-1>")
(helix-keymap-global-set 'normal "M-<mouse-1>" #'helix-toggle-cursor-on-click)

(helix-keymap-global-set 'normal
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
  "g g" #'helix-beginning-of-buffer
  "G"   #'helix-end-of-buffer
  "] p" #'helix-mark-paragraph-forward
  "[ p" #'helix-mark-paragraph-backward
  "}"   #'helix-mark-paragraph-forward
  "{"   #'helix-mark-paragraph-backward
  "] f" #'helix-mark-function-forward
  "[ f" #'helix-mark-function-backward
  "] s" #'helix-mark-sentence-forward
  "[ s" #'helix-mark-sentence-backward
  "] ." #'helix-mark-sentence-forward
  "[ ." #'helix-mark-sentence-backward
  "] e" #'next-error
  "[ e" #'previous-error

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
  "X"   #'helix-expand-line-selection-backward
  "%"   #'helix-mark-whole-buffer
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
  "M-;" #'helix-exchange-point-and-mark
  "g ;" #'helix-exchange-point-and-mark
  "_"   #'helix-trim-whitespaces-from-selection
  "&"   #'helix-align-selections

  ;; Search
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
  "z z" #'helix-smooth-scroll-line-not-to-very-top
  "z t" #'helix-smooth-scroll-line-to-top
  "z b" #'helix-smooth-scroll-line-to-bottom

  ;; Misc
  "C-o"   #'helix-backward-mark-ring
  "C-<i>" #'helix-forward-mark-ring
  "C-S-o" #'helix-backward-global-mark-ring
  "C-S-i" #'helix-forward-global-mark-ring
  "g c"   #'comment-dwim
  "g i"   #'imenu
  "g f"   #'find-file-at-point
  "g x"   #'browse-url-at-point
  "g q"   #'fill-region
  "] b"   #'next-buffer
  "[ b"   #'previous-buffer
  "] SPC" #'helix-add-blank-line-below
  "[ SPC" #'helix-add-blank-line-above

  ;; Narrow to region
  "z n" #'helix-narrow-to-region-indirectly
  "z w" #'helix-widen-indirectly-narrowed

  ;; Xref
  "g d" #'xref-find-definitions
  "g D" #'xref-find-references
  "[ x" #'xref-go-back
  "] x" #'xref-go-forward)

;;;; `m' keybindings

(helix-define-command helix-mark-digit-argument (arg)
  "Like `digit-argument' but keep `m' prefix key active."
  :multiple-cursors nil
  (interactive "P")
  (digit-argument arg)
  (set-transient-map (keymap-lookup nil "m")))

(helix-define-command helix-mark-negative-argument (arg)
  :multiple-cursors nil
  (interactive "P")
  (negative-argument arg)
  (set-transient-map (keymap-lookup nil "m")))

;; Do not show keys binded to `helix-mark-digit-argument' and
;; `helix-mark-negative-argument' commands in which-key popup.
(with-eval-after-load 'which-key
  (defvar which-key-replacement-alist)
  (dolist (elt '(((nil . "helix-mark-digit-argument") . ignore)
                 ((nil . "helix-mark-negative-argument") . ignore)))
    (cl-pushnew elt which-key-replacement-alist :test #'equal)))

(helix-keymap-global-set 'normal
  "m" #'helix-mark-map)

(helix-keymap-set helix-mark-map nil
  "-" #'helix-mark-negative-argument
  "0" #'helix-mark-digit-argument
  "1" #'helix-mark-digit-argument
  "2" #'helix-mark-digit-argument
  "3" #'helix-mark-digit-argument
  "4" #'helix-mark-digit-argument
  "5" #'helix-mark-digit-argument
  "6" #'helix-mark-digit-argument
  "7" #'helix-mark-digit-argument
  "8" #'helix-mark-digit-argument
  "9" #'helix-mark-digit-argument

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
  ;; sentence
  "."   #'helix-mark-inner-sentence
  "i ." #'helix-mark-inner-sentence
  "a ." #'helix-mark-a-sentence
  "i s" #'helix-mark-inner-sentence
  "a s" #'helix-mark-a-sentence
  ;; function
  "f"   #'helix-mark-inner-function
  "i f" #'helix-mark-inner-function
  "a f" #'helix-mark-a-function
  ;; paragraph
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

;;;; Windows

(helix-keymap-global-set 'normal
  "C-w" 'helix-window-map)

(helix-keymap-set helix-window-map nil
  ;; windows
  "n"   #'other-window-prefix
  "s"   #'helix-window-split
  "v"   #'helix-window-vsplit
  "S"   #'helix-root-window-split
  "V"   #'helix-root-window-vsplit
  "c"   #'helix-window-delete
  "o"   #'delete-other-windows

  "w"   #'other-window
  "h"   #'helix-window-left
  "j"   #'helix-window-down
  "k"   #'helix-window-up
  "l"   #'helix-window-right

  "H"   #'helix-move-window-left
  "J"   #'helix-move-window-down
  "K"   #'helix-move-window-up
  "L"   #'helix-move-window-right

  ;; buffers
  "r"   #'revert-buffer
  "d"   #'kill-current-buffer
  "q"   #'helix-kill-current-buffer-and-window
  "b"   #'clone-indirect-buffer-other-window
  "B"   #'helix-clone-indirect-buffer-same-window
  ;; xref
  "g d" #'xref-find-definitions-other-window

  ;; Duplicate all keys with ctrl prefix.
  "C-n" #'other-window-prefix
  "C-s" #'helix-window-split
  "C-v" #'helix-window-vsplit
  "C-S" #'helix-root-window-split
  "C-V" #'helix-root-window-vsplit
  "C-c" #'helix-window-delete
  "C-o" #'delete-other-windows
  ;; Jump over windows
  "C-w" #'other-window
  "C-h" #'helix-window-left
  "C-j" #'helix-window-down
  "C-k" #'helix-window-up
  "C-l" #'helix-window-right
  ;; buffers
  "C-r" #'revert-buffer
  "C-d" #'kill-current-buffer
  "C-q" #'helix-kill-current-buffer-and-window
  "C-b" #'clone-indirect-buffer-other-window)

;;; Motion state

(helix-keymap-global-set 'motion
  "C-w" 'helix-window-map
  "] b" #'next-buffer
  "[ b" #'previous-buffer

  ;; Scrolling
  "C-b" #'helix-smooth-scroll-page-up
  "C-f" #'helix-smooth-scroll-page-down
  "C-d" #'helix-smooth-scroll-down
  "C-u" #'helix-smooth-scroll-up
  "C-e" #'helix-mix-scroll-line-down
  "C-y" #'helix-mix-scroll-line-up
  "z z" #'helix-smooth-scroll-line-not-to-very-top
  "z t" #'helix-smooth-scroll-line-to-top
  "z b" #'helix-smooth-scroll-line-to-bottom)

;;; Insert state

(helix-keymap-global-set 'insert
  "<escape>" #'helix-normal-state
  "C-w" #'helix-delete-backward-word)

;;; Conditional keybindings

(when helix-want-helix-leader
  (require 'helix-leader)
  (dolist (state '(normal motion))
    (helix-keymap-global-set state
      "SPC"      #'helix-leader
      "C-w SPC"  #'helix-leader-other-window
      "C-h k"    #'helix-leader-describe-key
      "<f1> k"   #'helix-leader-describe-key
      "<help> k" #'helix-leader-describe-key)))

(when helix-want-zz-scroll-to-center
  (dolist (state '(normal motion))
    (helix-keymap-global-set state
      "z z" #'helix-smooth-scroll-line-to-center)))

(when helix-want-paragraph-motions-like-in-Helix
  (helix-keymap-global-set 'normal
    "] p" #'helix-mark-forward-to-beginning-of-paragraph
    "[ p" #'helix-mark-backward-to-beginning-of-paragraph
    "}"   #'helix-mark-forward-to-beginning-of-paragraph
    "{"   #'helix-mark-backward-to-beginning-of-paragraph
    "] f" #'helix-mark-forward-to-beginning-of-function
    "[ f" #'helix-mark-backward-to-beginning-of-function))

(provide 'helix-keybindings)
;;; helix-keybindings.el ends here
