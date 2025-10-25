;;; helix-keybindings.el --- Helix keybindings -*- lexical-binding: t; -*-
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
;;  Helix default keybindings.
;;
;;; Code:

(require 'helix-commands)
(require 'helix-scrolling)
(require 'helix-core)

;;; Universal argument

(defun helix--setup-universal-argument-keys-h ()
  "Rebind `universal-argument' to `M-u' since `C-u' is used for scrolling.
By default `M-u' is bound to `upcase-word', so we can use it."
  (if helix-mode
      (progn
        (keymap-global-set "M-u" #'universal-argument)
        (keymap-set universal-argument-map "M-u" #'universal-argument-more)
        ;; Unbind `C-u' so that \\[universal-argument] links in help buffers
        ;; are displayed as `M-u'.
        (keymap-global-unset "C-u" t)
        (keymap-unset universal-argument-map "C-u" t))
    ;; else
    (keymap-global-set "C-u" #'universal-argument)
    (keymap-set universal-argument-map "C-u" #'universal-argument-more)
    (keymap-global-set "M-u" #'upcase-word)
    (keymap-unset universal-argument-map "M-u" t)))

(add-hook 'helix-mode-hook #'helix--setup-universal-argument-keys-h)

;;; Normal state

(keymap-global-unset "M-<down-mouse-1>")
(helix-keymap-global-set :state 'normal
  "M-<mouse-1>" #'helix-toggle-cursor-on-click)

(helix-keymap-global-set :state 'normal
  ":" #'execute-extended-command

  ;; Arrows
  "<left>"  #'helix-backward-char
  "<down>"  #'helix-next-line
  "<up>"    #'helix-previous-line
  "<right>" #'helix-forward-char

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
  "f"   #'helix-find-char-forward
  "F"   #'helix-find-char-backward
  "t"   #'helix-till-char-forward
  "T"   #'helix-till-char-backward
  "g s" #'helix-beginning-of-line-command
  "g h" #'helix-first-non-blank
  "g l" #'helix-end-of-line-command
  "g g" #'helix-beginning-of-buffer
  "G"   #'helix-end-of-buffer
  "}"   #'helix-forward-paragraph
  "{"   #'helix-backward-paragraph
  "] p" #'helix-forward-paragraph-end
  "[ p" #'helix-backward-paragraph-end
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
  "<"   #'helix-indent-left
  ">"   #'helix-indent-right

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
  "C-;" #'helix-exchange-point-and-mark
  "M-;" #'helix-exchange-point-and-mark
  "g ;" #'helix-exchange-point-and-mark
  "_"   #'helix-trim-whitespaces-from-selection
  "&"   #'helix-align-selections

  ;; Surround
  "m m" #'helix-jump-to-match-item
  "m s" #'helix-surround
  "m d" #'helix-surround-delete
  "m r" #'helix-surround-change

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
  "."     #'repeat
  "C-o"   #'helix-backward-mark-ring
  "C-<i>" #'helix-forward-mark-ring
  "C-S-o" #'helix-backward-global-mark-ring
  "C-S-i" #'helix-forward-global-mark-ring
  "g a"   #'describe-char
  "g c"   #'comment-dwim
  "g i"   #'imenu
  "g f"   #'find-file-at-point
  "g x"   #'browse-url-at-point
  "g q"   #'fill-region
  "g Q"   #'fill-region-as-paragraph
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

;;;; Mark commands

(helix-define-command helix-mark-digit-argument (arg)
  "Like `digit-argument' but keep `m' prefix key active."
  :multiple-cursors nil
  (interactive "P")
  (digit-argument arg)
  (set-transient-map (keymap-lookup nil "m")))

(helix-define-command helix-mark-negative-argument (arg)
  "Like `negative-argument' but keep `m' prefix key active."
  :multiple-cursors nil
  (interactive "P")
  (negative-argument arg)
  (set-transient-map (keymap-lookup nil "m")))

;; Do not show keys bound to `helix-mark-digit-argument' and
;; `helix-mark-negative-argument' commands in which-key popup.
(with-eval-after-load 'which-key
  (dolist (cmd '(helix-mark-digit-argument
                 helix-mark-negative-argument))
    (cl-pushnew `((nil . ,(symbol-name cmd)) . ignore)
                which-key-replacement-alist :test #'equal)))

(helix-keymap-global-set :state 'normal
  "m -" #'helix-mark-negative-argument
  "m 0" #'helix-mark-digit-argument
  "m 1" #'helix-mark-digit-argument
  "m 2" #'helix-mark-digit-argument
  "m 3" #'helix-mark-digit-argument
  "m 4" #'helix-mark-digit-argument
  "m 5" #'helix-mark-digit-argument
  "m 6" #'helix-mark-digit-argument
  "m 7" #'helix-mark-digit-argument
  "m 8" #'helix-mark-digit-argument
  "m 9" #'helix-mark-digit-argument

  "m w"   #'helix-mark-inner-word
  "m i w" #'helix-mark-inner-word
  "m a w" #'helix-mark-a-word
  "m W"   #'helix-mark-inner-WORD
  "m i W" #'helix-mark-inner-WORD
  "m a W" #'helix-mark-a-WORD
  ;; sentence
  "m ."   #'helix-mark-inner-sentence
  "m i ." #'helix-mark-inner-sentence
  "m a ." #'helix-mark-a-sentence
  "m i s" #'helix-mark-inner-sentence
  "m a s" #'helix-mark-a-sentence
  ;; function
  "m f"   #'helix-mark-inner-function
  "m i f" #'helix-mark-inner-function
  "m a f" #'helix-mark-a-function
  ;; paragraph
  "m p"   #'helix-mark-inner-paragraph
  "m i p" #'helix-mark-inner-paragraph
  "m a p" #'helix-mark-a-paragraph

  "m \""   #'helix-mark-inner-double-quoted
  "m i \"" #'helix-mark-inner-double-quoted
  "m a \"" #'helix-mark-a-double-quoted
  "m '"    #'helix-mark-inner-single-quoted
  "m i '"  #'helix-mark-inner-single-quoted
  "m a '"  #'helix-mark-a-single-quoted
  "m `"    #'helix-mark-inner-back-quoted
  "m i `"  #'helix-mark-inner-back-quoted
  "m a `"  #'helix-mark-a-back-quoted

  "m ("   #'helix-mark-inner-paren
  "m )"   #'helix-mark-inner-paren
  "m i (" #'helix-mark-inner-paren
  "m i )" #'helix-mark-inner-paren
  "m a (" #'helix-mark-a-paren
  "m a )" #'helix-mark-a-paren

  "m ["   #'helix-mark-inner-bracket
  "m ]"   #'helix-mark-inner-bracket
  "m i [" #'helix-mark-inner-bracket
  "m i ]" #'helix-mark-inner-bracket
  "m a [" #'helix-mark-a-bracket
  "m a ]" #'helix-mark-a-bracket

  "m {"   #'helix-mark-inner-curly
  "m }"   #'helix-mark-inner-curly
  "m i {" #'helix-mark-inner-curly
  "m i }" #'helix-mark-inner-curly
  "m a {" #'helix-mark-a-curly
  "m a }" #'helix-mark-a-curly

  "m <"   #'helix-mark-inner-angle
  "m >"   #'helix-mark-inner-angle
  "m i <" #'helix-mark-inner-angle
  "m i >" #'helix-mark-inner-angle
  "m a <" #'helix-mark-an-angle
  "m a >" #'helix-mark-an-angle

  "m !"   #'helix-mark-inner-surround
  "m @"   #'helix-mark-inner-surround
  "m #"   #'helix-mark-inner-surround
  "m $"   #'helix-mark-inner-surround
  "m %"   #'helix-mark-inner-surround
  "m ^"   #'helix-mark-inner-surround
  "m &"   #'helix-mark-inner-surround
  "m *"   #'helix-mark-inner-surround
  "m ~"   #'helix-mark-inner-surround
  "m ="   #'helix-mark-inner-surround
  "m _"   #'helix-mark-inner-surround

  "m i !" #'helix-mark-inner-surround
  "m i @" #'helix-mark-inner-surround
  "m i #" #'helix-mark-inner-surround
  "m i $" #'helix-mark-inner-surround
  "m i %" #'helix-mark-inner-surround
  "m i ^" #'helix-mark-inner-surround
  "m i &" #'helix-mark-inner-surround
  "m i *" #'helix-mark-inner-surround
  "m i ~" #'helix-mark-inner-surround
  "m i =" #'helix-mark-inner-surround
  "m i _" #'helix-mark-inner-surround

  "m a !" #'helix-mark-a-surround
  "m a @" #'helix-mark-a-surround
  "m a #" #'helix-mark-a-surround
  "m a $" #'helix-mark-a-surround
  "m a %" #'helix-mark-a-surround
  "m a ^" #'helix-mark-a-surround
  "m a &" #'helix-mark-a-surround
  "m a *" #'helix-mark-a-surround
  "m a ~" #'helix-mark-a-surround
  "m a =" #'helix-mark-a-surround
  "m a _" #'helix-mark-a-surround)

;;;; Windows

(helix-keymap-global-set :state 'normal
  "C-w" 'helix-window-map)

(helix-keymap-set helix-window-map
  ;; windows
  "RET" #'same-window-prefix
  "n"   #'other-window-prefix
  "s"   #'helix-window-split
  "v"   #'helix-window-vsplit
  "S"   #'helix-root-window-split
  "V"   #'helix-root-window-vsplit
  "c"   #'helix-window-delete
  "o"   #'delete-other-windows
  "p"   #'toggle-window-dedicated

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
  "z"   #'bury-buffer ; mnemonics: "z" is the last letter
  "x"   #'scratch-buffer
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
  "C-p" #'toggle-window-dedicated
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
  "C-b" #'clone-indirect-buffer-other-window
  "C-x" #'scratch-buffer
  "C-z" #'bury-buffer)

;;; Motion state

(helix-keymap-global-set :state 'motion
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

(helix-keymap-global-set :state 'insert
  "<escape>" #'helix-normal-state
  "C-w" #'helix-delete-backward-word)

;;; Conditional keybindings

(when helix-want-zz-scroll-to-center
  (helix-keymap-global-set :state '(normal motion)
    "z z" #'helix-smooth-scroll-line-to-center))

(provide 'helix-keybindings)
;;; helix-keybindings.el ends here
