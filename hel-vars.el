;;; hel-vars.el --- Settings and variables -*- lexical-binding: t; -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/anuvyklack/hel.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;;
;;; Hel settings and variables.
;;;
;;; Code:

(require 'hel-macros)

(defvar hel-mode nil)
(declare-function hel-local-mode "hel-core")

;;; Customizable variables

(defgroup hel nil
  "Hel emulation."
  :group 'emulations
  :prefix 'hel-)

(defcustom hel-want-minibuffer t
  "Whether to enable Hel in minibuffer(s)."
  :type 'boolean
  :group 'hel
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (and hel-mode value)
             (add-hook 'minibuffer-setup-hook #'hel-local-mode)
           (remove-hook 'minibuffer-setup-hook #'hel-local-mode))))

(defcustom hel-use-pcre-regex t
  "If non-nil use PCRE regexp syntax instead of Emacs one."
  :type 'integer
  :group 'hel)

(defcustom hel-regex-history-max 16
  "Maximum length of regexp search ring before oldest elements are thrown away."
  :type 'integer
  :group 'hel)

(defcustom hel-want-zz-scroll-to-center nil
  "If non-nil `zz` keybinding will scroll current line to center of the screen.
This variable must be set before Hel is loaded!"
  :type 'boolean
  :group 'hel)

(defcustom hel-want-C-hjkl-keys t
  "If non-nil, bind `C-h', `C-j', `C-k', `C-l' to commands for crawling the AST.
To access help commands, use `F1' instead of `C-h'.
AST stands for Abstract Syntax Tree.

These commands are also bound to `M-i', `M-n', `M-p', `M-o' for compatibility
with the Hel text editor.

This variable must be set before Hel is loaded!"
  :type 'boolean
  :group 'hel)

(defcustom hel-match-fake-cursor-style t
  "If non-nil, attempt to match the `cursor-type' that the user has selected.
We only can match `bar' and `box' types.
If nil, the `box' cursor type will be used for all fake cursors."
  :type 'boolean
  :group 'hel)

(defcustom hel-reactivate-selection-after-insert-state t
  "When non-nil, the selection will be reactivated on exiting Insert state if it
was active on entering it."
  :type 'boolean
  :group 'hel)

(defcustom hel-normal-state-cursor
  `(bar ,(face-attribute 'cursor :background))
  "Cursor apperance when Hel is in Norman state.
Can be a cursor type as per `cursor-type', a color string as passed to
`set-cursor-color', a zero-argument function for changing the cursor,
or a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'hel)

(defcustom hel-insert-state-cursor
  `(box ,(face-attribute 'cursor :background))
  "Cursor apperance when Hel is in Insert state.
Can be a cursor type as per `cursor-type', a color string as passed to
`set-cursor-color', a zero-argument function for changing the cursor,
or a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'hel)

(defcustom hel-motion-state-cursor '(hbar . 4)
  "Cursor apperance when Hel is in Motion state.
Can be a cursor type as per `cursor-type', a color string as passed to
`set-cursor-color', a zero-argument function for changing the cursor, or
a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'hel)

(defcustom hel-bar-fake-cursor ?\u2000
  "Character used as the fake cursor when `cursor-type' is `bar'
and `hel-match-fake-cursor-style' is non-nil.

The value must be either a character or a single-character string,
representing a narrow space. Recommended candidates are:
- ?\\u2000 EN QUAD (default value)
- ?\\u2002 EN SPACE
- ?\\u2009 THIN SPACE

The appearance depends heavily on:
1. Your main font.
2. The font Emacs selects for this narrow space — it should be
   a variable-pitch font. Use `describe-char' to verify the chosen font.

Note: This all is a hack since Emacs can't render two characters in one
cell. The bar fake cursor character is virtually inserted between cells,
shifting subsequent content to the right."
  :type '(set character string)
  :group 'hel
  :set (lambda (symbol value)
         (set symbol (cond ((characterp value)
                            (char-to-string value))
                           ((and (stringp value) (length= value 1))
                            value)
                           (t
                            (char-to-string ?\u2000))))))

(defcustom hel-multiple-cursors-mode-line-indicator
  #("  Cursors: %s " 1 14 (face hel-mode-line-cursors-indicator))
  "What to display in the mode line while `hel-multiple-cursors-mode' is active."
  :type '(choice string (const nil))
  :group 'hel)

(defcustom hel-whitelist-file (locate-user-emacs-file "hel-multiple-cursors")
  "File to save users preferences which commands to execute for one cursor
and which for all."
  :type 'file
  :group 'hel)

(defcustom hel-max-cursors-number nil
  "Safety ceiling for the number of active cursors.
If your Emacs slows down or freezes when using too many cursors,
customize this value appropriately.

Cursors will be added until this value is reached, at which point
you can either temporarily override the value or abort the
operation entirely.

If this value is nil, there is no ceiling."
  :type '(integer)
  :group 'hel)

(defvar hel-minor-modes-incompatible-with-multiple-cursors
  '(corfu-mode
    company-mode
    flyspell-mode
    prettify-symbols-mode)
  "List of minor-modes that will be temporarily disabled while there are more
then one cursor in the buffer.")

(defcustom hel-update-highlight-delay 0.02
  "Time in seconds of idle before updating search highlighting.
Setting this to a period shorter than that of keyboard's repeat
rate allows highlights to update while scrolling."
  :type 'number
  :group 'hel)

(defvar hel-keep-search-highlight-commands
  '(hel-extend-selection
    hel-rotate-selections-forward
    hel-rotate-selections-backward
    hel-search-next
    hel-search-previous
    ;; switch windows
    hel-window-split
    hel-window-vsplit
    hel-window-delete
    hel-window-delete
    hel-window-left
    hel-window-down
    hel-window-up
    hel-window-right
    hel-window-left
    hel-window-down
    hel-window-up
    hel-window-right
    hel-move-window-left
    hel-move-window-down
    hel-move-window-up
    hel-move-window-right
    ;; scrolling
    pixel-scroll-start-momentum)
  "List of commands which should preserve search highlighting overlays.")

(hel-defvar-local hel-surround-alist
  '((?\) :pair ("(" . ")") :balanced t)
    (?\} :pair ("{" . "}") :balanced t)
    (?\] :pair ("[" . "]") :balanced t)
    (?\> :pair ("<" . ">") :balanced t)
    (?\( :pair ("( " . " )")
         :lookup (lambda () (hel-4-bounds-of-brackets-at-point ?\( ?\))))
    (?\[ :pair ("[ " . " ]")
         :lookup (lambda () (hel-4-bounds-of-brackets-at-point ?\[ ?\])))
    (?\{ :pair ("{ " . " }")
         :lookup (lambda () (hel-4-bounds-of-brackets-at-point ?{ ?})))
    (?\< :pair ("< " . " >")
         :lookup (lambda () (hel-4-bounds-of-brackets-at-point ?< ?>))
         ;; or
         ;; :lookup ("<[[:blank:]\n]*" . "[[:blank:]\n]*>")
         ;; :regexp t
         ;; :balanced t
         )
    (?\" :pair ("\"" . "\"")
         :lookup (lambda ()
                   (-when-let ((beg . end) (hel-bounds-of-quoted-at-point ?\"))
                     (list beg (1+ beg) (1- end) end)))))
  "Association list with (KEY . SPEC) elements for Hel surrounding functionality.

This variable is buffer-local so that users can modify it from major-mode hooks.

KEY is a character, SPEC is a plist with following keys:

`:pair'    Cons cell (LEFT . RIGHT) with strings, or function that returns such
         cons cell. The strigs that will be inserted by `hel-surround' and
         `hel-surround-change' functions.

`:lookup'  Any of:

         1. Cons cell with strings (LEFT . RIGHT), or function that return
            such cons cell. LEFT and RIGHT should be patterns that will be
            used to search of two substrings to delete in `hel-surround-delete'
            and `hel-surround-change' functions. If not specified `:pair' value
            will be used.

         2. Function that returns list with 4 positions:

                     (LEFT-START LEFT-END RIGHT-START RIGHT-END)

            of START and END of LEFT and RIGHT delimeters.
            Example:
                       LEFT                              RIGHT
                     |<tag> |Lorem ipsum dolor sit amet| </tag>|
                     ^      ^                          ^       ^
            LEFT-START      LEFT-END         RIGHT-START       RIGHT-END

Following keys are taken into account only when `:lookup' argument is a cons
cell with strings (LEFT . RIGHT) or a function, that returns such cons cell.
If `:lookup' is a function that returns list with 4 positions, they will be
ignored.

`:regexp'    If non-nil then LEFT and RIGHT strings specified in `:lookup' will be
           treated as regexp patterns. Otherwise they will be searched literally.

`:balanced'  When non-nil all nested balanced LEFT RIGHT pairs will be skipped.
           Otherwise the first found pattern will be accepted.

See the default value for examples.")

(defgroup hel-cjk nil
  "CJK support."
  :prefix "hel-cjk-"
  :group 'hel)

;; (defcustom hel-cjk-emacs-word-boundary nil
;;   "Determine word boundary exactly the same way as Emacs does."
;;   :type 'boolean
;;   :group 'hel-cjk)

(defcustom hel-cjk-word-separating-categories
  '(;; Kanji
    (?C . ?H) (?C . ?K) (?C . ?k) (?C . ?A) (?C . ?G)
    ;; Hiragana
    (?H . ?C) (?H . ?K) (?H . ?k) (?H . ?A) (?H . ?G)
    ;; Katakana
    (?K . ?C) (?K . ?H) (?K . ?k) (?K . ?A) (?K . ?G)
    ;; half-width Katakana
    (?k . ?C) (?k . ?H) (?k . ?K) ; (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?C) (?A . ?H) (?A . ?K) ; (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?C) (?G . ?H) (?G . ?K) ; (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `hel-cjk-word-boundary-p'. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'hel-cjk)

(defcustom hel-cjk-word-combining-categories
  '(;; default value in word-combining-categories
    (nil . ?^) (?^ . nil)
    ;; Roman
    (?r . ?k) (?r . ?A) (?r . ?G)
    ;; half-width Katakana
    (?k . ?r) (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?r) (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?r) (?G . ?k) (?G . ?A))
  "List of pair (cons) of categories to determine word boundary
used in `hel-cjk-word-boundary-p'. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'hel-cjk)

;;; Faces

(defface hel-normal-state-fake-cursor
  `((t ( :height ,(window-default-font-height)
         :background "red")))
  "The face used for fake cursors when Hel is in Normal state."
  :group 'hel)

(defface hel-insert-state-fake-cursor
  '((t ( :foreground "white"
         :background "SkyBlue3")))
  "The face used for fake cursors when Hel is in Insert state."
  :group 'hel)

(defface hel-extend-selection-cursor
  `((t ( :height ,(window-default-font-height)
         :background "orange")))
  "The face used for cursors when extending selection is active."
  :group 'hel)

(defface hel-search-highlight '((t :inherit lazy-highlight))
  "Face for lazy highlighting all matches during search."
  :group 'hel)

(defface hel-mode-line-cursors-indicator '((t :inherit warning))
  "Face for indicator with active cursors number in mode lilne."
  :group 'hel)

;;; Variables

(push (expand-file-name "extensions" (file-name-directory load-file-name))
      load-path)

(hel-defvar-local hel-mode-map-alist nil
  "Association list of keymaps for current Hel state.
This symbol lies in `emulation-mode-map-alists' and its contents are updated
every time the Hel state changes.  Elements have the form (MODE . KEYMAP),
with the first keymaps having higher priority.")

(hel-defvar-local hel-state nil
  "The current Hel state.")

(hel-defvar-local hel-previous-state nil
  "The previous Hel state.")

(defvar hel-state-properties nil
  "Specifications made by `hel-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `hel-state-property'.")

(hel-defvar-local hel-input-method nil
  "Input method used in Hel Insert state.")

(hel-defvar-local hel-overriding-local-map nil)

(hel-defvar-local hel--extend-selection nil
  "Extend selection.")

(hel-defvar-local hel-selection-history nil
  "The history of selections.")

(hel-defvar-local hel--insert-pos nil
  "The location of the point where we last time switched to Insert state.")

(hel-defvar-local hel--region-was-active-on-insert nil
  "Whether region was active when we last time switched to Insert state.")

(hel-defvar-local hel-scroll-count 0
  "Hold last used prefix for `hel-smooth-scroll-up' and
`hel-smooth-scroll-down' commands.
Determine how many lines should be scrolled.
Default value is 0 - scroll half the screen.")

(defvar hel-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")
(fset 'hel-window-map hel-window-map)

(defvar hel-regex-history nil
  "List with used regexp patterns.")

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'hel-regex-history))

(defvar hel-undo-commands '(hel-undo hel-redo undo undo-redo)
  "Commands that implement undo/redo functionality.")

(hel-defvar-local hel--cursors-table
  (make-hash-table :test 'eql :weakness t)
  "Table mapping fake cursors IDs to cursors overlays.")

(defvar hel--fake-cursor-last-used-id 0
  "Last used fake cursor ID.")

(defvar hel-commands-to-run-for-all-cursors nil
  "Commands to execute for all cursors.")

(defvar hel-commands-to-run-once nil
  "Commands to execute only once while multiple cursors are active.")

(defvar hel-fake-cursor-specific-vars
  '(transient-mark-mode ;; for (region-active-p)
    mark-active
    mark-ring
    kill-ring
    kill-ring-yank-pointer
    yank-undo-function
    temporary-goal-column
    hel--extend-selection
    hel--newline-at-eol
    ;; Dabbrev
    dabbrev--abbrev-char-regexp
    dabbrev--check-other-buffers
    dabbrev--friend-buffer-list
    dabbrev--last-abbrev-location
    dabbrev--last-abbreviation
    dabbrev--last-buffer
    dabbrev--last-buffer-found
    dabbrev--last-direction
    dabbrev--last-expansion
    dabbrev--last-expansion-location
    dabbrev--last-table)
  "A list of vars that need to be tracked on a per-cursor basis.")

(defvar hel--whitelist-file-loaded nil
  "Non-nil when `hel-whitelist-file' file has already been loaded.")

(hel-defvar-local hel-this-command nil
  "Like `this-command' but for fake cursors.
The command that that will be executed for each fake cursor.")

(hel-defvar-local hel-executing-command-for-fake-cursor nil
  "Non-nil if `this-command' is currently executing for fake cursor.")

(hel-defvar-local hel--temporarily-disabled-minor-modes nil
  "The list of temporarily disabled minor-modes while there are
multiple cursors.")

(hel-defvar-local hel--in-single-undo-step nil
  "Non-nil while we are in the single undo step.")

(hel-defvar-local hel--undo-list-pointer nil
  "Stores the start of the current undo step in `buffer-undo-list'.")

(hel-defvar-local hel--undo-boundary nil)

(hel-defvar-local hel--input-cache nil)

(hel-defvar-local hel--newline-at-eol nil
  "Non-nil when newline char at the end of the line should be considered a part
of the region (selection).

In Emacs, selecting a newline character at the end of a line moves point to the
next line. This contradicts Hel's and Vim's text editors behavior. We emulate
their behavior, by keeping the point at the end of the line and and set this
flag. To take it into account use `hel-restore-newline-at-eol' function.")

(hel-defvar-local hel-main-region-overlay nil
  "An overlay with region face that covers active region plus one newline
character after, to visually extend selection over full line while point remains
at the end of the line. Conterpart to `hel--newline-at-eol' flag.")

(hel-defvar-local hel--narrowed-base-buffer nil)

(provide 'hel-vars)
;;; hel-vars.el ends here
