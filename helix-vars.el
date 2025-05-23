;;; helix-vars.el --- Settings and variables -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

;;; Customization group

(defgroup helix nil
  "Helix emulation."
  :group 'emulations
  :prefix 'helix-)

(defcustom helix-select-on-insert t
  "Select inserted text on exiting Insert state."
  :type 'boolean
  :group 'helix)

(defcustom helix-use-pcre-regex t
  "Maximum length of regexp search ring before oldest elements are thrown away."
  :type 'integer
  :group 'helix)

(defcustom helix-regex-history-max 16
  "Maximum length of regexp search ring before oldest elements are thrown away."
  :type 'integer
  :group 'helix)

(defcustom helix-mc-match-cursor-style nil
  "If non-nil, attempt to match the cursor style that the user
has selected. Namely, use vertical bars the user has configured
Emacs to use that cursor.

If nil, just use standard rectangle cursors for all fake cursors.

In some modes/themes, the bar fake cursors are either not
rendered or shift text."
  :type '(boolean)
  :group 'helix)

(defface helix-fake-cursor
  '((t (:inverse-video t)))
  "The face used for fake cursors."
  :group 'helix)

(defface helix-fake-cursor-bar
  `((t (:height 1 :background ,(face-attribute 'cursor :background))))
  "The face used for fake cursors if the cursor-type is bar."
  :group 'helix)

;; (defface helix-search '((t :inherit isearch))
;;   "Face for interactive search."
;;   :group 'helix)

(defface helix-lazy-highlight '((t :inherit lazy-highlight))
  "Face for highlighting all matches in interactive search."
  :group 'helix)

(defface helix-region-face
  '((t :inherit region))
  "The face used for fake regions."
  :group 'helix)

(defcustom helix-mc-mode-line
  `(" mc:" (:eval (format ,(propertize "%d" 'face 'font-lock-warning-face)
                          (helix-number-of-cursors))))
  "What to display in the mode line while `helix-multiple-cursors-mode' is active."
  :type '(sexp)
  :group 'helix)

(put 'helix-mc-mode-line 'risky-local-variable t)

(defcustom helix-mc-list-file (locate-user-emacs-file ".helix-mc-lists.el")
  "File to save users preferences which commands to execute for one cursor
and which for all."
  :type 'file
  :group 'helix)

(defcustom helix-max-cursors nil
  "Safety ceiling for the number of active cursors.
If your Emacs slows down or freezes when using too many cursors,
customize this value appropriately.

Cursors will be added until this value is reached, at which point
you can either temporarily override the value or abort the
operation entirely.

If this value is nil, there is no ceiling."
  :type '(integer)
  :group 'helix)

(defcustom helix-mc-always-run-for-all nil
  "Disables whitelisting and always executes commands for every fake cursor."
  :type '(boolean)
  :group 'helix)

(defcustom helix-mc-always-repeat-command nil
  "Disables confirmation for `helix-mc-repeat-command' command."
  :type '(boolean)
  :group 'helix)

(defvar helix-mc-unsupported-minor-modes '(company-mode
                                           auto-complete-mode
                                           flyspell-mode
                                           jedi-mode)
  "List of minor-modes that does not work well with multiple cursors.
They are temporarily disabled when there are multiple cursors.")

(defcustom helix-update-highlight-delay 0.02
  "Time in seconds of idle before updating search highlighting.
Setting this to a period shorter than that of keyboard's repeat
rate allows highlights to update while scrolling."
  :type 'number
  :group 'helix)

(defvar helix-keep-search-highlight-commands
  '(helix-extend-selection
    helix-rotate-selections-forward
    helix-rotate-selections-backward
    helix-search-next
    helix-search-previous
    ;; scrolling
    helix-smooth-scroll-page-up
    helix-smooth-scroll-page-down
    helix-smooth-scroll-down
    helix-smooth-scroll-up
    helix-mix-scroll-line-down
    helix-mix-scroll-line-up
    helix-scroll-line-down
    helix-scroll-line-up
    helix-smooth-scroll-line-to-center
    helix-smooth-scroll-line-not-to-very-top
    helix-smooth-scroll-line-to-top
    helix-smooth-scroll-line-to-bottom
    pixel-scroll-precision
    pixel-scroll-start-momentum
    ;; switch windows
    helix-window-split
    helix-window-vsplit
    helix-window-delete
    helix-window-delete
    helix-window-left
    helix-window-down
    helix-window-up
    helix-window-right
    helix-window-left
    helix-window-down
    helix-window-up
    helix-window-right
    helix-move-window-left
    helix-move-window-down
    helix-move-window-up
    helix-move-window-right)
  "List of commands which should preserve search highlight overlays.")

(defgroup helix-cjk nil
  "CJK support."
  :prefix "helix-cjk-"
  :group 'helix)

(defcustom helix-cjk-emacs-word-boundary nil
  "Determine word boundary exactly the same way as Emacs does."
  :type 'boolean
  :group 'helix-cjk)

(defcustom helix-cjk-word-separating-categories
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
used in `helix-cjk-word-boundary-p'. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '(alist :key-type (choice character (const nil))
          :value-type (choice character (const nil)))
  :group 'helix-cjk)

(defcustom helix-cjk-word-combining-categories
  '(;; default value in word-combining-categories
    (nil . ?^) (?^ . nil)
    ;; Roman
    (?r . ?k) (?r . ?A) (?r . ?G)
    ;; half-width Katakana
    (?k . ?r) (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?r) (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?r) (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `helix-cjk-word-boundary-p'. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '(alist :key-type (choice character (const nil))
          :value-type (choice character (const nil)))
  :group 'helix-cjk)

;;; Variables

(defmacro helix-defvar-local (symbol &optional initvalue docstring)
  "The same as `defvar-local' but additionaly marks SYMBOL as permanent
buffer local variable."
  (declare (indent defun)
           (doc-string 3)
           (debug (symbolp &optional form stringp)))
  `(progn
     (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

(defvar helix-global-keymaps-alist nil
  "Association list of global Helix keymaps.
Entries have the form (STATE . KEYMAP), where STATE is a Helix state.")

(helix-defvar-local helix-mode-map-alist nil
  "Association list of keymaps for current Helix state.

This symbol lies in `emulation-mode-map-alists' and its contents are updated
every time the Helix state changes.  Elements have the form (MODE . KEYMAP),
with the first keymaps having higher priority.")

(helix-defvar-local helix-surround-alist
  `((?\) :insert ("(" . ")") :search ("(" . ")") :balanced t)
    (?\} :insert ("{" . "}") :search ("{" . "}") :balanced t)
    (?\] :insert ("[" . "]") :search ("[" . "]") :balanced t)
    (?\> :insert ("<" . ">") :search ("<" . ">") :balanced t)
    (?\( :insert ("( " . " )")
         :search (lambda ()
                   (helix-4-bounds-of-sexp-at-point (cons "(" ")"))))
    (?\[ :insert ("[" . "]")
         :search (lambda ()
                   (helix-4-bounds-of-sexp-at-point (cons "[" "]"))))
    (?\{ :insert ("{" . "}")
         :search (lambda ()
                   (helix-4-bounds-of-sexp-at-point (cons "{" "}"))))
    (?\< :insert ("< " . " >")
         :search ("<[[:blank:]\n]*" . "[[:blank:]\n]*>")
         :regexp t
         :balanced t)
    (?\" :insert ("\"" . "\"")
         :search (lambda ()
                   (when-let* ((bounds (helix-bounds-of-string-at-point ?\")))
                     (pcase-let ((`(,l . ,r) bounds))
                       (list l (1+ l) (1- r) r))))))
  "Association list with (KEY . SPEC) elements for Helix surrounding
functionality.

SPEC is a plist with next keys:
:insert   - Cons cell (LEFT . RIGHT) with strings, or function that returns such
            cons cell. The strigs that will be inserted by `helix-surround' and
            `helix-surround-change' functions.
:search   - Any of:
            1. Cons cell with strings (LEFT . RIGHT). Should be patterns that
               will be used to search of two substrings to delete in
               `helix-surround-delete' and `helix-surround-change'functions.
               If not specified INSERT pair will be used.
            2. Function that return cons cell with strings (LEFT . RIGHT)
               like in 1.
            3. Function that returns list with 4 positions:
                        (LEFT-START LEFT-END RIGHT-START RIGHT-END)
               of START and END of LEFT and RIGHT delimeters.
               Example:
                          LEFT             RIGHT
                        |<tag> |Some text| </tag>|
                        ^      ^         ^       ^
                        LS     LE        RS      RE

Following keys are taken into account only when :SEARCH argument is a cons cell
with strings (LEFT . RIGHT) or a function, that returns such cons cell. If
:SEARCH is a function that returns list with 4 positions, they will be ignored.

:regexp   - If non-nil then LEFT and RIGHT strings specified in :SEARCH will be
            treated as regexp patterns. Otherwise they will searched literally.
:balanced - When non-nil all nested balanced LEFT RIGHT pairs will be skipped.
            Otherwise the first found pattern will be accepted.

This function populates the buffer local `helix-surround-alist' variable,
and thus should be called from major-modes hooks.
See the defaul value of `helix-surround-alist' variable for examples.")

(helix-defvar-local helix-snipe-aliases '()
  "A list of characters mapped to regexps '(CHAR REGEX).
If CHAR is used in a snipe, it will be replaced with REGEX.
To set an alias for a specific mode use:

    (add-hook 'c++-mode-hook
      (lambda ()
        (push '(?\[ . \"[[{(]\") evil-snipe-aliases)))")

(helix-defvar-local helix--state nil
  "The current Helix state.")

(helix-defvar-local helix--previous-state nil
  "The previous Helix state.")

(defvar helix-state-properties nil
  "Specifications made by `helix-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `helix-state-property'.")

(helix-defvar-local helix--extend-selection nil
  "Extend selection.")

(helix-defvar-local helix-selection-history nil
  "The history of selections.")

(helix-defvar-local helix--insert-pos nil
  "The location of the point where we last time switched to Insert state.")

(helix-defvar-local helix--region-was-active-on-insert nil
  "Whether region was active when we last time switched to Insert state.")

(helix-defvar-local helix-scroll-count 0
  "Hold last used prefix for `helix-smooth-scroll-up' and
`helix-smooth-scroll-down' commands.
Determine how many lines should be scrolled.
Default value is 0 - scroll half the screen.")

(defvar helix-match-map (make-sparse-keymap)
  "Keymap for Helix `m' key.")

(defvar helix-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")

(defvar helix-regex-history nil
  "List with used pcre regexes.")

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'helix-regex-history))

(defvar helix-undo-commands '(helix-undo helix-redo undo undo-redo)
  "Commands that implement undo/redo functionality.")

(defvar helix-multiple-cursors-map (make-sparse-keymap)
  "Transient keymap for `helix-multiple-cursors-mode'.
It is active while there are multiple cursors.
Main goal of the keymap is to rebind `C-g' to conclude multiple
cursors editing.")

(defvar helix-commands-to-run-for-all-cursors nil
  "Commands to execute for all cursors.")

(defvar helix-commands-to-run-once nil
  "Commands to execute only once while multiple cursors are active.")

(defvar helix-default-commands-to-run-for-all-cursors
  '(helix-backward-char            ;; h
    helix-next-line                ;; j
    helix-previous-line            ;; k
    helix-forward-char             ;; l
    helix-forward-word-start       ;; w
    helix-backward-word-start      ;; b
    helix-forward-word-end         ;; e
    helix-forward-WORD-start       ;; W
    helix-backward-WORD-start      ;; B
    helix-forward-WORD-end         ;; E
    helix-beginning-of-line        ;; gs
    helix-first-non-blank          ;; gh
    helix-end-of-line              ;; gl
    helix-forward-paragraph        ;; ]p or }}
    helix-backward-paragraph       ;; [p or {{
    helix-insert                   ;; i
    helix-append                   ;; a
    helix-change                   ;; c
    helix-delete                   ;; d
    helix-mark-line                ;; x
    helix-mark-line-upward         ;; X
    helix-extend-selection         ;; v
    helix-collapse-selection       ;; ;
    helix-trim-whitespaces-from-selection ;; _
    helix-surround                 ;; ms
    helix-mark-inner-word          ;; miw
    helix-mark-inner-WORD          ;; miW
    helix-mark-inner-paragraph     ;; mip
    helix-mark-a-paragraph         ;; map
    helix-mark-inner-double-quoted ;; mi"
    helix-mark-a-double-quoted     ;; ma"
    helix-mark-inner-single-quoted ;; mi'
    helix-mark-a-single-quoted     ;; ma'
    helix-mark-inner-back-quoted   ;; mi`
    helix-mark-a-back-quoted       ;; ma`
    helix-mark-inner-paren         ;; mi( mi)
    helix-mark-a-paren             ;; ma( ma)
    helix-mark-inner-bracket       ;; mi[ mi]
    helix-mark-a-bracket           ;; ma[ ma]
    helix-mark-inner-curly         ;; mi{ mi}
    helix-mark-a-curly             ;; ma{ ma}
    helix-mark-inner-angle         ;; mi< mi>
    helix-mark-an-angle            ;; ma< ma>
    helix-copy-selection           ;; C
    helix-copy-selection-up        ;; M-c
    helix--construct-search-pattern-1 ;; * inner command
    helix--construct-search-pattern-no-bounds-1 ;; M-* inner command
    self-insert-command
    quoted-insert
    previous-line
    next-line
    newline
    newline-and-indent
    open-line
    delete-blank-lines
    transpose-chars
    transpose-lines
    transpose-paragraphs
    transpose-regions
    join-line
    right-char
    right-word
    forward-char
    forward-word
    left-char
    left-word
    backward-char
    backward-word
    forward-paragraph
    backward-paragraph
    upcase-word
    downcase-word
    capitalize-word
    forward-list
    backward-list
    hippie-expand
    hippie-expand-lines
    yank
    yank-pop
    append-next-kill
    kill-word
    kill-line
    kill-whole-line
    backward-kill-word
    backward-delete-char-untabify
    delete-char delete-forward-char
    delete-backward-char
    py-electric-backspace
    c-electric-backspace
    org-delete-backward-char
    cperl-electric-backspace
    python-indent-dedent-line-backspace
    paredit-backward-delete
    autopair-backspace
    just-one-space
    zap-to-char
    end-of-line
    set-mark-command
    exchange-point-and-mark
    cua-set-mark
    cua-replace-region
    cua-delete-region
    move-end-of-line
    beginning-of-line
    move-beginning-of-line
    kill-ring-save
    back-to-indentation
    subword-forward
    subword-backward
    subword-mark
    subword-kill
    subword-backward-kill
    subword-transpose
    subword-capitalize
    subword-upcase
    subword-downcase
    er/expand-region
    er/contract-region
    smart-forward
    smart-backward
    smart-up
    smart-down)
  "Default set of commands to execute for all cursors.")

(defvar helix-default-commands-to-run-once
  '(helix-smooth-scroll-up                   ;; C-u
    helix-smooth-scroll-down                 ;; C-d
    helix-smooth-scroll-page-up              ;; C-b
    helix-smooth-scroll-page-down            ;; C-f
    helix-scroll-line-down                   ;; C-e
    helix-smooth-scroll-line-down            ;; C-e
    helix-mix-scroll-line-down               ;; C-e
    helix-scroll-line-up                     ;; C-y
    helix-smooth-scroll-line-up              ;; C-y
    helix-mix-scroll-line-up                 ;; C-y
    helix-smooth-scroll-line-to-center       ;; zz
    helix-smooth-scroll-line-not-to-very-top ;; zz
    helix-smooth-scroll-line-to-top          ;; zt
    helix-smooth-scroll-line-to-bottom       ;; zb
    helix-disable-multiple-cursors-mode      ;; ,
    helix-normal-state-escape                ;; ESC in normal state
    helix-normal-state                       ;; ESC
    helix-toggle-cursor-on-click             ;; M-mouse1
    helix-goto-first-line                    ;; gg
    helix-goto-last-line                     ;; G
    helix-rotate-selections-backward         ;; (
    helix-rotate-selections-forward          ;; )
    helix-rotate-selections-content-backward ;; M-(
    helix-rotate-selections-content-forward  ;; M-)
    helix-select-regex                       ;; s
    helix-split-region                       ;; S
    helix-split-region-on-newline            ;; M-s
    helix-keep-selections                    ;; K
    helix-remove-selections                  ;; M-K
    helix-align-selections                   ;; &
    helix-undo                               ;; u
    helix-redo                               ;; U
    helix-search-forward                     ;; /
    helix-search-backward                    ;; ?
    helix-construct-search-pattern           ;; *
    helix-construct-search-pattern-no-bounds ;; M-*
    helix-search-next                        ;; n
    helix-search-previous                    ;; N
    keypad                                   ;; SPC
    tab-next
    tab-previous
    ;; helix-mc-edit-lines
    ;; helix-mc-edit-ends-of-lines
    ;; helix-mc-edit-beginnings-of-lines
    ;; helix-mc-mark-next-like-this
    ;; helix-mc-mark-next-like-this-word
    ;; helix-mc-mark-next-like-this-symbol
    ;; helix-mc-mark-next-word-like-this
    ;; helix-mc-mark-next-symbol-like-this
    ;; helix-mc-mark-previous-like-this
    ;; helix-mc-mark-previous-like-this-word
    ;; helix-mc-mark-previous-like-this-symbol
    ;; helix-mc-mark-previous-word-like-this
    ;; helix-mc-mark-previous-symbol-like-this
    ;; helix-mc-mark-all-like-this
    ;; helix-mc-mark-all-words-like-this
    ;; helix-mc-mark-all-symbols-like-this
    ;; helix-mc-mark-more-like-this-extended
    ;; helix-mc-mark-all-like-this-in-defun
    ;; helix-mc-mark-all-words-like-this-in-defun
    ;; helix-mc-mark-all-symbols-like-this-in-defun
    ;; helix-mc-mark-all-like-this-dwim
    ;; helix-mc-mark-all-dwim
    ;; helix-mc-mark-sgml-tag-pair
    ;; helix-mc-insert-numbers
    ;; helix-mc-insert-letters
    ;; helix-mc-sort-regions
    ;; helix-mc-reverse-regions
    ;; helix-mc-cycle-forward
    ;; helix-mc-cycle-backward
    ;; helix-mc-add-cursor-on-click
    ;; helix-mc-mark-pop
    ;; helix-mc-add-cursors-to-all-matches
    ;; helix-mc-mmlte--left
    ;; helix-mc-mmlte--right
    ;; helix-mc-mmlte--up
    ;; helix-mc-mmlte--down
    ;; helix-mc-unmark-next-like-this
    ;; helix-mc-unmark-previous-like-this
    ;; helix-mc-skip-to-next-like-this
    ;; helix-mc-skip-to-previous-like-this
    ;; rrm/switch-to-multiple-cursors
    ;; mc-hide-unmatched-lines-mode
    helix-mc-repeat-command
    save-buffer
    ido-exit-minibuffer
    ivy-done
    exit-minibuffer
    minibuffer-complete-and-exit
    execute-extended-command
    eval-expression
    undo
    redo
    undo-tree-undo
    undo-tree-redo
    undo-fu-only-undo
    undo-fu-only-redo
    universal-argument
    universal-argument-more
    universal-argument-other-key
    negative-argument
    digit-argument
    top-level
    recenter-top-bottom
    describe-mode
    describe-key-1
    describe-function
    describe-bindings
    describe-prefix-bindings
    view-echo-area-messages
    other-window
    kill-buffer-and-window
    split-window-right
    split-window-below
    delete-other-windows
    toggle-window-split
    mwheel-scroll
    scroll-up-command
    scroll-down-command
    mouse-set-point
    mouse-drag-region
    quit-window
    toggle-read-only
    windmove-left
    windmove-right
    windmove-up
    windmove-down
    repeat-complex-command
    edebug-next-mode)
  "Default set of commands to execute only once while multiple cursors are
active.")

(defvar helix-fake-cursor-specific-vars '(kill-ring
                                          kill-ring-yank-pointer
                                          helix--extend-selection
                                          transient-mark-mode
                                          mark-ring
                                          mark-active
                                          yank-undo-function
                                          temporary-goal-column
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

(defvar helix--motion-command nil
  "List of command, after which `helix-merge-overlapping-regions'
would be invoked if `helix--extend-selection' is t.")

(defvar helix--merge-regions-commands '(helix-copy-selection
                                        helix-copy-selection-up)
  "List of command, after which `helix-merge-overlapping-regions'
would be invoked.")

(defvar helix-mc--list-file-loaded nil
  "Non-nil when `helix-mc-list-file' file has already been loaded.")

(helix-defvar-local helix--this-command nil
  "The command that all fake cursors are currently executing.")

(helix-defvar-local helix-mc-temporarily-disabled-minor-modes nil
  "The list of temporarily disabled minor-modes while there are
multiple cursors.")

(provide 'helix-vars)
;;; helix-vars.el ends here
