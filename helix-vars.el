;;; helix-vars.el --- Settings and variables -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;;
;;; Helix settings and variables.
;;;
;;; Code:

(defvar helix-mode nil)
(declare-function helix-local-mode "helix-core")

(defmacro helix-defvar-local (symbol &optional initvalue docstring)
  "The same as `defvar-local' but additionaly marks SYMBOL as permanent
buffer local variable."
  (declare (indent defun)
           (doc-string 3)
           (debug (symbolp &optional form stringp)))
  `(prog1 (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

;;; Customization group

(defgroup helix nil
  "Helix emulation."
  :group 'emulations
  :prefix 'helix-)

(defcustom helix-want-minibuffer t
  "Whether to enable Helix in minibuffer(s)."
  :type 'boolean
  :group 'helix
  :set #'(lambda (sym value)
           (set-default sym value)
           (if (and helix-mode value)
               (add-hook 'minibuffer-setup-hook #'helix-local-mode)
             (remove-hook 'minibuffer-setup-hook #'helix-local-mode))))

(defcustom helix-use-pcre-regex t
  "If non-nil use PCRE regexp syntax instead of Emacs one."
  :type 'integer
  :group 'helix)

(defcustom helix-regex-history-max 16
  "Maximum length of regexp search ring before oldest elements are thrown away."
  :type 'integer
  :group 'helix)

(defcustom helix-match-fake-cursor-style nil
  "If non-nil, attempt to match the cursor style that the user
has selected. Namely, use vertical bars the user has configured
Emacs to use that cursor.

If nil, just use standard rectangle cursors for all fake cursors.

In some modes/themes, the bar fake cursors are either not
rendered or shift text."
  :type 'boolean
  :group 'helix)

(defcustom helix-normal-state-main-cursor
  `(bar ,(face-attribute 'cursor :background))
  "Cursor apperance when Helix is in Norman state.
Can be a cursor type as per `cursor-type', a color string as passed to
`set-cursor-color', a zero-argument function for changing the cursor, or
a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'helix)

(defcustom helix-insert-state-main-cursor
  `(box ,(face-attribute 'cursor :background))
  "Cursor apperance when Helix is in Insert state.
Can be a cursor type as per `cursor-type', a color string as passed to
`set-cursor-color', a zero-argument function for changing the cursor, or
a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'helix)

(defface helix-normal-state-fake-cursor
  '((t (:inverse-video t)))
  "The face used for fake cursors when Helix is in Normal state."
  :group 'helix)

(defface helix-insert-state-fake-cursor
  '((t (:foreground "white"
        :background "SkyBlue3")))
  "The face used for fake cursors when Helix is in Insert state."
  :group 'helix)

(defface helix-extend-selection-cursor
  '((t (:background "orange")))
  "The face used for cursors when extending selection is active."
  :group 'helix)

(defface helix-region-face '((t :inherit region))
  "The face used for fake regions."
  :group 'helix)

(defface helix-lazy-highlight '((t :inherit lazy-highlight))
  "Face for highlighting all matches in interactive search."
  :group 'helix)

(defcustom helix-mc-mode-line
  `(" mc:" (:eval (format ,(propertize "%d" 'face 'font-lock-warning-face)
                          (helix-number-of-cursors))))
  "What to display in the mode line while `helix-multiple-cursors-mode' is active."
  :type '(sexp)
  :group 'helix)

(put 'helix-mc-mode-line 'risky-local-variable t)

(defcustom helix-whitelist-file (locate-user-emacs-file "helix-multiple-cursors")
  "File to save users preferences which commands to execute for one cursor
and which for all."
  :type 'file
  :group 'helix)

(defcustom helix-max-cursors-number nil
  "Safety ceiling for the number of active cursors.
If your Emacs slows down or freezes when using too many cursors,
customize this value appropriately.

Cursors will be added until this value is reached, at which point
you can either temporarily override the value or abort the
operation entirely.

If this value is nil, there is no ceiling."
  :type '(integer)
  :group 'helix)

(defvar helix-minor-modes-incompatible-with-multiple-cursors
  '(corfu-mode
    company-mode
    flyspell-mode)
  "List of minor-modes that does not work well with multiple cursors.
They are temporarily disabled when there are more then one cursor
in the buffer.")

(defcustom helix-update-highlight-delay 0.02
  "Time in seconds of idle before updating search highlighting.
Setting this to a period shorter than that of keyboard's repeat
rate allows highlights to update while scrolling."
  :type 'number
  :group 'helix)

(defcustom helix-jump-commands
  '(xref-find-definitions
    xref-find-references
    xref-go-back
    xref-go-forward
    outline-up-heading
    outline-next-visible-heading
    outline-previous-visible-heading
    outline-forward-same-level
    outline-backward-same-level)
  "List of commands that move point.
For these commands:
- Mark will be deactivated.
- Overlapping selections will be merged."
  :type '(list symbol)
  :group 'helix
  :set #'(lambda (symbol value)
           (set-default symbol value)
           (mapc #'(lambda (cmd)
                     (put cmd 'helix-deactivate-mark t)
                     (put cmd 'helix-merge-regions 'extend-selection))
                 value)))

(defvar helix-keep-search-highlight-commands
  '(helix-extend-selection
    helix-rotate-selections-forward
    helix-rotate-selections-backward
    helix-search-next
    helix-search-previous
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
    helix-move-window-right
    ;; scrolling
    pixel-scroll-start-momentum)
  "List of commands which should preserve search highlighting overlays.")

(helix-defvar-local helix-surround-alist
  '((?\) :insert ("(" . ")") :search ("(" . ")") :regexp nil :balanced t)
    (?\} :insert ("{" . "}") :search ("{" . "}") :regexp nil :balanced t)
    (?\] :insert ("[" . "]") :search ("[" . "]") :regexp nil :balanced t)
    (?\> :insert ("<" . ">") :search ("<" . ">") :regexp nil :balanced t)
    (?\( :insert ("( " . " )")
         :search (lambda () (helix-4-bounds-of-brackets-at-point ?\( ?\))))
    (?\[ :insert ("[ " . " ]")
         :search (lambda () (helix-4-bounds-of-brackets-at-point ?\[ ?\])))
    (?\{ :insert ("{ " . " }")
         :search (lambda () (helix-4-bounds-of-brackets-at-point ?{ ?})))
    (?\< :insert ("< " . " >")
         :search (lambda () (helix-4-bounds-of-brackets-at-point ?< ?>))
         ;; :search ("<[[:blank:]\n]*" . "[[:blank:]\n]*>")
         ;; :regexp t
         ;; :balanced t
         )
    (?\" :insert ("\"" . "\"")
         :search (lambda ()
                   (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?\"))
                     (list beg (1+ beg) (1- end) end)))))
  "Association list with (KEY . SPEC) elements for Helix surrounding
functionality.

KEY is a character. SPEC is a plist with next keys:

`:insert'  Cons cell (LEFT . RIGHT) with strings, or function that returns such
         cons cell. The strigs that will be inserted by `helix-surround' and
         `helix-surround-change' functions.

`:search'  Any of:
         1. Cons cell with strings (LEFT . RIGHT). Should be patterns that
            will be used to search of two substrings to delete in
            `helix-surround-delete' and `helix-surround-change' functions.
            If not specified INSERT pair will be used.
         2. Function that return cons cell with strings (LEFT . RIGHT) like
            in 1.
         3. Function that returns list with 4 positions:
                     (LEFT-START LEFT-END RIGHT-START RIGHT-END)
            of START and END of LEFT and RIGHT delimeters.
            Example:
                       LEFT                              RIGHT
                     |<tag> |Lorem ipsum dolor sit amet| </tag>|
                     ^      ^                          ^       ^
            LEFT-START      LEFT-END         RIGHT-START       RIGHT-END

Following keys are taken into account only when `:SEARCH' argument is a cons cell
with strings (LEFT . RIGHT) or a function, that returns such cons cell. If
`:SEARCH' is a function that returns list with 4 positions, they will be ignored.

`:regexp'    If non-nil then LEFT and RIGHT strings specified in `:SEARCH' will be
           treated as regexp patterns. Otherwise they will be searched literally.

`:balanced'  When non-nil all nested balanced LEFT RIGHT pairs will be skipped.
           Otherwise the first found pattern will be accepted.

See the defaul value and `helix-integration.el' file for examples.")

(defgroup helix-cjk nil
  "CJK support."
  :prefix "helix-cjk-"
  :group 'helix)

;; (defcustom helix-cjk-emacs-word-boundary nil
;;   "Determine word boundary exactly the same way as Emacs does."
;;   :type 'boolean
;;   :group 'helix-cjk)

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
    (?G . ?r) (?G . ?k) (?G . ?A))
  "List of pair (cons) of categories to determine word boundary
used in `helix-cjk-word-boundary-p'. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '(alist :key-type (choice character (const nil))
          :value-type (choice character (const nil)))
  :group 'helix-cjk)

;;; Variables

(defvar helix-global-keymaps-alist nil
  "Association list of global Helix keymaps.
Entries have the form (STATE . KEYMAP), where STATE is a Helix state.")

(helix-defvar-local helix-mode-map-alist nil
  "Association list of keymaps for current Helix state.
This symbol lies in `emulation-mode-map-alists' and its contents are updated
every time the Helix state changes.  Elements have the form (MODE . KEYMAP),
with the first keymaps having higher priority.")

(helix-defvar-local helix-state nil
  "The current Helix state.")

(helix-defvar-local helix-previous-state nil
  "The previous Helix state.")

(defvar helix-state-properties nil
  "Specifications made by `helix-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `helix-state-property'.")

(defvar helix--advices nil
  "Inner variable for `helix-define-advice'.")

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

(defvar helix-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")

(defvar helix-regex-history nil
  "List with used regexp patterns.")

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'helix-regex-history))

(defvar helix-undo-commands '(helix-undo helix-redo undo undo-redo)
  "Commands that implement undo/redo functionality.")

(helix-defvar-local helix--cursors-table
  (make-hash-table :test 'eql :weakness t)
  "Table mapping fake cursors IDs to cursors overlays.")

(defvar helix--fake-cursor-last-used-id 0
  "Last used fake cursor ID.")

(defvar helix-multiple-cursors-map (make-sparse-keymap)
  "Transient keymap for `helix-multiple-cursors-mode'.
It is active while there are multiple cursors.")

(defvar helix-commands-to-run-for-all-cursors nil
  "Commands to execute for all cursors.")

(defvar helix-commands-to-run-once nil
  "Commands to execute only once while multiple cursors are active.")

(defvar helix-fake-cursor-specific-vars '(helix--extend-selection
                                          transient-mark-mode
                                          mark-ring
                                          mark-active
                                          kill-ring
                                          kill-ring-yank-pointer
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

(defvar helix--whitelist-file-loaded nil
  "Non-nil when `helix-whitelist-file' file has already been loaded.")

(defvar helix-this-command nil
  "Like `this-command' but for fake cursors.
The command that that will be executed by each fake cursor.")

(helix-defvar-local helix--temporarily-disabled-minor-modes nil
  "The list of temporarily disabled minor-modes while there are
multiple cursors.")

(defvar helix--executing-command-for-fake-cursor nil
  "Non-nil if `this-command' is currently executing for fake cursor.")

(helix-defvar-local helix--in-single-undo-step nil
  "Non-nil while we are in the single undo step.")

(helix-defvar-local helix--undo-list-pointer nil
  "Stores the start of the current undo step in `buffer-undo-list'.")

(helix-defvar-local helix--undo-boundary nil)

(provide 'helix-vars)
;;; helix-vars.el ends here
