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

(defface helix-mc-cursor-face
  '((t (:inverse-video t)))
  "The face used for fake cursors."
  :group 'helix)

(defface helix-mc-cursor-bar-face
  `((t (:height 1 :background ,(face-attribute 'cursor :background))))
  "The face used for fake cursors if the cursor-type is bar."
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

(defface helix-mc-region-face
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
  `((?\( :insert ("(" . ")") :search ("(" . ")") :balanced t)
    (?\{ :insert ("{" . "}") :search ("{" . "}") :balanced t)
    (?\[ :insert ("[" . "]") :search ("[" . "]") :balanced t)
    (?\< :insert ("<" . ">") :search ("<" . ">") :balanced t)
    (?\) :insert ("( " . " )")
         :search (lambda ()
                   (helix--bounds-of-sexp-with-inner-whitespaces-at-point
                    (cons "(" ")"))))
    (?\] :insert ("[" . "]")
         :search (lambda ()
                   (helix--bounds-of-sexp-with-inner-whitespaces-at-point
                    (cons "[" "]"))))
    (?\} :insert ("{" . "}")
         :search (lambda ()
                   (helix--bounds-of-sexp-with-inner-whitespaces-at-point
                    (cons "{" "}"))))
    (?\> :insert ("< " . " >")
         :search ("<[[:blank:]\n]*" . "[[:blank:]\n]*>")
         :regexp t
         :balanced t)
    (?\" :insert ("\"" . "\"")
         :search (lambda ()
                   (when-let* ((bounds (helix-bounds-of-string-at-point ?\")))
                     (list (car bounds)
                           (1+ (car bounds))
                           (1- (cdr bounds))
                           (cdr bounds))))))

  "Association list with (KEY . SPEC) elements for Helix surrounding
functionality.

SPEC is a plist with next keys:
:insert   - Cons cell (LEFT . RIGHT) with strings, or function that returns such
            cons cell. The strigs that will be inserted by `helix-surround' and
            `helix-surround-change' functions.
:search   - Cons cell (LEFT . RIGHT) with strings, or function that returns such
            cons cell. The patterns that will be used to search of two substrings
            to delete in `helix-surround-delete' and `helix-surround-change'
            functions. If not specified INSERT pair will be used.
:regexp   - Should be strings specified in SEARCH argument be treated as regexp
            patterns or literally?
:balanced - When non-nil all nested balanced LEFT RIGHT pairs will be skipped,
            else the first found pattern will be accepted.

This function populates the buffer local `helix-surround-alist' variable,
and thus should be called from major-modes hooks.
See the defaul value of `helix-surround-alist' variable for examples.")

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
  "Extend selection")

(helix-defvar-local helix-selection-history nil
  "The history of selections")

(helix-defvar-local helix--insert-pos nil
  "The location of the point where we last time switched to Insert state.")

(helix-defvar-local helix--region-was-active-on-insert nil
  "Whether region was active when we last time switched to Insert state.")

(helix-defvar-local helix-scroll-count 0
  "Hold last used prefix for `helix-scroll-up' and `helix-scroll-down'.
Determine how many lines should be scrolled.
Default value is 0 - scroll half the screen.")

(defvar helix-match-map (make-sparse-keymap)
  "Keymap for Helix `m' key.")

(defvar helix-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")

(provide 'helix-vars)
;;; helix-vars.el ends here
