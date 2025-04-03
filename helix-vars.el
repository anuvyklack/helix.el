;;; helix-vars.el --- Settings and variables -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
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

(provide 'helix-vars)
;;; helix-vars.el ends here
