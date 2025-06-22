;;; keypad.el -*- lexical-binding: t; -*-
;;; Code:
;;;
;;; │ Key  -> │ `read-key'   ->  │ `single-key-description' │
;;; ├─────────┼──────────────────┼──────────────────────────┤
;;; │ TAB     │ 9                │ "TAB"                    │
;;; │ C-i     │ 9                │ "TAB"                    │
;;; │         │ 'tab             │ "<tab>"                  │
;;; │ C-TAB   │ 'C-tab           │ "C-<tab>"                │
;;; │ S-TAB   │ 'backtab         │ "<bactab>"               │
;;; │ M-TAB   │                  │                          │
;;; │ C-S-TAB │ 'C-S-iso-lefttab │ "C-S-<iso-lefttab>"      │
;;; │ C-M-TAB │                  │                          │
;;; │ M-S-TAB │                  │                          │
;;; ├─────────┼──────────────────┼──────────────────────────┤
;;; │ RET     │ 13               │ "RET"                    │
;;; │ C-m     │ 13               │ "RET"                    │
;;; │         │ 'return          │ "<return>"               │
;;; │ C-RET   │ 'C-return        │ "C-<return>"             │
;;; │ S-RET   │ 'S-return        │ "S-<return>"             │
;;; │ M-RET   │ 134217741        │ "M-RET"                  │
;;; │ C-S-RET │ 'C-S-return      │ "C-S-<return>"           │
;;; │ C-M-RET │ 'C-M-return      │ "C-M-<return>"           │
;;; │ M-S-RET │ 'M-S-return      │ "M-S-<return>"           │
;;;

(require 'dash)
(require 's)

(declare-function which-key--create-buffer-and-show "which-key"
                  (&optional prefix-keys from-keymap filter prefix-title))
(declare-function which-key--hide-popup "which-key" ())
(defvar which-key-show-prefix)
(defvar which-key-idle-delay)

;;; Custom variables

(defgroup keypad nil
  "Custom group for keypad."
  :group 'keypad-module)

(defcustom keypad-literal-prefix "SPC"
  "The key disables all other modifiers."
  :group 'keypad
  :type 'string)

(defcustom keypad-meta-prefix "m"
  "The key coresponding to M- modifier."
  :group 'keypad
  :type 'string)

(defcustom keypad-ctrl-meta-prefix "g"
  "The key coresponding to C-M- modifier."
  :group 'keypad
  :type 'string)

;; (defcustom keypad-start-keys '(("c" . "C-c")
;;                                ("x" . "C-x"))
;;   "Alist of keys to begin keypad translation.
;; When a key char is pressed,it's corresponding value is appended
;; to C- and the user is prompted to finish the command."
;;   :group 'keypad
;;   :type '(alist
;;           :key-type (string :tag "From")
;;           :value-type (string :tag "To")))

(defcustom keypad-leader-keymap nil ; mode-specific-map
  "The keymap in which Keypad will search keybindings with no modifiers.
If nil Keypad will look under \"C-c\" prefix."
  :group 'keypad
  :type 'variable)

(defcustom keypad-echo t
  "Whether to show keypad messages in the echo area."
  :group 'keypad
  :type 'boolean)

(defcustom keypad-message-prefix "Keypad: "
  "The prefix string for keypad messages."
  :group 'keypad
  :type 'string)

(defvar-keymap keypad-map
  :doc "Keypad service keys."
  "DEL" #'keypad-undo
  "<backspace>" #'keypad-undo
  "ESC" #'keypad-quit
  "<escape>" #'keypad-quit
  "C-g" #'keypad-quit
  ;; "<remap> <keyboard-quit>" #'keypad-quit
  )

;;; Internal vars

(defvar keypad--keys nil
  "List with keys entered in Keypad state.
Keys are strings that satisfies `key-valid-p' and stored ordered
most recent first. For example, key sequence \"C-f M-t h\" will
be stored as \\='(\"h\" \"M-t\" \"C-f\")")

;; (defvar keypad--prefix-arg nil
;;   "Prefix argument. during the current keypad session.
;; Can be nil, an integer, a list like \\='(4) for `C-u',
;; or \\='- for `M--' (raw minus).")

(defvar keypad--pending-modifier nil
  "Stores the pending modifier symbol to be applied to the next key.")

(defvar keypad--command nil
  "The command that Keypad found.")

(defvar keypad--use-leader-map? nil
  "When non-nil seek for key bindings in `keypad-leader-keymap'.
Other way seek in top level.")

(defvar keypad--preview-is-active nil)

;;; Interactive commands

(defun keypad ()
  "Activate Keypad and call the found command."
  (interactive)
  (when-let* ((cmd (keypad-start)))
    (setq this-command cmd)
    (call-interactively cmd)))

(defun keypad-describe-key (key-list &optional buffer)
  "Wrapper around `describe-key', that correctly handle key chords entered
with Keypad. If Helpful package is loaded, `helpful-key' will be used instead
of `describe-key'."
  (interactive (list (help--read-key-sequence)))
  (pcase (key-binding (cdar key-list))
    ('keypad (when-let* ((cmd (keypad-start)))
               (if (fboundp 'helpful-command)
                   (helpful-command cmd)
                 (describe-command cmd))))
    (_ (if (fboundp 'helpful-key)
           (helpful-key (cdar key-list))
         (describe-key key-list buffer)))))

;;; Core

(defun keypad-start ()
  "Enter keypad state.
When EXECUTE is non-nil execute the found command.
Return the found command."
  ;; Try to make this command transparent.
  (setq this-command last-command)
  (setq keypad--keys nil
        keypad--pending-modifier nil
        keypad--use-leader-map? nil
        keypad--command nil)
  (let ((which-key-show-prefix 'echo))
    (unwind-protect
        (progn
          (keypad--show-message)
          (keypad--open-preview)
          (while (not (eq (keypad--handle-input-event (read-key))
                          :quit))
            (keypad--show-message)
            (keypad--open-preview)))
      (keypad--close-preview)))
  keypad--command)

(defun keypad--handle-input-event (event)
  "Handle input EVENT. Return `:quit' if handling is completed."
  ;; (setq last-command-event last-input-event)
  (let ((key (single-key-description event)))
    (if-let* ((cmd (keymap-lookup keypad-map key)))
        (call-interactively cmd)
      (keypad--handle-input-key key))))

(defun keypad--handle-input-key (key)
  (cond ((and (keypad--C-x-or-C-c?)
              (equal "SPC" key))
         ;; Toggle pending modifier if we are in C-x or C-c state.
         (setq keypad--pending-modifier
               (if (or (null keypad--pending-modifier)
                       (eq 'literal keypad--pending-modifier))
                   'control
                 'literal)))
        (keypad--pending-modifier
         (push (pcase keypad--pending-modifier
                 ('control      (keypad--add-control key))
                 ('meta         (keypad--add-meta key))
                 ('control-meta (keypad--add-control-meta key))
                 ('literal      key))
               keypad--keys)
         (setq keypad--pending-modifier
               (cond ((eq keypad--pending-modifier 'literal) 'literal)
                     ((keypad--C-x-or-C-c?) 'control)
                     (t nil))))
        ((and (equal keypad-meta-prefix key)
              (keypad--meta-keybindings-available-p))
         (setq keypad--pending-modifier 'meta))
        ((and (equal keypad-ctrl-meta-prefix key)
              (keypad--meta-keybindings-available-p))
         (setq keypad--pending-modifier 'control-meta))
        (keypad--keys
         (push key keypad--keys))
        ((member key '("x" "c")) ; keypad--keys are empty
         (push (pcase key ("x" "C-x") ("c" "C-c"))
               keypad--keys)
         (setq keypad--pending-modifier 'control))
        (t
         (setq keypad--use-leader-map? t)
         (push key keypad--keys)))
  (when keypad--keys
    (keypad--try-execute)))

(defun keypad--try-execute ()
  "Try execute command, return t when the translation progress can be ended.
This function supports a fallback behavior, where it allows to use
`SPC x f' to execute `C-x C-f' or `C-x f' when `C-x C-f' is not bound."
  (when-let* ((keys (keypad--collected-keys)))
    (let ((cmd (keypad--lookup-key keys)))
      (cond ((commandp cmd t)
             (setq keypad--command cmd)
             :quit)
            ((keymapp cmd))
            ((s-contains? "C-" (car keypad--keys))
             ;; Remove "C-" from the last entered key and try again.
             (setcar keypad--keys (s-replace "C-" "" (car keypad--keys)))
             (keypad--try-execute))
            (t
             (message "%s is undefined" keys)
             :quit)))))

(defun keypad-undo ()
  "Pop the last input."
  (interactive)
  (setq this-command last-command)
  (cond (keypad--pending-modifier
         (setq keypad--pending-modifier nil))
        (keypad--keys
         (pop keypad--keys))
        (t
         (when keypad-echo (message "KEYPAD exit"))
         :quit)))

(defun keypad-quit ()
  "Quit keypad state."
  (interactive)
  (setq this-command last-command)
  (when keypad-echo (message "KEYPAD exit"))
  :quit) ; Indicate that keypad loop should be stopped

(defun keypad--add-control (key)
  (pcase key
    ("TAB" "C-<tab>")
    ("RET" "C-<return>")
    ("ESC" "ESC")
    (_ (if (s-contains? "C-" key)
           key
         (concat "C-" (keypad--handle-shift key))))))

(defun keypad--add-meta (key)
  (if (s-contains? "C-" key)
      (keypad--add-control-meta key)
    (pcase key
      ("TAB" "M-<tab>")
      ("RET" "M-<return>")
      ("ESC" "ESC")
      (_ (concat "M-" key)))))

(defun keypad--add-control-meta (key)
  (pcase key
    ("TAB" "C-M-<tab>")
    ("RET" "C-M-<return>")
    ("ESC" "ESC")
    (_ (concat "C-M-" (s-with key
                        (s-replace "C-" "")
                        (s-replace "M-" "")
                        (keypad--handle-shift))))))

(defun keypad--handle-shift (str)
  "Convert capical ASCII letters: \"K\" -> \"S-k\".
`key-parse' for \"C-K\" and \"C-k\" returns the same event. You must
pass \"C-S-k\" instead. This is relevant only for ASCII. For Unicode,
for example for Cyrillic letters \"C-Ф\" and \"C-ф\" `key-parse' returns
different events."
  (if (and (length= str 1)
           (<= ?A (string-to-char str) ?Z))
      (concat "S-" (downcase str))
    str)
  ;; (let ((event (seq-first (key-parse key))))
  ;;   (if (equal '(shift) (event-modifiers event))
  ;;       (concat "S-" (single-key-description
  ;;                     (event-basic-type event)))
  ;;     key))
  )

(defun keypad--collected-keys ()
  "Return string with keys collected by Keypad. If no keys return nil."
  (if keypad--keys
      (string-join (reverse keypad--keys) " ")))

(defun keypad--C-x-or-C-c? ()
  "Return t if keys collected by Keypad begin from C-x or C-c."
  (if keypad--keys
      (let ((first-key (-last-item keypad--keys)))
        (member first-key '("C-x" "C-c")))))

(defun keypad--meta-keybindings-available-p ()
  "Return non-nil if there are keybindings that starts with Meta prefix."
  (let ((keys (keypad--collected-keys)))
    (or (not keys)
        (if-let* ((keymap (keypad--lookup-key keys))
                  ((keymapp keymap)))
            ;; Key sequences starts with ESC are accessible via Meta key.
            (keymap-lookup keymap "ESC")))))

(defun keypad--lookup-key (keys)
  "Return the command bound to KEYS."
  (let ((keymap (if keypad--use-leader-map?
                    (keypad--leader-keymap))))
    (keymap-lookup keymap keys)))

(defun keypad--leader-keymap ()
  "Return Keypad leader keymap."
  (or keypad-leader-keymap
      (keymap-lookup nil "C-c")))

;;; Which-key integration

(defun keypad--open-preview ()
  "Show preview with possible continuations for the keys
that were entered in the Keypad."
  (when-let* ((which-key-mode)
              ((or keypad--preview-is-active
                   (sit-for which-key-idle-delay t)))
              (keymap (keypad--keymap-for-preview)))
    (which-key--create-buffer-and-show nil keymap nil
                                       (concat keypad-message-prefix
                                               (keypad--format-prefix)
                                               (keypad--format-keys)))
    (setq keypad--preview-is-active t)))

(defun keypad--close-preview ()
  (when keypad--preview-is-active
    (which-key--hide-popup)
    (setq keypad--preview-is-active nil)))

(defun keypad--show-message ()
  "Show message in echo area for current keypad input."
  (when keypad-echo
    (let ((message-log-max)) ; disable message logging
      (message "%s%s%s"
               keypad-message-prefix
               (propertize (keypad--format-prefix) 'face 'font-lock-comment-face)
               (propertize (keypad--format-keys) 'face 'font-lock-string-face)))))

(defun keypad--keymap-for-preview ()
  "Get a keymap for Which-key preview."
  (let ((keys (keypad--collected-keys)))
    (cond (keypad--pending-modifier
           (let ((control-p    #'(lambda (key) (s-contains? "C-" key)))
                 (no-control-p #'(lambda (key) (not (s-contains? "C-" key)))))
             (pcase keypad--pending-modifier
               ('literal (let ((keymap (if keys
                                           (keypad--lookup-key keys)
                                         (keypad--leader-keymap))))
                           (keypad--filter-keymap keymap no-control-p)))
               ('control (if (keypad--C-x-or-C-c?)
                             (keypad--C-x-or-C-c-preview-keymap)
                           ;; else
                           (let ((keymap (if keys
                                             (keypad--lookup-key keys)
                                           (keypad--leader-keymap))))
                             (keypad--filter-keymap keymap control-p))))
               ('meta (if-let* ((keys (concat keys (if keys " ") "ESC"))
                                (keymap (keypad--lookup-key keys))
                                (keymap (keypad--filter-keymap keymap no-control-p)))
                          (define-keymap "ESC" keymap)))
               ('control-meta (if-let* ((keys (concat keys (if keys " ") "ESC"))
                                        (keymap (keypad--lookup-key keys))
                                        (keymap (keypad--filter-keymap keymap control-p)))
                                  (define-keymap "ESC" keymap))))))
          (keys (if-let* ((keymap (keypad--lookup-key keys))
                          ((keymapp keymap)))
                    keymap))
          (t ; no keys
           (let* ((ignored (list "x" "c"
                                 keypad-meta-prefix
                                 keypad-ctrl-meta-prefix))
                  (keymap (keypad--filter-keymap (keypad--leader-keymap)
                                                 #'(lambda (key)
                                                     (not (or (s-contains? "C-" key)
                                                              (member key ignored)))))))
             (keymap-set keymap "x" "C-x")
             (keymap-set keymap "c" "C-c")
             (when keypad-meta-prefix
               (define-key keymap keypad-meta-prefix "M-prefix"))
             (when keypad-ctrl-meta-prefix
               (define-key keymap keypad-ctrl-meta-prefix "C-M-prefix"))
             keymap)))))

(defun keypad--C-x-or-C-c-preview-keymap ()
  (let* ((keymap (keypad--lookup-key (keypad--collected-keys)))
         (ignored-keys `("SPC" ,@(if (keypad--meta-keybindings-available-p)
                                     (list keypad-meta-prefix
                                           keypad-ctrl-meta-prefix))))
         (k1 (keypad--filter-keymap keymap
                                    #'(lambda (key)
                                        (and (s-contains? "C-" key)
                                             (not (member key ignored-keys))))))
         (k2 (keypad--filter-keymap keymap
                                    #'(lambda (key)
                                        (not (or (s-contains? "C-" key)
                                                 (keymap-lookup k1 (concat "C-" key))
                                                 (member key ignored-keys)))))))
    (make-composed-keymap k1 k2)))

(defun keypad--filter-keymap (keymap predicate)
  "Return new keymap that contains only elements from KEYMAP
for which PREDICATE is non-nil."
  (if (keymapp keymap)
      (let ((result (define-keymap)))
        (map-keymap #'(lambda (event command)
                        (unless (eq command 'digit-argument)
                          (when-let* ((key (single-key-description event))
                                      ((key-valid-p key))
                                      ((not (keypad--occupied-key-p key)))
                                      ((not (keymap-lookup result key)))
                                      ((funcall predicate key)))
                            (keymap-set result key command))))
                    keymap)
        result)))

(defun keypad--occupied-key-p (key)
  "Return non-nil if KEY with all modifiers stripped is used by Keypad
itself and hence unavailable."
  (keymap-lookup keypad-map (s-with key
                              (s-replace "C-" "")
                              (s-replace "M-" "")
                              (s-replace "S-" ""))))

(defun keypad--format-keys ()
  "Return a string to display for current input keys."
  (let* ((keys (keypad--collected-keys))
         (keys (cond ((not (or keys
                               keypad-leader-keymap
                               (memq keypad--pending-modifier '(meta control-meta))))
                      "C-c")
                     ((and keys
                           keypad--use-leader-map?
                           (not keypad-leader-keymap))
                      (concat "C-c " keys))
                     (keys)))
         (modifier (pcase keypad--pending-modifier
                     ('control "C-")
                     ('meta    "M-")
                     ('control-meta "C-M-"))))
    (concat keys (if keys " ") modifier)))

(defun keypad--format-prefix ()
  "Return a string do display for active prefix."
  (pcase current-prefix-arg
    ('(4) "C-u ")
    ('(16) "C-u C-u ")
    ('- "C-u -")
    ((pred integerp)
     (format "C-u %d" current-prefix-arg))
    ((guard current-prefix-arg)
     (concat current-prefix-arg " "))
    (_ "")))

(provide 'keypad)
;;; keypad.el ends here
