;;; keypad.el -*- lexical-binding: t; -*-
;;; Code:

(require 'dash)
(require 's)

(declare-function which-key--create-buffer-and-show "which-key")
(declare-function which-key--hide-popup "which-key")

;;; Custom

(defgroup keypad nil
  "Custom group for keypad."
  :group 'keypad-module)

(defcustom keypad-ctrl-prefix "SPC"
  "The key disables all other modifiers."
  :group 'keypad
  :type 'string)

(defcustom keypad-meta-prefix "m"
  "The key coresponding to M- modifier."
  :group 'keypad
  :type 'string)

(defcustom keypad-ctrl-meta-prefix "M"
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

(defcustom keypad-leader "C-c" ; mode-specific-map
  "Where to search keybindings by default?

The value can be a string, a keymap or nil:
- keymap — Search keybindings in this keymap.
- string — Use keymap under this prefix.
- nil — Search in top level."
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
  :doc "Keypad keymap."
  "DEL" #'keypad-undo
  "<backspace>" #'keypad-undo
  "ESC" #'keypad-quit
  "<escape>" #'keypad-quit
  ;; "<remap> <keyboard-quit>" #'keypad-quit
  )

;;; Internal vars

(defvar keypad--keys nil
  "Stores keys entered in Keypad state in reverse order.

For example, key sequence \"C-f M-t h\" will be stored like
(\"h\" \"M-t\" \"C-f\")")

(defvar keypad--use-literal nil)
(defvar keypad--use-meta nil)
(defvar keypad--use-both nil)

(defvar keypad--prefix-arg nil)
(defvar keypad--modifier nil)
(defvar keypad--keypad-help nil "If keypad in help mode.")

(defvar keypad--use-leader nil
  "When non-nil seek for key bindings in `keypad-leader'.
Other way seek in top level.")

(defvar keypad--preview-is-active nil)

;;; Core

(defun keypad ()
  "Enter keypad state."
  (interactive)
  (keypad-start))

(defun keypad-start ()
  "Enter keypad state."
  ;; Try to make this command transparent.
  (setq this-command last-command)
  (setq keypad--keys nil
        keypad--prefix-arg current-prefix-arg
        keypad--use-leader nil)
  (unwind-protect
      (progn
        (keypad--show-message)
        (keypad--open-preview)
        (while (not (eq (keypad--handle-input-event (read-key))
                        :quit))))
    (keypad--quit)))

(defun keypad--handle-input-event (event)
  "Handle input EVENT. Return `:quit' if handling is completed."
  (if (equal 'escape last-input-event)
      (keypad--quit)
    (setq last-command-event last-input-event)
    (if-let* ((key (single-key-description event))
              (cmd (keymap-lookup keypad-map key)))
        (call-interactively cmd)
      (keypad--handle-input-key key))))

(defun keypad--handle-input-key (key)
  (cond (keypad--modifier
         (push (pcase keypad--modifier
                 ('control      (keypad--add-control key))
                 ('meta         (keypad--add-meta key))
                 ('control-meta (keypad--add-control-meta key)))
               keypad--keys)
         (setq keypad--modifier nil))
        ((equal keypad-ctrl-prefix key)
         (setq keypad--modifier 'control))
        ((and (equal keypad-meta-prefix key)
              (keypad--meta-keybindings-available-p))
         (setq keypad--modifier 'meta))
        ((and (equal keypad-ctrl-meta-prefix key)
              (keypad--meta-keybindings-available-p))
         (setq keypad--modifier 'control-meta))
        (keypad--keys
         (push key keypad--keys))
        ;; ((equal "c" key)
        ;;  (push "C-c" keypad--keys)
        ;;  (setq keypad--modifier 'control))
        ((equal "x" key)
         (push "C-x" keypad--keys)
         (setq keypad--modifier 'control))
        (t
         (setq keypad--use-leader t)
         (push key keypad--keys)))
  ;; Try execute if the input is valid.
  (if keypad--modifier
      (progn
        (keypad--show-message)
        (keypad--open-preview))
    (keypad--try-execute)))

(defun keypad--try-execute ()
  "Try execute command, return t when the translation progress can be ended.
This function supports a fallback behavior, where it allows to use
`SPC x f' to execute `C-x C-f' or `C-x f' when `C-x C-f' is not bound."
  (let* ((keys (keypad--entered-keys))
         (cmd  (keypad--lookup-key keys)))
    (cond ((commandp cmd t)
           (setq current-prefix-arg keypad--prefix-arg)
           (keypad--quit)
           (setq real-this-command cmd
                 this-command cmd)
           (call-interactively cmd)
           :quit)
          ((keymapp cmd)
           (keypad--show-message)
           (keypad--open-preview))
          (t
           (keypad--quit)
           (message "%s is undefined" keys)
           :quit))))

(defun keypad-undo ()
  "Pop the last input."
  (interactive)
  (setq this-command last-command)
  (cond (keypad--modifier (setq keypad--modifier nil)
                          (keypad--open-preview))
        (keypad--keys (pop keypad--keys)
                      (keypad--open-preview))
        (t (when keypad-echo (message "KEYPAD exit"))
           (keypad--quit))))

(defun keypad-quit ()
  "Quit keypad state."
  (interactive)
  (setq this-command last-command)
  (when keypad-echo (message "KEYPAD exit"))
  (keypad--quit))

(defun keypad--quit ()
  "Quit keypad state."
  (setq keypad--keys nil
        keypad--modifier nil
        keypad--prefix-arg nil
        keypad--use-leader nil)
  (keypad--close-preview)
  :quit) ; Indicate that keypad loop should be stopped

(defun keypad--add-control (key)
  (pcase key
    ("TAB" "C-<tab>")
    ("RET" "C-<return>")
    ("ESC" "ESC")
    (_ (if (s-contains? "C-" key) key
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
    (_ (concat "C-M-" (->> key
                           (s-replace "C-" "")
                           (s-replace "M-" "")
                           (keypad--handle-shift))))))

(defun keypad--handle-shift (key)
  "Convert capical letters: \"K\" -> \"S-k\".
It is needed when Shift is used along with Ctrl.
For Emacs, \"C-K\" and \"C-k\" are the same event, and to use
Shift with Ctrl, you must write \"C-S-k\"."
  (let ((event (seq-first (key-parse key))))
    (if (equal '(shift) (event-modifiers event))
        (concat "S-" (single-key-description
                      (event-basic-type event)))
      key)))

(defun keypad--meta-keybindings-available-p ()
  "Return non-nil if there are keybindings that starts with Meta prefix."
  (let ((keys (keypad--entered-keys)))
    (or (not keys)
        (if-let* ((keymap (keypad--lookup-key keys))
                  ((keymapp keymap)))
            ;; Key sequences starts with ESC are accessible via Meta key.
            (keymap-lookup keymap "ESC")))))

(defun keypad--lookup-key (keys)
  "Lookup the command which is bound at KEYS."
  (let ((keymap (if keypad--use-leader
                    (keypad--leader-keymap))))
    (keymap-lookup keymap keys)))

(defun keypad--leader-keymap ()
  "Return Keypad leader keymap."
  (if (stringp keypad-leader)
      (keymap-lookup nil keypad-leader)
    keypad-leader))

(defun keypad--entered-keys ()
  "Return entered keys as a string or nil."
  (if keypad--keys
      (string-join (reverse keypad--keys) " ")))

;;; Which-key integration

(defun keypad-show-preview (keymap)
  "Show the KEYMAP content in a popup preview.
Inside it calls Which-Key API, and if you want to redefine this,
you should redefine this particular function."
  (when (and which-key-mode keymap)
    (which-key--create-buffer-and-show
     nil keymap nil (concat keypad-message-prefix (keypad--format-keys)))))

(defun keypad--open-preview ()
  "Show preview with possible continuations for the keys
that were entered in the Keypad."
  (when (or keypad--preview-is-active
            (sit-for which-key-idle-delay t))
    (keypad-show-preview (keypad--keymap-for-preview))
    (setq keypad--preview-is-active t)))

(defun keypad--close-preview ()
  (when which-key-mode
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
  (let ((keys (keypad--entered-keys))
        (control-p (lambda (key)
                     (and (s-contains? "C-" key)
                          (not (keypad--service-key-p key)))))
        (literal-p (lambda (key)
                     (not (or (s-contains? "C-" key)
                              (keypad--service-key-p key))))))
    (cond (keypad--modifier
           (pcase keypad--modifier
             ('control (keypad--filter-keymap
                        (if keys (keypad--lookup-key keys)
                          (keypad--leader-keymap))
                        control-p))
             ('meta    (define-keymap
                         "ESC" (keypad--filter-keymap
                                (keypad--lookup-key (concat keys (if keys " ") "ESC"))
                                literal-p)))
             ('control-meta (define-keymap
                              "ESC" (keypad--filter-keymap
                                     (keypad--lookup-key (concat keys (if keys " ") "ESC"))
                                     control-p)))))
          (keys (if-let* ((keymap (keypad--lookup-key keys))
                          ((keymapp keymap)))
                    keymap))
          (t
           (let* ((ignored (list "x" ; "c"
                                 keypad-ctrl-prefix
                                 keypad-meta-prefix
                                 keypad-ctrl-meta-prefix)))
             (keypad--filter-keymap (keypad--leader-keymap)
                                    (lambda (key)
                                      (not (or (s-contains? "C-" key)
                                               (member key ignored)
                                               (keypad--service-key-p key))))))))))

(defun keypad--filter-keymap (keymap predicate)
  "Return new keymap that contains only elements from KEYMAP
for which PREDICATE is non-nil."
  (when (keymapp keymap)
    (let ((result (define-keymap)))
      (map-keymap (lambda (event command)
                    (when-let* ((key (single-key-description event))
                                ((key-valid-p key))
                                ((not (keymap-lookup result key)))
                                ((funcall predicate key)))
                      (keymap-set result key command)))
                  keymap)
      result)))

(defun keypad--service-key-p (key)
  (keymap-lookup keypad-map (s-with key
                              (s-replace "C-" "")
                              (s-replace "M-" "")
                              (s-replace "S-" ""))))

(defun keypad--format-keys ()
  "Return a display format for current input keys."
  (let ((keys (keypad--entered-keys))
        (modifier (pcase keypad--modifier
                    ('control      "C-")
                    ('meta         "M-")
                    ('control-meta "C-M-"))))
    (cond ((or keys modifier)
           (concat keys (if keys " ") modifier))
          ((not keypad-leader)
           "C-c")
          (t ""))))

(defun keypad--format-prefix ()
  "Return a display format for current prefix."
  (cond ((equal '(4) keypad--prefix-arg)
         "C-u ")
        ((equal '(16) keypad--prefix-arg)
         "C-u C-u ")
        (keypad--prefix-arg
         (concat keypad--prefix-arg " "))
        (t "")))

(provide 'keypad)
;;; keypad.el ends here
