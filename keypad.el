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

(defcustom keypad-leader-map mode-specific-map
  "The fallback dispatching in KEYPAD when there's no translation.
The value can be either a string or a keymap:
A keymap stands for a base keymap used for further translation.
A string stands for finding the keymap at a specified key binding.
Nil stands for taking leader keymap from `meow-keymap-alist'."
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

(defvar keypad--use-leader-map nil
  "When non-nil seek for key bindings in `keypad-leader-map'.
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
        keypad--use-leader-map nil)
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
        ((equal "c" key)
         (push "C-c" keypad--keys)
         (setq keypad--modifier 'control))
        ((equal "x" key)
         (push "C-x" keypad--keys))
        (keypad--keys
         (push key keypad--keys))
        (t
         (setq keypad--use-leader-map t)
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
  (if keypad--modifier
      (setq keypad--modifier nil)
    (pop keypad--keys))
  (if keypad--keys
      (keypad--open-preview)
    (when keypad-echo (message "KEYPAD exit"))
    (keypad--quit)))

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
        keypad--use-leader-map nil)
  (keypad--close-preview)
  :quit) ; Indicate that keypad loop should be stopped

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

(defun keypad--meta-keybindings-available-p ()
  "Return t if there are keybindins that starts with Meta prefix."
  (let ((keys (keypad--entered-keys)))
    (or (not keys)
        (if-let* ((keymap (keypad--lookup-key keys))
                  (keymapp keymap))
            ;; Key sequences starts with ESC are accessible via Meta key.
            (keymap-lookup keymap "ESC")))))

(defun keypad--lookup-key (keys)
  "Lookup the command which is bound at KEYS."
  ;; (if keypad--use-leader-map
  ;;     (keymap-lookup keypad-leader-map keys)
  ;;   (key-binding (key-parse keys)))
  ;; (let ((keymap (if keypad--use-leader-map keypad-leader-map)))
  ;;   (keymap-lookup keymap keys))
  (keymap-lookup (if keypad--use-leader-map keypad-leader-map)
                 keys))

(defun keypad--entered-keys ()
  "Return entered keys as a string."
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
        (control-p (lambda (key modifiers)
                     (and (not (keypad--service-key-p key))
                          (memq 'control modifiers))))
        (literal-p (lambda (key modifiers)
                     (not (or (keypad--service-key-p key)
                              (memq 'control modifiers))))))
    (cond (keypad--modifier
           (pcase keypad--modifier
             ('control (keypad--filter-keymap
                        (keypad--lookup-key keys)
                        control-p))
             ('meta    (define-keymap
                         "ESC" (keypad--filter-keymap
                                (keypad--lookup-key (concat keys (if keys " ") "ESC"))
                                literal-p)))
             ('control-meta (define-keymap
                              "ESC" (keypad--filter-keymap
                                     (keypad--lookup-key (concat keys (if keys " ") "ESC"))
                                     control-p)))))
          (keys (let ((keymap (keypad--lookup-key keys)))
                  (if (keymapp keymap) keymap)))
          (t (keypad--filter-keymap
              keypad-leader-map
              (lambda (key modifiers)
                (not (or (keypad--service-key-p key)
                         (member 'control modifiers)
                         (member key (list "x" "c"
                                           keypad-ctrl-prefix
                                           keypad-meta-prefix
                                           keypad-ctrl-meta-prefix))))))))))

(defun keypad--filter-keymap (keymap predicate)
  "Return new keymap that contains only elements from KEYMAP
for which PREDICATE is non-nil."
  (when (keymapp keymap)
    (let ((result (define-keymap)))
      (map-keymap (lambda (event command)
                    (let ((key (single-key-description event))
                          (modifiers (event-modifiers event)))
                      (when (and (key-valid-p key)
                                 (funcall predicate key modifiers))
                        (keymap-set result key command))))
                  keymap)
      result)))

(defun keypad--service-key-p (key-or-event)
  (when key-or-event
    (let* ((event (if (key-valid-p key-or-event)
                      (seq-first (key-parse key-or-event))
                    key-or-event))
           (key (single-key-description event))
           (basic-key (single-key-description (event-basic-type event))))
      (keymap-lookup keypad-map basic-key)
      ;; (keymap-lookup keypad-map key)
      )))

(defun keypad--format-keys ()
  "Return a display format for current input keys."
  (let ((keys (keypad--entered-keys)))
    (concat keys (if keys " ")
            (pcase keypad--modifier
              ('control      "C-")
              ('meta         "M-")
              ('control-meta "C-M-")))))

(defun keypad--format-prefix ()
  "Return a display format for current prefix."
  (cond ((equal '(4) keypad--prefix-arg)
         "C-u ")
        (keypad--prefix-arg
         (concat keypad--prefix-arg " "))
        (t "")))

(defun keypad--strip-ctrl-meta-from-event (event)
  "Strip `control' and `meta' modifiers from EVENT.
Return vector suitable to pass to `define-key'."
  (let ((e (event-basic-type event)))
    (vector (if (and (integerp e)
                     (memq 'shift (event-modifiers event)))
                (upcase e)
              e))))

(defun keypad--describe-key ()
  "Describe key via KEYPAD input."
  (interactive)
  (setq keypad--keypad-help t)
  (meow-keypad))

(provide 'keypad)
;;; keypad.el ends here
