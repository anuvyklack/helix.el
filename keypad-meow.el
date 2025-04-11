;;; keypad-meow.el -*- lexical-binding: t; -*-
;;; Code:

(require 'dash)
(require 's)

(declare-function which-key--create-buffer-and-show "which-key")
(declare-function which-key--hide-popup "which-key")

;;; Custom

(defgroup keypad nil
  "Custom group for keypad."
  :group 'keypad-module)

(defcustom keypad-meta-prefix "m"
  "The key coresponding to M- modifier."
  :group 'keypad
  :type 'string)

(defcustom keypad-ctrl-meta-prefix "g"
  "The key coresponding to C-M- modifier."
  :group 'keypad
  :type 'string)

(defcustom keypad-literal-prefix "SPC"
  "The key disables all other modifiers."
  :group 'keypad
  :type 'string)

(defcustom keypad-start-keys '(("c" . "c")
                               ("x" . "x"))
  "Alist of keys to begin keypad translation.
When a key char is pressed,it's corresponding value is appended
to C- and the user is prompted to finish the command."
  :group 'keypad
  :type '(alist
          :key-type (string :tag "From")
          :value-type (string :tag "To")))

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
  ;; :suppress 'nodigits
  "DEL" #'keypad-undo
  "<backspace>" #'keypad-undo
  "ESC" #'keypad-quit
  "<escape>" #'keypad-quit
  ;; "<remap> <keyboard-quit>" #'keypad-quit
  )

;;; Internal vars

(defvar keypad--keys nil
  "Stores keys entered in Keypad state in the form (MODIFIER . KEY)
in reverse order.

For example, key sequence \"C-f M-t h\" coresponds to

  (('literal . \"h\") ('meta . \"t\") ('control . \"f\"))")

(defvar keypad--use-literal nil)
(defvar keypad--use-meta nil)
(defvar keypad--use-both nil)

(defvar keypad-prefix-arg nil)
(defvar keypad--modifier nil)
(defvar keypad--keypad-help nil "If keypad in help mode.")

(defvar keypad--use-leader-map nil
  "When non-nil seek for key bindings in `keypad-leader-map'.
Other way seek in top level.")

(defvar keypad-transparent-leader nil)

(defvar keypad-keymaps-to-ingnore-for-transparent-leader
  (list helix-motion-state-map))

(defvar keypad-keymaps-transparent-leader-should-ignore
  (list helix-motion-state-map))

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
        keypad-prefix-arg current-prefix-arg
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
    (if-let* ((cmd (keymap-lookup keypad-map (single-key-description event))))
        (call-interactively cmd)
      (keypad--handle-input-key (single-key-description event)))))

(defsubst keypad--add-control (key)
  (concat "C-" (s-replace "C-" "" key)))

(defsubst keypad--add-control-meta (key)
  (concat "C-M-" (s-with key
                   (s-replace "C-" "")
                   (s-replace "M-" ""))))

(defsubst keypad--add-meta (key)
  (if (s-contains? "C-" key)
      (concat "C-M-" (s-replace "C-" "" key))
    (concat "M-" key)))

(defun keypad--handle-input-key (key)
  (cond (keypad--modifier
         (let ((k (pcase keypad--modifier
                    ('control      (keypad--add-control key))
                    ('meta         (keypad--add-meta key))
                    ('control-meta (keypad--add-control-meta key))
                    ('literal key))))
           (push k keypad--keys)
           (setq keypad--modifier nil)))
        ((and (equal key keypad-meta-prefix)
              (keypad--meta-keybindings-available-p))
         (setq keypad--modifier 'meta))
        ((and (equal key keypad-ctrl-meta-prefix)
              (keypad--meta-keybindings-available-p))
         (setq keypad--modifier 'control-meta))
        ((and keypad--keys
              (equal key keypad-literal-prefix))
         (setq keypad--modifier 'literal))
        (keypad--keys
         (push (keypad--add-control key) keypad--keys))
        ((when-let* ((k (alist-get key keypad-start-keys nil nil 'equal)))
           (push (keypad--add-control k) keypad--keys)
           t)) ; exit cond
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
  (unless keypad--modifier
    (let* ((keys (keypad--entered-keys))
           (cmd  (keypad--lookup-key keys)))
      (cond ((commandp cmd t)
             (setq current-prefix-arg keypad-prefix-arg
                   keypad-prefix-arg nil)
             (keypad--close-preview)
             (setq real-this-command cmd
                   this-command cmd)
             (call-interactively cmd)
             :quit)
            ((keymapp cmd)
             (keypad--show-message)
             (keypad--open-preview))
            ((when (s-contains? "C-" (car keypad--keys))
               (setcar keypad--keys (s-replace "C-" "" (car keypad--keys)))
               (keypad--try-execute)))
            (t
             (setq keypad-prefix-arg nil)
             (keypad--quit)
             (if keypad-transparent-leader
                 (keypad-transparent-leader)
               (message "%s is undefined" keys))
             :quit)))))

(defun keypad-transparent-leader ()
  (let* ((key (single-key-description last-input-event))
         (origin-cmd (cl-some (lambda (keymap)
                                (unless (memq keymap keypad-keymaps-transparent-leader-should-ignore)
                                  (keymap-lookup keymap key)))
                              (current-active-maps)))
         (remapped-cmd (command-remapping origin-cmd))
         (cmd (if (memq remapped-cmd '(undefined nil))
                  (or origin-cmd 'undefined)
                remapped-cmd)))
    (call-interactively cmd)))

(defun keypad-quit ()
  "Quit keypad state."
  (interactive)
  (setq this-command last-command)
  (when keypad-echo (message "KEYPAD exit"))
  (keypad--quit))

(defun keypad-undo ()
  "Pop the last input."
  (interactive)
  (setq this-command last-command)
  (if keypad--modifier
      (setq keypad--modifier nil)
    (pop keypad--keys))
  (if keypad--keys
      (progn
        ;; (meow--update-indicator)
        (keypad--open-preview))
    (when keypad-echo (message "KEYPAD exit"))
    (keypad--quit)))

(defun keypad--quit ()
  "Quit keypad state."
  (setq keypad--keys nil
        keypad--modifier nil
        keypad--use-leader-map nil)
  (keypad--close-preview)
  :quit) ; Indicate that keypad loop should be stopped

(defun keypad--meta-keybindings-available-p ()
  "Return t if there are keybindins that starts with Meta prefix."
  (or (not keypad--keys)
      (if-let* ((keymap (keypad--lookup-key (keypad--entered-keys)))
                (keymapp keymap))
          ;; Key sequences starts with ESC are accessible via Meta key.
          (keymap-lookup keymap "ESC"))))

(defun keypad--lookup-key (keys)
  "Lookup the command which is bound at KEYS."
  (if keypad--use-leader-map
      (keymap-lookup keypad-leader-map keys)
    (key-binding (key-parse keys))))

(defun keypad--entered-keys ()
  "Return entered keys as a string."
  (-> (reverse keypad--keys)
      (string-join " ")))

;;; Which-key integration

(defun keypad-show-preview (keymap)
  "Show the KEYMAP content in a popup preview.
Inside it calls Which-Key API, and if you want to redefine this,
you should redefine this particular function."
  (when (and which-key-mode keymap)
    (which-key--create-buffer-and-show
     nil keymap nil (concat keypad-message-prefix
                            (keypad--format-prefix)
                            (keypad--format-keys)))))

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
  (cond (keypad--modifier
         (keypad--preview-keymap-for-entered-keys-with-modifier))
        (keypad--keys
         (keypad--keymap-to-describe-entered-keys))
        (t
         (keypad--keymap-to-describe-leader-key))))

(defun keypad--filter-keymap (keymap predicate)
  "Return a copy of KEYMAP with only literal — non Ctrl events.
When CONTROL is non-nil leave only Ctrl-... events instead."
  (when (keymapp keymap)
    (let (;; (result (define-keymap :suppress 'nodigits))
          (result (define-keymap)))
      (map-keymap (lambda (event command)
                    (let ((key (single-key-description event))
                          (modifiers (event-modifiers event)))
                      (when (and (key-valid-p key)
                                 (funcall predicate key modifiers))
                        (keymap-set result key command))))
                  keymap)
      result)))

(defsubst keypad--service-key-p (key-or-event)
  (when key-or-event
    (let* ((event (if (key-valid-p key-or-event)
                      (seq-first (key-parse key-or-event))
                    key-or-event))
           (key (single-key-description event))
           (basic-key (single-key-description (event-basic-type event))))
      (keymap-lookup keypad-map basic-key)
      ;; (keymap-lookup keypad-map key)
      )))

(defun keypad--preview-keymap-for-entered-keys-with-modifier ()
  "Return a keymap with continuations for prefix keys and modifiers
entered in Keypad. This keymap is intended to be passed further
to Which-key API."
  (let* ((keys (keypad--entered-keys))
         (control-p (lambda (key modifiers)
                      (and (not (keypad--service-key-p key))
                           (memq 'control modifiers))))
         (literal-p (lambda (key modifiers)
                      (not (or (keypad--service-key-p key)
                               (memq 'control modifiers))))))
    (pcase keypad--modifier
      ('meta         (define-keymap
                       ;; :suppress 'nodigits
                       "ESC" (keypad--filter-keymap
                              (keypad--lookup-key (concat keys " ESC"))
                              literal-p)))
      ('control-meta (define-keymap
                       ;; :suppress 'nodigits
                       "ESC" (keypad--filter-keymap
                              (keypad--lookup-key (concat keys " ESC"))
                              control-p)))
      ('literal      (keypad--filter-keymap
                      (keypad--lookup-key keys)
                      literal-p)))))

(defun keypad--keymap-to-describe-leader-key ()
  "Return a keymap with the content of the `keypad-leader-map'.
This keymap is intended to be passed further to Which-key API."
  (keypad--filter-keymap keypad-leader-map
                         (lambda (key modifiers)
                           (not (or (keypad--service-key-p key)
                                    (member 'control modifiers)
                                    (member key (list keypad-meta-prefix
                                                      keypad-ctrl-meta-prefix
                                                      keypad-literal-prefix))
                                    (alist-get key keypad-start-keys nil nil 'equal))))))

(defun keypad--keymap-to-describe-entered-keys ()
  "Return a keymap with continuations for prefix keys entered in Keypad.
This keymap is intended to be passed further to Which-key API."
  (when-let* ((keymap (keypad--lookup-key (keypad--entered-keys)))
              (keymapp keymap))
    (let (;; (result (define-keymap :suppress 'nodigits))
          (result (define-keymap))
          (ignored `(,keypad-literal-prefix
                     ,@(if (keypad--meta-keybindings-available-p)
                           (list keypad-meta-prefix
                                 keypad-ctrl-meta-prefix))))
          occupied-keys)
      (map-keymap (lambda (event command)
                    (let ((key (single-key-description event))
                          (modifiers (event-modifiers event)))
                      (when (and (key-valid-p key)
                                 (not (keypad--service-key-p event))
                                 (memq 'control modifiers)
                                 (not (member key ignored)))
                        (push (cons (event-basic-type event)
                                    (delq 'control modifiers))
                              occupied-keys)
                        (keymap-set result key command))))
                  keymap)
      (map-keymap (lambda (event command)
                    (let ((key (single-key-description event))
                          (modifiers (event-modifiers event)))
                      (unless (or (key-valid-p key)
                                  (keypad--service-key-p event)
                                  (memq 'control modifiers)
                                  (member key ignored)
                                  (member (cons (event-basic-type event) modifiers)
                                          occupied-keys))
                        (keymap-set result key command))))
                  keymap)
      result)))

(defun keypad--format-key (key)
  "Convert cons cell (MODIFIER . KEY) to string representation."
  (pcase (car key)
    ('control (format "C-%s" (keypad--format-upcase (cdr key))))
    ('meta    (format "M-%s" (cdr key)))
    ('control-meta (format "C-M-%s" (keypad--format-upcase (cdr key))))
    ('literal (cdr key))))

(defun keypad--format-keys ()
  "Return a display format for current input keys."
  (let ((keys (keypad--entered-keys)))
    (concat keys
            (if (not (string-empty-p keys)) " ")
            (pcase keypad--modifier
              ('meta         "M-")
              ('control-meta "C-M-")
              ('literal      "○")
              ;; (_ (if (not (string-empty-p keys)) "C-"))
              ))))

(defun keypad--format-upcase (k)
  "Return \"S-k\" for upcase \"K\"."
  (let ((case-fold-search nil))
    (if (and (stringp k)
             (string-match-p "^[A-Z]$" k))
        (format "S-%s" (downcase k))
      k)))

(defun keypad--format-prefix ()
  "Return a display format for current prefix."
  (cond ((equal '(4) keypad-prefix-arg)
         "C-u ")
        (keypad-prefix-arg
         (format "%s " keypad-prefix-arg))
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

(provide 'keypad-meow)
;;; keypad-meow.el ends here
