;;; keypad.el -*- lexical-binding: t; -*-

(require 'dash)

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
          :value-type (string :tag "To"))
  ;; :set #'(lambda (symbol value)
  ;;          (set symbol (mapcar (lambda (pair)
  ;;                                (pcase-let ((`(,from . ,to) pair))
  ;;                                  (cons (seq-first (kbd from))
  ;;                                        (seq-first (kbd to)))))
  ;;                              value)))
  )

(defcustom keypad-leader-map mode-specific-map
  "The fallback dispatching in KEYPAD when there's no translation.
The value can be either a string or a keymap:
A keymap stands for a base keymap used for further translation.
A string stands for finding the keymap at a specified key binding.
Nil stands for taking leader keymap from `meow-keymap-alist'."
  :group 'keypad
  :type 'variable)

(defcustom keypad-message t
  "Whether to show keypad messages in the echo area."
  :group 'meow
  :type 'boolean)

(defcustom keypad-message-prefix "Keypad: "
  "The prefix string for keypad messages."
  :group 'keypad
  :type 'string)

(defvar-keymap keypad-map
  :doc "Keypad keymap."
  :suppress 'nodigits
  "DEL" #'keypad-undo
  "<backspace>" #'keypad-undo
  "<escape>" #'keypad-quit
  "<remap> <keyboard-quit>" #'keypad-quit)

;;; Code

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
  (keypad--show-message)
  (keypad--display-message)
  (while (not (eq (keypad--handle-input-event (read-key))
                  :quit)))
  ;; (unwind-protect
  ;;     (progn
  ;;       (keypad--show-message)
  ;;       (keypad--display-message)
  ;;       (while (not (eq (keypad--handle-input-event (read-key))
  ;;                       :quit))))
  ;;   (when (bound-and-true-p meow-keypad-mode)
  ;;     (keypad--quit)))
  )

(defun keypad--handle-input-event (input-event)
  "Handle INPUT-EVENT.
Return `:finish' if handling is completed."
  (if (equal 'escape last-input-event)
      (keypad--quit)
    (setq last-command-event last-input-event)
    (if-let* ((cmd (lookup-key keypad-map
                               (-> (single-key-description input-event)
                                   (read-kbd-macro)))))
        (call-interactively cmd)
      (keypad--handle-input-event-2 input-event))))

(defun keypad--handle-input-event-2 (input-event)
  "Handle the INPUT-EVENT.
Add a parsed key and its modifier to current key sequence. Then invoke a
command when there's one available on current key sequence."
  (keypad--clear-message)
  (when-let* ((key (single-key-description input-event)))
    (let ((meta? (keypad--meta-keybindings-available-p)))
      (cond (keypad--modifier
             (push (cons keypad--modifier key) keypad--keys)
             (setq keypad--modifier nil))
            ((and meta?
                  (equal key keypad-meta-prefix))
             (setq keypad--modifier 'meta))
            ((and meta?
                  (equal key keypad-ctrl-meta-prefix))
             (setq keypad--modifier 'control-meta))
            ((and keypad--keys
                  (equal key keypad-literal-prefix))
             (setq keypad--modifier 'literal))
            (keypad--keys
             (push (cons 'control key) keypad--keys))
            ((when-let* ((input (alist-get key keypad-start-keys nil nil 'equal)))
               (push (cons 'control (keypad--parse-input-event input))
                     keypad--keys)
               t)) ; exit cond
            (t
             (setq keypad--use-leader-map t)
             (push (cons 'literal key) keypad--keys)))))
  ;; Try execute if the input is valid.
  (if keypad--modifier
      (progn
        (when keypad-message (keypad--show-message))
        (keypad--display-message))
    (keypad--try-execute)))

(defun keypad--try-execute ()
  "Try execute command, return t when the translation progress can be ended.
This function supports a fallback behavior, where it allows to use
`SPC x f' to execute `C-x C-f' or `C-x f' when `C-x C-f' is not bound."
  (unless keypad--modifier
    (let* ((keys (keypad--get-entered-keys-as-string))
           (cmd  (keypad--lookup-key (kbd keys))))
      (cond ((commandp cmd t)
             (setq current-prefix-arg keypad-prefix-arg
                   keypad-prefix-arg nil)
             (keypad--clear-message)
             (setq real-this-command cmd
                   this-command cmd)
             (call-interactively cmd)
             :quit)
            ((keymapp cmd)
             (when keypad-message (keypad--show-message))
             (keypad--display-message))
            ((equal 'control (caar keypad--keys))
             (setcar keypad--keys (cons 'literal (cdar keypad--keys)))
             (keypad--try-execute))
            (t
             (setq keypad-prefix-arg nil)
             (keypad--quit)
             (if keypad-transparent-leader
                 (keypad-transparent-leader)
               (message "%s is undefined" keys))
             :quit)))))

(defun keypad-transparent-leader ()
  (let* ((key (keypad--parse-input-event last-input-event))
         (origin-cmd
          (cl-some #'(lambda (keymap)
                       (when (not (memq keymap keypad-keymaps-transparent-leader-should-ignore))
                         (lookup-key keymap key)))
                   (current-active-maps)))
         (remapped-cmd (command-remapping origin-cmd))
         (cmd (if (member remapped-cmd '(undefined nil))
                  (or origin-cmd 'undefined)
                remapped-cmd)))
    (call-interactively cmd)))

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
        (keypad--display-message))
    ;; else
    (when keypad-message (message "KEYPAD exit"))
    (keypad--quit)))

(defun keypad-quit ()
  "Quit keypad state."
  (interactive)
  (setq this-command last-command)
  (when keypad-message (message "KEYPAD exit"))
  (keypad--quit))

(defun keypad--quit ()
  "Quit keypad state."
  (setq keypad--keys nil
        keypad--modifier nil
        keypad--use-leader-map nil)
  (keypad--clear-message)
  :quit) ; Indicate that keypad loop should be stopped

(defun keypad--meta-keybindings-available-p ()
  "Return t if there are keybindins that starts with Meta prefix."
  (or (not keypad--keys)
      (let* ((keys (keypad--get-entered-keys-as-string))
             (keymap (keypad--lookup-key (kbd keys))))
        (if (keymapp keymap)
            ;; A key sequences starts with ESC is accessible via Meta key.
            (lookup-key keymap (kbd "ESC"))))))

(defun keypad--parse-input-event (e)
  (pcase e
    (32 "SPC")
    ('tab "TAB")
    ('return "RET")
    ('escape "ESC")
    ('backspace "DEL")
    ((pred characterp) (string e))
    ((pred symbolp) (format "<%s>" e))
    ((pred stringp) e)))

(defun keypad--lookup-key (keys)
  "Lookup the command which is bound at KEYS."
  (if keypad--use-leader-map
      (lookup-key keypad-leader-map keys)
    (key-binding keys)))

(defun keypad--get-entered-keys-as-string ()
  "Return entered keys as a string."
  (-> (mapcar #'keypad--format-key-1 keypad--keys)
      (nreverse)
      (string-join " ")))

(defun keypad--format-keys ()
  "Return a display format for current input keys."
  (let ((result (keypad--get-entered-keys-as-string)))
    (concat result
            (if (not (string-empty-p result)) " ")
            (pcase keypad--modifier
              ('meta "M-")
              ('control-meta "C-M-")
              ('literal "○")))))

(defun keypad--format-key-1 (key)
  "Convert cons cell (MODIFIER . KEY) to string representation."
  (pcase (car key)
    ('control (format "C-%s" (keypad--format-upcase (cdr key))))
    ('meta    (format "M-%s" (cdr key)))
    ('control-meta (format "C-M-%s" (keypad--format-upcase (cdr key))))
    ('literal (cdr key))))

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

;;; which-key

(defvar keypad--describe-keymap-function nil
  "The function used to describe (KEYMAP) during keypad execution.
To integrate WhichKey-like features with keypad.
Currently, keypad is not working well with which-key,
so Meow ships a default `meow-describe-keymap'.
Use (setq keypad--describe-keymap-function \\='nil) to disable popup.")

(defvar keypad--clear-describe-keymap-function nil
  "The function used to clear the effect of `keypad--describe-keymap-function'.")

(setq keypad--describe-keymap-function
      #'(lambda (keymap)
          (which-key--create-buffer-and-show nil keymap nil
                                             (concat keypad-message-prefix
                                                     (keypad--format-keys))))
      keypad--clear-describe-keymap-function #'which-key--hide-popup)

(defun keypad--show-message ()
  "Show message for current keypad input."
  (let ((message-log-max)) ; disable message logging
    (message "%s%s%s"
             keypad-message-prefix
             (propertize (keypad--format-prefix) 'face 'font-lock-comment-face)
             (propertize (keypad--format-keys) 'face 'font-lock-string-face))))

(defun keypad--display-message ()
  "Display a message for current input state."
  (when (and keypad--describe-keymap-function
             (or meow--keypad-keymap-description-activated
                 (setq meow--keypad-keymap-description-activated
                       (sit-for meow-keypad-describe-delay t))))
    (let ((keymap (keypad--get-keymap-for-describe)))
      (funcall keypad--describe-keymap-function keymap))))

;; (read-kbd-macro "M-j")
;; (read-kbd-macro "ESC j")
;; (kbd "M-j")
;; (kbd "ESC j")
;;
;; (key-binding (kbd "M-j"))
;; (key-binding (kbd "ESC j"))
;; (key-binding (read-kbd-macro "M-j"))
;; (key-binding (read-kbd-macro "ESC j"))

;; (kbd (string meow-keypad-meta-prefix))

;; (single-key-description 120)
;; (single-key-description 'tab)
;; (kbd "<tab>")
;; (seq-first (kbd "ESC"))
;; (seq-first (read-kbd-macro "ESC"))

(defun keypad--get-keymap-for-describe ()
  "Get a keymap for describe."
  (cond (keypad--modifier
         (keypad--keymap-to-describe-entered-keys-with-modifier))
        (keypad--keys
         (keypad--keymap-to-describe-entered-keys))
        (t
         (keypad--keymap-to-describe-leader-key))))

;; (single-key-description 'lol)
;; (event-basic-type 'C-del)
;; (event-basic-type 'C-tab)

(defun keypad--filter-keymap (keymap predicate)
  "Return a copy of KEYMAP with only literal — non Ctrl events.
When CONTROL is non-nil leave only Ctrl-... events instead."
  (when (keymapp keymap)
    (let ((result (make-sparse-keymap))) ;; make-keymap
      (suppress-keymap result t)
      (map-keymap (lambda (event command)
                    (when (and command
                               (funcall predicate event))
                      (let ((e (keypad--strip-ctrl-meta-from-event event)))
                        (unless (lookup-key result e)
                          (define-key result e command))))
                    ;; (let ((key (single-key-description event))
                    ;;       (modifiers (event-modifiers event)))
                    ;;   (when (funcall predicate key modifiers)
                    ;;     (define-key result
                    ;;                 (keypad--strip-ctrl-meta-from-event event)
                    ;;                 command)))
                    )
                  keymap)
      result)))

(defun keypad--keymap-to-describe-entered-keys-with-modifier ()
  "Return a keymap with continuations for prefix keys + modifiers
entered in Keypad. This keymap is intended to be passed further
to Which-key API."
  (let* ((input (keypad--get-entered-keys-as-string))
         (ctrl-predicate (lambda (event)
                           (let ((key (single-key-description event))
                                 (modifiers (event-modifiers event)))
                             (not (or (equal key "DEL")
                                      (member 'control modifiers))))))
         (literal-predicate (lambda (event)
                              (let ((key (single-key-description event))
                                    (modifiers (event-modifiers event)))
                                (and (not (equal key "DEL"))
                                     (member 'control modifiers))))))
    (pcase keypad--modifier
      ('meta         (keypad--filter-keymap
                      (keypad--lookup-key (kbd (concat input " ESC")))
                      literal-predicate))
      ('control-meta (keypad--filter-keymap
                      (keypad--lookup-key (kbd (concat input " ESC"))) ;; (read-kbd-macro)
                      ctrl-predicate))
      ('literal      (keypad--filter-keymap
                      (keypad--lookup-key (kbd input)) ;; (read-kbd-macro)
                      literal-predicate)))))

(defun keypad--keymap-to-describe-leader-key ()
  "Return a keymap with the content of the `keypad-leader-map'.
This keymap is intended to be passed further to Which-key API."
  (keypad--filter-keymap keypad-leader-map
                         (lambda (event)
                           (let ((key (single-key-description event))
                                 (modifiers (event-modifiers event)))
                             (not (or (member 'control modifiers)
                                      (member key (list keypad-meta-prefix
                                                        keypad-ctrl-meta-prefix
                                                        keypad-literal-prefix))
                                      (alist-get key keypad-start-keys nil nil 'equal)))))))

;; (keypad--strip-ctrl-meta-from-event (seq-first (read-kbd-macro "C-<tab>")))

(defun keypad--keymap-to-describe-entered-keys ()
  "Return a keymap with continuations for prefix keys entered in Keypad.
This keymap is intended to be passed further to Which-key API."
  (let ((keymap (-> (keypad--get-entered-keys-as-string)
                    (read-kbd-macro)
                    (keypad--lookup-key))))
    (when (and keymap (keymapp keymap))
      (let* ((ignored (append (list keypad-literal-prefix "DEL")
                              (if (keypad--meta-keybindings-available-p)
                                  (list keypad-meta-prefix
                                        keypad-ctrl-meta-prefix))))
             (result (keypad--filter-keymap keymap
                                            (lambda (event)
                                              (let ((key (single-key-description event))
                                                    (modifiers (event-modifiers event)))
                                                (and (memq 'control modifiers)
                                                     (not (member key ignored))))))))
        (map-keymap (lambda (event command)
                      (let ((key (single-key-description event))
                            (modifiers (event-modifiers event)))
                        (unless (and (memq 'control modifiers)
                                     (member key ignored))
                          (let ((e (keypad--strip-ctrl-meta-from-event event)))
                            (unless (lookup-key result e)
                              (define-key result e command))))))
                    keymap)
        result))))

;; (char-or-string-p 127)
;; (integerp 127)
;; (integer-or-null-p 127)

;; (single-key-description 127)

;; (event-basic-type 'C-tab)
;; (event-basic-type "C-<tab>")
;; (get 'C-tab 'event-symbol-elements)
;; (symbol-plist 'C-tab)

;; (let ((l (list keypad-literal-prefix "DEL")))
;;   (cons l (list keypad-meta-prefix
;;                 keypad-ctrl-meta-prefix)))
;; (read-kbd-macro "DEL")

;; (append (list keypad-literal-prefix "DEL")
;;         (list keypad-meta-prefix
;;               keypad-ctrl-meta-prefix))

;; (event-basic-type (read-kbd-macro "DEL"))
;; (seq-first (read-kbd-macro "DEL"))

;; (read-kbd-macro "C-<tab>")
;; (event-basic-type 'C-tab)
;; (event-modifiers 'C-tab)

;; (upcase (event-basic-type (seq-first (read-kbd-macro "k"))))
;; (event-basic-type (seq-first (read-kbd-macro "K")))

(defun keypad--strip-ctrl-meta-from-event (key-event)
  "Strip `control' and `meta' modifiers from KEY-EVENT.
Return vector suitable to pass to `define-key'."
  (vector (if (and (integerp (event-basic-type key-event))
                   (member 'shift (event-modifiers key-event)))
              (upcase (event-basic-type key-event))
            (event-basic-type key-event))))

(defun keypad--clear-message ()
  "Clear displayed message by calling `keypad--clear-describe-keymap-function'."
  (when keypad--clear-describe-keymap-function
    (funcall keypad--clear-describe-keymap-function)))

(defun keypad--describe-key ()
  "Describe key via KEYPAD input."
  (interactive)
  (setq keypad--keypad-help t)
  (meow-keypad))

(provide 'keypad)
;;; keypad.el ends here
