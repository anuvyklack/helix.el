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

(defcustom keypad-literal-prefix "SPC"
  "The key disables all other modifiers."
  :group 'keypad
  :type 'string)

(defcustom keypad-ctrl-prefix nil ; "SPC"
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

(defcustom keypad-leader nil ; mode-specific-map
  "The keymap in which Keypad will search keybindings by default.
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

(defvar keypad--prefix-arg nil)
(defvar keypad--modifier nil)
(defvar keypad--describe-key nil)

(defvar keypad--use-leader nil
  "When non-nil seek for key bindings in `keypad-leader'.
Other way seek in top level.")

(defvar keypad--preview-is-active nil)

;;; Core

(defun keypad ()
  "Enter keypad state."
  (interactive)
  (keypad-start))

(defun keypad-start (&optional return)
  "Enter keypad state.
When RETURN non-nil return the entered key sequence instead of executing it."
  ;; Try to make this command transparent.
  (setq this-command last-command)
  (setq keypad--keys nil
        keypad--modifier nil
        keypad--prefix-arg current-prefix-arg
        keypad--use-leader nil
        keypad--describe-key return)
  (unwind-protect
      (progn
        (keypad--show-message)
        (keypad--open-preview)
        (while (not (eq (keypad--handle-input-event (read-key))
                        :quit))))
    (keypad--close-preview))
  (when return
    (setq return keypad--describe-key
          keypad--describe-key nil)
    return))

(defun keypad--handle-input-event (event)
  "Handle input EVENT. Return `:quit' if handling is completed."
  (if (equal 'escape last-input-event)
      (progn (keypad--close-preview)
             :quit)
    ;; else
    (setq last-command-event last-input-event)
    (if-let* ((key (single-key-description event))
              (cmd (keymap-lookup keypad-map key)))
        (call-interactively cmd)
      (keypad--handle-input-key key))))

(defun keypad--handle-input-key (key)
  (cond ((and (keypad--C-x-or-C-c?)
              (equal "SPC" key))
         (setq keypad--modifier (if (or (null keypad--modifier)
                                        (eq 'literal keypad--modifier))
                                    'control
                                  'literal)))
        (keypad--modifier
         (let ((k (pcase keypad--modifier
                    ('control      (keypad--add-control key))
                    ('meta         (keypad--add-meta key))
                    ('control-meta (keypad--add-control-meta key))
                    ('literal      key))))
           (push k keypad--keys)
           (setq keypad--modifier (cond ((eq keypad--modifier 'literal) 'literal)
                                        ((keypad--C-x-or-C-c?) 'control)
                                        (t nil)))))
        ((and (equal keypad-ctrl-prefix key))
         (setq keypad--modifier 'control))
        ((and (equal keypad-meta-prefix key)
              (keypad--meta-keybindings-available-p))
         (setq keypad--modifier 'meta))
        ((and (equal keypad-ctrl-meta-prefix key)
              (keypad--meta-keybindings-available-p))
         (setq keypad--modifier 'control-meta))
        (keypad--keys
         (push key keypad--keys))
        ((member key '("x" "c")) ; (not keypad--keys)
         (push (pcase key ("x" "C-x") ("c" "C-c"))
               keypad--keys)
         (setq keypad--modifier 'control))
        (t
         (setq keypad--use-leader t)
         (push key keypad--keys)))
  (if keypad--keys
      (keypad--try-execute)
    (keypad--show-message)
    (keypad--open-preview)))

(defun keypad--try-execute ()
  "Try execute command, return t when the translation progress can be ended.
This function supports a fallback behavior, where it allows to use
`SPC x f' to execute `C-x C-f' or `C-x f' when `C-x C-f' is not bound."
  (when-let* ((keys (keypad--entered-keys)))
    (let ((cmd (keypad--lookup-key keys)))
      (cond ((commandp cmd t)
             (setq current-prefix-arg keypad--prefix-arg)
             (keypad--close-preview)
             (if (keypad--describe-key)
                 (setq keypad--describe-key keys)
               ;; else
               (setq real-this-command cmd
                     this-command cmd)
               (call-interactively cmd))
             :quit)
            ((keymapp cmd)
             (keypad--show-message)
             (keypad--open-preview))
            ((s-contains? "C-" (car keypad--keys))
             ;; Remove "C-" from the last entered key and try again.
             (setcar keypad--keys (s-replace "C-" "" (car keypad--keys)))
             (keypad--try-execute))
            (t
             (keypad--close-preview)
             (message "%s is undefined" keys)
             :quit)))))

(defun keypad-undo ()
  "Pop the last input."
  (interactive)
  (setq this-command last-command)
  (cond (keypad--modifier (setq keypad--modifier nil)
                          (keypad--open-preview))
        (keypad--keys (pop keypad--keys)
                      (keypad--open-preview))
        (t (when keypad-echo (message "KEYPAD exit"))
           (keypad--close-preview)
           :quit)))

(defun keypad-quit ()
  "Quit keypad state."
  (interactive)
  (setq this-command last-command)
  (when keypad-echo (message "KEYPAD exit"))
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

(defun keypad--entered-keys ()
  "Return entered keys as a string or nil."
  (if keypad--keys
      (string-join (reverse keypad--keys) " ")))

(defun keypad--C-x-or-C-c? ()
  (if-let* ((first-key (car (last keypad--keys))))
      (member first-key '("C-x" "C-c"))))

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
  (or keypad-leader
      (keymap-lookup nil "C-c")))

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
  (let ((keys (keypad--entered-keys)))
    (cond (keypad--modifier
           (let ((control-p    (lambda (key) (s-contains? "C-" key)))
                 (no-control-p (lambda (key) (not (s-contains? "C-" key)))))
             (pcase keypad--modifier
               ('literal (let ((keymap (if keys (keypad--lookup-key keys)
                                         (keypad--leader-keymap))))
                           (keypad--filter-keymap keymap no-control-p)))
               ('control (if (keypad--C-x-or-C-c?)
                             (keypad--C-x-or-C-c-preview-keymap)
                           (let ((keymap (if keys (keypad--lookup-key keys)
                                           (keypad--leader-keymap))))
                             (keypad--filter-keymap keymap control-p))))
               ('meta    (let* ((keys (concat keys (if keys " ") "ESC"))
                                (keymap (keypad--lookup-key keys))
                                (keymap (keypad--filter-keymap keymap no-control-p)))
                           (define-keymap "ESC" keymap)))
               ('control-meta (let* ((keys (concat keys (if keys " ") "ESC"))
                                     (keymap (keypad--lookup-key keys))
                                     (keymap (keypad--filter-keymap keymap control-p)))
                                (define-keymap "ESC" keymap))))))
          (keys (if-let* ((keymap (keypad--lookup-key keys))
                          ((keymapp keymap)))
                    keymap))
          (t ; (not keys)
           (let ((ignored (list "x" "c"
                                keypad-ctrl-prefix
                                keypad-meta-prefix
                                keypad-ctrl-meta-prefix)))
             (keypad--filter-keymap (keypad--leader-keymap)
                                    (lambda (key)
                                      (not (or (s-contains? "C-" key)
                                               (member key ignored))))))))))

(defun keypad--C-x-or-C-c-preview-keymap ()
  (let* ((keymap (keypad--lookup-key (keypad--entered-keys)))
         (ignored `("SPC"
                    ,@(if (keypad--meta-keybindings-available-p)
                          (list keypad-meta-prefix
                                keypad-ctrl-meta-prefix))))
         (r1 (keypad--filter-keymap keymap
                                    (lambda (key)
                                      (and (s-contains? "C-" key)
                                           (not (member key ignored))))))
         (r2 (keypad--filter-keymap keymap
                                    (lambda (key)
                                      (not (or (s-contains? "C-" key)
                                               (keymap-lookup r1 (concat "C-" key))
                                               (member key ignored)))))))
    (make-composed-keymap r1 r2)))

(defun keypad--filter-keymap (keymap predicate)
  "Return new keymap that contains only elements from KEYMAP
for which PREDICATE is non-nil."
  (when (keymapp keymap)
    (let ((result (define-keymap)))
      (map-keymap (lambda (event command)
                    (unless (eq command 'digit-argument)
                      (when-let* ((key (single-key-description event))
                                  ((key-valid-p key))
                                  ((not (keypad--service-key-p key)))
                                  ((not (keymap-lookup result key)))
                                  ((funcall predicate key)))
                        (keymap-set result key command))))
                  keymap)
      result)))

(defun keypad--service-key-p (key)
  (keymap-lookup keypad-map (s-with key
                              (s-replace "C-" "")
                              (s-replace "M-" "")
                              (s-replace "S-" ""))))

(defun keypad--format-keys ()
  "Return a display format for current input keys."
  (let* ((keys (keypad--entered-keys))
         (keys (cond ((not (or keys
                               keypad-leader
                               (memq keypad--modifier '(meta control-meta))))
                      "C-c")
                     ((and keys
                           keypad--use-leader
                           (not keypad-leader))
                      (concat "C-c " keys))
                     (keys)))
         (modifier (pcase keypad--modifier
                     ('control      "C-")
                     ('meta         "M-")
                     ('control-meta "C-M-"))))
    (concat keys (if keys " ") modifier)))

(defun keypad--format-prefix ()
  "Return a display format for current prefix."
  (cond ((equal '(4) keypad--prefix-arg)
         "C-u ")
        ((equal '(16) keypad--prefix-arg)
         "C-u C-u ")
        (keypad--prefix-arg
         (concat keypad--prefix-arg " "))
        (t "")))

;; Describe key

(defun keypad-describe-key (key-list &optional buffer)
  (interactive (list (help--read-key-sequence)))
  (let* ((keys (cdar key-list))
         (cmd (key-binding (key-parse keys))))
    (when (eq cmd #'keypad)
      (setq keys (keypad-start t)
            key-list (key-parse keys)))
    (if (fboundp 'helpful-key)
        (helpful-key keys)
      (describe-key key-list buffer))))

(provide 'keypad)
;;; keypad.el ends here
