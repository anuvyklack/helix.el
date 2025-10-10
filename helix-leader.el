;;; helix-leader.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;
;; │ Key  -> │ `read-key'   ->  │ `single-key-description' │
;; ├─────────┼──────────────────┼──────────────────────────┤
;; │ TAB     │ 9                │ "TAB"                    │
;; │ C-i     │ 9                │ "TAB"                    │
;; │         │ 'tab             │ "<tab>"                  │
;; │ C-TAB   │ 'C-tab           │ "C-<tab>"                │
;; │ S-TAB   │ 'backtab         │ "<bactab>"               │
;; │ M-TAB   │                  │                          │
;; │ C-S-TAB │ 'C-S-iso-lefttab │ "C-S-<iso-lefttab>"      │
;; │ C-M-TAB │                  │                          │
;; │ M-S-TAB │                  │                          │
;; ├─────────┼──────────────────┼──────────────────────────┤
;; │ RET     │ 13               │ "RET"                    │
;; │ C-m     │ 13               │ "RET"                    │
;; │         │ 'return          │ "<return>"               │
;; │ C-RET   │ 'C-return        │ "C-<return>"             │
;; │ S-RET   │ 'S-return        │ "S-<return>"             │
;; │ M-RET   │ 134217741        │ "M-RET"                  │
;; │ C-S-RET │ 'C-S-return      │ "C-S-<return>"           │
;; │ C-M-RET │ 'C-M-return      │ "C-M-<return>"           │
;; │ M-S-RET │ 'M-S-return      │ "M-S-<return>"           │
;;

(require 'dash)
(require 's)
(require 'helix-macros)

(declare-function which-key--create-buffer-and-show "which-key"
                  (&optional prefix-keys from-keymap filter prefix-title))
(declare-function which-key--hide-popup "which-key" ())
(defvar which-key-show-prefix)
(defvar which-key-idle-delay)

;;; Custom variables

(defgroup helix-leader nil
  "Custom group for helix-leader."
  :group 'helix-leader-module)

(defcustom helix-leader-literal-prefix "SPC"
  "The key disables all other modifiers."
  :group 'helix-leader
  :type 'string)

(defcustom helix-leader-meta-prefix "m"
  "The key coresponding to M- modifier."
  :group 'helix-leader
  :type 'string)

(defcustom helix-leader-ctrl-meta-prefix "g"
  "The key coresponding to C-M- modifier."
  :group 'helix-leader
  :type 'string)

;; (defcustom helix-leader-start-keys '(("c" . "C-c")
;;                                      ("x" . "C-x"))
;;   "Alist of keys to begin helix-leader translation.
;; When a key char is pressed,it's corresponding value is appended
;; to C- and the user is prompted to finish the command."
;;   :group 'helix-leader
;;   :type '(alist
;;           :key-type (string :tag "From")
;;           :value-type (string :tag "To")))

(defcustom helix-leader-send-C-x-with-control-modifier t
  "When non-nil, pressing \"x\" in helix-leader initial state will send \"C-x C-\",
i.e. \"C-x\" followed by another `Control' modifier.

When nil, pressing \"x\" will send \"C-x\" without an additional `Control'
modifier."
  :group 'helix-leader
  :type 'string)

(defcustom helix-leader-leader-keymap nil ; mode-specific-map
  "The keymap in which helix-leader will search keybindings with no modifiers.
If nil helix-leader will look under \"C-c\" prefix."
  :group 'helix-leader
  :type 'variable)

(defcustom helix-leader-echo t
  "Whether to show helix-leader messages in the echo area."
  :group 'helix-leader
  :type 'boolean)

(defcustom helix-leader-message-prefix "helix-leader: "
  "The prefix string for helix-leader messages."
  :group 'helix-leader
  :type 'string)

(defvar-keymap helix-leader-map
  :doc "helix-leader service keys."
  "DEL" #'helix-leader-undo ;; DEL in Emacs corresponds to the backspace key
  "<backspace>" #'helix-leader-undo
  "ESC" #'helix-leader-quit
  "<escape>" #'helix-leader-quit
  "C-g" #'helix-leader-quit
  ;; "<remap> <keyboard-quit>" #'helix-leader-quit
  )

;;; Internal vars

(defvar helix-leader--keys nil
  "List with keys entered in helix-leader state.
Keys are strings that satisfies `key-valid-p' and stored ordered
most recent first. For example, key sequence \"C-f M-t h\" will
be stored as \\='(\"h\" \"M-t\" \"C-f\")")

;; (defvar helix-leader--prefix-arg nil
;;   "Prefix argument. during the current helix-leader session.
;; Can be nil, an integer, a list like \\='(4) for `C-u',
;; or \\='- for `M--' (raw minus).")

(defvar helix-leader--pending-modifier nil
  "Stores the pending modifier symbol to be applied to the next key.")

(defvar helix-leader--command nil
  "The command that helix-leader found.")

(defvar helix-leader--use-leader-map? nil
  "When non-nil seek for key bindings in `helix-leader-leader-keymap'.
Other way seek in top level.")

(defvar helix-leader--preview-is-active nil)

;;; Interactive commands

(helix-define-command helix-leader ()
  "Activate helix-leader and interactively evaluate found command."
  :multiple-cursors nil
  (interactive)
  (when-let* ((cmd (helix-leader-start)))
    (setq this-command cmd
          helix-this-command this-command)
    (call-interactively cmd)))

(helix-define-command helix-leader-other-window ()
  (interactive)
  :multiple-cursors nil
  (other-window-prefix)
  (helix-leader))

(helix-define-command helix-leader-describe-key (key-list &optional buffer)
  "Wrapper around `describe-key', that correctly handle key chords entered
with helix-leader. If Helpful package is loaded, `helpful-key' will be used instead
of `describe-key'."
  :multiple-cursors nil
  (interactive (list (help--read-key-sequence)))
  (pcase (key-binding (caar key-list))
    ('helix-leader (when-let* ((cmd (helix-leader-start)))
                     (if (fboundp 'helpful-command)
                         (helpful-command cmd)
                       (describe-command cmd))))
    (_ (if (fboundp 'helpful-key)
           (helpful-key (caar key-list))
         (describe-key key-list buffer)))))

;;; Core

(defun helix-leader-start ()
  "Enter helix-leader state.
When EXECUTE is non-nil execute the found command.
Return the found command."
  ;; Try to make this command transparent.
  (setq this-command last-command)
  (setq helix-leader--keys nil
        helix-leader--pending-modifier nil
        helix-leader--use-leader-map? nil
        helix-leader--command nil)
  (unwind-protect
      (progn
        (helix-leader--show-preview)
        (while (not (eq (helix-leader--handle-input-event (read-key))
                        :quit))
          (helix-leader--show-preview)))
    (helix-leader--hide-preview))
  helix-leader--command)

(defun helix-leader--handle-input-event (event)
  "Handle input EVENT. Return `:quit' if handling is completed."
  ;; (setq last-command-event last-input-event)
  (let ((key (single-key-description event)))
    (if-let* ((cmd (keymap-lookup helix-leader-map key)))
        (call-interactively cmd)
      (helix-leader--handle-input-key key))))

(defun helix-leader--handle-input-key (key)
  (cond ((and (helix-leader--C-x-or-C-c?)
              (equal "SPC" key))
         ;; Toggle pending modifier if we are in C-x or C-c state.
         (setq helix-leader--pending-modifier
               (if (or (null helix-leader--pending-modifier)
                       (eq 'literal helix-leader--pending-modifier))
                   'control
                 'literal)))
        (helix-leader--pending-modifier
         (push (pcase helix-leader--pending-modifier
                 ('control      (helix-leader--add-control key))
                 ('meta         (helix-leader--add-meta key))
                 ('control-meta (helix-leader--add-control-meta key))
                 ('literal      key))
               helix-leader--keys)
         (setq helix-leader--pending-modifier
               (cond ((eq helix-leader--pending-modifier 'literal) 'literal)
                     ((helix-leader--C-x-or-C-c?) 'control)
                     (t nil))))
        ((and (equal helix-leader-meta-prefix key)
              (helix-leader--meta-keybindings-available-p))
         (setq helix-leader--pending-modifier 'meta))
        ((and (equal helix-leader-ctrl-meta-prefix key)
              (helix-leader--meta-keybindings-available-p))
         (setq helix-leader--pending-modifier 'control-meta))
        (helix-leader--keys
         (push key helix-leader--keys))
        ;; All following conditions assumes that `helix-leader--keys' are empty.
        ((equal "c" key)
         (push "C-c" helix-leader--keys)
         (setq helix-leader--pending-modifier 'control))
        ((equal "x" key)
         (push "C-x" helix-leader--keys)
         (when helix-leader-send-C-x-with-control-modifier
           (setq helix-leader--pending-modifier 'control)))
        (t
         (setq helix-leader--use-leader-map? t)
         (push key helix-leader--keys)))
  (when helix-leader--keys
    (helix-leader--try-execute)))

(defun helix-leader--try-execute ()
  "Try execute command, return t when the translation progress can be ended.
This function supports a fallback behavior, where it allows to use
`SPC x f' to execute `C-x C-f' or `C-x f' when `C-x C-f' is not bound."
  (when-let* ((keys (helix-leader--collected-keys)))
    (let ((cmd (helix-leader--lookup-key keys)))
      (cond ((commandp cmd t)
             (setq helix-leader--command cmd)
             :quit)
            ((keymapp cmd))
            ((s-contains? "C-" (car helix-leader--keys))
             ;; Remove "C-" from the last entered key and try again.
             (setcar helix-leader--keys (s-replace "C-" "" (car helix-leader--keys)))
             (helix-leader--try-execute))
            (t
             (message "%s is undefined" keys)
             :quit)))))

(defun helix-leader-undo ()
  "Pop the last input."
  (interactive)
  (setq this-command last-command)
  (cond (helix-leader--pending-modifier
         (setq helix-leader--pending-modifier nil))
        (helix-leader--keys
         (pop helix-leader--keys))
        (t
         (when helix-leader-echo (message "helix-leader exit"))
         :quit)))

(defun helix-leader-quit ()
  "Quit helix-leader state."
  (interactive)
  (setq this-command last-command)
  (when helix-leader-echo (message "helix-leader exit"))
  :quit) ; Indicate that helix-leader loop should be stopped

(defun helix-leader--add-control (key)
  (pcase key
    ("TAB" "C-<tab>")
    ("RET" "C-RET") ;; "C-<return>"
    ("ESC" "ESC")
    (_ (if (s-contains? "C-" key)
           key
         (concat "C-" (helix-leader--handle-shift key))))))

(defun helix-leader--add-meta (key)
  (if (s-contains? "C-" key)
      (helix-leader--add-control-meta key)
    (pcase key
      ("TAB" "M-TAB") ;; "M-<tab>"
      ("RET" "M-RET") ;; "M-<return>"
      ("ESC" "ESC")
      (_ (concat "M-" key)))))

(defun helix-leader--add-control-meta (key)
  (pcase key
    ("TAB" "C-M-<tab>")
    ("RET" "C-M-<return>")
    ("ESC" "ESC")
    (_ (concat "C-M-" (s-with key
                        (s-replace "C-" "")
                        (s-replace "M-" "")
                        (helix-leader--handle-shift))))))

(defun helix-leader--handle-shift (str)
  "Convert capical ASCII letters following way: \"K\" -> \"S-k\".
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

(defun helix-leader--collected-keys ()
  "Return string with keys collected by helix-leader. If no keys return nil."
  (if helix-leader--keys
      (string-join (reverse helix-leader--keys) " ")))

(defun helix-leader--C-x-or-C-c? ()
  "Return t if keys collected by helix-leader begin from C-x or C-c."
  (if helix-leader--keys
      (let ((first-key (-last-item helix-leader--keys)))
        (member first-key '("C-x" "C-c")))))

(defun helix-leader--meta-keybindings-available-p ()
  "Return non-nil if there are keybindings that starts with Meta prefix."
  (let ((keys (helix-leader--collected-keys)))
    (or (not keys)
        (if-let* ((keymap (helix-leader--lookup-key keys))
                  ((keymapp keymap)))
            ;; Key sequences starts with ESC are accessible via Meta key.
            (keymap-lookup keymap "ESC")))))

(defun helix-leader--lookup-key (keys)
  "Return the command bound to KEYS."
  (let ((keymap (if helix-leader--use-leader-map?
                    (helix-leader--leader-keymap))))
    (keymap-lookup keymap keys)))

(defun helix-leader--leader-keymap ()
  "Return helix-leader leader keymap."
  (or helix-leader-leader-keymap
      (keymap-lookup nil "C-c")))

;;; Which-key integration

(defun helix-leader--show-preview ()
  "Show preview with possible continuations for the keys
that were entered in the helix-leader."
  (helix-leader--show-message)
  (when-let* ((which-key-mode)
              ((or helix-leader--preview-is-active
                   (sit-for which-key-idle-delay t)))
              (keymap (helix-leader--keymap-for-preview)))
    (let ((which-key-show-prefix nil))
      (which-key--create-buffer-and-show nil keymap nil nil))
    (helix-leader--show-message)
    (setq helix-leader--preview-is-active t)))

(defun helix-leader--hide-preview ()
  (when helix-leader--preview-is-active
    (which-key--hide-popup)
    (setq helix-leader--preview-is-active nil)))

(defun helix-leader--show-message ()
  "Show message in echo area for current helix-leader input."
  (when helix-leader-echo
    (let ((message-log-max)) ; disable message logging
      (message "%s%s%s"
               helix-leader-message-prefix
               (propertize (helix-leader--format-prefix) 'face 'font-lock-comment-face)
               (propertize (helix-leader--format-keys) 'face 'font-lock-string-face)))))

(defun helix-leader--keymap-for-preview ()
  "Get a keymap for Which-key preview."
  (let ((keys (helix-leader--collected-keys)))
    (cond (helix-leader--pending-modifier
           (let ((control-p    (lambda (key) (s-contains? "C-" key)))
                 (no-control-p (lambda (key) (not (s-contains? "C-" key)))))
             (pcase helix-leader--pending-modifier
               ('literal
                (let ((keymap (if keys
                                  (helix-leader--lookup-key keys)
                                (helix-leader--leader-keymap))))
                  (helix-leader--filter-keymap keymap no-control-p)))
               ('control
                (if (helix-leader--C-x-or-C-c?)
                    (helix-leader--C-x-or-C-c-preview-keymap)
                  ;; else
                  (let ((keymap (if keys
                                    (helix-leader--lookup-key keys)
                                  (helix-leader--leader-keymap))))
                    (helix-leader--filter-keymap keymap control-p))))
               ('meta
                (if-let* ((keys (concat keys (if keys " ") "ESC"))
                          (keymap (helix-leader--lookup-key keys))
                          (keymap (helix-leader--filter-keymap keymap no-control-p)))
                    (define-keymap "ESC" keymap)))
               ('control-meta
                (if-let* ((keys (concat keys (if keys " ") "ESC"))
                          (keymap (helix-leader--lookup-key keys))
                          (keymap (helix-leader--filter-keymap keymap control-p)))
                    (define-keymap "ESC" keymap))))))
          (keys (if-let* ((keymap (helix-leader--lookup-key keys))
                          ((keymapp keymap)))
                    keymap))
          (t ; no keys
           (let* ((ignored (list "x" "c"
                                 helix-leader-meta-prefix
                                 helix-leader-ctrl-meta-prefix))
                  (keymap (helix-leader--filter-keymap (helix-leader--leader-keymap)
                                                       (lambda (key)
                                                         (not (or (s-contains? "C-" key)
                                                                  (member key ignored)))))))
             (keymap-set keymap "x" "C-x")
             (keymap-set keymap "c" "C-c")
             (when helix-leader-meta-prefix
               (define-key keymap helix-leader-meta-prefix "M-prefix"))
             (when helix-leader-ctrl-meta-prefix
               (define-key keymap helix-leader-ctrl-meta-prefix "C-M-prefix"))
             keymap)))))

(defun helix-leader--C-x-or-C-c-preview-keymap ()
  (let* ((keymap (helix-leader--lookup-key (helix-leader--collected-keys)))
         (ignored-keys `("SPC" ,@(if (helix-leader--meta-keybindings-available-p)
                                     (list helix-leader-meta-prefix
                                           helix-leader-ctrl-meta-prefix))))
         (k1 (helix-leader--filter-keymap keymap
                                          (lambda (key)
                                            (and (s-contains? "C-" key)
                                                 (not (member key ignored-keys))))))
         (k2 (helix-leader--filter-keymap keymap
                                          (lambda (key)
                                            (not (or (s-contains? "C-" key)
                                                     (keymap-lookup k1 (concat "C-" key))
                                                     (member key ignored-keys)))))))
    (make-composed-keymap k1 k2)))

(defun helix-leader--filter-keymap (keymap predicate)
  "Return new keymap that contains only elements from KEYMAP
for which PREDICATE is non-nil."
  (if (keymapp keymap)
      (let ((result (define-keymap)))
        (map-keymap (lambda (event command)
                      (unless (eq command 'digit-argument)
                        (when-let* ((key (single-key-description event))
                                    ((key-valid-p key))
                                    ((not (helix-leader--occupied-key-p key)))
                                    ((not (keymap-lookup result key)))
                                    ((funcall predicate key)))
                          (keymap-set result key command))))
                    keymap)
        result)))

(defun helix-leader--occupied-key-p (key)
  "Return non-nil if KEY with all modifiers stripped is used by helix-leader
itself and hence unavailable."
  (keymap-lookup helix-leader-map (s-with key
                                    (s-replace "C-" "")
                                    (s-replace "M-" "")
                                    (s-replace "S-" ""))))

(defun helix-leader--format-keys ()
  "Return a string to display for current input keys."
  (let* ((keys (helix-leader--collected-keys))
         (keys (cond ((not (or keys
                               helix-leader-leader-keymap
                               (memq helix-leader--pending-modifier '(meta control-meta))))
                      "C-c")
                     ((and keys
                           helix-leader--use-leader-map?
                           (not helix-leader-leader-keymap))
                      (concat "C-c " keys))
                     (keys)))
         (modifier (pcase helix-leader--pending-modifier
                     ('control "C-")
                     ('meta    "M-")
                     ('control-meta "C-M-"))))
    (concat keys (if keys " ") modifier)))

(defun helix-leader--format-prefix ()
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

(provide 'helix-leader)
;;; helix-leader.el ends here
