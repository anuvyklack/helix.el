;;; hel-leader.el -*- lexical-binding: t; -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/anuvyklack/hel.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
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
;;; Code:

(require 's)
(require 'dash)
(require 'hel-macros)
(require 'hel-core)

;;; Keybindings

(hel-keymap-global-set :state '(normal motion)
  "SPC"      'hel-leader
  "C-w SPC"  'hel-leader-other-window
  "C-h k"    'hel-leader-describe-key
  "<f1> k"   'hel-leader-describe-key
  "<help> k" 'hel-leader-describe-key)

;;; Custom variables

(defgroup hel-leader nil
  "Custom group for hel-leader."
  :group 'hel-leader-module)

(defcustom hel-leader-literal-prefix "SPC"
  "The key disables all other modifiers."
  :group 'hel-leader
  :type 'string)

(defcustom hel-leader-meta-prefix "m"
  "The key coresponding to M- modifier."
  :group 'hel-leader
  :type 'string)

(defcustom hel-leader-ctrl-meta-prefix "g"
  "The key coresponding to C-M- modifier."
  :group 'hel-leader
  :type 'string)

(defcustom hel-leader-send-C-x-with-control-modifier t
  "When non-nil, pressing \"x\" in hel-leader initial state will send \"C-x C-\",
i.e. \"C-x\" followed by another `Control' modifier.

When nil, pressing \"x\" will send \"C-x\" without an additional `Control'
modifier."
  :group 'hel-leader
  :type 'string)

(defcustom hel-leader-leader-keymap nil ; mode-specific-map
  "The keymap in which hel-leader will search keybindings with no modifiers.
If nil hel-leader will look under \"C-c\" prefix."
  :group 'hel-leader
  :type 'variable)

(defcustom hel-leader-echo t
  "Whether to show hel-leader messages in the echo area."
  :group 'hel-leader
  :type 'boolean)

(defcustom hel-leader-message-prefix "hel-leader: "
  "The prefix string for hel-leader messages."
  :group 'hel-leader
  :type 'string)

(defvar-keymap hel-leader-map
  :doc "hel-leader service keys."
  "DEL" #'hel-leader-undo ;; DEL in Emacs corresponds to the backspace key
  "<backspace>" #'hel-leader-undo
  "ESC" #'hel-leader-quit
  "<escape>" #'hel-leader-quit
  "C-g" #'hel-leader-quit
  ;; "<remap> <keyboard-quit>" #'hel-leader-quit
  )

;;; Internal vars

(defvar hel-leader--keys nil
  "List with keys entered in hel-leader state.
Keys are strings that satisfies `key-valid-p' and stored ordered
most recent first. For example, key sequence \"C-f M-t h\" will
be stored as \\='(\"h\" \"M-t\" \"C-f\")")

;; (defvar hel-leader--prefix-arg nil
;;   "Prefix argument. during the current hel-leader session.
;; Can be nil, an integer, a list like \\='(4) for `C-u',
;; or \\='- for `M--' (raw minus).")

(defvar hel-leader--pending-modifier nil
  "Stores the pending modifier symbol to be applied to the next key.")

(defvar hel-leader--command nil
  "The command that hel-leader found.")

(defvar hel-leader--use-leader-map? nil
  "When non-nil seek for key bindings in `hel-leader-leader-keymap'.
Other way seek in top level.")

(defvar hel-leader--preview-is-active nil)

;;; Interactive commands

(hel-define-command hel-leader ()
  "Activate hel-leader and interactively evaluate found command."
  :multiple-cursors nil
  (interactive)
  (when-let* ((cmd (hel-leader-start)))
    (setq this-command cmd
          hel-this-command this-command)
    (call-interactively cmd)))

(hel-define-command hel-leader-other-window ()
  (interactive)
  :multiple-cursors nil
  (other-window-prefix)
  (hel-leader))

(hel-define-command hel-leader-describe-key (key-list &optional buffer)
  "Wrapper around `describe-key', that correctly handle key chords entered
with hel-leader. If Helpful package is loaded, `helpful-key' will be used instead
of `describe-key'."
  :multiple-cursors nil
  (interactive (list (help--read-key-sequence)))
  (pcase (key-binding (caar key-list))
    ('hel-leader (when-let* ((cmd (hel-leader-start)))
                   (if (fboundp 'helpful-command)
                       (helpful-command cmd)
                     (describe-command cmd))))
    (_ (if (fboundp 'helpful-key)
           (helpful-key (caar key-list))
         (describe-key key-list buffer)))))

;;; Core

(defun hel-leader-start ()
  "Enter hel-leader state.
When EXECUTE is non-nil execute the found command.
Return the found command."
  ;; Try to make this command transparent.
  (setq this-command last-command)
  (setq hel-leader--keys nil
        hel-leader--pending-modifier nil
        hel-leader--use-leader-map? nil
        hel-leader--command nil)
  (unwind-protect
      (progn
        (hel-leader--show-preview)
        (while (not (eq (hel-leader--handle-input-event (read-key))
                        :quit))
          (hel-leader--show-preview)))
    (hel-leader--hide-preview))
  hel-leader--command)

(defun hel-leader--handle-input-event (event)
  "Handle input EVENT. Return `:quit' if handling is completed."
  ;; (setq last-command-event last-input-event)
  (let ((key (single-key-description event)))
    (if-let* ((cmd (keymap-lookup hel-leader-map key)))
        (call-interactively cmd)
      (hel-leader--handle-input-key key))))

(defun hel-leader--handle-input-key (key)
  (cond ((and (hel-leader--C-x-or-C-c?)
              (equal "SPC" key))
         ;; Toggle pending modifier if we are in C-x or C-c state.
         (setq hel-leader--pending-modifier
               (if (or (null hel-leader--pending-modifier)
                       (eq 'literal hel-leader--pending-modifier))
                   'control
                 'literal)))
        (hel-leader--pending-modifier
         (push (pcase hel-leader--pending-modifier
                 ('control      (hel-leader--add-control key))
                 ('meta         (hel-leader--add-meta key))
                 ('control-meta (hel-leader--add-control-meta key))
                 ('literal      key))
               hel-leader--keys)
         (setq hel-leader--pending-modifier
               (cond ((eq hel-leader--pending-modifier 'literal) 'literal)
                     ((hel-leader--C-x-or-C-c?) 'control)
                     (t nil))))
        ((and (equal hel-leader-meta-prefix key)
              (hel-leader--meta-keybindings-available-p))
         (setq hel-leader--pending-modifier 'meta))
        ((and (equal hel-leader-ctrl-meta-prefix key)
              (hel-leader--meta-keybindings-available-p))
         (setq hel-leader--pending-modifier 'control-meta))
        (hel-leader--keys
         (push key hel-leader--keys))
        ;; All following conditions assumes that `hel-leader--keys' are empty.
        ((equal "c" key)
         (push "C-c" hel-leader--keys)
         (setq hel-leader--pending-modifier 'control))
        ((equal "x" key)
         (push "C-x" hel-leader--keys)
         (when hel-leader-send-C-x-with-control-modifier
           (setq hel-leader--pending-modifier 'control)))
        (t
         (setq hel-leader--use-leader-map? t)
         (push key hel-leader--keys)))
  (when hel-leader--keys
    (hel-leader--try-execute)))

(defun hel-leader--try-execute ()
  "Try execute command, return t when the translation progress can be ended.
This function supports a fallback behavior, where it allows to use
`SPC x f' to execute `C-x C-f' or `C-x f' when `C-x C-f' is not bound."
  (when-let* ((keys (hel-leader--collected-keys)))
    (let ((cmd (hel-leader--lookup-key keys)))
      (cond ((commandp cmd t)
             (setq hel-leader--command cmd)
             :quit)
            ((keymapp cmd))
            ((s-contains? "C-" (car hel-leader--keys))
             ;; Remove "C-" from the last entered key and try again.
             (setcar hel-leader--keys (s-replace "C-" "" (car hel-leader--keys)))
             (hel-leader--try-execute))
            (t
             (message "%s is undefined" keys)
             :quit)))))

(defun hel-leader-undo ()
  "Pop the last input."
  (interactive)
  (setq this-command last-command)
  (cond (hel-leader--pending-modifier
         (setq hel-leader--pending-modifier nil))
        (hel-leader--keys
         (pop hel-leader--keys))
        (t
         (when hel-leader-echo (message "hel-leader exit"))
         :quit)))

(defun hel-leader-quit ()
  "Quit hel-leader state."
  (interactive)
  (setq this-command last-command)
  (when hel-leader-echo (message "hel-leader exit"))
  :quit) ; Indicate that hel-leader loop should be stopped

(defun hel-leader--add-control (key)
  (pcase key
    ("TAB" "C-<tab>")
    ("RET" "C-RET") ;; "C-<return>"
    ("ESC" "ESC")
    (_ (if (s-contains? "C-" key)
           key
         (concat "C-" (hel-leader--handle-shift key))))))

(defun hel-leader--add-meta (key)
  (if (s-contains? "C-" key)
      (hel-leader--add-control-meta key)
    (pcase key
      ("TAB" "M-TAB") ;; "M-<tab>"
      ("RET" "M-RET") ;; "M-<return>"
      ("ESC" "ESC")
      (_ (concat "M-" key)))))

(defun hel-leader--add-control-meta (key)
  (pcase key
    ("TAB" "C-M-<tab>")
    ("RET" "C-M-<return>")
    ("ESC" "ESC")
    (_ (concat "C-M-" (s-with key
                        (s-replace "C-" "")
                        (s-replace "M-" "")
                        (hel-leader--handle-shift))))))

(defun hel-leader--handle-shift (str)
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

(defun hel-leader--collected-keys ()
  "Return string with keys collected by hel-leader. If no keys return nil."
  (if hel-leader--keys
      (string-join (reverse hel-leader--keys) " ")))

(defun hel-leader--C-x-or-C-c? ()
  "Return t if keys collected by hel-leader begin from C-x or C-c."
  (if hel-leader--keys
      (let ((first-key (-last-item hel-leader--keys)))
        (member first-key '("C-x" "C-c")))))

(defun hel-leader--meta-keybindings-available-p ()
  "Return non-nil if there are keybindings that starts with Meta prefix."
  (let ((keys (hel-leader--collected-keys)))
    (or (not keys)
        (if-let* ((keymap (hel-leader--lookup-key keys))
                  ((keymapp keymap)))
            ;; Key sequences starts with ESC are accessible via Meta key.
            (keymap-lookup keymap "ESC")))))

(defun hel-leader--lookup-key (keys)
  "Return the command bound to KEYS."
  (let ((keymap (if hel-leader--use-leader-map?
                    (hel-leader--leader-keymap))))
    (keymap-lookup keymap keys)))

(defun hel-leader--leader-keymap ()
  "Return hel-leader leader keymap."
  (or hel-leader-leader-keymap
      (keymap-lookup nil "C-c")))

;;; Which-key integration

(declare-function which-key--create-buffer-and-show "which-key"
                  (&optional prefix-keys from-keymap filter prefix-title))
(declare-function which-key--hide-popup "which-key" ())
(defvar which-key-show-prefix)
(defvar which-key-idle-delay)

(defun hel-leader--show-preview ()
  "Show preview with possible continuations for the keys
that were entered in the hel-leader."
  (hel-leader--show-message)
  (when-let* ((which-key-mode)
              ((or hel-leader--preview-is-active
                   (sit-for which-key-idle-delay t)))
              (keymap (hel-leader--keymap-for-preview)))
    (let ((which-key-show-prefix nil))
      (which-key--create-buffer-and-show nil keymap nil nil))
    (hel-leader--show-message)
    (setq hel-leader--preview-is-active t)))

(defun hel-leader--hide-preview ()
  (when hel-leader--preview-is-active
    (which-key--hide-popup)
    (setq hel-leader--preview-is-active nil)))

(defun hel-leader--show-message ()
  "Show message in echo area for current hel-leader input."
  (when hel-leader-echo
    (let ((message-log-max)) ; disable message logging
      (message "%s%s%s"
               hel-leader-message-prefix
               (propertize (hel-leader--format-prefix) 'face 'font-lock-comment-face)
               (propertize (hel-leader--format-keys) 'face 'font-lock-string-face)))))

(defun hel-leader--keymap-for-preview ()
  "Get a keymap for Which-key preview."
  (let ((keys (hel-leader--collected-keys)))
    (cond (hel-leader--pending-modifier
           (let ((control-p    (lambda (key) (s-contains? "C-" key)))
                 (no-control-p (lambda (key) (not (s-contains? "C-" key)))))
             (pcase hel-leader--pending-modifier
               ('literal
                (let ((keymap (if keys
                                  (hel-leader--lookup-key keys)
                                (hel-leader--leader-keymap))))
                  (hel-leader--filter-keymap keymap no-control-p)))
               ('control
                (if (hel-leader--C-x-or-C-c?)
                    (hel-leader--C-x-or-C-c-preview-keymap)
                  ;; else
                  (let ((keymap (if keys
                                    (hel-leader--lookup-key keys)
                                  (hel-leader--leader-keymap))))
                    (hel-leader--filter-keymap keymap control-p))))
               ('meta
                (if-let* ((keys (concat keys (if keys " ") "ESC"))
                          (keymap (hel-leader--lookup-key keys))
                          (keymap (hel-leader--filter-keymap keymap no-control-p)))
                    (define-keymap "ESC" keymap)))
               ('control-meta
                (if-let* ((keys (concat keys (if keys " ") "ESC"))
                          (keymap (hel-leader--lookup-key keys))
                          (keymap (hel-leader--filter-keymap keymap control-p)))
                    (define-keymap "ESC" keymap))))))
          (keys (if-let* ((keymap (hel-leader--lookup-key keys))
                          ((keymapp keymap)))
                    keymap))
          (t ; no keys
           (let* ((ignored (list "x" "c"
                                 hel-leader-meta-prefix
                                 hel-leader-ctrl-meta-prefix))
                  (keymap (hel-leader--filter-keymap (hel-leader--leader-keymap)
                                                     (lambda (key)
                                                       (not (or (s-contains? "C-" key)
                                                                (member key ignored)))))))
             (keymap-set keymap "x" "C-x")
             (keymap-set keymap "c" "C-c")
             (when hel-leader-meta-prefix
               (define-key keymap hel-leader-meta-prefix "M-prefix"))
             (when hel-leader-ctrl-meta-prefix
               (define-key keymap hel-leader-ctrl-meta-prefix "C-M-prefix"))
             keymap)))))

(defun hel-leader--C-x-or-C-c-preview-keymap ()
  (let* ((keymap (hel-leader--lookup-key (hel-leader--collected-keys)))
         (ignored-keys `("SPC" ,@(if (hel-leader--meta-keybindings-available-p)
                                     (list hel-leader-meta-prefix
                                           hel-leader-ctrl-meta-prefix))))
         (k1 (hel-leader--filter-keymap keymap
                                        (lambda (key)
                                          (and (s-contains? "C-" key)
                                               (not (member key ignored-keys))))))
         (k2 (hel-leader--filter-keymap keymap
                                        (lambda (key)
                                          (not (or (s-contains? "C-" key)
                                                   (keymap-lookup k1 (concat "C-" key))
                                                   (member key ignored-keys)))))))
    (make-composed-keymap k1 k2)))

(defun hel-leader--filter-keymap (keymap predicate)
  "Return new keymap that contains only elements from KEYMAP
for which PREDICATE is non-nil."
  (if (keymapp keymap)
      (let ((result (define-keymap)))
        (map-keymap (lambda (event command)
                      (unless (eq command 'digit-argument)
                        (when-let* ((key (single-key-description event))
                                    ((key-valid-p key))
                                    ((not (hel-leader--occupied-key-p key)))
                                    ((not (keymap-lookup result key)))
                                    ((funcall predicate key)))
                          (keymap-set result key command))))
                    keymap)
        result)))

(defun hel-leader--occupied-key-p (key)
  "Return non-nil if KEY with all modifiers stripped is used by hel-leader
itself and hence unavailable."
  (keymap-lookup hel-leader-map (s-with key
                                  (s-replace "C-" "")
                                  (s-replace "M-" "")
                                  (s-replace "S-" ""))))

(defun hel-leader--format-keys ()
  "Return a string to display for current input keys."
  (let* ((keys (hel-leader--collected-keys))
         (keys (cond ((not (or keys
                               hel-leader-leader-keymap
                               (memq hel-leader--pending-modifier '(meta control-meta))))
                      "C-c")
                     ((and keys
                           hel-leader--use-leader-map?
                           (not hel-leader-leader-keymap))
                      (concat "C-c " keys))
                     (keys)))
         (modifier (pcase hel-leader--pending-modifier
                     ('control "C-")
                     ('meta    "M-")
                     ('control-meta "C-M-"))))
    (concat keys (if keys " ") modifier)))

(defun hel-leader--format-prefix ()
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

(provide 'hel-leader)
;;; hel-leader.el ends here
