;;; helix-core.el --- Core functionality -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Helix states are similar to Emacs minor modes, but they are not minor modes
;; in the sense that they are not created with `define-minor-mode' macro.
;;
;; The internal mechanism in general terms is as follows: `helix-mode-map-alist'
;; symbol is stored in `emulation-mode-map-alists' list, and keymap binded to it
;; is changed on every Helix state change.
;;
;; Every state has general globally shared keymap, and "nested" keymaps that are
;; stored in other keymaps (typical expample are major-mode maps) under special
;; keys like "<normal-state>" or "<insert-state>", that are associated with
;; particular Helix states and can not be produced by a keyboard. On every Helix
;; state change, the algorithm traverse all currently active keymaps looking for
;; these keys, and activates nested keymaps associated with them.
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'helix-vars)
(require 'helix-common)

;;; helix-mode

(define-minor-mode helix-local-mode
  "Minor mode for setting up Helix in a current buffer."
  :global nil
  (if helix-local-mode
      (progn
        ;; Just push the symbol here. We update its content on every
        ;; Helix state change.
        (cl-pushnew 'helix-mode-map-alist emulation-mode-map-alists)
        (helix-normal-state 1))
    ;; else
    (helix-disable-current-state)))

;;; Helix state

(defmacro helix-define-state (state doc &rest body)
  "Define Helix state STATE.
DOC is a general description and shows up in all docstrings.

BODY is executed each time the state is enabled or disabled.

Optional keyword arguments:
- `:cursor' - default cursor specification.
- `:hook' - list of functions to run on each entry/exit of this state.
- `:after-hook' - Lisp form to evaluate after state hooks have been run.

\(fn STATE DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body)))
  (let* ((state-name (concat (capitalize (symbol-name state))
                             " state"))
         (symbol (intern (format "helix-%s-state" state)))
         (variable symbol)
         (statefun symbol)
         (cursor (intern (format "%s-cursor" symbol)))
         (hook   (intern (format "%s-hook" symbol)))
         (keymap (intern (format "%s-map" symbol)))
         key arg cursor-value hook-value after-hook)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (pcase key
        (:cursor (setq cursor-value arg))
        (:hook (setq hook-value arg)
               (unless (listp hook-value)
                 (setq hook-value (list hook-value))))
        (:after-hook (setq after-hook arg))))
    `(progn
       ;; Save the state's properties in `helix-state-properties' for
       ;; runtime lookup.
       (helix--add-to-alist helix-state-properties ',state
                            (list :name     ,state-name
                                  :variable ',variable
                                  :fun      ',statefun
                                  :cursor   ',cursor
                                  :keymap   ',keymap
                                  :hook     ',hook))
       (defvar ,cursor ,cursor-value
         ,(format "Cursor for %s.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above." state-name))
       ;; hook
       (defvar ,hook nil
         ,(format "Hooks to run on entry/exit %s." state-name))
       (dolist (func ',hook-value)
         (add-hook ',hook func))
       ;; keymap
       (defvar ,keymap (make-sparse-keymap)
         ,(format "Global keymap for Helix %s." state-name))
       ;; (helix--add-to-alist helix-global-keymaps-alist ',symbol ',keymap)
       ;; (push helix-global-keymaps-alist ',keymap)
       ;; state variable
       (helix-defvar-local ,variable nil
         ,(format "Non nil if current Helix state is %s." state-name))
       ;; state function
       (defun ,statefun (&optional arg)
         ,(format "Switch Helix into %s.
When ARG is non-positive integer and Helix is in %s — disable it.\n\n%s"
                  state-name state-name doc)
         (interactive)
         (if (and (numberp arg) (< arg 1))
             (when (eq helix--state ',state)
               (setq helix--state nil
                     helix--previous-state ',state
                     ,variable nil))
           ;; else
           (unless helix-local-mode (helix-local-mode))
           (helix-disable-current-state)
           (setq helix--state ',state
                 ,variable t)
           (helix-update-cursor))
         (helix-update-active-keymaps)
         ,@body
         (run-hooks ',hook)
         ,@(when after-hook `(,after-hook))
         (force-mode-line-update)))))

(defun helix-disable-current-state ()
  "Disable current Helix state."
  (when-let* ((state helix--state)
              (func (helix-state-property state :fun))
              ((functionp func)))
    (funcall func -1)))

(defun helix-state-property (state property)
  "Return the value of PROPERTY for STATE.
PROPERTY is a keyword as used by `helix-define-state'.
STATE is the state's symbolic name.

If STATE is t, return an association list of states and their PROP
values instead."
  ;; (if (eq state t)
  ;;     (cl-loop for (key . plist) in helix-state-properties with result do
  ;;              (let ((p (plist-member plist prop)))
  ;;                (when p (push (cons key (cadr p)) result)))
  ;;              finally return result)
  ;;   (let ((val (plist-get (cdr (assq state helix-state-properties))
  ;;                         prop)))
  ;;     (if (and value (symbolp val) (boundp val))
  ;;         (symbol-value val)
  ;;       val)))
  (let* ((state-properties (cdr (assq state helix-state-properties)))
         (val (plist-get state-properties property)))
    (if (memq property '(:keymap :cursor))
        (symbol-value val)
      val)))

(defun helix-state (&optional state)
  "Return current Helix state.
If optional STATE argument is passed return t if Helix is currently
in STATE, else return nil."
  (if state
      (eq state helix--state)
    helix--state))

(defun helix-state-p (symbol)
  "Return non-nil if SYMBOL corresponds to Helix state."
  (assq symbol helix-state-properties))

;;; Keymaps

(defun helix-update-active-keymaps ()
  "Reset keymaps for current Helix state."
  (helix-activate-state-keymaps helix--state))

(defun helix-activate-state-keymaps (state)
  "Set the value of the `helix-mode-map-alist' in the current buffer
according to the Helix STATE."
  (setq helix-mode-map-alist
        (if state
            (let ((global-keymap (cons t (helix-state-property state :keymap)))
                  other-maps)
              (dolist (keymap (current-active-maps) (nreverse other-maps))
                (when-let* ((helix-map (helix-get-nested-helix-keymap keymap state))
                            (mode      (helix-get-minor-mode-for-keymap keymap)))
                  (push (cons mode helix-map) other-maps)))
              `(,@other-maps ,global-keymap)))))

(defun helix-get-minor-mode-for-keymap (keymap)
  "Return the minor mode associated with KEYMAP or t if it doesn't have one."
  (when (symbolp keymap)
    (setq keymap (symbol-value keymap)))
  (or (car (rassq keymap minor-mode-map-alist))
      t))

(defun helix-get-nested-helix-keymap (keymap state)
  "Get from KEYMAP the nested keymap associated with Helix STATE."
  (when state
    (let* ((key (vector (intern (format "%s-state" state))))
           (helix-map (lookup-key keymap key)))
      (if (helix-nested-keymap-p helix-map)
          helix-map))))

(defun helix-create-nested-helix-keymap (keymap state)
  "Create in KEYMAP a nested keymap for Helix STATE."
  (let ((helix-map (make-sparse-keymap))
        (key (vector (intern (format "%s-state" state))))
        (prompt (format "Helix keymap for %s"
                        (or (helix-state-property state :name)
                            (format "%s state" state)))))
    (helix-set-keymap-prompt helix-map prompt)
    (define-key keymap key helix-map)
    helix-map))

(defun helix-set-keymap-prompt (map prompt)
  "Set the prompt-string of MAP to PROMPT."
  (delq (keymap-prompt map) map)
  (when prompt
    (setcdr map (cons prompt (cdr map)))))

(defun helix-nested-keymap-p (keymap)
  "Return non-nil if KEYMAP is a Helix nested keymap."
  (if-let* ((prompt (keymap-prompt keymap)))
      (string-prefix-p "Helix keymap" prompt)))

(defun helix-keymap-set (keymap state key definition &rest rest)
  "Create keybinding from KEY to DEFINITION for Helix STATE in KEYMAP.
Accepts any number of KEY DEFINITION pairs.
The defined keybindings will be active in specified Helix STATE.
KEYMAP can be nil, then keybindings will be set in main STATE keymap.
If STATE is nil this function will work like `keymap-set' with addition
that multiple keybindings can be set at once.
KEY, DEFINITION arguments are like those of `keymap-set'.
For example:

   (helix-keymap-set text-mode-map 'normal
      \"f\" #'foo
      \"b\" #'bar)"
  (declare (indent defun))
  (when (and state (not (helix-state-p state)))
    (user-error "Helix state `%s' not known to be defined" state))
  (unless (cl-evenp (length rest))
    (user-error "The number of `key definition' pairs is not even"))
  (let ((map (cond ((and keymap state)
                    (or (helix-get-nested-helix-keymap keymap state)
                        (helix-create-nested-helix-keymap keymap state)))
                   (state (helix-state-property state :keymap))
                   (keymap)
                   (t (current-global-map)))))
    (keymap-set map key definition)
    (while rest
      (let ((key (pop rest))
            (definition (pop rest)))
        (keymap-set map key definition)))))

;;; Cursor

(defun helix-update-cursor ()
  "Update the cursor for current Helix STATE in current buffer."
  (when (eq (window-buffer) (current-buffer))
    (let* ((state (or helix--state 'normal))
           (cursor (helix-state-property state :cursor)))
      (helix-set-cursor cursor))))

(defun helix-set-cursor (specs)
  "Change the cursor's apperance according to SPECS.
SPECS may be a cursor type as per `cursor-type', a color
string as passed to `set-cursor-color', a zero-argument
function for changing the cursor, or a list of the above."
  (unless (and (not (functionp specs))
               (listp specs)
               (null (cdr-safe (last specs))))
    (setq specs (list specs)))
  (dolist (spec specs)
    (cond ((functionp spec)
           (ignore-errors (funcall spec)))
          ((stringp spec)
           (helix-set-cursor-color spec))
          (t
           (helix-set-cursor-type spec)))))

(defun helix-set-cursor-type (type)
  "Set cursor TYPE."
  (if (display-graphic-p)
      (setq cursor-type type)
    (let* ((type (or (car-safe type) type))
           (code (pcase type
                   ('bar "6")
                   ('hbar "4")
                   (_ "2"))))
      (send-string-to-terminal (concat "\e[" code " q")))))

(defun helix-set-cursor-color (color)
  "Set the cursor color to COLOR."
  ;; `set-cursor-color' forces a redisplay, so only
  ;; call it when the color actually changes
  (unless (equal (frame-parameter nil 'cursor-color) color)
    (set-cursor-color color)))

(provide 'helix-core)
;;; helix-core.el ends here
