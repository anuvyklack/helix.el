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
;; in the sense that they are not created with `define-minor-mode'.
;;
;; Every state has general globally shared keymap, and auxiliary keymaps stored
;; in other keymaps under special keys like "<normal-state>" or "<insert-state>",
;; that are associated with particular Helix states and can not be produced by
;; a keyboard. On every Helix state change, the algorithm traverse all currently
;; active keymaps looking for these keys, and activates keymaps associated with
;; them.
;;
;;; Code:

(require 'cl-lib)
(require 'dash)

(define-minor-mode helix-local-mode
  "Minor mode for setting up Helix in a current buffer."
  :global nil
  (if helix-local-mode
      (progn
        ;; Just push the symbol here. We will update its content
        ;; on every Helix state change.
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
                            (list :name     ',state-name
                                  :variable ',variable
                                  :fun      ',statefun
                                  :cursor   ',cursor
                                  :keymap   ',keymap
                                  :hook     ',hook))
       (defvar ,cursor ',cursor-value
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
       (helix--add-to-alist helix-global-keymaps-alist ',symbol ',keymap)
       ;; state variable
       (helix-defvar-local ,variable nil
         ,(format "Non nil if current Helix state is %s." state-name))
       ;; state function
       (defun ,statefun (&optional arg)
         ,(format "Enable Helix %s. Disable when ARG is non-positive integer.\n\n%s"
                  state-name doc)
         (interactive)
         (setq ,variable (cond ((and (numberp arg) (< arg 1)) nil)
                               (t t)))
         (helix-disable-current-state)
         (setq helix-state (if ,variable ',state))
         (when ,variable
           (unless helix-local-mode (helix-local-mode))
           (when (eq (window-buffer) (current-buffer))
             (helix-setup-cursor ',state)))
         (helix-update-active-keymaps)
         ,@body
         (run-hooks ',hook)
         ,@(when after-hook `(,after-hook))
         (force-mode-line-update)))))

(defun helix-state-p (sym)
  "Whether SYM is the name of a state."
  (assq sym helix-state-properties))

(defun helix-state-property (state prop)
  "Return the value of property PROP for STATE.
PROP is a keyword as used by `helix-define-state'. STATE is the state's
symbolic name.

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
  (plist-get (cdr (assq state helix-state-properties))
             prop))

(defun helix-disable-current-state ()
  "Disable current Helix state."
  (when-let* ((state helix-state)
              (func (helix-state-property state :fun)))
    (when (functionp func)
      (funcall func -1))))

;;; Keymaps

(defun helix-update-active-keymaps ()
  "Set the value of the `helix-mode-map-alist' in the current buffer
according to current Helix STATE."
  (setq helix-mode-map-alist
        (when-let* ((state helix-state))
          (let ((global-keymap (cons (helix-state-property state :symbol)
                                     (-> (helix-state-property state :keymap)
                                         (symbol-value))))
                result)
            (dolist (keymap (current-active-maps) (nreverse result))
              (when-let* ((helix-map (helix-get-nested-helix-keymap keymap state))
                          (mode      (helix-get-minor-mode-for-keymap keymap)))
                (push (cons mode helix-map) result)))
            (list result global-keymap)))))

(defun helix-get-minor-mode-for-keymap (keymap)
  "Return either Helix state or the minor mode associated with KEYMAP
or t if none is found."
  ;; First, for speed purposes, check if it is ours keymap.
  ;; Then, check `minor-mode-map-alist'.
  (or (if (symbolp keymap)
          (car (rassq keymap helix-global-keymaps-alist)))
      (let ((map (if (keymapp keymap) keymap
                   (symbol-value keymap))))
        (or (car (rassq map (mapcar #'(lambda (e)
                                        ;; from (MODE-SYMBOL . KEYMAP-SYMBOL)
                                        ;; to   (MODE-SYMBOL . KEYMAP)
                                        (cons (car-safe e)
                                              (symbol-value (cdr-safe e))))
                                    helix-global-keymaps-alist)))
            (car (rassq map minor-mode-map-alist))))
      t))

(defun helix-get-nested-helix-keymap (keymap state)
  "Get from KEYMAP the nested keymap for Helix STATE."
  (when state
    (let* ((key (vector (intern (format "%s-state" state))))
           (hx  (keymap-lookup keymap key)))
      (if (helix-keymap-p hx) hx))))

(defun helix-create-nested-helix-keymap (keymap state)
  "Create in KEYMAP a nested keymap for Helix STATE."
  (let ((hx  (make-sparse-keymap))
        (key (vector (intern (format "%s-state" state))))
        (prompt (format "Helix keymap for %s"
                        (or (helix-state-property state :name)
                            (format "%s state" state)))))
    (helix-set-keymap-prompt hx prompt)
    (define-key keymap key hx)
    hx))

(defun helix-keymap-p (keymap)
  "Whether KEYMAP is an Helix keymap."
  (let ((prompt (keymap-prompt keymap)))
    (when prompt (string-prefix-p "Helix keymap" prompt))))

;;; Cursor

(defun helix-refresh-cursor (&optional state buffer)
  "Refresh the cursor for STATE in BUFFER.
BUFFER defaults to the current buffer.
If STATE is nil use the buffer current state."
  (let* ((state (or state helix-state 'normal))
         (cursor (-> (helix-state-property state :cursor)
                     (symbol-value)))
         (color (or (and (stringp cursor) cursor)
                    (and (listp cursor) (evil-member-if #'stringp cursor))
                    (frame-parameter nil 'cursor-color))))
    (with-current-buffer (or buffer (current-buffer))
      ;; if both STATE and `evil-default-cursor'
      ;; specify a color, don't set it twice
      (evil-set-cursor (if (and color (listp default))
                           (cl-remove-if #'stringp default)
                         default))
      (evil-set-cursor cursor))))

;;; Utils

(defmacro helix--add-to-alist (alist &rest elements)
  "Add the association of KEY and VAL to the value of ALIST.
If the list already contains an entry for KEY, update that entry;
otherwise prepend it to the list.

\(fn ALIST [KEY VAL]...)"
  `(progn
     ,@(cl-loop
        for (key val) on elements by #'cddr collect
        `(setf (alist-get ,key ,alist nil nil #'equal) ,val))
     ,alist))

(defun helix-set-keymap-prompt (map prompt)
  "Set the prompt-string of MAP to PROMPT."
  (delq (keymap-prompt map) map)
  (when prompt
    (setcdr map (cons prompt (cdr map)))))

(provide 'helix-core)
;;; helix-core.el ends here
