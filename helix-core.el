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

(define-minor-mode helix-local-mode
  "Minor mode for setting up Helix in a current buffer."
  :global nil
  (cond (helix-local-mode
         ;; Just push the symbol here. We will update its content
         ;; on every Helix state change.
         (cl-pushnew 'helix-mode-map-alist emulation-mode-map-alists))
        (t
         (helix-deactivate-keymaps))))

;;; Helix states

(defmacro helix-define-state (state doc &rest body)
  "Define Helix state STATE.
DOC is a general description and shows up in all docstrings.

BODY is executed each time the state is enabled or disabled.

Optional keyword arguments:
- `:cursor' - default cursor specification.
- `:entry-hook' - list of functions to run when entering this state.
- `:exit-hook' - list of functions to run when exiting this state.

\(fn STATE DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (doc-string 2)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body)))
  (string-match "^\\(.+\\)\\(\\(?:.\\|\n\\)*\\)" doc)
  (let* ((name (let ((n (match-string 1 doc)))
                 (string-match "^\\(.+?\\)\\.?$" n)
                 (match-string 1 n)))
         (doc  (let ((d (match-string 2 doc)))
                 (if (or (null d) (string= d ""))
                     ""
                   (format "\n%s" d))))
         (symbol   (intern (format "helix-%s-state" state)))
         ;; (predicate  (intern (format "%s-p" symbol)))
         (cursor     (intern (format "%s-cursor" symbol)))
         (keymap     (intern (format "%s-map" symbol)))
         (entry-hook (intern (format "%s-entry-hook" symbol)))
         (exit-hook  (intern (format "%s-exit-hook" symbol)))
         key arg cursor-value entry-hook-value exit-hook-value)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond ((eq key :cursor)
             (setq cursor-value arg))
            ((eq key :entry-hook)
             (setq entry-hook-value arg)
             (unless (listp entry-hook-value)
               (setq entry-hook-value (list entry-hook-value))))
            ((eq key :exit-hook)
             (setq exit-hook-value arg)
             (unless (listp exit-hook-value)
               (setq exit-hook-value (list exit-hook-value))))))
    `(progn
       ;; Save the state's properties in `helix-state-properties' for
       ;; runtime lookup.
       (helix--add-to-alist helix-state-properties ',state
                            (list
                             :symbol ',symbol
                             :name ',name
                             :cursor (defvar ,cursor ',cursor-value
                                       ,(format "Cursor for %s.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above." name))
                             :keymap (defvar ,keymap (make-sparse-keymap)
                                       ,(format "Keymap for %s." name))
                             :entry-hook (defvar ,entry-hook nil
                                           ,(format "Hooks to run when entering %s." name))
                             :exit-hook (defvar ,exit-hook nil
                                          ,(format "Hooks to run when exiting %s." name))
                             ;; :predicate ',predicate
                             ))
       ;; hooks
       (dolist (func ',entry-hook-value) (add-hook ',entry-hook func))
       (dolist (func ',exit-hook-value) (add-hook ',exit-hook func))
       ;; state variable
       (helix-defvar-local ,symbol nil
         ,(format "Non nil if Helix is in %s." ,name))
       ;; state function
       (defun ,symbol (&optional arg)
         ,(format "Enable Helix %s. Disable with negative ARG.%s" name doc)
         (interactive)
         (cond ((and (numberp arg) (< arg 0)) ; deactivate
                (run-hooks ',exit-hook)
                ,@body
                (helix-deactivate-keymaps)
                (setq helix-state nil
                      ,symbol nil))
               (t ; activate
                (unless helix-local-mode (helix-local-mode))
                (setq helix-state ',state
                      ,symbol t)
                (helix-activate-keymaps ',state)
                (when (eq (window-buffer) (current-buffer))
                  (helix-setup-cursor ',state))
                ,@body
                (run-hooks ',entry-hook))))
       ;; ;; state predicate
       ;; (defun ,predicate ()
       ;;   ,(format "Whether the current Helix state is %s." name)
       ;;   (and helix-local-mode
       ;;        (eq helix-state ',state)))
       ;; keymap
       ;; (helix-define-keymap ,keymap)
       )))

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

;;; Keymaps

(defmacro helix-define-keymap (state)
  "Define a keymap KEYMAP listed in `helix-mode-map-alist'."

  )

(defun helix-activate-keymaps (&optional state)
  "Set the value of the `helix-mode-map-alist' in the current buffer
according to Helix STATE."
  (setq state (or state (helix-state)))
  ;; (setq helix-mode-map-alist (helix-get-state-keymaps state))
  (setq helix-mode-map-alist 
        (let (result
              (global-keymap (cons (helix-state-property state :symbol)
                                   (thread-first
                                     (helix-state-property state :keymap)
                                     (symbol-value)))))
          (dolist (keymap (current-active-maps) (nreverse result))
            (when-let* ((helix-map (helix-get-nested-helix-keymap keymap state))
                        (mode      (helix-get-minor-mode-for-keymap keymap)))
              (push (cons mode helix-map) result)))
          (list result global-keymap))))

(defun helix-deactivate-keymaps ()
  "Deactivate all Helix keymaps."
  (setq helix-mode-map-alist nil))

;; (defun helix-get-state-keymaps (state)
;;   "Get the keymap alist of Helix keymaps to activate for STATE."
;;   (let (result
;;         (global-keymap (cons (helix-state-property state :symbol)
;;                              (thread-first (helix-state-property state :keymap)
;;                                            (symbol-value)))))
;;     (dolist (keymap (current-active-maps) (nreverse result))
;;       (when-let* ((helix-map (helix-get-nested-helix-keymap keymap state))
;;                   (mode      (helix-get-minor-mode-for-keymap keymap)))
;;         (push (cons mode helix-map) result)))
;;     (list result global-keymap)))

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
                                        ;; from (MODE-SYMBOL . MAP-SYMBOL)
                                        ;; to (MODE-SYMBOL . MAP)
                                        (cons (car-safe e)
                                              (symbol-value (cdr-safe e))))
                                    helix-global-keymaps-alist)))
            (car (rassq map minor-mode-map-alist))))
      t))

;;;; Auxiliary keymaps

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
