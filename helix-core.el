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
;;  Core functionality
;;
;;; Code:

(require 'cl-lib)

(define-minor-mode helix-local-mode
  "Minor mode for setting up Helix in a current buffer."
  :global nil
  (if helix-local-mode
      (progn
        ;; Just push symbol here.
        ;; Later we will update its contents on every helix state change.
        (cl-pushnew 'helix-mode-map-alist emulation-mode-map-alists)
        )
    (progn)))

;;; Helix states

;; Every state has general globaly shared keymap, and auxiliary per buffer
;; keymap pined to major mode.
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
         (state-id   (intern (format "helix-%s-state" state)))
         (predicate  (intern (format "%s-p" state-id)))
         (cursor     (intern (format "%s-cursor" state-id)))
         (keymap     (intern (format "%s-map" state-id)))
         (entry-hook (intern (format "%s-entry-hook" state-id)))
         (exit-hook  (intern (format "%s-exit-hook" state-id)))
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
                             :predicate ',predicate))
       ;; hooks
       (dolist (func ',entry-hook-value) (add-hook ',entry-hook func))
       (dolist (func ',exit-hook-value) (add-hook ',exit-hook func))
       ;; state predicate
       (defun ,predicate ()
         ,(format "Whether the current Helix state is %s." name)
         (and helix-local-mode
              (eq helix-state ',state)))
       ;; state function
       (defun ,state-id (&optional arg)
         ,(format "Enable Helix %s. Disable with negative ARG.%s" name doc)
         (interactive)
         (cond ((and (numberp arg) (< arg 0))
                (run-hooks ',exit-hook)
                ,@body
                (helix-deactivate-keymaps)
                (setq helix-state nil))
               (t
                (unless helix-local-mode (helix-local-mode))
                (setq helix-state ',state)
                (helix-activate-keymaps ',state)
                (when (eq (window-buffer) (current-buffer))
                  (helix-setup-cursor ',state))
                ,@body
                (run-hooks ',entry-hook))))
       ;; keymap
       ;; (helix-define-keymap ,keymap)
       )))

(defun helix-state-p (sym)
  "Whether SYM is the name of a state."
  (assq sym helix-state-properties))

(defun helix-state-property (state prop &optional value)
  "Return the value of property PROP for STATE.
PROP is a keyword as used by `helix-define-state'. STATE is the state's
symbolic name. If VALUE is non-nil and the value is a variable, return
the value of that variable.

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
  (let ((val (plist-get (cdr (assq state helix-state-properties))
                        prop)))
    (if (and value (symbolp val) (boundp val))
        (symbol-value val)
      val))
  )

;;; Keymaps

(defun helix-activate-keymaps (&optional state)
  "Set the value of the `helix-mode-map-alist' in the current buffer
according to Helix STATE."
  (setq state (or state (helix-state)))
  (setq helix-mode-map-alist (helix-get-state-keymaps state)))

(defun helix-deactivate-keymaps ()
  "Deactivate all Helix keymaps."
  (setq helix-mode-map-alist nil))

(defun helix-get-state-keymaps (state)
  "Get a keymap alist of Helix keymaps for STATE."
  (let (result)
    (dolist (keymap (current-active-maps) (nreverse result))
      (when-let* ((hxmap (helix-get-helix-keymap keymap state))
                  (mode  (helix-get-minor-mode-for-keymap keymap)))
        (push (cons mode hxmap) result)))))

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

(defun helix-get-helix-keymap (keymap state)
  "Get from KEYMAP the STATE Helix keymap."
  (when state
    (let* ((key (vector (intern (format "%s-state" state))))
           (hxmap (keymap-lookup keymap key)))
      (if (helix-keymap-p hxmap) hxmap))))

(defun helix-create-helix-keymap (keymap state)
  "Create a nested keymap in KYEMAP for Helix STATE."
  (let ((hxmap (make-sparse-keymap)))
    (helix-set-keymap-prompt
     hxmap (format "Helix keymap for %s"
                   (or (helix-state-property state :name)
                       (format "%s state" state))))
    (define-key keymap (vector (intern (format "%s-state" state))) hxmap)
    hxmap))

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
