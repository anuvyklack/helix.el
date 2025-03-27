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
         (state-predicate (intern (format "%s-p" state-id)))
         (cursor     (intern (format "%s-cursor" state-id)))
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
       ;; hooks
       (dolist (func ',entry-hook-value) (add-hook ',entry-hook func))
       (dolist (func ',exit-hook-value) (add-hook ',exit-hook func))
       ;; state predicate
       (defun ,state-predicate ()
         ,(format "Whether the current Helix state is %s." name)
         (and helix-local-mode
              (eq helix-state ',state)))
       ;; state function
       (defun ,state-id (&optional arg)
         ,(format "Enable Helix %s. Disable with negative ARG.%s" name doc)
         (interactive)
         (cond ((and (numberp arg) (< arg 0))
                (setq helix-state nil)
                (helix-deactivate-keymaps))
               (t
                (unless helix-local-mode (helix-local-mode))
                (helix-setup-keymaps ',state)
                ))
         )
       )))

(helix-define-state normal
  "Normal state"
  (lol))

;; (progn ; normal state
;;   (defun helix-normal-state-p ()
;;     (and helix-local-mode (eq helix-state 'normal)))
;;
;;   (defun helix-normal-state ()
;;     (setq helix-state 'normal)
;;     (helix-switch-keymaps 'select)
;;     ))

;; (helix-define-state select
;;   "Select/extend state"
;;   (cond ((helix-select-state-p)
;;          ())
;;         (t
;;          ())))
;; (progn ; select state
;;   (defun helix-select-state-p ()
;;     (and helix-local-mode (eq helix-state 'select)))
;;
;;   (defun helix-select-state ()
;;     (setq helix-state 'select)
;;     (helix-switch-keymaps 'select)
;;     ))

(defun helix-setup-keymaps (&optional state)
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

;;; Keymaps
;;;; Auxiliary keymaps

(defun helix-get-helix-keymap (keymap state)
  "Get from KEYMAP the STATE Helix keymap."
  (when state
    (let* ((key (vector (intern (format "%s-state" state))))
           (hxmap (keymap-lookup keymap key)))
      (if (helix-keymap-p hxmap) hxmap))))

(defun helix-create-helix-keymap (keymap state)
  "Create in KYEMAP a nested keymap for Helix STATE."
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

(defun helix-set-keymap-prompt (map prompt)
  "Set the prompt-string of MAP to PROMPT."
  (delq (keymap-prompt map) map)
  (when prompt
    (setcdr map (cons prompt (cdr map)))))

(provide 'helix-core)
;;; helix-core.el ends here
