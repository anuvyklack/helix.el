;;; helix-core.el --- Core functionality -*- lexical-binding: t; -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/anuvyklack/helix.el
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
;; symbol is stored in `emulation-mode-map-alists' list, and keymap bound to it
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
(require 'helix-macros)
(require 'helix-vars)
(require 'helix-common)
(require 'helix-multiple-cursors-core)

(defvar edebug-mode nil)
(defvar edebug-mode-map)
(declare-function helix-delete-all-fake-cursors "helix-commands")

;;; Helix mode

(defun helix--pre-commad-hook ()
  "Hook run before each command is executed. See `pre-command-hook'."
  (when (and helix--extend-selection (not mark-active))
    (set-mark (point)))
  (unless helix-executing-command-for-fake-cursor
    (setq helix-this-command this-command)
    (helix--single-undo-step-beginning)))

(defun helix--post-command-hook ()
  "Hook run after each command is executed. See `post-command-hook'."
  (unless helix-executing-command-for-fake-cursor
    (when (and helix-multiple-cursors-mode
               (not (eq helix-this-command #'ignore))
               ;; TODO: This condition skips keyboard macros.
               ;; We need to handle these! They will generate actual commands
               ;; that are also run in the command loop.
               (functionp helix-this-command))
      ;; Wrap in `condition-case' to protect `helix--post-command-hook' from
      ;; being removed from `post-command-hook', because the function throwing
      ;; the error is unconditionally removed from `post-command-hook'.
      (condition-case error
          (helix--execute-command-for-all-fake-cursors helix-this-command)
        (error
         (message "[Helix] error while executing command for fake cursor: %s"
                  (error-message-string error))))
      (when (helix-merge-regions-p helix-this-command)
        (helix-merge-overlapping-regions)))
    (helix--single-undo-step-end)
    (setq helix-this-command nil
          helix--input-cache nil)))

(define-minor-mode helix-local-mode
  "Minor mode for setting up Helix in a current buffer."
  :global nil
  (if helix-local-mode
      (progn
        ;; Just push the symbol into `emulation-mode-map-alists'.
        ;; We will update its content on every Helix state change.
        (cl-pushnew 'helix-mode-map-alist emulation-mode-map-alists)
        (helix-load-whitelists)
        (add-hook 'pre-command-hook #'helix--pre-commad-hook nil t)
        (add-hook 'post-command-hook #'helix--post-command-hook 90 t)
        (add-hook 'deactivate-mark-hook #'helix-disable-newline-at-eol nil t)
        (add-hook 'after-revert-hook #'helix-delete-all-fake-cursors nil t)
        (helix-switch-state (helix-initial-state)))
    ;; else
    (remove-hook 'post-command-hook #'helix--post-command-hook t)
    (remove-hook 'pre-command-hook #'helix--pre-commad-hook t)
    (remove-hook 'deactivate-mark-hook #'helix-disable-newline-at-eol t)
    (remove-hook 'after-revert-hook #'helix-delete-all-fake-cursors t)
    (helix--single-undo-step-end)
    (setq helix-this-command nil
          helix--input-cache nil)
    (when helix-multiple-cursors-mode (helix-multiple-cursors-mode -1))
    (helix-disable-current-state)))

(put 'helix-local-mode 'permanent-local t)

;;;###autoload (autoload 'helix-mode "helix" nil t)
(define-globalized-minor-mode helix-mode helix-local-mode helix--initialize
  :group 'helix
  (if helix-mode
      (progn
        (dolist (fun-how-advice helix--advices)
          (apply #'advice-add fun-how-advice))
        (when helix-want-minibuffer
          (add-hook 'minibuffer-setup-hook #'helix-local-mode))
        (add-hook 'window-configuration-change-hook #'helix-update-cursor)
        (add-to-list 'mode-line-misc-info '(:eval (helix-multiple-cursors--indicator))))
    ;; else
    (cl-loop for (fun _how advice) in helix--advices
             do (advice-remove fun advice))
    (remove-hook 'minibuffer-setup-hook #'helix-local-mode)
    (remove-hook 'window-configuration-change-hook #'helix-update-cursor)))

(defun helix--initialize ()
  "Turn on `helix-local-mode' in current buffer if appropriate."
  (cond (helix-local-mode
         ;; Set Helix state according to new major-mode.
         (helix-switch-state (helix-initial-state)))
        ((not (minibufferp))
         (helix-local-mode 1))))

(helix-define-advice select-window (:after (&rest _))
  (helix-update-cursor))

(helix-advice-add 'use-global-map :after #'helix-update-active-keymaps-a)
(helix-advice-add 'use-local-map  :after #'helix-update-active-keymaps-a)

;;; Helix states

(defmacro helix-define-state (state doc &rest body)
  "Define new Helix STATE.
DOC is a general description and shows up in all docstrings.
BODY is executed each time the state is enabled or disabled.

Optional keyword arguments:

`:keymap'      Keymap that will be active while Helix is in STATE.
             Can be accessed via `helix-STATE-state-map' variable.

`:cursor'      Cursor apperance when Helix is in STATE.
             Can be a cursor type as per `cursor-type', a color string
             as passed to `set-cursor-color', a zero-argument function
             for changing the cursor, or a list of the above.
             Can be accessed via `helix-STATE-state-cursor' variable.

`:enter-hook'  List of functions to run on each entry to STATE.
             Can be accessed via `helix-STATE-state-enter-hook' variable.

`:exit-hook'   List of functions to run on each exit from STATE.
             Can be accessed via `helix-STATE-state-exit-hook' variable.

\(fn STATE DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (doc-string 2)
           (debug ( &define name
                    [&optional stringp]
                    [&rest [keywordp sexp]]
                    def-body)))
  (let* ((state-name (concat (capitalize (symbol-name state)) " state"))
         (symbol (intern (format "helix-%s-state" state)))
         (variable symbol)
         (statefun symbol)
         (cursor (intern (format "%s-cursor" symbol)))
         (enter-hook (intern (format "%s-enter-hook" symbol)))
         (exit-hook (intern (format "%s-exit-hook" symbol)))
         (keymap (intern (format "%s-map" symbol)))
         (modes  (intern (format "%s-modes" symbol)))
         key arg keymap-value cursor-value enter-hook-value exit-hook-value)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (pcase key
        (:keymap (setq keymap-value arg))
        (:cursor (setq cursor-value arg))
        (:enter-hook (setq enter-hook-value (ensure-list arg)))
        (:exit-hook (setq exit-hook-value (ensure-list arg)))))
    `(progn
       (defvar ,cursor ,cursor-value
         ,(format "Cursor for %s.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above." state-name))
       (defvar ,keymap ,(or keymap-value '(make-sparse-keymap))
         ,(format "Global keymap for Helix %s." state-name))
       (defvar ,modes nil
         ,(format "List of major and minor modes for which Helix initial state is %s." state-name))
       (defvar ,enter-hook nil ,(format "Hooks to run on entry %s." state-name))
       (defvar ,exit-hook  nil ,(format "Hooks to run on exit %s." state-name))
       (dolist (func ,enter-hook-value) (add-hook ',enter-hook func))
       (dolist (func ,exit-hook-value)  (add-hook ',exit-hook func))
       ;; Save state properties in `helix-state-properties' for runtime lookup.
       (setf (alist-get ',state helix-state-properties)
             '( :name ,state-name
                :variable ,variable
                :function ,statefun
                :keymap ,keymap
                :cursor ,cursor
                :enter-hook ,enter-hook
                :exit-hook ,exit-hook
                :modes ,modes))
       ;; State variable
       (helix-defvar-local ,variable nil
         ,(format "Non nil if Helix is in %s." state-name))
       ;; State function
       (defun ,statefun (&optional arg)
         ,(format "Switch Helix into %s.
When ARG is non-positive integer and Helix is in %s — disable it.\n\n%s"
                  state-name state-name doc)
         (interactive)
         (if (and (numberp arg) (< arg 1))
             (when (eq helix-state ',state)
               (setq helix-state nil
                     helix-previous-state ',state
                     ,variable nil)
               ,@body
               (run-hooks ',exit-hook))
           ;; else
           (unless helix-local-mode (helix-local-mode))
           (helix-disable-current-state)
           (setq helix-state ',state
                 ,variable t)
           ,@body
           ;; Switch color and shape of all cursors.
           (setq helix--extend-selection nil)
           (helix-update-cursor) ;; main cursor
           (when helix-multiple-cursors-mode
             (helix-save-window-scroll
               (helix-save-excursion
                 (dolist (cursor (helix-all-fake-cursors))
                   (helix-with-fake-cursor cursor
                     (setq helix--extend-selection nil))))))
           (run-hooks ',enter-hook))
         (helix-update-active-keymaps)
         (force-mode-line-update)))))

(defun helix-state-p (symbol)
  "Return non-nil if SYMBOL corresponds to Helix state."
  (assq symbol helix-state-properties))

(defun helix-switch-state (state)
  "Switch Helix into STATE."
  (when (and state
             (not (eq state helix-state)))
    (-> (helix-state-property state :function)
        (funcall 1))))

(defun helix-switch-to-initial-state ()
  (helix-switch-state (helix-initial-state)))

(defun helix-disable-current-state ()
  "Disable current Helix state."
  (-some-> helix-state
    (helix-state-property :function)
    (funcall -1)))

(defun helix-state-property (state property)
  "Return the value of PROPERTY for STATE.
PROPERTY is a keyword as used by `helix-define-state'.
STATE is the state's symbolic name."
  (let* ((state-properties (cdr (assq state helix-state-properties)))
         (val (plist-get state-properties property)))
    (if (memq property '(:keymap :cursor))
        (symbol-value val)
      val)))

(defun helix-initial-state (&optional buffer)
  "Return the state in which Helix should start in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (or (if (minibufferp) 'insert)
        ;; Check minor modes
        (cl-loop for (mode) in minor-mode-map-alist
                 when (and (boundp mode)
                           (symbol-value mode))
                 thereis (helix-initial-state-for-mode mode))
        ;; Check major mode
        (helix-initial-state-for-mode major-mode t)
        (if (helix-letters-are-self-insert-p) 'normal 'motion))))

(defun helix-initial-state-for-mode (mode &optional follow-parent checked-modes)
  "Return the Helix state to use for MODE or its alias.
The initial state for MODE should be set beforehand by the
`helix-set-initial-state' function.

If FOLLOW-PARENT is non-nil, also check parent modes of MODE and its alias.

CHECKED-MODES is used internally and should not be set initially."
  (when (memq mode checked-modes)
    (error "Circular reference detected in ancestors of `%s'\n%s"
           major-mode checked-modes))
  (let ((mode-alias (let ((func (symbol-function mode)))
                      (if (symbolp func)
                          func))))
    (or (cl-loop for (state . properties) in helix-state-properties
                 for modes = (-> (plist-get properties :modes)
                                 (symbol-value))
                 when (or (memq mode modes)
                          (and mode-alias
                               (memq mode-alias modes)))
                 return state)
        (if-let* ((follow-parent)
                  (parent (get mode 'derived-mode-parent)))
            (helix-initial-state-for-mode parent t (cons mode checked-modes)))
        (if-let* ((follow-parent)
                  (mode-alias)
                  (parent (get mode-alias 'derived-mode-parent)))
            (helix-initial-state-for-mode parent t
                                          (cons mode-alias checked-modes))))))

(defun helix-set-initial-state (mode state)
  "Set the Helix initial STATE for the major MODE.
MODE and STATE should be symbols."
  ;; Remove current settings.
  (cl-loop for (_state . plist) in helix-state-properties
           for modes = (plist-get plist :modes)
           do (set modes (delq mode (symbol-value modes))))
  ;; Add new settings.
  (add-to-list (helix-state-property state :modes)
               mode))

;;; Normal, Insert and Motion states

(helix-define-state normal
  "Normal state."
  :cursor helix-normal-state-cursor
  :keymap (define-keymap :full t :suppress t))

(helix-define-state insert
  "Insert state."
  :cursor helix-insert-state-cursor
  (if helix-insert-state
      (progn
        (when (and helix-reactivate-selection-after-insert-state
                   (region-active-p))
          (setq helix--region-was-active-on-insert t))
        (helix-with-each-cursor
          (deactivate-mark)))
    ;; else
    (when helix--region-was-active-on-insert
      (setq helix--region-was-active-on-insert nil)
      (helix-with-each-cursor
        (activate-mark)))))

(helix-define-state motion
  "Motion state."
  :cursor helix-motion-state-cursor)

(defun helix-inhibit-insert-state (keymap)
  "Unmap insertion keys from normal state.
This is useful for read-only modes that starts in normal state."
  (helix-keymap-set keymap
    "<remap> <helix-insert>" #'ignore
    "<remap> <helix-append>" #'ignore
    "<remap> <helix-insert-line>" #'ignore
    "<remap> <helix-append-line>" #'ignore
    "<remap> <helix-open-below>" #'ignore
    "<remap> <helix-open-above>" #'ignore
    "<remap> <helix-change>" #'ignore
    "<remap> <helix-cut>" #'ignore
    "<remap> <helix-delete>" #'ignore
    "<remap> <helix-undo>" #'ignore
    "<remap> <helix-redo>" #'ignore
    "<remap> <helix-paste-after>" #'ignore
    "<remap> <helix-paste-before>" #'ignore
    "<remap> <helix-replace-with-kill-ring>" #'ignore
    "<remap> <helix-paste-pop>" #'ignore
    "<remap> <helix-paste-undo-pop>" #'ignore
    "<remap> <helix-join-line>" #'ignore
    "<remap> <helix-downcase>" #'ignore
    "<remap> <helix-upcase>" #'ignore
    "<remap> <helix-invert-case>" #'ignore
    "<remap> <indent-region>" #'ignore
    "<remap> <indent-rigidly-left>" #'ignore
    "<remap> <indent-rigidly-right>" #'ignore))

;;; Keymaps

(defun helix-update-active-keymaps ()
  "Reset keymaps for current Helix state."
  (helix-activate-state-keymaps helix-state))

(defun helix-update-active-keymaps-a (&rest _)
  "Refresh Helix keymaps."
  (helix-activate-state-keymaps helix-state))

(defun helix-activate-state-keymaps (state)
  "Set the value of the `helix-mode-map-alist' in the current buffer
according to the Helix STATE."
  (setq helix-mode-map-alist
        (if state
            ;; Order matters: the first found binding will be accepted,
            ;; so earlier keymaps has higher priority.
            `(
              ;; Edebug if active
              ,@(if edebug-mode
                    (list `(edebug-mode . ,edebug-mode-map)))
              ;; ,@(if edebug-mode
              ;;       (let ((map (or (helix-get-nested-helix-keymap edebug-mode-map state)
              ;;                      edebug-mode-map)))
              ;;         `((edebug-mode . ,map))))
              ;; Helix buffer local overriding map
              ,@(-if-let (map (helix-get-nested-helix-keymap
                               helix-overriding-local-map state))
                    (list `(:helix-override-map . ,map)))
              ;; Helix keymaps nested in other keymaps
              ,@(let (helix-map maps)
                  (dolist (keymap (current-active-maps))
                    (setq helix-map (helix-get-nested-helix-keymap keymap state))
                    (when helix-map
                      (push (cons (helix-minor-mode-for-keymap keymap) helix-map)
                            maps)))
                  (nreverse maps))
              ;; Main state keymap
              ,(cons (helix-state-property state :variable)
                     (helix-state-property state :keymap))))))

(defun helix-minor-mode-for-keymap (keymap)
  "Return the minor mode associated with KEYMAP or t if it doesn't have one."
  (when (symbolp keymap)
    (cl-callf symbol-value keymap))
  (or (car (rassq keymap minor-mode-map-alist))
      t))

(defun helix-get-nested-helix-keymap (keymap state)
  "Get from KEYMAP the nested keymap associated with Helix STATE."
  (when (and keymap state)
    (let ((key (vector (intern (format "%s-state" state)))))
      (if-let* ((helix-map (lookup-key keymap key))
                ((helix-nested-keymap-p helix-map)))
          helix-map))))

(defun helix-create-nested-helix-keymap (keymap state)
  "Create a nested keymap for Helix STATE inside the given KEYMAP."
  (let ((helix-map (make-sparse-keymap))
        (key (vector (intern (format "%s-state" state))))
        (prompt (format "Helix keymap for %s"
                        (or (helix-state-property state :name)
                            (format "%s state" state)))))
    (helix-set-keymap-prompt helix-map prompt)
    (define-key keymap key helix-map)
    helix-map))

(defun helix-set-keymap-prompt (keymap prompt)
  "Set the prompt-string of the KEYMAP to PROMPT."
  (delq (keymap-prompt keymap) keymap)
  (when prompt
    (setcdr keymap (cons prompt (cdr keymap)))))

(defun helix-nested-keymap-p (keymap)
  "Return non-nil if KEYMAP is a Helix nested keymap."
  (-if-let (prompt (keymap-prompt keymap))
      (string-prefix-p "Helix keymap" prompt)))

(defun helix-keymap-set (keymap &rest args)
  "Create keybinding from KEY to DEFINITION in KEYMAP.

`:STATE' is an optional keyword argument that specifies the Helix state in
which the keybindings will be active. Can be a symbol or list of symbols.
It must appear before any KEY/DEFINITION pairs.

KEY and DEFINITION arguments are like those in `keymap-set'.
If DEFINITION is nil, the corresponding key binding will be removed from KEYMAP.
Any number of KEY/DEFINITION pairs can be provided.

Without `:STATE', this function works like `keymap-set' except that multiple
keybindings can be set at once.

Example:

   (helix-keymap-set keymap :state \\='(normal motion)
      \"f\" \\='foo
      \"b\" nil) ; unbind

\(fn KEYMAP [:STATE STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (let ((states (pcase (car-safe args)
                  (:state (pop args)
                          (ensure-list (pop args))))))
    (dolist (state states)
      (cl-assert (helix-state-p state) nil
                 "Helix state `%s' is not known to be defined" state))
    (cl-assert (cl-evenp (length args)) nil
               "The number of [KEY DEFINITION] pairs is not even")
    (let ((maps (if states
                    (cl-loop for state in states
                             collect
                             (or (helix-get-nested-helix-keymap keymap state)
                                 (helix-create-nested-helix-keymap keymap state)))
                  (list keymap))))
      (dolist (map maps)
        (cl-loop for (key definition) on args by #'cddr
                 do (if definition
                        (keymap-set map key definition)
                      (keymap-unset map key :remove)))))))

(defun helix-keymap-global-set (&rest args)
  "Create keybinding from KEY to DEFINITION in `global-map'.

`:STATE' is an optional keyword argument. If provided, keybindings are set in
the main keymap for specified Helix state. Can be a symbol or list of symbols.
It must appear before any KEY/DEFINITION pairs.

KEY, DEFINITION arguments are like those of `keymap-global-set'.
If DEFINITION is nil, then keybinding will be remove from keymap.
Any number of KEY DEFINITION pairs are accepted.

Without `:STATE', this function works like `keymap-global-set' except that
multiple keybindings can be set at once.

Example:

   (helix-keymap-global-set :state \\='(normal motion)
      \"f\" \\='foo
      \"b\" nil) ; unbind

\(fn [:STATE STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (let ((states (pcase (car-safe args)
                  (:state (pop args)
                          (ensure-list (pop args))))))
    (dolist (state states)
      (cl-assert (helix-state-p state) nil
                 "Helix state `%s' is not known to be defined" state))
    (unless (cl-evenp (length args))
      (user-error "The number of [KEY DEFINITION] pairs is not even"))
    (let ((maps (if states
                    (cl-loop for state in states
                             collect (helix-state-property state :keymap))
                  (list (current-global-map)))))
      (dolist (map maps)
        (cl-loop for (key definition) on args by #'cddr
                 do (if definition
                        (keymap-set map key definition)
                      (keymap-unset map key :remove)))))))

(defun helix-keymap-local-set (&rest args)
  "Create keybinding from KEY to DEFINITION in current local keymap.

`:STATE' is an optional keyword argument that specifies the Helix state
in which the keybindings will be active. It must appear before any
KEY/DEFINITION pairs.

KEY, DEFINITION arguments are like those of `keymap-global-set'.
If DEFINITION is nil, then keybinding will be remove from keymap.
Any number of KEY DEFINITION pairs are accepted.

\(fn [:STATE STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (let ((local-map (or (current-local-map)
                       (-doto (make-sparse-keymap)
                         (use-local-map)))))
    (apply #'helix-keymap-set local-map args)))

(defun helix-keymap-overriding-set (&rest args)
  "Create keybindings from KEY to DEFINITION for Helix STATE in
the current buffer-local overriding keymap. These keybindings
are buffer local and take precedence over all others.

`:STATE' is an optional keyword argument that specifies the Helix state
in which the keybindings will be active. It must appear before any
KEY/DEFINITION pairs.

\(fn [:STATE STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (unless helix-overriding-local-map
    (setq helix-overriding-local-map (make-sparse-keymap)))
  (apply #'helix-keymap-set helix-overriding-local-map args))

;;; Cursor shape and color

;; set-window-cursor-type
;; window-cursor-type

(defun helix-update-cursor ()
  "Update the cursor shape and color for current Helix state in current buffer."
  (when (eq (window-buffer) (current-buffer))
    (helix-set-cursor-type-and-color
     (helix-state-property helix-state :cursor))
    (when helix--extend-selection
      (set-cursor-color (face-attribute 'helix-extend-selection-cursor
                                        :background)))))

(defun helix-set-cursor-type-and-color (&optional specs)
  "Change the cursor's apperance according to SPECS.
SPECS may be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the cursor,
or a list of the above."
  (setq specs (cond ((null specs) '(t))
                    ((not (or (functionp specs)
                              (proper-list-p specs)))
                     (list specs))
                    (t specs)))
  (dolist (spec specs)
    (pcase spec
      ((and color (pred stringp))
       ;; Cursor color can only be set for each frame but not for each buffer.
       ;; Also `set-cursor-color' forces a redisplay, so only call it when the
       ;; color actually changes.
       (unless (equal color (frame-parameter nil 'cursor-color))
         (set-cursor-color color)))
      ((and fun (pred functionp))
       (ignore-errors (funcall fun)))
      (type
       (setq cursor-type type)))))

(provide 'helix-core)
;;; helix-core.el ends here
