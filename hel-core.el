;;; hel-core.el --- Core functionality -*- lexical-binding: t; -*-
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
;; Hel states are similar to Emacs minor modes, but they are not minor modes
;; in the sense that they are not created with `define-minor-mode' macro.
;;
;; The internal mechanism in general terms is as follows: `hel-mode-map-alist'
;; symbol is stored in `emulation-mode-map-alists' list, and keymap bound to it
;; is changed on every Hel state change.
;;
;; Every state has general globally shared keymap, and "nested" keymaps that are
;; stored in other keymaps (typical expample are major-mode maps) under special
;; keys like "<normal-state>" or "<insert-state>", that are associated with
;; particular Hel states and can not be produced by a keyboard. On every Hel
;; state change, the algorithm traverse all currently active keymaps looking for
;; these keys, and activates nested keymaps associated with them.
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'hel-macros)
(require 'hel-vars)
(require 'hel-common)
(require 'hel-multiple-cursors-core)

(defvar edebug-mode nil)
(defvar edebug-mode-map)
(declare-function hel-delete-all-fake-cursors "hel-commands")

;;; Hel mode

(defun hel--pre-commad-hook ()
  "Hook run before each command is executed. See `pre-command-hook'."
  (when (and hel--extend-selection (not mark-active))
    (set-mark (point)))
  (unless hel-executing-command-for-fake-cursor
    (setq hel-this-command this-command)
    (hel--single-undo-step-beginning)))

(defun hel--post-command-hook ()
  "Hook run after each command is executed. See `post-command-hook'."
  (unless hel-executing-command-for-fake-cursor
    (when (and hel-multiple-cursors-mode
               (not (eq hel-this-command #'ignore))
               ;; TODO: This condition skips keyboard macros.
               ;; We need to handle these! They will generate actual commands
               ;; that are also run in the command loop.
               (functionp hel-this-command))
      ;; Wrap in `condition-case' to protect `hel--post-command-hook' from
      ;; being removed from `post-command-hook', because the function throwing
      ;; the error is unconditionally removed from `post-command-hook'.
      (condition-case err
          (progn
            (hel--execute-command-for-all-fake-cursors hel-this-command)
            (when (hel-merge-regions-p hel-this-command)
              (hel-merge-overlapping-regions)))
        (error
         (message "[Hel] error while executing command for fake cursor: %s"
                  (error-message-string err)))
        (quit))) ;; "C-g" during multistage command.
    (hel--single-undo-step-end)
    (setq hel-this-command nil
          hel--input-cache nil)))

(put 'hel--pre-commad-hook 'permanent-local-hook t)
(put 'hel--post-command-hook 'permanent-local-hook t)

(define-minor-mode hel-local-mode
  "Minor mode for setting up Hel in a current buffer."
  :global nil
  (if hel-local-mode
      (progn
        ;; Just push the symbol into `emulation-mode-map-alists'.
        ;; We will update its content on every Hel state change.
        (cl-pushnew 'hel-mode-map-alist emulation-mode-map-alists)
        (hel-load-whitelists)
        (add-hook 'pre-command-hook #'hel--pre-commad-hook nil t)
        (add-hook 'post-command-hook #'hel--post-command-hook 90 t)
        (add-hook 'deactivate-mark-hook #'hel-disable-newline-at-eol nil t)
        (add-hook 'after-revert-hook #'hel-delete-all-fake-cursors nil t)
        (setq hel-input-method current-input-method)
        (add-hook 'input-method-activate-hook #'hel-activate-input-method 90 t)
        (add-hook 'input-method-deactivate-hook #'hel-deactivate-input-method 90 t)
        (hel-switch-state (hel-initial-state)))
    ;; else
    (remove-hook 'post-command-hook #'hel--post-command-hook t)
    (remove-hook 'pre-command-hook #'hel--pre-commad-hook t)
    (remove-hook 'deactivate-mark-hook #'hel-disable-newline-at-eol t)
    (remove-hook 'after-revert-hook #'hel-delete-all-fake-cursors t)
    (remove-hook 'input-method-activate-hook #'hel-activate-input-method t)
    (remove-hook 'input-method-deactivate-hook #'hel-deactivate-input-method t)
    (hel--single-undo-step-end)
    (setq hel-this-command nil
          hel--input-cache nil)
    (when hel-multiple-cursors-mode (hel-multiple-cursors-mode -1))
    (hel-disable-current-state)
    (activate-input-method hel-input-method)))

(put 'hel-local-mode 'permanent-local t)

;;;###autoload (autoload 'hel-mode "hel" nil t)
(define-globalized-minor-mode hel-mode hel-local-mode hel--initialize
  :group 'hel
  (if hel-mode
      (progn
        (dolist (fun-how-advice hel--advices)
          (apply #'advice-add fun-how-advice))
        (when hel-want-minibuffer
          (add-hook 'minibuffer-setup-hook #'hel-local-mode))
        (cl-pushnew #'hel--fundamental-mode-hack window-buffer-change-functions)
        (add-hook 'window-configuration-change-hook #'hel-update-cursor)
        (add-to-list 'mode-line-misc-info '(:eval (hel-multiple-cursors--indicator))))
    ;; else
    (cl-loop for (fun _how advice) in hel--advices
             do (advice-remove fun advice))
    (remove-hook 'minibuffer-setup-hook #'hel-local-mode)
    (cl-callf2 cl-remove #'hel--fundamental-mode-hack window-buffer-change-functions)
    (remove-hook 'window-configuration-change-hook #'hel-update-cursor)))

(defun hel--initialize ()
  "Turn on `hel-local-mode' in current buffer if appropriate."
  (cond (hel-local-mode
         ;; Set Hel state according to new major-mode.
         (hel-switch-state (hel-initial-state)))
        ((not (minibufferp))
         (hel-local-mode 1))))

(defun hel--fundamental-mode-hack (_)
  "Activate `hel-local-mode' in current buffer if it is in `fundamental-mode'.
Emacs sometimes creates random empty buffers in `fundamental-mode'. For
these buffers, `after-change-major-mode-hook' is not called, so they
remain invisible to `define-globalized-minor-mode'. This function ensures
`hel-local-mode' is activated in such cases."
  (if (and (eq major-mode 'fundamental-mode)
           (null hel-local-mode))
      (hel-local-mode 1)))

(hel-define-advice select-window (:after (&rest _))
  (hel-update-cursor))

(hel-advice-add 'use-global-map :after #'hel-update-active-keymaps-a)
(hel-advice-add 'use-local-map  :after #'hel-update-active-keymaps-a)

;;; Hel states

(defmacro hel-define-state (state doc &rest body)
  "Define new Hel STATE.
DOC is a general description and shows up in all docstrings.
BODY is executed each time the state is enabled or disabled.

Optional KEY keyword arguments:

`:keymap'        Keymap that will be active while Hel is in STATE.
               Can be accessed via `hel-STATE-state-map' variable.

`:cursor'        Cursor apperance when Hel is in STATE.
               Can be a cursor type as per `cursor-type', a color string
               as passed to `set-cursor-color', a zero-argument function
               for changing the cursor, or a list of the above.
               Can be accessed via `hel-STATE-state-cursor' variable.

`:input-method'  Activate enabled input method when Hel is in STATE.

`:enter-hook'    List of functions to run on each entry to STATE.
               Can be accessed via `hel-STATE-state-enter-hook' variable.

`:exit-hook'     List of functions to run on each exit from STATE.
               Can be accessed via `hel-STATE-state-exit-hook' variable.

\(fn STATE DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (doc-string 2)
           (debug ( &define name
                    [&optional stringp]
                    [&rest [keywordp sexp]]
                    def-body)))
  (let* ((state-name (concat (capitalize (symbol-name state)) " state"))
         (symbol (intern (format "hel-%s-state" state)))
         (variable symbol)
         (statefun symbol)
         (cursor (intern (format "%s-cursor" symbol)))
         (enter-hook (intern (format "%s-enter-hook" symbol)))
         (exit-hook (intern (format "%s-exit-hook" symbol)))
         (keymap (intern (format "%s-map" symbol)))
         (modes  (intern (format "%s-modes" symbol)))
         key arg keymap-value cursor-value input-method
         enter-hook-value exit-hook-value)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (pcase key
        (:keymap (setq keymap-value arg))
        (:cursor (setq cursor-value arg))
        (:input-method (setq input-method arg))
        (:enter-hook (setq enter-hook-value (ensure-list arg)))
        (:exit-hook (setq exit-hook-value (ensure-list arg)))))
    ;; macro expansion
    `(progn
       (defvar ,cursor ,cursor-value
         ,(format "Cursor for %s.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above." state-name))
       (defvar ,keymap ,(or keymap-value '(make-sparse-keymap))
         ,(format "Global keymap for Hel %s." state-name))
       (defvar ,modes nil
         ,(format "List of major and minor modes for which Hel initial state is %s."
                  state-name))
       (defvar ,enter-hook nil ,(format "Hooks to run on entry %s." state-name))
       (defvar ,exit-hook  nil ,(format "Hooks to run on exit %s." state-name))
       (dolist (func ,enter-hook-value) (add-hook ',enter-hook func))
       (dolist (func ,exit-hook-value)  (add-hook ',exit-hook func))
       ;; Save state properties in `hel-state-properties' for runtime lookup.
       (setf (alist-get ',state hel-state-properties)
             '( :name ,state-name
                :variable ,variable
                :function ,statefun
                :keymap ,keymap
                :cursor ,cursor
                :input-method ,input-method
                :enter-hook ,enter-hook
                :exit-hook ,exit-hook
                :modes ,modes))
       ;; State variable
       (hel-defvar-local ,variable nil
         ,(format "Non nil if Hel is in %s." state-name))
       ;; State function
       (defun ,statefun (&optional arg)
         ,(format "Switch Hel into %s.
When ARG is non-positive integer and Hel is in %s — disable it.\n\n%s"
                  state-name state-name doc)
         (interactive)
         (if (and (numberp arg) (< arg 1))
             ;; disable STATE
             (when (eq hel-state ',state)
               (setq hel-state nil
                     hel-previous-state ',state
                     ,variable nil)
               ,@body
               (run-hooks ',exit-hook))
           ;; enable STATE
           (unless hel-local-mode (hel-local-mode))
           (hel-disable-current-state)
           (setq hel-state ',state
                 ,variable t)
           (let (input-method-activate-hook
                 input-method-deactivate-hook)
             ,(if input-method
                  '(activate-input-method hel-input-method)
                '(deactivate-input-method)))
           ,@body
           ;; Switch color and shape of all cursors.
           (setq hel--extend-selection nil)
           (hel-update-cursor) ;; main cursor
           (when hel-multiple-cursors-mode
             (hel-save-window-scroll
               (hel-save-excursion
                 (dolist (cursor (hel-all-fake-cursors))
                   (hel-with-fake-cursor cursor
                     (setq hel--extend-selection nil))))))
           (run-hooks ',enter-hook))
         (hel-update-active-keymaps)
         (force-mode-line-update)))))

(defun hel-state-p (symbol)
  "Return non-nil if SYMBOL corresponds to Hel state."
  (assq symbol hel-state-properties))

(defun hel-switch-state (state)
  "Switch Hel into STATE."
  (when (and state
             (not (eq state hel-state)))
    (-> (hel-state-property state :function)
        (funcall 1))))

(defun hel-switch-to-initial-state ()
  (hel-switch-state (hel-initial-state)))

(defun hel-disable-current-state ()
  "Disable current Hel state."
  (-some-> hel-state
    (hel-state-property :function)
    (funcall -1)))

(defun hel-state-property (state property)
  "Return the value of PROPERTY for STATE.
PROPERTY is a keyword as used by `hel-define-state'.
STATE is the state's symbolic name."
  (let* ((state-properties (cdr (assq state hel-state-properties)))
         (val (plist-get state-properties property)))
    (if (memq property '(:keymap :cursor))
        (symbol-value val)
      val)))

(defun hel-initial-state (&optional buffer)
  "Return the state in which Hel should start in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (or (if (minibufferp) 'insert)
        ;; Check minor modes
        (cl-loop for (mode) in minor-mode-map-alist
                 when (and (boundp mode)
                           (symbol-value mode))
                 thereis (hel-initial-state-for-mode mode))
        ;; Check major mode
        (hel-initial-state-for-mode major-mode t)
        (if (hel-letters-are-self-insert-p) 'normal 'motion))))

(defun hel-initial-state-for-mode (mode &optional follow-parent checked-modes)
  "Return the Hel state to use for MODE or its alias.
The initial state for MODE should be set beforehand by the
`hel-set-initial-state' function.

If FOLLOW-PARENT is non-nil, also check parent modes of MODE and its alias.

CHECKED-MODES is used internally and should not be set initially."
  (when (memq mode checked-modes)
    (error "Circular reference detected in ancestors of `%s'\n%s"
           major-mode checked-modes))
  (let ((mode-alias (let ((func (symbol-function mode)))
                      (if (symbolp func)
                          func))))
    (or (cl-loop for (state . properties) in hel-state-properties
                 for modes = (-> (plist-get properties :modes)
                                 (symbol-value))
                 when (or (memq mode modes)
                          (and mode-alias
                               (memq mode-alias modes)))
                 return state)
        (if-let* ((follow-parent)
                  (parent (get mode 'derived-mode-parent)))
            (hel-initial-state-for-mode parent t (cons mode checked-modes)))
        (if-let* ((follow-parent)
                  (mode-alias)
                  (parent (get mode-alias 'derived-mode-parent)))
            (hel-initial-state-for-mode parent t
                                        (cons mode-alias checked-modes))))))

(defun hel-set-initial-state (mode state)
  "Set the Hel initial STATE for the major MODE.
MODE and STATE should be symbols."
  ;; Remove current settings.
  (cl-loop for (_state . plist) in hel-state-properties
           for modes = (plist-get plist :modes)
           do (set modes (delq mode (symbol-value modes))))
  ;; Add new settings.
  (add-to-list (hel-state-property state :modes)
               mode))

;;; Normal, Insert and Motion states

(hel-define-state normal
  "Normal state."
  :cursor hel-normal-state-cursor
  :keymap (define-keymap :full t :suppress t))

(hel-define-state insert
  "Insert state."
  :cursor hel-insert-state-cursor
  :input-method t
  (if hel-insert-state
      (progn
        (when (and hel-reactivate-selection-after-insert-state
                   (region-active-p))
          (setq hel--region-was-active-on-insert t))
        (hel-with-each-cursor
          (deactivate-mark)))
    ;; else
    (when hel--region-was-active-on-insert
      (setq hel--region-was-active-on-insert nil)
      (hel-with-each-cursor
        (activate-mark)))))

(hel-define-state motion
  "Motion state."
  :cursor hel-motion-state-cursor)

(defun hel-inhibit-insert-state (keymap)
  "Unmap insertion keys from normal state.
This is useful for read-only modes that starts in normal state."
  (hel-keymap-set keymap
    "<remap> <hel-insert>"                 #'ignore
    "<remap> <hel-append>"                 #'ignore
    "<remap> <hel-insert-line>"            #'ignore
    "<remap> <hel-append-line>"            #'ignore
    "<remap> <hel-open-below>"             #'ignore
    "<remap> <hel-open-above>"             #'ignore
    "<remap> <hel-change>"                 #'ignore
    "<remap> <hel-cut>"                    #'ignore
    "<remap> <hel-delete>"                 #'ignore
    "<remap> <hel-undo>"                   #'ignore
    "<remap> <hel-redo>"                   #'ignore
    "<remap> <hel-paste-after>"            #'ignore
    "<remap> <hel-paste-before>"           #'ignore
    "<remap> <hel-replace-with-kill-ring>" #'ignore
    "<remap> <hel-paste-pop>"              #'ignore
    "<remap> <hel-paste-undo-pop>"         #'ignore
    "<remap> <hel-join-line>"              #'ignore
    "<remap> <hel-downcase>"               #'ignore
    "<remap> <hel-upcase>"                 #'ignore
    "<remap> <hel-invert-case>"            #'ignore
    "<remap> <indent-region>"              #'ignore
    "<remap> <indent-rigidly-left>"        #'ignore
    "<remap> <indent-rigidly-right>"       #'ignore))

;;; Input-method

(defun hel-activate-input-method ()
  "Enable input method in states with :input-method non-nil."
  (let (input-method-activate-hook
        input-method-deactivate-hook)
    (when (and hel-local-mode hel-state)
      (setq hel-input-method current-input-method)
      (unless (hel-state-property hel-state :input-method)
        (deactivate-input-method)))))

(defun hel-deactivate-input-method ()
  "Disable input method in all states."
  (let (input-method-activate-hook
        input-method-deactivate-hook)
    (when (and hel-local-mode hel-state)
      (setq hel-input-method nil))))

(put 'hel-activate-input-method 'permanent-local-hook t)
(put 'hel-deactivate-input-method 'permanent-local-hook t)

(defmacro hel-with-input-method (&rest body)
  "Execute body with current input method active."
  (declare (indent defun))
  `(if hel-input-method
       (unwind-protect
           (progn
             (remove-hook 'input-method-activate-hook #'hel-activate-input-method t)
             (remove-hook 'input-method-deactivate-hook #'hel-deactivate-input-method t)
             (prog2
                 (activate-input-method hel-input-method)
                 (progn ,@body)
               (deactivate-input-method)))
         (add-hook 'input-method-activate-hook #'hel-activate-input-method 90 t)
         (add-hook 'input-method-deactivate-hook #'hel-deactivate-input-method 90 t))
     ;; else
     ,@body))

(hel-advice-add 'toggle-input-method :around #'hel--refresh-input-method-a)

(defun hel--refresh-input-method-a (orig-fun &rest args)
  "Refresh `hel-input-method'."
  (cond ((not hel-local-mode)
         (apply orig-fun args))
        ((hel-state-property hel-state :input-method)
         (apply orig-fun args))
        (t
         (let ((current-input-method hel-input-method))
           (apply orig-fun args)))))

;;; Keymaps

(defun hel-update-active-keymaps ()
  "Reset keymaps for current Hel state."
  (hel-activate-state-keymaps hel-state))

(defun hel-update-active-keymaps-a (&rest _)
  "Refresh Hel keymaps."
  (hel-activate-state-keymaps hel-state))

(defun hel-activate-state-keymaps (state)
  "Set the value of the `hel-mode-map-alist' in the current buffer
according to the Hel STATE."
  (setq hel-mode-map-alist
        (if state
            ;; Order matters: the first found binding will be accepted,
            ;; so earlier keymaps has higher priority.
            `(
              ;; Edebug if active
              ,@(if edebug-mode
                    (list `(edebug-mode . ,edebug-mode-map)))
              ;; ,@(if edebug-mode
              ;;       (let ((map (or (hel-get-nested-hel-keymap edebug-mode-map state)
              ;;                      edebug-mode-map)))
              ;;         `((edebug-mode . ,map))))
              ;; Hel buffer local overriding map
              ,@(if-let ((map (hel-get-nested-hel-keymap
                               hel-overriding-local-map state)))
                    (list `(:hel-override-map . ,map)))
              ;; Hel keymaps nested in other keymaps
              ,@(let (hel-map maps)
                  (dolist (keymap (current-active-maps))
                    (setq hel-map (hel-get-nested-hel-keymap keymap state))
                    (when hel-map
                      (push (cons (hel-minor-mode-for-keymap keymap) hel-map)
                            maps)))
                  (nreverse maps))
              ;; Main state keymap
              ,(cons (hel-state-property state :variable)
                     (hel-state-property state :keymap))))))

(defun hel-minor-mode-for-keymap (keymap)
  "Return the minor mode associated with KEYMAP or t if it doesn't have one."
  (when (symbolp keymap)
    (cl-callf symbol-value keymap))
  (or (car (rassq keymap minor-mode-map-alist))
      t))

(defun hel-get-nested-hel-keymap (keymap state)
  "Get from KEYMAP the nested keymap associated with Hel STATE."
  (when (and keymap state)
    (let ((key (vector (intern (format "%s-state" state)))))
      (if-let* ((hel-map (lookup-key keymap key))
                ((hel-nested-keymap-p hel-map)))
          hel-map))))

(defun hel-create-nested-hel-keymap (keymap state)
  "Create a nested keymap for Hel STATE inside the given KEYMAP."
  (let ((hel-map (make-sparse-keymap))
        (key (vector (intern (format "%s-state" state))))
        (prompt (format "Hel keymap for %s"
                        (or (hel-state-property state :name)
                            (format "%s state" state)))))
    (hel-set-keymap-prompt hel-map prompt)
    (define-key keymap key hel-map)
    hel-map))

(defun hel-set-keymap-prompt (keymap prompt)
  "Set the prompt-string of the KEYMAP to PROMPT."
  (delq (keymap-prompt keymap) keymap)
  (when prompt
    (setcdr keymap (cons prompt (cdr keymap)))))

(defun hel-nested-keymap-p (keymap)
  "Return non-nil if KEYMAP is a Hel nested keymap."
  (-if-let (prompt (keymap-prompt keymap))
      (string-prefix-p "Hel keymap" prompt)))

(defun hel-keymap-set (keymap &rest args)
  "Create keybinding from KEY to DEFINITION in KEYMAP.

STATE is an optional keyword argument that specifies the Hel state in
which the keybindings will be active. Can be a symbol or list of symbols.
It must appear before any KEY / DEFINITION pairs.

KEY and DEFINITION arguments are like those in `keymap-set'.
If DEFINITION is nil, the corresponding key binding will be removed from KEYMAP.
Any number of KEY / DEFINITION pairs can be provided.

Without STATE, this function works like `keymap-set' except that multiple
keybindings can be set at once.

Example:

   (hel-keymap-set keymap :state \\='(normal motion)
      \"f\" \\='foo
      \"b\" nil) ; unbind

\(fn KEYMAP [:state STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (let ((states (pcase (car-safe args)
                  (:state (pop args)
                          (ensure-list (pop args))))))
    (dolist (state states)
      (cl-assert (hel-state-p state) nil
                 "Hel state `%s' is not known to be defined" state))
    (cl-assert (cl-evenp (length args)) nil
               "The number of [KEY DEFINITION] pairs is not even")
    (let ((maps (if states
                    (cl-loop for state in states
                             collect
                             (or (hel-get-nested-hel-keymap keymap state)
                                 (hel-create-nested-hel-keymap keymap state)))
                  (list keymap))))
      (dolist (map maps)
        (cl-loop for (key definition) on args by #'cddr
                 do (if definition
                        (keymap-set map key definition)
                      (keymap-unset map key :remove)))))
    keymap))

(defun hel-keymap-global-set (&rest args)
  "Create keybinding from KEY to DEFINITION in `global-map'.

STATE is an optional keyword argument. If provided, keybindings are set in
the main keymap for specified Hel state. Can be a symbol or list of symbols.
It must appear before any KEY / DEFINITION pairs.

KEY, DEFINITION arguments are like those of `keymap-global-set'.
If DEFINITION is nil, then keybinding will be remove from keymap.
Any number of KEY DEFINITION pairs are accepted.

Without STATE, this function works like `keymap-global-set' except that
multiple keybindings can be set at once.

Example:

   (hel-keymap-global-set :state \\='(normal motion)
      \"f\" \\='foo
      \"b\" nil) ; unbind

\(fn [:state STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (let ((states (pcase (car-safe args)
                  (:state (pop args)
                          (ensure-list (pop args))))))
    (dolist (state states)
      (cl-assert (hel-state-p state) nil
                 "Hel state `%s' is not known to be defined" state))
    (unless (cl-evenp (length args))
      (user-error "The number of [KEY DEFINITION] pairs is not even"))
    (let ((maps (if states
                    (cl-loop for state in states
                             collect (hel-state-property state :keymap))
                  (list (current-global-map)))))
      (dolist (map maps)
        (cl-loop for (key definition) on args by #'cddr
                 do (if definition
                        (keymap-set map key definition)
                      (keymap-unset map key :remove)))))))

(defun hel-keymap-local-set (&rest args)
  "Create keybinding from KEY to DEFINITION in current buffer local keymap.
See `current-local-map' for details on what a local keymap is.

STATE is an optional keyword argument that specifies the Hel state
in which the keybindings will be active. It must appear before any
KEY / DEFINITION pairs.

KEY, DEFINITION arguments are like those of `keymap-set'.
If DEFINITION is nil, then keybinding will be remove from keymap.
Any number of KEY DEFINITION pairs are accepted.

\(fn [:state STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (let ((local-map (or (current-local-map)
                       (-doto (make-sparse-keymap)
                         (use-local-map)))))
    (apply #'hel-keymap-set local-map args)))

(defun hel-keymap-overriding-set (&rest args)
  "Create buffer-local keybindings from KEY to DEFINITION for Hel STATE which
take precedence over all others.

STATE is an optional keyword argument that specifies the Hel state
in which the keybindings will be active. It must appear before any
KEY / DEFINITION pairs.

\(fn [:state STATE] &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (unless hel-overriding-local-map
    (setq hel-overriding-local-map (make-sparse-keymap)))
  (apply #'hel-keymap-set hel-overriding-local-map args))

;;; Cursor shape and color

;; set-window-cursor-type
;; window-cursor-type

(defun hel-update-cursor ()
  "Update the cursor shape and color for current Hel state in current buffer."
  (when (eq (window-buffer) (current-buffer))
    (hel-set-cursor-type-and-color
     (hel-state-property hel-state :cursor))
    (when hel--extend-selection
      (set-cursor-color (face-attribute 'hel-extend-selection-cursor
                                        :background)))))

(defun hel-set-cursor-type-and-color (&optional specs)
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

(provide 'hel-core)
;;; hel-core.el ends here
