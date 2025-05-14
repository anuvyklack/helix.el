;;; helix-multiple-cursors-core.el --- Multiple cursors for Helix -*- lexical-binding: t; -*-

;; Authors: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Keywords: editing cursors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The core functionality for multiple cursors. This module is heavily based
;; on Magnar Sveen `multiple-cursors.el' package.
;;
;; ID 0 is always coresponding to real cursor.

;;; Code:

(require 'cl-lib)
(require 'rect)
(require 'dash)
(require 'helix-common)

;;; Fake cursor

(defvar helix--cursor-last-used-id 0
  "Last used cursor ID.")

(defconst helix--cursors-table (make-hash-table :test 'eql :weakness t)
  "Table mapping fake cursors IDs to cursors overlays.")

(defun helix--new-cursor-id ()
  "Return new unique cursor id.
IDs' are used to keep track of cursors for undo."
  (cl-incf helix--cursor-last-used-id))

(defvar helix--max-cursors-original nil
  "This variable maintains the original maximum number of cursors.
When `helix-create-fake-cursor' is called and `helix-max-cursors' is
overridden, this value serves as a backup so that `helix-max-cursors'
can take on a new value. When `helix--delete-fake-cursors' is called,
the values are reset.")

(defun helix-create-fake-cursor (point &optional mark id)
  "Create a fake cursor at POINT position.
If MARK is passed a fake active region overlay between POINT
and MARK will be created.
The ID, if specified, will be assigned to the new cursor.
Otherwise, the new unique ID will be assigned.
The current state is stored in the overlay for later retrieval."
  (unless helix--max-cursors-original
    (setq helix--max-cursors-original helix-max-cursors))
  (when-let* ((helix-max-cursors)
              (num (helix-number-of-cursors))
              ((<= helix-max-cursors num)))
    (if (yes-or-no-p (format "%d active cursors. Continue? " num))
        (setq helix-max-cursors (read-number "Enter a new, temporary maximum: "))
      (helix--delete-fake-cursors)
      (error "Aborted: too many cursors")))
  (prog1 (helix--create-fake-cursor-1 point mark id)
    (helix-maybe-enable-multiple-cursors-mode)))

(defun helix--create-fake-cursor-1 (point &optional mark id)
  (or id (setq id (helix--new-cursor-id)))
  (save-excursion
    (goto-char point)
    (let ((cursor (helix--set-cursor-overlay nil (point))))
      (overlay-put cursor 'id (or id (helix--new-cursor-id)))
      (overlay-put cursor 'type 'fake-cursor)
      (overlay-put cursor 'priority 100)
      (helix--store-point-state cursor)
      (helix--set-region-overlay cursor point mark)
      (puthash id cursor helix--cursors-table)
      cursor)))

(defun helix-create-fake-cursor-from-point (&optional id)
  "Add a fake cursor and possibly a fake active region overlay
based on point and mark.

Assign the ID to the new cursor, if specified.
The current state is stored in the overlay for later retrieval."
  (helix-create-fake-cursor (point) (if (use-region-p) (mark)) id))

(defun helix-move-fake-cursor (cursor point &optional mark)
  "Set fake CURSOR to new POINT and MARK and update its state."
  (helix--set-cursor-overlay cursor point)
  (move-marker (overlay-get cursor 'point) point)
  (when mark
    (move-marker (overlay-get cursor 'mark) mark)
    (if (and mark mark-active)
        (helix--set-region-overlay cursor point mark)
      (helix--delete-region-overlay cursor)))
  (helix--store-point-state cursor point mark)
  cursor)

(defun helix-set-fake-cursor (cursor-or-id point &optional mark)
  "Set or create fake cursor at POINT position.

CURSOR-OR-ID can be either:
- fake cursor overlay;
- fake cursor ID;
- nil — new fake cursor with unique ID will be created.

If MARK is passed a fake active region overlay between POINT and MARK
will be set."
  (let* ((id (if (numberp cursor-or-id)
                 cursor-or-id))
         (cursor (if id (gethash id helix--cursors-table)
                   cursor-or-id)))
    (if cursor
        (progn
          (helix-move-fake-cursor cursor point mark)
          (helix-maybe-enable-multiple-cursors-mode))
      (helix-create-fake-cursor point mark id))))

(defun helix-remove-fake-cursor (cursor)
  "Delete fake CURSOR and disable `helix-multiple-cursors-mode'
if no more fake cursors are remaining."
  (helix--delete-fake-cursor cursor)
  (unless (helix-any-fake-cursors-p)
    (helix-disable-multiple-cursors-mode)))

(defun helix--set-cursor-overlay (cursor position)
  "Move CURSOR overlay to POSITION.
If CURSOR is nil — create new fake cursor overlay at POSITION.
Return CURSOR."
  (save-excursion
    (goto-char position)
    (let* ((pos position)
           ;; Special case for end of line, because overlay over
           ;; a newline highlights the entire width of the window.
           (cursor (cond ((and cursor (eolp))
                          (move-overlay cursor pos pos))
                         (cursor
                          (move-overlay cursor pos (1+ pos)))
                         ((eolp)
                          (make-overlay pos pos nil t nil))
                         (t
                          (make-overlay pos (1+ pos) nil t nil)))))
      (cond ((and helix-mc-match-cursor-style
                  (helix-cursor-is-bar-p))
             (overlay-put cursor 'before-string (propertize "|" 'face 'helix-mc-cursor-bar-face))
             (overlay-put cursor 'after-string nil)
             (overlay-put cursor 'face nil))
            ((eolp)
             (overlay-put cursor 'after-string (propertize " " 'face 'helix-mc-cursor-face))
             (overlay-put cursor 'before-string nil)
             (overlay-put cursor 'face nil))
            (t
             (overlay-put cursor 'face 'helix-mc-cursor-face)
             (overlay-put cursor 'before-string nil)
             (overlay-put cursor 'after-string nil)))
      cursor)))

(defalias 'helix--move-cursor-overlay #'helix--set-cursor-overlay)

(defun helix--set-region-overlay (cursor point mark)
  "Set the overlay looking like active region between POINT and MARK
and bind it to CURSOR."
  (if (and mark mark-active)
      (if-let* ((region (overlay-get cursor 'fake-region)))
          (move-overlay region point mark)
        (let ((region (make-overlay point mark nil nil t)))
          (overlay-put region 'face 'helix-mc-region-face)
          (overlay-put region 'type 'fake-region)
          (overlay-put region 'id (overlay-get cursor 'id))
          (overlay-put cursor 'fake-region region)))))

(defun helix--delete-region-overlay (cursor)
  "Remove the dependent region overlay for a given CURSOR overlay."
  (when-let* ((region (overlay-get cursor 'fake-region)))
    (delete-overlay region)))

(defun helix--store-point-state (overlay &optional point mark)
  "Store POINT, MARK and variables relevant to fake cursor into OVERLAY."
  (or point (setq point (point)))
  (or mark (setq mark (mark t)))
  (let ((pnt-marker (or (overlay-get overlay 'point)
                        (let ((m (make-marker)))
                          (set-marker-insertion-type m t)
                          (overlay-put overlay 'point m))))
        (mrk-marker (or (overlay-get overlay 'mark)
                        (overlay-put overlay 'mark (make-marker)))))
    (set-marker pnt-marker point)
    (when mark
      (set-marker mrk-marker mark)))
  (dolist (var helix-fake-cursor-specific-vars)
    (when (boundp var)
      (overlay-put overlay var (symbol-value var))))
  overlay)

(defun helix--restore-point-state (overlay)
  "Restore point, mark and cursor variables saved in OVERLAY."
  (goto-char (overlay-get overlay 'point))
  (when-let* ((mark (overlay-get overlay 'mark)))
    (set-marker (mark-marker) mark))
  (dolist (var helix-fake-cursor-specific-vars)
    (when (boundp var)
      (set var (overlay-get overlay var)))))

(defun helix-restore-point-from-fake-cursor (cursor)
  "Restore point, mark and saved variables from CURSOR overlay, and delete it."
  (helix--restore-point-state cursor)
  (helix--delete-fake-cursor cursor))

(defun helix--delete-fake-cursor (cursor)
  "Delete CURSOR overlay."
  (set-marker (overlay-get cursor 'point) nil)
  (set-marker (overlay-get cursor 'mark) nil)
  (helix--delete-region-overlay cursor)
  (delete-overlay cursor))

;;; Undo functionality

(defmacro helix--add-fake-cursor-to-undo-list (cursor &rest body)
  "Make sure point is in the right place when undoing."
  (declare (indent defun) (debug (&rest form)))
  (let ((id (make-symbol "id"))
        (deactivate-cursor (make-symbol "deactivate-cursor")))
    `(let* ((,id (overlay-get ,cursor 'id))
            (,deactivate-cursor `(apply helix-undo--deactivate-cursor ,,id)))
       (push ,deactivate-cursor buffer-undo-list)
       ,@body
       ;; If nothing has been added to the undo-list
       (if (eq (car buffer-undo-list) ,deactivate-cursor)
           (pop buffer-undo-list)
         (push `(apply helix-undo--activate-cursor ,,id)
               buffer-undo-list)))))

(defvar helix--point-state-during-undo nil
  "The variable to keep the state of the real cursor while undoing a fake one.")

;;;###autoload
(defun helix-undo--activate-cursor (id)
  "Called when undoing to temporarily activate the fake cursor
which action is being undone."
  (setq helix--point-state-during-undo
        (helix--store-point-state
         (make-overlay (point) (point) nil nil t)))
  (when-let* ((cursor (helix-cursor-with-id id)))
    (helix--restore-point-state cursor))
  (push `(apply helix-undo--deactivate-cursor ,id)
        buffer-undo-list))

(defun helix-undo--deactivate-cursor (id)
  "Called when undoing to reinstate the real cursor after undoing a fake one."
  ;; Update or create fake cursor
  (helix-set-fake-cursor id (point) (mark t))
  (push `(apply helix-undo--activate-cursor ,id)
        buffer-undo-list)
  ;; Restore real cursor
  (helix--restore-point-state helix--point-state-during-undo)
  (delete-overlay helix--point-state-during-undo)
  (setq helix--point-state-during-undo nil))

;;; Executing commands

(defmacro helix-save-window-scroll (&rest body)
  "Save the window scroll position, execute BODY, restore it."
  (let ((win-start (make-symbol "win-start"))
        (win-hscroll (make-symbol "win-hscroll")))
    `(let ((,win-start (set-marker (make-marker) (window-start)))
           (,win-hscroll (window-hscroll)))
       ,@body
       (set-window-start nil ,win-start t)
       (set-window-hscroll nil ,win-hscroll)
       (set-marker ,win-start nil))))

(defmacro helix-save-excursion (&rest body)
  "Like `save-excursion' but additionally save and restore all
the data needed for multiple cursors functionality."
  (let ((state (make-symbol "point-state")))
    `(let ((,state (make-overlay (point) (point) nil nil t)))
       (overlay-put ,state 'type 'original-cursor)
       (helix--store-point-state ,state)
       (save-excursion ,@body)
       (helix--restore-point-state ,state)
       (delete-overlay ,state))))

(defun helix-all-fake-cursors ()
  (helix-fake-cursors-in (point-min) (point-max)))

(defun helix-fake-cursors-in (start end)
  (cl-remove-if-not #'helix-fake-cursor-p
                    (overlays-in start end)))

(defmacro helix-for-each-fake-cursor (cursor &rest body)
  "Evaluate BODY with CURSOR bound to each fake cursor in turn."
  (declare (indent 1) (debug (&rest form)))
  `(dolist (,cursor (overlays-in (point-min) (point-max)))
     (when (helix-fake-cursor-p ,cursor)
       ,@body)))

(defmacro helix-for-each-cursor-ordered (cursor &rest body)
  "Evaluate BODY with CURSOR bound to each real or fake cursors
in order they are follow in buffer."
  (declare (indent 1) (debug (&rest form)))
  (let ((real-cursor (make-symbol "real-cursor"))
        (overlays (make-symbol "overlays")))
    `(let ((,real-cursor (helix--create-fake-cursor-1 (point) (mark t) 0))
           (,overlays (sort (helix-all-fake-cursors)
                            #'helix--compare-by-overlay-start)))
       (dolist (,cursor ,overlays)
         ,@body)
       (helix-restore-point-from-fake-cursor ,real-cursor))))

(defun helix-execute-command-for-all-cursors (command)
  "Call COMMAND interactively for the real cursor and all fake ones."
  (call-interactively command)
  (helix-execute-command-for-all-fake-cursors command))

(defun helix-execute-command-for-all-fake-cursors (command)
  "Call COMMAND interactively for each fake cursor.

Internaly it moves point to the fake cursor, restore the environment
from it, execute COMMAND, update fake cursor.

It uses two lists of commands to know what to do: the `run-once'list
and the `run-for-all' list. If a command is in neither of these lists,
it will prompt for the proper action and then save that preference.

Some commands are so unsupported that they are even prevented for
the original cursor, to inform about the lack of support."
  (cond ((get command 'helix-unsupported)
         (message "%S is not supported with multiple cursors" command))
        ((or
          ;; If it's a lambda, we can't know if it's supported or not -
          ;; so go ahead and assume it's ok.
          (not (symbolp command))
          (and (not (memq command helix-default-commands-to-run-once))
               (not (memq command helix-commands-to-run-once))
               (or helix-mc-always-run-for-all
                   (memq command helix-default-commands-to-run-for-all-cursors)
                   (memq command helix-commands-to-run-for-all-cursors)
                   (helix--prompt-for-unknown-command command))))
         (helix-save-window-scroll
          (helix-save-excursion
           (helix-for-each-fake-cursor cursor
             (helix-execute-command-for-fake-cursor cursor command))))
         (helix--reset-input-cache)
         (helix--remove-entries-that-move-point-from-current-step-in-undo-list))))

(defvar helix--executing-command-for-fake-cursor nil)

(defun helix-execute-command-for-fake-cursor (cursor command)
  (let ((helix--executing-command-for-fake-cursor t))
    (helix--add-fake-cursor-to-undo-list cursor
      (helix--restore-point-state cursor)
      (helix--execute-command command)
      (helix-move-fake-cursor cursor (point) (mark t)))))

(defun helix--execute-command (command)
  "Run COMMAND, simulating the parts of the command loop that
makes sense for fake cursors."
  (setq this-command command)
  (ignore-errors
    (run-hooks 'pre-command-hook)
    (unless (eq this-command 'ignore)
      (call-interactively command))
    (run-hooks 'post-command-hook)
    (when deactivate-mark (deactivate-mark))))

(defun helix--remove-entries-that-move-point-from-current-step-in-undo-list ()
  "Remove all \"move point to POSITION\" entries from `buffer-undo-list',
within the current undo step (until the first nil boundary), to prevent
desynchronization between real cursor and fake ones during `undo'."
  (let* ((undo-list buffer-undo-list)
         (equiv (gethash (car undo-list)
                         undo-equiv-table))
         (it undo-list))
    (while (cadr it)
      ;; Remove POSITION entry: we set the cdr of current node
      ;; to the cdr of next node, effectively removing next node.
      (when (numberp (cadr it))
        (setcdr it (cddr it)))
      (setq it (cdr it)))
    ;; Restore "undo" status of the tip of `buffer-undo-list'.
    (when equiv
      (puthash (car undo-list) equiv
               undo-equiv-table))
    undo-list))

(defun helix-cursor-with-id (id)
  "Return the cursor with the given ID if it is stil alive."
  (if-let* ((cursor (gethash id helix--cursors-table))
            ((helix-overlay-live-p cursor)))
      cursor))

(defun helix-number-of-cursors ()
  "The number of cursors (real and fake) in the buffer."
  (1+ (cl-count-if #'helix-fake-cursor-p
                   (overlays-in (point-min) (point-max)))))

(defun helix-any-fake-cursors-p ()
  "Return non-nil if currently there are any fake cursors in the buffer."
  (cl-find-if #'helix-fake-cursor-p
              (overlays-in (point-min) (point-max))))

(defun helix--delete-fake-cursors ()
  "Remove all fake cursors.
Do not use this funtion to conclude editing with multiple cursors!
Disable `helix-multiple-cursors-mode' instead."
  (when helix--max-cursors-original
    (setq helix-max-cursors helix--max-cursors-original
          helix--max-cursors-original nil))
  (helix-for-each-fake-cursor cursor
    (helix--delete-fake-cursor cursor)))

(defun helix-mc--maybe-set-killed-rectangle ()
  "Add the latest `kill-ring' entry for each cursor to `killed-rectangle'.
So you can paste it in later with `yank-rectangle'."
  (let (helix-max-cursors)
    (let ((entries (let (lst)
                     (helix-for-each-cursor-ordered cursor
                       (push (car (overlay-get cursor 'kill-ring))
                             lst))
                     (nreverse lst))))
      (unless (helix-all-elements-are-the-same-p entries)
        (setq killed-rectangle entries)))))

(defun helix-mc-temporarily-disable-minor-mode (mode)
  "If MODE is available and turned on, remember that and turn it off."
  (when (and (boundp mode) (symbol-value mode))
    (push mode helix-mc-temporarily-disabled-minor-modes)
    (funcall mode -1)))

(defun helix-mc-temporarily-disable-unsupported-minor-modes ()
  (mapc #'helix-mc-temporarily-disable-minor-mode
        helix-mc-unsupported-minor-modes))

(defun helix-mc-enable-temporarily-disabled-minor-modes ()
  (mapc #'(lambda (mode) (funcall mode 1))
        helix-mc-temporarily-disabled-minor-modes)
  (setq helix-mc-temporarily-disabled-minor-modes nil))

(defvar helix--undo-list-pointer nil
  "Store the beginning of the undo step.")

(defun helix--pre-commad-hook-function ()
  "Used with `pre-command-hook' to store the original command being run.
Since that cannot be reliably determined in the `post-command-hook'.

Specifically, `this-original-command' isn't always right, because it
could have been remapped. And certain modes (cua comes to mind) will
change their remapping based on state. So a command that changes the
state will afterwards not be recognized through the `command-remapping'
lookup."
  (unless helix--executing-command-for-fake-cursor
    (setq helix--this-command (or (command-remapping this-original-command)
                                  this-original-command))))

(defun helix--post-command-hook-function ()
  (unless helix--executing-command-for-fake-cursor
    ;; Wrap in `condition-case' to protect `post-command-hook'.
    (condition-case error
        (progn
          ;; (helix-undo--store-point-3)
          (when-let* ((this-original-command)
                      (command (or helix--this-command
                                   (command-remapping this-original-command)
                                   this-original-command))
                      ;; Skip keyboard macros, since they will generate actual
                      ;; commands that are also run in the command loop - we will
                      ;; handle those later instead.
                      ((functionp command)))
            (helix-execute-command-for-all-fake-cursors command)
            ;; (helix-undo--restore-point-3)
            ))
      (error (message "[Helix] error in `helix-execute-command-for-all-fake-cursors': %s"
                      (error-message-string error))))))

;;;###autoload
(define-minor-mode helix-multiple-cursors-mode
  "Minor mode, which is active when there are multiple cursors."
  :init-value nil
  :lighter helix-mc-mode-line
  :keymap helix-multiple-cursors-map
  (if helix-multiple-cursors-mode
      (progn
        (helix-mc-load-lists) ;; Lazy-load the user's list file
        (helix-mc-temporarily-disable-unsupported-minor-modes)
        (add-hook 'pre-command-hook 'helix--pre-commad-hook-function nil t)
        (add-hook 'post-command-hook 'helix--post-command-hook-function t t))
    (remove-hook 'post-command-hook 'helix--post-command-hook-function t)
    (remove-hook 'pre-command-hook 'helix--pre-commad-hook-function t)
    (setq helix--this-command nil)
    (helix-mc--maybe-set-killed-rectangle)
    (helix--delete-fake-cursors)
    (helix-mc-enable-temporarily-disabled-minor-modes)))

(defun helix-maybe-enable-multiple-cursors-mode ()
  "Enable `helix-multiple-cursors-mode' if not already enabled
and fake cursors are present in the buffer."
  (when (and (not helix-multiple-cursors-mode)
             (helix-any-fake-cursors-p))
    (helix-multiple-cursors-mode 1)))

(defun helix-disable-multiple-cursors-mode ()
  "Disable `helix-multiple-cursors-mode'."
  (interactive)
  (when helix-multiple-cursors-mode
    (helix-multiple-cursors-mode -1)))

;;; Integration with other packages

(add-hook 'after-revert-hook 'helix-disable-multiple-cursors-mode)

(define-advice execute-kbd-macro (:around (orig-fun &rest args) multiple-cursors)
  "`execute-kbd-macro' should never be run for fake cursors.
The real cursor will execute the keyboard macro, resulting in new commands
in the command loop, and the fake cursors can pick up on those instead."
  (unless helix--executing-command-for-fake-cursor
    (apply orig-fun args)))

;; Intercept some reading commands so you won't have to
;; answer them for every single cursor
(defvar helix--input-cache nil)

(defmacro helix--advice-to-cache-input (fn-name)
  "Advise `read-char' like command to cache users input to use
for all fake cursors.

The FN-NAME command must have PROMPT first argument, that will be
used as a hash key, to distinguish different calls of FN-NAME within
one command. So calls with equal PROMPT or without it wouldn't be
distinguished."
  `(define-advice ,fn-name (:around (orig-fun &rest args) multiple-cursors)
     "Cache the users input to use it with multiple cursors."
     (if (not (bound-and-true-p helix-multiple-cursors-mode))
         (apply orig-fun args)
       (let* (;; Use PROMPT argument as a hash key to distinguish different
              ;; calls of `read-char' like functions within one command.
              (prompt (car args))
              (key (list ,(symbol-name fn-name) prompt))
              (value (alist-get key helix--input-cache nil nil #'equal)))
         (unless value
           (setq value (apply orig-fun args))
           (push (cons key value) helix--input-cache))
         value))))

(defun helix--reset-input-cache ()
  (setq helix--input-cache nil))

(helix--advice-to-cache-input read-char)
(helix--advice-to-cache-input read-quoted-char)
(helix--advice-to-cache-input register-read-with-preview) ; used by insert-register
(helix--advice-to-cache-input read-char-from-minibuffer)  ; used by zap-to-char

(defmacro helix-unsupported-command (command)
  "Adds command to list of unsupported commands and prevents it
from being executed if in `helix-multiple-cursors-mode'."
  `(progn
     (put ',command 'helix-unsupported t)
     (define-advice ,command (:around (orig-fun &rest args)
                                      multiples-cursors-unsupported)
       "Don't execute an unsupported command while multiple cursors are active."
       (unless (and helix-multiple-cursors-mode
                    (called-interactively-p 'any))
         (apply orig-fun args)))))

;; Commands that don't work with multiple-cursors
(helix-unsupported-command isearch-forward)
(helix-unsupported-command isearch-backward)

(define-advice current-kill (:before (n &optional do-not-move) multiple-cursors)
  "Make sure pastes from other programs are added to `kill-ring's
of all cursors when yanking."
  (when-let* ((interprogram-paste (and (eql n 0)
                                       interprogram-paste-function
                                       (funcall interprogram-paste-function))))
    ;; Add interprogram-paste to normal kill ring, just like current-kill
    ;; usually does for itself. We have to do the work for it though, since
    ;; the funcall only returns something once. It is not a pure function.
    (let ((interprogram-cut-function nil))
      (if (listp interprogram-paste)
          (mapc 'kill-new (reverse interprogram-paste))
        (kill-new interprogram-paste))
      ;; And then add interprogram-paste to the kill-rings
      ;; of all the other cursors too.
      (helix-for-each-fake-cursor cursor
        (let ((kill-ring (overlay-get cursor 'kill-ring))
              (kill-ring-yank-pointer (overlay-get cursor 'kill-ring-yank-pointer)))
          (if (listp interprogram-paste)
              (mapc 'kill-new (nreverse interprogram-paste))
            (kill-new interprogram-paste))
          (overlay-put cursor 'kill-ring kill-ring)
          (overlay-put cursor 'kill-ring-yank-pointer kill-ring-yank-pointer))))))

(define-advice execute-extended-command (:after (&rest _) multiple-cursors)
  "Execute selected command for all cursors."
  (when helix-multiple-cursors-mode
    (helix-execute-command-for-all-fake-cursors this-command)))

;;; Whitelists

(defun helix--prompt-for-unknown-command (command)
  "Ask the user whether the COMMAND should be executed for one cursor
or all of them, and remember the choice.

Return t if COMMMAND should be executed for all cursors."
  (let ((for-all? (y-or-n-p (format "Do %S for all cursors?" command))))
    (if for-all?
        (push command helix-commands-to-run-for-all-cursors)
      (push command helix-commands-to-run-once))
    (helix-mc-save-lists)
    for-all?))

(defun helix-mc-load-lists ()
  "Load `helix-mc-list-file' file if not yet."
  (unless helix-mc--list-file-loaded
    (load helix-mc-list-file 'noerror 'nomessage)
    (setq helix-mc--list-file-loaded t)))

(defun helix-mc-dump-list (list-symbol)
  "Insert (setq \\='LIST-SYMBOL LIST-VALUE) into current buffer."
  (cl-symbol-macrolet ((value (symbol-value list-symbol)))
    (insert "(setq " (symbol-name list-symbol) "\n"
            "      '(")
    (newline-and-indent)
    (set list-symbol
         (sort value #'(lambda (x y)
                         (string-lessp (symbol-name x)
                                       (symbol-name y)))))
    (mapc #'(lambda (cmd)
              (insert (format "%S" cmd))
              (newline-and-indent))
          value)
    (insert "))")
    (newline)))

(defun helix-mc-save-lists ()
  "Save users preferences which commands to execute for one cursor
and which for all to `helix-mc-list-file' file."
  (with-temp-file helix-mc-list-file
    (emacs-lisp-mode)
    (insert ";; This file is automatically generated by the Helix.")
    (newline)
    (insert ";; It keeps track of your preferences for running commands with multiple cursors.")
    (newline)
    (newline)
    (helix-mc-dump-list 'helix-commands-to-run-for-all-cursors)
    (newline)
    (helix-mc-dump-list 'helix-commands-to-run-once)))

;;; Utils

(defun helix-fake-cursor-p (overlay)
  "Return t if an OVERLAY is a fake cursor."
  (eq (overlay-get overlay 'type) 'fake-cursor))

(defun helix-fake-region-p (overlay)
  (eq (overlay-get overlay 'type)
      'fake-region))

(defun helix-fake-regions-in (beg end)
  (cl-remove-if-not #'helix-fake-region-p
                    (overlays-in beg end)))

(defun helix--overlays-overlap-p (o1 o2)
  (< (overlay-start o2)
     (overlay-end o1)))

(defun helix--compare-by-overlay-start (o1 o2)
  (< (overlay-start o1)
     (overlay-start o2)))

(defun helix-overlay-live-p (overlay)
  (if-let* ((buffer (overlay-buffer overlay)))
      (buffer-live-p buffer)))

(defun helix-fake-cursor-at-pos (position)
  "Return the fake cursor at POSITION, or nil if no one."
  (-find #'(lambda (cursor)
             (= position (overlay-get cursor 'point)))
         (helix-fake-cursors-in position (1+ position))))

(provide 'helix-multiple-cursors-core)
;;; helix-multiple-cursors-core.el ends here
