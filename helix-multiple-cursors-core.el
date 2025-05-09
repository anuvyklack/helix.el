;;; helix-multiple-cursors-core.el --- Multiple cursors for Helix -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2025 Magnar Sveen
;; Copyright (C)      2025 Yuriy Artemyev

;; Authors: Magnar Sveen <magnars@gmail.com>
;;          Yuriy Artemyev <anuvyklack@gmail.com>
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

;; The core functionality for multiple cursors.
;; ID 0 is always coresponding to real cursor.

;;; Code:

(require 'cl-lib)
(require 'rect)
(require 'dash)
(require 'helix-common)

;;; Fake cursor

(defun helix--make-cursor-overlay-at-point ()
  "Create overlay to look like cursor.
Special case for end of line, because overlay over a newline
highlights the entire width of the window."
  (if (eolp)
      (helix--make-cursor-overlay-at-eol)
    (helix--make-cursor-overlay-inline)))

(defun helix--make-cursor-overlay-at-eol ()
  "Create overlay to look like cursor at end of line."
  (let ((overlay (make-overlay (point) (point) nil nil nil)))
    (if (and helix-mc-match-cursor-style (helix-cursor-is-bar-p))
        (overlay-put overlay 'before-string (propertize "|" 'face 'helix-mc-cursor-bar-face))
      (overlay-put overlay 'after-string (propertize " " 'face 'helix-mc-cursor-face)))
    overlay))

(defun helix--make-cursor-overlay-inline ()
  "Create overlay to look like cursor at point."
  (let ((overlay (make-overlay (point) (1+ (point)) nil nil nil)))
    (if (and helix-mc-match-cursor-style (helix-cursor-is-bar-p))
        (overlay-put overlay 'before-string (propertize "|" 'face 'helix-mc-cursor-bar-face))
      (overlay-put overlay 'face 'helix-mc-cursor-face))
    overlay))

(defun helix--move-cursor-overlay (overlay pos)
  (save-excursion
    (goto-char pos)
    (if (eolp)
        (helix--move-cursor-overlay-at-eol pos)
      (helix--move-cursor-overlay-inline pos))))

(defun helix--move-cursor-overlay-at-eol (overlay pos)
  (move-overlay overlay pos pos)
  (cond ((and helix-mc-match-cursor-style (helix-cursor-is-bar-p))
         (overlay-put overlay 'before-string (propertize "|" 'face 'helix-mc-cursor-bar-face))
         (overlay-put overlay 'after-string nil)
         (overlay-put overlay 'face nil))
        (t
         (overlay-put overlay 'after-string (propertize " " 'face 'helix-mc-cursor-face))
         (overlay-put overlay 'before-string nil)
         (overlay-put overlay 'face nil))))

(defun helix--move-cursor-overlay-inline (overlay pos)
  (move-overlay overlay pos (1+ pos))
  (cond ((and helix-mc-match-cursor-style (helix-cursor-is-bar-p))
         (overlay-put overlay 'before-string (propertize "|" 'face 'helix-mc-cursor-bar-face))
         (overlay-put overlay 'after-string nil)
         (overlay-put overlay 'face nil))
        (t
         (overlay-put overlay 'face 'helix-mc-cursor-face)
         (overlay-put overlay 'before-string nil)
         (overlay-put overlay 'after-string nil))))

(defun helix--make-region-overlay (point mark cursor-overlay)
  "Create overlay to look like active region."
  (let ((overlay (make-overlay mark point nil nil t)))
    (overlay-put overlay 'face 'helix-mc-region-face)
    (overlay-put overlay 'type 'fake-region)
    (overlay-put overlay 'fake-cursor cursor-overlay)
    overlay))

(defvar helix-mc-cursor-specific-vars '(transient-mark-mode
                                        kill-ring
                                        kill-ring-yank-pointer
                                        mark-ring
                                        mark-active
                                        yank-undo-function
                                        temporary-goal-column
                                        dabbrev--abbrev-char-regexp
                                        dabbrev--check-other-buffers
                                        dabbrev--friend-buffer-list
                                        dabbrev--last-abbrev-location
                                        dabbrev--last-abbreviation
                                        dabbrev--last-buffer
                                        dabbrev--last-buffer-found
                                        dabbrev--last-direction
                                        dabbrev--last-expansion
                                        dabbrev--last-expansion-location
                                        dabbrev--last-table)
  "A list of vars that need to be tracked on a per-cursor basis.")

(defun helix--store-point-state-in-overlay (overlay point mark)
  "Store POINT, MARK and variables relevant to multiple cursors
functionality into OVERLAY."
  (overlay-put overlay 'point (set-marker (make-marker) point))
  (overlay-put overlay 'mark  (set-marker (make-marker) mark))
  (dolist (var helix-mc-cursor-specific-vars)
    (when (boundp var)
      (overlay-put overlay var (symbol-value var))))
  overlay)

(defun helix--restore-point-state-from-overlay (overlay)
  "Restore point, mark and stored variables from OVERLAY."
  (let ((pnt (overlay-get overlay 'point))
        (mrk  (overlay-get overlay 'mark)))
    (goto-char pnt)
    (set-marker (mark-marker) mrk)
    ;; reset markers
    (set-marker pnt nil)
    (set-marker mrk nil))
  (dolist (var helix-mc-cursor-specific-vars)
    (when (boundp var)
      (set var (overlay-get overlay var)))))

(defun helix-restore-point-from-fake-cursor (cursor)
  "Restore point, mark and saved variables from CURSOR overlay, and delete it."
  (helix--restore-point-state-from-overlay cursor)
  (helix--remove-fake-cursor cursor))

(defun helix--remove-fake-cursor (cursor)
  "Delete CURSOR overlay."
  (set-marker (overlay-get cursor 'point) nil)
  (set-marker (overlay-get cursor 'mark) nil)
  (helix--delete-fake-region cursor)
  (delete-overlay cursor))

(defun helix--delete-fake-region (cursor)
  "Remove the dependent region overlay for a given CURSOR overlay."
  (when-let* ((region (overlay-get cursor 'fake-region)))
    (overlay-put region 'fake-cursor nil) ;; Break cyclic link
    (delete-overlay region)))

(defvar helix--max-cursors-original nil
  "This variable maintains the original maximum number of cursors.
When `helix-create-fake-cursor' is called and `helix-max-cursors' is
overridden, this value serves as a backup so that `helix-max-cursors'
can take on a new value. When `helix--remove-fake-cursors' is called,
the values are reset.")

(defvar helix--cursor-last-used-id 1
  "Last used cursor ID.")

(defun helix--new-cursor-id ()
  "Return new unique cursor id.
IDs' are used to keep track of cursors for undo."
  (cl-incf helix--cursor-last-used-id))

(defun helix-create-fake-cursor (point &optional mark id)
  "Create a fake cursor at POINT position.
If MARK is specified — a fake active region overlay between POINT and MARK
will be added. The ID will be assigned to the new cursor, if specified.
The current state saves in the overlay to be restored later."
  (unless helix--max-cursors-original
    (setq helix--max-cursors-original helix-max-cursors))
  (when helix-max-cursors
    (unless (< (helix-number-of-cursors) helix-max-cursors)
      (if (yes-or-no-p (format "%d active cursors. Continue? "
                               (helix-number-of-cursors)))
          (setq helix-max-cursors (read-number "Enter a new, temporary maximum: "))
        (helix--remove-fake-cursors)
        (error "Aborted: too many cursors"))))
  (prog1 (helix--create-fake-cursor-1 point mark id)
    (helix-maybe-enable-multiple-cursors-mode)))

(defun helix--create-fake-cursor-1 (point &optional mark id)
  (save-excursion
    (goto-char point)
    (let ((cursor (helix--make-cursor-overlay-at-point)))
      (overlay-put cursor 'id (or id (helix--new-cursor-id)))
      (overlay-put cursor 'type 'fake-cursor)
      (overlay-put cursor 'priority 100)
      (helix--store-point-state-in-overlay cursor point mark)
      (when mark
        (overlay-put cursor 'fake-region
                     (helix--make-region-overlay point mark cursor)))
      cursor)))

(defun helix-move-fake-cursor (cursor point &optional mark)
  (move-marker (overlay-get cursor 'point) point)
  (move-marker (overlay-get cursor 'mark) mark)
  )

(defun helix-create-fake-cursor-from-point (&optional id)
  "Add a fake cursor and possibly a fake active region overlay
based on point and mark.

Assign the ID to the new cursor, if specified.
The current state saves in the overlay to be restored later."
  (helix-create-fake-cursor (point)
                            (if (use-region-p) (mark))
                            id)

  )

(defun helix-remove-fake-cursor (cursor)
  "Delete fake CURSOR and disable `helix-multiple-cursors-mode'
if no more fake cursors are remaining."
  (helix--remove-fake-cursor cursor)
  (when (eql 1 (helix-number-of-cursors))
    (helix-disable-multiple-cursors-mode)))

;;; Undo functionality

(defmacro helix--add-fake-cursor-to-undo-list (id &rest body)
  "Make sure point is in the right place when undoing."
  (declare (indent defun))
  (let ((uc (make-symbol "undo-cleaner")))
    `(let ((,uc `(apply helix-deactivate-cursor-after-undo ,,id)))
       (push ,uc buffer-undo-list)
       ,@body
       ;; If nothing has been added to the undo-list
       (if (eq ,uc (car buffer-undo-list))
           ;; then pop the cleaner right off again
           (pop buffer-undo-list)
         ;; otherwise add a function to activate this cursor
         (push `(apply helix-activate-cursor-for-undo ,,id)
               buffer-undo-list)))))

(defvar helix-mc--stored-state-for-undo nil
  "The variable to keep the state of the real cursor while undoing a fake one.")

;;;###autoload
(defun helix-activate-cursor-for-undo (id)
  "Called when undoing to temporarily activate the fake cursor
which action is being undone."
  (when-let* ((cursor (helix-cursor-with-id id)))
    (setq helix-mc--stored-state-for-undo
          (helix--store-point-state-in-overlay
           (make-overlay (point) (point) nil nil t)
           (point)
           (mark t)))))

(defun helix-deactivate-cursor-after-undo (id)
  "Called when undoing to reinstate the real cursor after undoing a fake one."
  (when helix-mc--stored-state-for-undo
    ;; (deactivate-mark)
    ;; Update fake cursor
    (helix-create-fake-cursor (point) nil id)
    ;; Restore real cursor
    (helix--restore-point-state-from-overlay helix-mc--stored-state-for-undo)
    (delete-overlay helix-mc--stored-state-for-undo)
    (setq helix-mc--stored-state-for-undo nil)))

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
       (helix--store-point-state-in-overlay ,state (point) (mark t))
       (save-excursion ,@body)
       (helix--restore-point-state-from-overlay ,state)
       (delete-overlay ,state))))

(defun helix-all-fake-cursors (&optional start end)
  (cl-remove-if-not #'helix-fake-cursor-p
                    (overlays-in (or start (point-min))
                                 (or end   (point-max)))))

(defmacro helix-for-each-fake-cursor (cursor &rest body)
  "Evaluate BODY with CURSOR bound to each fake cursor in turn."
  (declare (indent 1))
  `(dolist (,cursor (overlays-in (point-min) (point-max)))
     (when (helix-fake-cursor-p ,cursor)
       ,@body)))

(defmacro helix-for-each-cursor-ordered (cursor &rest body)
  "Evaluate BODY with CURSOR bound to each real or fake cursors
in order they are follow in buffer."
  (declare (indent 1))
  (let ((real-cursor (make-symbol "real-cursor"))
        (overlays (make-symbol "overlays")))
    `(let ((,real-cursor (helix-create-fake-cursor (point)))
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
from it, execute COMMAND, update fake cursor."
  (helix-save-window-scroll
   (helix-save-excursion
    (helix-for-each-fake-cursor cursor
      (helix-execute-command-for-fake-cursor cursor command))))
  (helix--reset-input-cache))

(defvar helix--executing-command-for-fake-cursor? nil)

(defun helix-execute-command-for-fake-cursor (cursor command)
  (let ((helix--executing-command-for-fake-cursor? t)
        (id (overlay-get cursor 'id)))
    (helix--add-fake-cursor-to-undo-list id
      (helix-restore-point-from-fake-cursor cursor)
      (helix--execute-command command)
      (helix-create-fake-cursor-from-point id))))

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

(defun helix-cursor-with-id (id)
  "Return the cursor with the given ID."
  (cl-find-if #'(lambda (o)
                  (and (helix-fake-cursor-p o)
                       (= id (overlay-get o 'id))))
              (overlays-in (point-min) (point-max))))

(defun helix-number-of-cursors ()
  "The number of cursors (real and fake) in the buffer."
  (1+ (cl-count-if #'helix-fake-cursor-p
                   (overlays-in (point-min) (point-max)))))

(defun helix--execute-command-for-all-cursors ()
  "Wrap around `helix--execute-command-for-all-cursors-1' to protect hook."
  (condition-case error
      (helix--execute-command-for-all-cursors-1)
    (error
     (message "[mc] problem in `helix--execute-command-for-all-cursors': %s"
              (error-message-string error)))))

(defun helix--execute-command-for-all-cursors-1 ()
  "Used with `post-command-hook' to execute supported commands for all cursors.

It uses two lists of commands to know what to do: the `run-once'list
and the `run-for-all' list. If a command is in neither of these lists,
it will prompt for the proper action and then save that preference.

Some commands are so unsupported that they are even prevented for
the original cursor, to inform about the lack of support."
  (unless helix--executing-command-for-fake-cursor?
    (if (eq 1 (helix-number-of-cursors))
        (helix-disable-multiple-cursors-mode) ;; No fake cursors? Disable mc-mode.
      (when-let* ((this-original-command)
                  (command (or helix--this-command
                               (command-remapping this-original-command)
                               this-original-command))
                  ;; Skip keyboard macros, since they will generate actual
                  ;; commands that are also run in the command loop - we will
                  ;; handle those later instead.
                  ((functionp command)))
        (cond (;; If it's a lambda, we can't know if it's supported or not -
               ;; so go ahead and assume it's ok.
               (not (symbolp command))
               (helix-execute-command-for-all-fake-cursors command))
              ((get command 'helix-mc--unsupported)
               (message "%S is not supported with multiple cursors%s"
                        command
                        (get command 'helix-mc--unsupported)))
              ((and command
                    (not (memq command helix-default-commands-to-run-once))
                    (not (memq command helix-commands-to-run-once))
                    (or helix-mc-always-run-for-all
                        (memq command helix-default-commands-to-run-for-all-cursors)
                        (memq command helix-commands-to-run-for-all-cursors)
                        (helix-mc-prompt-for-inclusion-in-whitelist command)))
               (helix-execute-command-for-all-fake-cursors command)))))))

(defvar helix--cursors-history nil)

(defun helix--remove-fake-cursors ()
  "Remove all fake cursors.
Do not use this funtion to conclude editing with multiple cursors!
Disable `helix-multiple-cursors-mode' instead."
  (when helix--max-cursors-original
    (setq helix-max-cursors helix--max-cursors-original
          helix--max-cursors-original nil))
  (let (cursors-info)
    (helix-for-each-fake-cursor cursor
      (push (list (overlay-get cursor 'id)
                  (marker-position (overlay-get cursor 'point)))
            cursors-info)
      (helix--remove-fake-cursor cursor))
    (push (point) cursors-info)
    (setq helix--cursors-history cursors-info)))

(defun helix-mc-keyboard-quit ()
  "Exit `helix-multiple-cursors-mode'."
  (interactive)
  (helix-disable-multiple-cursors-mode))

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

(defun helix--record-this-command ()
  "Used with `pre-command-hook' to store the original command being run.
Since that cannot be reliably determined in the `post-command-hook'.

Specifically, `this-original-command' isn't always right, because it
could have been remapped. And certain modes (cua comes to mind) will
change their remapping based on state. So a command that changes the
state will afterwards not be recognized through the `command-remapping'
lookup."
  (unless helix--executing-command-for-fake-cursor?
    (setq helix--this-command (or (command-remapping this-original-command)
                                  this-original-command))))

(defvar helix---inside-helix-multiple-cursors-mode nil
  "This variable is t only while we are inside `helix-multiple-cursors-mode'
function to prevent it recursive calls.")

;;;###autoload
(define-minor-mode helix-multiple-cursors-mode
  "Minor mode, which is active when there are multiple cursors."
  :init-value nil
  :lighter helix-mc-mode-line
  :keymap helix-multiple-cursors-map
  (let ((helix---inside-helix-multiple-cursors-mode t)
        (add-undo? (not (or (eq buffer-undo-list t)
                            (helix-undo-command-p this-command)))))
    (if helix-multiple-cursors-mode
        (progn
          (helix-mc-load-lists) ;; Lazy-load the user's list file
          (helix-mc-temporarily-disable-unsupported-minor-modes)
          (add-hook 'pre-command-hook 'helix--record-this-command nil t)
          (add-hook 'post-command-hook 'helix--execute-command-for-all-cursors t)
          ;; (when add-undo?
          ;;   (push '(apply helix-disable-multiple-cursors-mode)
          ;;         buffer-undo-list))
          ;; (when add-undo?
          ;;   (push '(apply helix-multiple-cursors-mode toggle)
          ;;         buffer-undo-list))
          )
      (remove-hook 'post-command-hook 'helix--execute-command-for-all-cursors t)
      (remove-hook 'pre-command-hook 'helix--record-this-command t)
      (setq helix--this-command nil)
      (helix-mc--maybe-set-killed-rectangle)
      (helix--remove-fake-cursors)
      ;; (let ((cursors-info (helix--remove-fake-cursors)))
      ;;   (when add-undo?
      ;;     (push `(apply undo-helix-multiple-cursors-mode ,cursors-info)
      ;;           buffer-undo-list)))
      (helix-mc-enable-temporarily-disabled-minor-modes)
      ;; (when add-undo?
      ;;   (push '(apply helix-disable-multiple-cursors-mode)
      ;;         buffer-undo-list))
      )
    ;; Add to undo list
    ;; (when add-undo?
    ;;   (pcase-let ((`(,0th ,1st) buffer-undo-list)
    ;;               (cmd '(apply helix-multiple-cursors-mode toggle)))
    ;;     (if (and (null 0th) ;; undo border
    ;;              (equal 1st cmd))
    ;;         ;; pop first 2 elements
    ;;         (setq buffer-undo-list (cddr buffer-undo-list))
    ;;       (push cmd buffer-undo-list))))
    ))

(defun undo-helix-multiple-cursors-mode (cursors-data)
  (if helix-multiple-cursors-mode
      (helix-multiple-cursors-mode -1)
    ;; else
    (let ((pnt  (car cursors-data))
          (data (cdr cursors-data)))
      (dolist (cursor data)
        (pcase-let ((`(,id ,pnt) cursor))
          (helix-create-fake-cursor pnt nil id)))
      (goto-char pnt))))

(defun helix-maybe-enable-multiple-cursors-mode ()
  "Enable `helix-multiple-cursors-mode' if not yet."
  (unless (or helix-multiple-cursors-mode
              helix---inside-helix-multiple-cursors-mode
              (eql (helix-number-of-cursors) 1))
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
  (unless helix--executing-command-for-fake-cursor?
    (apply orig-fun args)))

;; Intercept some reading commands so you won't have to
;; answer them for every single cursor
(defvar helix--input-cache nil)

(defmacro helix--cache-input-advice (fn-name)
  "Advise `read-char' like command to cache users input to use
for all fake cursors.

The FN-NAME command must have PROMPT first argument, that will be
used as a hash key, to distinguish different calls of FN-NAME within
one command. So calls with equal PROMPT or without it wouldn't be
distinguished."
  `(define-advice ,fn-name (:around (orig-fun &rest args) multiple-cursors)
     "Cache the users input use it with multiple cursors."
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

(helix--cache-input-advice read-char)
(helix--cache-input-advice read-quoted-char)
(helix--cache-input-advice register-read-with-preview) ; used by insert-register
(helix--cache-input-advice read-char-from-minibuffer)  ; used by zap-to-char

(defun helix-mc-unsupported-a (orig-fun &rest args)
  "Don't execute an unsupported command while multiple cursors are active."
  (unless (and helix-multiple-cursors-mode
               (called-interactively-p 'any))
    (apply orig-fun args)))

(defmacro helix-mc-unsupported-cmd (cmd msg)
  "Adds command to list of unsupported commands and prevents it
from being executed if in `helix-multiple-cursors-mode'."
  `(progn
     (put ,cmd 'helix-mc--unsupported ,msg)
     (advice-add ,cmd :around #'helix-mc-unsupported-a)))

;; Commands that don't work with multiple-cursors
(helix-mc-unsupported-cmd 'isearch-forward "Feel free to add a compatible version.")
(helix-mc-unsupported-cmd 'isearch-backward "Feel free to add a compatible version.")

(define-advice current-kill (:before (n &optional do-not-move) multiple-cursors)
  "Make sure pastes from other programs are added to `kill-ring's
of all cursors when yanking."
  (when-let* ((interprogram-paste (and (= n 0)
                                       interprogram-paste-function
                                       (funcall interprogram-paste-function))))
    ;; Add interprogram-paste to normal kill ring, just
    ;; like current-kill usually does for itself.
    ;; We have to do the work for it though, since the funcall only returns
    ;; something once. It is not a pure function.
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
    (unless (or helix-mc-always-run-for-all
                (not (symbolp this-command))
                (memq this-command helix-commands-to-run-for-all-cursors)
                (memq this-command helix-commands-to-run-once)
                (memq this-command helix-default-commands-to-run-for-all-cursors)
                (memq this-command helix-default-commands-to-run-once))
      (helix-mc-prompt-for-inclusion-in-whitelist this-command))
    (when (or helix-mc-always-run-for-all
              (memq this-command helix-commands-to-run-for-all-cursors)
              (memq this-command helix-default-commands-to-run-for-all-cursors))
      (helix-execute-command-for-all-fake-cursors this-command))))

;;; Whitelists

(defun helix-mc-prompt-for-inclusion-in-whitelist (original-command)
  "Ask the user, then add the command either to
`helix-commands-to-run-for-all-cursors' list or
`helix-commands-to-run-once'."
  (let ((all? (y-or-n-p (format "Do %S for all cursors?" original-command))))
    (if all?
        (push original-command helix-commands-to-run-for-all-cursors)
      (push original-command helix-commands-to-run-once))
    (helix-mc-save-lists)
    all?))

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

(provide 'helix-multiple-cursors-core)

;;; Merge overlapping regions

;; (defun helix-overlapping-regions (start end)
;;   "Return list with overlaping regions real and fake between
;; START and END positions."
;;   (let (helix-max-cursors
;;         real-cursor)
;;     (when (<= start (point) end)
;;       (setq real-cursor (helix-create-fake-cursor-from-point 0)))
;;     (->> (helix-fake-regions-in start end)
;;          (helix-group-overlapping-overlays)
;;          (cl-remove-if #'(lambda (x) (length= x 1))))))

(defun helix-merge-overlapping-fake-regions (start end)
  (dolist (regions (helix-overlapping-fake-regions start end))
    (let* ((beg (-reduce-from #'(lambda (cur-min region)
                                  (min cur-min (overlay-start region)))
                              (point-max) regions))
           (end (-reduce-from #'(lambda (cur-max region)
                                  (max cur-max (overlay-end region)))
                              (point-min) regions))
           (dir (let* ((region (car regions))
                       (cursor (overlay-get region 'fake-cursor))
                       (pnt (marker-position (overlay-get cursor 'point)))
                       (mrk (marker-position (overlay-get cursor 'mark))))
                  (if (< pnt mrk) -1 1)))
           (region (if (< dir 0)
                       (car regions)
                     (car (last regions))))
           (cursor (overlay-get region 'fake-cursor))
           (rest-regions (if (< dir 0)
                             (-drop 1 regions)
                           (-drop-last 1 regions)))
           (rest-cursors (mapcar #'(lambda (r)
                                     (overlay-get r 'fake-cursor))
                                 rest-regions)))
      (-each rest-cursors #'helix--remove-fake-cursor)
      ;; Swap BEG and END if backward direction.
      (when (< dir 0)
        (pcase-setq `(,beg . ,end) (cons end beg)))
      (cond ((eql id 0) ;; ID 0 denotes real cursors
             (helix-remove-fake-cursor cursor)
             (goto-char end)
             (set-mark beg))
            (t
             (helix--move-cursor-overlay cursor end)
             (move-marker (overlay-get cursor 'point) end)
             (move-marker (overlay-get cursor 'mark) beg)
             (move-overlay region beg end))))))

(defun helix-overlapping-fake-regions (start end)
  "Return list with overlaping fake regions between START and END positions."
  (helix-overlapping-overlays
   (helix-fake-regions-in start end)))

(defun helix-overlapping-overlays (overlays)
  "Group sorted overlays into connected components based on overlap.
The input OVERLAYS must be sorted by their start positions."
  (let ((result nil)
        (current-group nil)
        (current-end (point-min)))
    (dolist (ov overlays)
      (let ((start (overlay-start ov)))
        (if (< start current-end)
            (push ov current-group)
          ;; else
          (when (length> current-group 1)
            (push (nreverse current-group) result))
          (setq current-group (list ov)))
        (setq current-end (max current-end
                               (overlay-end ov)))))
    (nreverse result)))

;;; Utils

(defun helix-fake-cursor-p (overlay)
  "Return t if an OVERLAY is a fake cursor."
  (eq (overlay-get overlay 'type) 'fake-cursor))

(defun helix-fake-region-p (overlay)
  (eq (overlay-get overlay 'type)
      'fake-region))

(defun helix-fake-regions-in (beg end)
  (->> (overlays-in beg end)
       (cl-remove-if-not #'helix-fake-region-p)
       (-sort #'helix--compare-by-overlay-start)))

(defun helix--overlays-overlap-p (o1 o2)
  (< (overlay-start o2)
     (overlay-end o1)))

(defun helix--compare-by-overlay-start (o1 o2)
  (< (overlay-start o1)
     (overlay-start o2)))

(defun overlay-live-p (overlay)
  (if-let* ((buffer (overlay-buffer overlay)))
      (buffer-live-p buffer)))

;;; helix-multiple-cursors-core.el ends here
