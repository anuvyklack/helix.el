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

;;; Code:

(require 'cl-lib)
(require 'rect)
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
        (overlay-put overlay 'before-string
                     (propertize "|" 'face 'helix-mc-cursor-bar-face))
      (overlay-put overlay 'after-string
                   (propertize " " 'face 'helix-mc-cursor-face)))
    overlay))

(defun helix--make-cursor-overlay-inline ()
  "Create overlay to look like cursor at point."
  (let ((overlay (make-overlay (point) (1+ (point)) nil nil nil)))
    (if (and helix-mc-match-cursor-style (helix-cursor-is-bar-p))
        (overlay-put overlay 'before-string (propertize "|" 'face 'helix-mc-cursor-bar-face))
      (overlay-put overlay 'face 'helix-mc-cursor-face))
    overlay))

(defun helix--make-region-overlay-between-point-and-mark ()
  "Create overlay to look like active region."
  (let ((overlay (make-overlay (mark) (point) nil nil t)))
    (overlay-put overlay 'face 'helix-mc-region-face)
    (overlay-put overlay 'type 'additional-region)
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

(defun helix--store-point-state-in-overlay (overlay)
  "Store point, mark and variables relevant to multiple cursors
functionality into OVERLAY."
  (overlay-put overlay 'point (set-marker (make-marker) (point)))
  (overlay-put overlay 'mark  (set-marker (make-marker) (mark t)))
  (dolist (var helix-mc-cursor-specific-vars)
    (when (boundp var)
      (overlay-put overlay var (symbol-value var))))
  overlay)

(defun helix--restore-point-state-from-overlay (overlay)
  "Restore point, mark and stored variables from OVERLAY."
  (let ((stored-point (overlay-get overlay 'point))
        (stored-mark  (overlay-get overlay 'mark)))
    (goto-char stored-point)
    (set-marker (mark-marker) stored-mark)
    ;; reset markers
    (set-marker stored-point nil)
    (set-marker stored-mark nil))
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
  (helix--delete-region-overlay cursor)
  (delete-overlay cursor))

(defun helix--delete-region-overlay (cursor)
  "Remove the dependent region overlay for a given CURSOR overlay."
  (when-let* ((region (overlay-get cursor 'region-overlay)))
    (delete-overlay region)))

(defvar helix--max-cursors-original nil
  "This variable maintains the original maximum number of cursors.
When `helix-create-fake-cursor-from-point' is called and
`helix-max-cursors' is overridden, this value serves as a backup
so that `helix-max-cursors' can take on a new value. When
`helix--remove-fake-cursors' is called, the values are reset.")

(defvar helix--cursor-last-used-id 0
  "Last used cursor ID.")

(defun helix--new-cursor-id ()
  "Return new unique cursor id.
IDs' are used to keep track of cursors for undo."
  (cl-incf helix--cursor-last-used-id))

(defun helix-create-fake-cursor-from-point (&optional id no-region)
  "Add a fake cursor and possibly a fake active region overlay
based on point and mark.

Assign the ID to the new cursor, if specified.
If NO-REGION is non-nil ignore active region and create fake
cursor only for point.
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
  (let ((cursor (helix--make-cursor-overlay-at-point)))
    (overlay-put cursor 'mc-id (or id (helix--new-cursor-id)))
    (overlay-put cursor 'type 'fake-cursor)
    (overlay-put cursor 'priority 100)
    (helix--store-point-state-in-overlay cursor)
    (when (and (not no-region)
               (use-region-p))
      (overlay-put cursor 'region-overlay
                   (helix--make-region-overlay-between-point-and-mark)))
    cursor))

;;; Undo functionality

(defmacro helix--add-fake-cursor-to-undo-list (id &rest body)
  "Make sure point is in the right place when undoing."
  (declare (indent defun))
  (let ((uc (make-symbol "undo-cleaner")))
    `(let ((,uc `(apply helix-deactivate-cursor-after-undo . (,,id))))
       (push ,uc buffer-undo-list)
       ,@body
       ;; If nothing has been added to the undo-list
       (if (eq ,uc (car buffer-undo-list))
           ;; then pop the cleaner right off again
           (pop buffer-undo-list)
         ;; otherwise add a function to activate this cursor
         (push `(apply activate-cursor-for-undo . (,,id))
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
           (make-overlay (point) (point) nil nil t)))))

(defun helix-deactivate-cursor-after-undo (id)
  "Called when undoing to reinstate the real cursor after undoing a fake one."
  (when helix-mc--stored-state-for-undo
    ;; Update fake cursor
    (helix-create-fake-cursor-from-point id)
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
    `(let ((,state (helix--store-point-state-in-overlay
                    (make-overlay (point) (point) nil nil t))))
       (overlay-put ,state 'type 'original-cursor)
       (save-excursion ,@body)
       (helix--restore-point-state-from-overlay ,state)
       (delete-overlay ,state))))

(defmacro helix-for-each-fake-cursor (&rest body)
  "Run the BODY for each fake cursor.
Inside this macro, the current proceeded cursor is bound to the symbol
CURSOR to be accessible from inside BODY."
  `(mapc
    #'(lambda (cursor) ,@body)
    (helix-all-fake-cursors)))

(defun helix-all-fake-cursors (&optional start end)
  (cl-remove-if-not #'helix-fake-cursor-p
                    (overlays-in (or start (point-min))
                                 (or end   (point-max)))))

(defmacro helix-for-each-cursor-ordered (&rest body)
  "Run the BODY for each cursor, fake and real, bound to the symbol CURSOR."
  (let ((real-cursor (gensym "real-cursor"))
        (overlays (gensym "overlays")))
    `(let ((,real-cursor (helix-create-fake-cursor-from-point))
           (,overlays (sort (helix-all-fake-cursors)
                            #'helix--compare-by-overlay-start)))
       (mapc #'(lambda (cursor) ,@body)
             ,overlays)
       (helix-restore-point-from-fake-cursor ,real-cursor))))

(defun helix--compare-by-overlay-start (o1 o2)
  (< (overlay-start o1) (overlay-start o2)))

(defun helix-execute-command-for-all-cursors (command)
  "Call COMMAND interactively for the real cursor and all fake ones."
  (call-interactively command)
  (helix-execute-command-for-all-fake-cursors command))

(defun helix-execute-command-for-all-fake-cursors (command)
  "Call COMMAND interactively for each cursor.
Internaly it moves point to the fake cursor, restore the environment
from it, execute COMMAND, update fake cursor."
  (helix-save-window-scroll
   (helix-save-excursion
    (helix-for-each-fake-cursor
     (helix-execute-command-for-fake-cursor command cursor))))
  (helix--reset-input-cache))

(defvar helix--executing-command-for-fake-cursor? nil)

(defun helix-execute-command-for-fake-cursor (command cursor)
  (let ((helix--executing-command-for-fake-cursor? t)
        (id (overlay-get cursor 'mc-id)))
    (helix--add-fake-cursor-to-undo-list id
      (helix-restore-point-from-fake-cursor cursor)
      (helix--execute-command command)
      (helix-create-fake-cursor-from-point id))))

(defun overlay-live-p (overlay)
  (if-let* ((buffer (overlay-buffer overlay)))
      (buffer-live-p buffer)))

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

(defun helix-fake-cursor-p (overlay)
  "Return t if an OVERLAY is a fake cursor."
  (eq (overlay-get overlay 'type) 'fake-cursor))

(defun helix-cursor-with-id (id)
  "Return the cursor with the given ID."
  (cl-find-if #'(lambda (o)
                  (and (helix-fake-cursor-p o)
                       (= id (overlay-get o 'mc-id))))
              (overlays-in (point-min) (point-max))))

(defun helix-mc-prompt-for-inclusion-in-whitelist (original-command)
  "Asks the user, then adds the command either to the once-list or the all-list."
  (let ((all? (y-or-n-p (format "Do %S for all cursors?" original-command))))
    (if all?
        (push original-command helix-commands-to-run-for-all-cursors)
      (push original-command helix-commands-to-run-once))
    (helix-mc-save-lists)
    all?))

(defun helix-number-of-cursors ()
  "The number of cursors (real and fake) in the buffer."
  (1+ (cl-count-if #'helix-fake-cursor-p
                   (overlays-in (point-min) (point-max)))))

(helix-defvar-local helix-mc--this-command nil
  "The original command being run.")

(defun helix-mc-make-a-note-of-the-command-being-run ()
  "Used with `pre-command-hook' to store the original command being run.
Since that cannot be reliably determined in the `post-command-hook'.

Specifically, `this-original-command' isn't always right, because it
could have been remapped. And certain modes (cua comes to mind) will
change their remapping based on state. So a command that changes the
state will afterwards not be recognized through the `command-remapping'
lookup."
  (unless helix--executing-command-for-fake-cursor?
    (setq helix-mc--this-command (or (command-remapping this-original-command)
                                     this-original-command))))

(defun helix-mc-execute-this-command-for-all-cursors ()
  "Wrap around `helix-mc-execute-this-command-for-all-cursors-1' to protect hook."
  (condition-case error
      (helix-mc-execute-this-command-for-all-cursors-1)
    (error
     (message "[mc] problem in `helix-mc-execute-this-command-for-all-cursors': %s"
              (error-message-string error)))))

(defun helix-mc-execute-this-command-for-all-cursors-1 ()
  "Used with `post-command-hook' to execute supported commands for all cursors.

It uses two lists of commands to know what to do: the `run-once'list
and the `run-for-all' list. If a command is in neither of these lists,
it will prompt for the proper action and then save that preference.

Some commands are so unsupported that they are even prevented for
the original cursor, to inform about the lack of support."
  (unless helix--executing-command-for-fake-cursor?

    (if (eq 1 (helix-number-of-cursors))
        ;; No fake cursors? Disable mc-mode.
        (helix-disable-multiple-cursors-mode)
      (when this-original-command
        (let ((original-command (or helix-mc--this-command
                                    (command-remapping this-original-command)
                                    this-original-command)))

          ;; skip keyboard macros, since they will generate actual commands that are
          ;; also run in the command loop - we'll handle those later instead.
          (when (functionp original-command)

            ;; if it's a lambda, we can't know if it's supported or not
            ;; - so go ahead and assume it's ok, because we're just optimistic like that
            (if (or (not (symbolp original-command))
                    ;; lambda registered by smartrep
                    (string-prefix-p "(" (symbol-name original-command)))
                (helix-execute-command-for-all-fake-cursors original-command)

              ;; smartrep `intern's commands into own obarray to help
              ;; `describe-bindings'.  So, let's re-`intern' here to
              ;; make the command comparable by `eq'.
              (setq original-command (intern (symbol-name original-command)))

              ;; otherwise it's a symbol, and we can be more thorough
              (if (get original-command 'helix-mc--unsupported)
                  (message "%S is not supported with multiple cursors%s"
                           original-command
                           (get original-command 'helix-mc--unsupported))
                ;; lazy-load the user's list file
                (helix-mc-load-lists)

                (when (and original-command
                           (not (memq original-command
                                      helix-default-commands-to-run-once))
                           (not (memq original-command
                                      helix-commands-to-run-once))
                           (or helix-mc-always-run-for-all
                               (memq original-command
                                     helix-default-commands-to-run-for-all-cursors)
                               (memq original-command
                                     helix-commands-to-run-for-all-cursors)
                               (helix-mc-prompt-for-inclusion-in-whitelist original-command)))
                  (helix-execute-command-for-all-fake-cursors original-command))))))))))

(defun helix--remove-fake-cursors ()
  "Remove all fake cursors.
Do not use it to conclude editing with multiple cursors!
Disable `helix-multiple-cursors-mode' instead."
  (helix-for-each-fake-cursor
   (helix--remove-fake-cursor cursor))
  (when helix--max-cursors-original
    (setq helix-max-cursors helix--max-cursors-original))
  (setq helix--max-cursors-original nil))

(defun helix-mc-keyboard-quit ()
  "Deactivate mark if there are any active, otherwise exit
`helix-multiple-cursors-mode'."
  (interactive)
  (if (not (use-region-p))
      (helix-disable-multiple-cursors-mode)
    (deactivate-mark)))

(defun helix-mc-repeat-command ()
  "Run last command from `command-history' for every fake cursor."
  (interactive)
  (when (or helix-mc-always-repeat-command
            (y-or-n-p (format "[mc] repeat complex command: %s? " (caar command-history))))
    (helix-execute-command-for-all-fake-cursors
     #'(lambda () (interactive)
         (cl-letf (((symbol-function 'read-from-minibuffer)
                    (lambda (p &optional i k r h d m) (read i))))
           (repeat-complex-command 0))))))

(defun helix-mc--kill-ring-entries ()
  "Return the latest `kill-ring' entry for each cursor.
The entries are returned in the order they are found in the buffer."
  (let (entries)
    (helix-for-each-cursor-ordered
     (push (car (overlay-get cursor 'kill-ring))
           entries))
    (nreverse entries)))

(defun helix-mc--maybe-set-killed-rectangle ()
  "Add the latest `kill-ring' entry for each cursor to killed-rectangle.
So you can paste it in later with `yank-rectangle'."
  (let (helix-max-cursors)
    (let ((entries (helix-mc--kill-ring-entries)))
      (unless (helix-all-elements-are-the-same-p entries)
        (setq killed-rectangle entries)))))

(defvar helix-mc-unsupported-minor-modes '(company-mode
                                           auto-complete-mode
                                           flyspell-mode
                                           jedi-mode)
  "List of minor-modes that does not play well with multiple-cursors.
They are temporarily disabled when multiple-cursors are active.")

(helix-defvar-local helix-mc-temporarily-disabled-minor-modes nil
  "The list of temporarily disabled minor-modes.")

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

(defvar-keymap helix-multiple-cursors-map
  :doc "Transient keymap for `helix-multiple-cursors-mode'.
It is active while there are multiple cursors.
Main goal of the keymap is to rebind `C-g' to conclude multiple
cursors editing."
  "C-g" #'helix-mc-keyboard-quit
  ;; "C-:" #'helix-mc-repeat-command
  )
(when (fboundp 'phi-search)
  (keymap-set helix-multiple-cursors-map "C-s" 'phi-search))
(when (fboundp 'phi-search-backward)
  (keymap-set helix-multiple-cursors-map "C-r" 'phi-search-backward))

;;;###autoload
(define-minor-mode helix-multiple-cursors-mode
  "Mode while multiple cursors are active."
  :init-value nil
  :lighter helix-mc-mode-line
  :keymap helix-multiple-cursors-map
  (if helix-multiple-cursors-mode
      (progn
        (helix-mc-temporarily-disable-unsupported-minor-modes)
        (add-hook 'pre-command-hook 'helix-mc-make-a-note-of-the-command-being-run nil t)
        (add-hook 'post-command-hook 'helix-mc-execute-this-command-for-all-cursors t t)
        (run-hooks 'helix-multiple-cursors-mode-enabled-hook))
    (remove-hook 'post-command-hook 'helix-mc-execute-this-command-for-all-cursors t)
    (remove-hook 'pre-command-hook 'helix-mc-make-a-note-of-the-command-being-run t)
    (setq helix-mc--this-command nil)
    (helix-mc--maybe-set-killed-rectangle)
    (helix--remove-fake-cursors)
    (helix-mc-enable-temporarily-disabled-minor-modes)
    (run-hooks 'helix-multiple-cursors-mode-disabled-hook)))

(defun helix-disable-multiple-cursors-mode ()
  "Disable `helix-multiple-cursors-mode' and run the corresponding hook."
  (helix-multiple-cursors-mode 0)
  (run-hooks 'helix-multiple-cursors-mode-disabled-hook))

(add-hook 'after-revert-hook 'helix-disable-multiple-cursors-mode)

(defun helix-maybe-multiple-cursors-mode ()
  "Enable `helix-multiple-cursors-mode' if there is more than one
currently active cursor."
  (if (> (helix-number-of-cursors) 1)
      (helix-multiple-cursors-mode 1)
    (helix-disable-multiple-cursors-mode)))

;;; Advices

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
      (helix-for-each-fake-cursor
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

;;; File

(defvar helix-mc--list-file-loaded nil
  "Non-nil when `helix-mc-list-file' file has already been loaded.")

(defun helix-mc-load-lists ()
  "Load `helix-mc-list-file' file."
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

;;; helix-multiple-cursors-core.el ends here
