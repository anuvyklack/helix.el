;;; helix-multiple-cursors-core.el --- Multiple cursors for Helix -*- lexical-binding: t; -*-

;; Authors: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>

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

;; The core functionality for multiple cursors. This module is heavily inspired
;; by `multiple-cursors.el' package from Magnar Sveen.
;;
;; Command is first executed for real cursor by Emacs command loop and then in
;; `post-command-hook' it executed by all fake cursors. Fake cursor is an
;; overlay that emulates cursor and stores inside point, mark, kill-ring and
;; some other variables (full list is in `helix-fake-cursor-specific-vars').
;; Executing command for fake cursor looks as follows: set point and mark to
;; positions saved in fake cursor overlay, restore all variables from it,
;; execute command in this environment, store point, mark and new state into
;; fake cursor overlay.
;;
;; ID 0 is always coresponding to real cursor.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'subr-x)
(require 'rect)
(require 'helix-common)

;;; Undo

(defun helix--single-undo-step-beginning ()
  "Initiate atomic undo step.
All following buffer modifications are grouped together as a single
action. The step is terminated with `helix--single-undo-step-end'."
  (unless (or helix--in-single-undo-step
              (helix-undo-command-p this-command)
              (eq buffer-undo-list t))
    (setq helix--in-single-undo-step t)
    (unless (null (car-safe buffer-undo-list))
      (undo-boundary))
    (setq helix--undo-list-pointer buffer-undo-list)
    (helix--undo-boundary-1)))

(defvar helix-insert-state)

(defun helix--single-undo-step-end ()
  "Finalize atomic undo step started by `helix--single-undo-step-beginning'."
  (when (and helix--in-single-undo-step
             ;; Merged all changes in Insert state into one undo step.
             (not helix-insert-state))
    (helix--undo-boundary-2)
    (unless (eq buffer-undo-list helix--undo-list-pointer)
      (let ((undo-list buffer-undo-list))
        (while (and (consp undo-list)
                    (eq (car undo-list) nil))
          (setq undo-list (cdr undo-list)))
        (let ((equiv (gethash (car undo-list)
                              undo-equiv-table)))
          ;; Remove undo boundaries from `buffer-undo-list' withing current undo
          ;; step. Also remove entries that move point during undo, because we
          ;; handle cursors positions manually to synchronize real cursor with
          ;; fake ones.
          (setq undo-list (helix-destructive-filter
                           #'(lambda (i) (or (numberp i) (null i)))
                           undo-list
                           helix--undo-list-pointer))
          ;; Restore "undo" status of the tip of `buffer-undo-list'.
          (when equiv
            (puthash (car undo-list) equiv
                     undo-equiv-table)))
        (setq buffer-undo-list undo-list)))
    (setq helix--in-single-undo-step nil
          helix--undo-list-pointer nil)))

(defun helix--undo-boundary-1 ()
  (setq helix--undo-boundary
        `(apply helix--undo-step-end ,(helix-cursors-positions)))
  (push helix--undo-boundary buffer-undo-list))

(defun helix--undo-boundary-2 ()
  (when helix--undo-boundary
    (let ((undo-list buffer-undo-list))
      (while (and (consp undo-list)
                  (eq (car undo-list) nil))
        (pop undo-list))
      (if (equal (car undo-list) helix--undo-boundary)
          (pop undo-list)
        ;; else
        (push `(apply helix--undo-step-start ,(helix-cursors-positions))
              undo-list))
      (setq helix--undo-boundary nil
            buffer-undo-list undo-list))))

;; (defmacro helix-with-undo-boundaries (&rest body)
;;   (declare (indent 0) (debug t))
;;   `(progn
;;      (helix--undo-boundary-1)
;;      (unwind-protect
;;          (progn
;;            ,@body)
;;        (helix--undo-boundary-2))))

(defun helix--undo-step-start (cursors-positions)
  "This function always called from `buffer-undo-list' during undo by
`primitive-undo' function. It is the first one from a pair of functions:
`helix--undo-step-start' and `helix--undo-step-end', which are executed
at beginning and end of a single undo step and restores real and fake
cursors points and regions after undo/redo step.

CURSORS-POSITIONS is an alist with cons cells (ID . (POINT MARK))
returned by `helix-cursors-positions' function."
  (push `(apply helix--undo-step-end ,cursors-positions)
        buffer-undo-list))

(defun helix--undo-step-end (&optional cursors-positions)
  "This function always called from `buffer-undo-list' during undo by
`primitive-undo' function. It is the second one from a pair of functions:
`helix--undo-step-start' and `helix--undo-step-end', which are executed
at beginning and end of a single undo step and restores real and fake
cursors points and regions after undo/redo step.

CURSORS-POSITIONS is an alist with cons cells (ID . (POINT MARK))
returned by `helix-cursors-positions' function."
  (maphash #'(lambda (id cursor)
               (unless (assoc id cursors-positions #'eql)
                 (helix--delete-fake-cursor cursor)))
           helix--cursors-table)
  (dolist (id-point-mark cursors-positions)
    (apply #'helix-set-cursor id-point-mark))
  (helix-maybe-disable-multiple-cursors-mode)
  (push `(apply helix--undo-step-start ,cursors-positions)
        buffer-undo-list))

;;; Fake cursor object

(defun helix--new-fake-cursor-id ()
  "Return new unique cursor id.
IDs' are used to keep track of cursors for undo."
  (cl-incf helix--fake-cursor-last-used-id))

(defvar helix--max-cursors-original nil
  "This variable maintains the original maximum number of cursors.
When `helix-create-fake-cursor' is called and `helix-max-cursors-number' is
overridden, this value serves as a backup so that `helix-max-cursors-number'
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
    (setq helix--max-cursors-original helix-max-cursors-number))
  (when-let* ((helix-max-cursors-number)
              (num (helix-number-of-cursors))
              ((<= helix-max-cursors-number num)))
    (if (yes-or-no-p (format "%d active cursors. Continue? " num))
        (setq helix-max-cursors-number (read-number "Enter a new, temporary maximum: "))
      (helix--delete-fake-cursors)
      (error "Aborted: too many cursors")))
  (prog1 (helix--create-fake-cursor-1 point mark id)
    (helix-maybe-enable-multiple-cursors-mode)))

(defun helix--create-fake-cursor-1 (point &optional mark id)
  (unless id (setq id (helix--new-fake-cursor-id)))
  (save-excursion
    (goto-char point)
    (let ((cursor (helix--set-cursor-overlay nil (point))))
      (overlay-put cursor 'id (or id (helix--new-fake-cursor-id)))
      (overlay-put cursor 'type 'fake-cursor)
      (overlay-put cursor 'priority 100)
      (helix--store-point-state cursor point mark)
      (helix--set-region-overlay cursor point mark)
      (puthash id cursor helix--cursors-table)
      cursor)))

(defun helix--delete-fake-cursors ()
  "Remove all fake cursors overlays form current buffer.
It is likely that you need `helix-remove-all-fake-cursors' function,
not this one."
  (when helix--max-cursors-original
    (setq helix-max-cursors-number helix--max-cursors-original
          helix--max-cursors-original nil))
  (mapc #'helix--delete-fake-cursor (helix-all-fake-cursors)))

(defun helix-create-fake-cursor-from-point (&optional id)
  "Add a fake cursor and possibly a fake active region overlay
based on point and mark.

Assign the ID to the new cursor, if specified.
The current state is stored in the overlay for later retrieval."
  (helix-create-fake-cursor (point) (if (use-region-p) (mark)) id))

(defun helix-set-cursor (cursor-or-id point &optional mark)
  "Move or create cursor at POINT position.

CURSOR-OR-ID can be either:
- fake cursor overlay;
- fake cursor ID;
- nil or 0 — real point and mark will be set.

If MARK is passed, for real cursor active region will be set,
for a fake cursor overlay looking like active region between
POINT and MARK will be set."
  (pcase cursor-or-id
    ((or 'nil 0)
     (goto-char point)
     (when mark (set-mark mark)))
    ((and (pred numberp) id)
     (if-let* ((cursor (gethash id helix--cursors-table)))
         (helix-move-fake-cursor cursor point mark)
       (helix-create-fake-cursor point mark id)))
    ((and (pred helix-fake-cursor-p) cursor)
     (helix-move-fake-cursor cursor point mark))))

(defun helix-move-fake-cursor (cursor point &optional mark)
  "Set fake CURSOR to new POINT and MARK and update its state."
  (set-marker (overlay-get cursor 'point) point)
  (set-marker (overlay-get cursor 'mark) (or mark point))
  (helix--set-cursor-overlay cursor point)
  (if (and mark mark-active)
      (helix--set-region-overlay cursor point mark)
    (helix--delete-region-overlay cursor))
  cursor)

(defun helix-remove-fake-cursor (cursor)
  "Delete fake CURSOR and disable `helix-multiple-cursors-mode' if no
more fake cursors are remaining."
  (helix--delete-fake-cursor cursor)
  (helix-maybe-disable-multiple-cursors-mode))

(defun helix--set-cursor-overlay (cursor pos)
  "Move or create fake CURSOR overlay at position POS.
If CURSOR is nil — create new fake cursor overlay at POS.
Return CURSOR."
  (save-excursion
    (goto-char pos)
    ;; Special case for end of line, because overlay over
    ;; a newline highlights the entire width of the window.
    (setq cursor (cond ((and cursor (eolp))
                        (move-overlay cursor pos pos))
                       (cursor
                        (move-overlay cursor pos (1+ pos)))
                       ((eolp)
                        (make-overlay pos pos nil t nil))
                       (t
                        (make-overlay pos (1+ pos) nil t nil))))
    (let ((face (cond (helix--extend-selection
                       'helix-extend-selection-cursor)
                      (helix-insert-state
                       'helix-insert-state-fake-cursor)
                      (t
                       'helix-normal-state-fake-cursor))))
      (cond ((and helix-match-fake-cursor-style
                  (helix-cursor-is-bar-p))
             (overlay-put cursor 'face nil)
             (overlay-put cursor 'before-string (propertize "|" 'face face))
             (overlay-put cursor 'after-string nil))
            ((eolp)
             (overlay-put cursor 'face nil)
             (overlay-put cursor 'before-string nil)
             (overlay-put cursor 'after-string (propertize " " 'face face)))
            (t
             (overlay-put cursor 'face face)
             (overlay-put cursor 'before-string nil)
             (overlay-put cursor 'after-string nil))))
    cursor))

(defun helix--set-region-overlay (cursor point mark)
  "Set the overlay looking like active region between POINT and MARK
and bind it to CURSOR."
  (when (and mark-active mark
             (not (eql point mark)))
    (if-let* ((region (overlay-get cursor 'fake-region)))
        (move-overlay region point mark)
      ;; else
      (setq region (-doto (make-overlay point mark nil nil t)
                     (overlay-put 'face 'helix-region-face)
                     (overlay-put 'type 'fake-region)
                     (overlay-put 'priority 99)
                     (overlay-put 'id (overlay-get cursor 'id))))
      (overlay-put cursor 'fake-region region))))

(defun helix--delete-fake-cursor (cursor)
  "Delete CURSOR overlay."
  (remhash (overlay-get cursor 'id)
           helix--cursors-table)
  (set-marker (overlay-get cursor 'point) nil)
  (set-marker (overlay-get cursor 'mark) nil)
  (helix--delete-region-overlay cursor)
  (delete-overlay cursor))

(defun helix--delete-region-overlay (cursor)
  "Remove the dependent region overlay for a given CURSOR overlay."
  (-some-> (overlay-get cursor 'fake-region)
    (delete-overlay)))

(defun helix--store-point-state (overlay point mark)
  "Store POINT, MARK and variables relevant to fake cursor into OVERLAY."
  (unless mark (setq mark point))
  (let ((pnt-marker (or (overlay-get overlay 'point)
                        (overlay-put overlay 'point (-doto (make-marker)
                                                      (set-marker-insertion-type t)))))
        (mrk-marker (or (overlay-get overlay 'mark)
                        (overlay-put overlay 'mark (make-marker)))))
    (set-marker pnt-marker point)
    (set-marker mrk-marker mark))
  (dolist (var helix-fake-cursor-specific-vars)
    (when (boundp var)
      (overlay-put overlay var (symbol-value var))))
  overlay)

(defun helix-update-fake-cursor-state (cursor)
  "Update variables stored in fake CURSOR."
  (dolist (var helix-fake-cursor-specific-vars)
    (when (boundp var)
      (overlay-put cursor var (symbol-value var)))))

(defun helix-restore-point-from-fake-cursor (cursor)
  "Restore point, mark and saved variables from CURSOR overlay, and delete it."
  (helix--restore-point-state cursor)
  (helix--delete-fake-cursor cursor))

(defun helix--restore-point-state (overlay)
  "Restore point, mark and cursor variables saved in OVERLAY."
  (goto-char (overlay-get overlay 'point))
  (set-marker (mark-marker) (overlay-get overlay 'mark))
  (dolist (var helix-fake-cursor-specific-vars)
    (when (boundp var)
      (set var (overlay-get overlay var))))
  (helix--delete-region-overlay overlay)
  (delete-overlay overlay))

(defun helix-create-cursors (ranges)
  "Create set of active regions.
RANGES is a list of cons cells (START . END) with bounds of regions.
The real region will be set for the first range in RANGES, and fake one
for others."
  (when ranges
    (-let (((mark . point) (car ranges)))
      (set-mark mark)
      (goto-char point))
    (cl-loop for (mark . point) in (cdr ranges)
             do (helix-create-fake-cursor point mark))))

(defun helix-remove-fake-cursor-from-buffer (cursor)
  (helix--delete-region-overlay cursor)
  (delete-overlay cursor)
  cursor)

(defun helix-restore-fake-cursor-in-buffer (cursor)
  (let ((point (overlay-get cursor 'point))
        (mark  (overlay-get cursor 'mark)))
    (helix-set-cursor cursor point mark)))

;;; Access fake cursors

(defun helix-all-fake-cursors (&optional sort)
  "Return list with all fake cursors in current buffer.
If SORT is non-nil sort cursors in order they are located in buffer."
  (let ((cursors (hash-table-values helix--cursors-table)))
    (if sort
        (sort cursors #'(lambda (c1 c2)
                          (< (overlay-get c1 'point)
                             (overlay-get c2 'point))))
      cursors)))

(defun helix-fake-cursors-in (start end)
  "Return list of fake cursors within START...END buffer positions."
  (-filter #'helix-fake-cursor-p
           (overlays-in start end)))

;; (defun helix-fake-regions-in (start end)
;;   "Return a list of fake-cursors in START ... END region."
;;   (cl-remove-if-not #'helix-fake-region-p
;;                     (overlays-in start end)))

(defun helix-cursor-with-id (id)
  "Return the cursor with the given ID if it is stil alive."
  (if-let* ((cursor (gethash id helix--cursors-table))
            ((helix-overlay-live-p cursor)))
      cursor))

(defun helix-fake-cursor-at (position)
  "Return the fake cursor at POSITION, or nil if no one."
  (--find (= position (overlay-get it 'point))
          (helix-fake-cursors-in position (1+ position))))

(defun helix-next-fake-cursor (&optional position)
  "Return the next fake cursor after the POSITION."
  ;; (unless position (setq position (point)))
  (cl-loop for pos = (next-overlay-change position)
           then (next-overlay-change pos)
           until (eql pos (point-max))
           thereis (helix-fake-cursor-at pos)))

(defun helix-previous-fake-cursor (position)
  "Return the first fake cursor before the POSITION."
  (cl-loop for pos = (previous-overlay-change position)
           then (previous-overlay-change pos)
           until (eql pos (point-min))
           thereis (helix-fake-cursor-at pos)))

(defun helix-first-fake-cursor ()
  "Return the first fake cursor in the buffer."
  (-min-by #'(lambda (a b)
               (> (overlay-get a 'point)
                  (overlay-get b 'point)))
           (helix-all-fake-cursors)))

(defun helix-last-fake-cursor ()
  "Return the last fake cursor in the buffer."
  (-max-by #'(lambda (a b)
               (> (overlay-get a 'point)
                  (overlay-get b 'point)))
           (helix-all-fake-cursors)))

(defun helix-number-of-cursors ()
  "The number of cursors (real and fake) in the buffer."
  (1+ (hash-table-count helix--cursors-table)))

(defun helix-any-fake-cursors-p ()
  "Return non-nil if there are fake cursors in the buffer."
  (not (hash-table-empty-p helix--cursors-table)))

(defun helix-cursors-positions ()
  "Return alist of cons cells (ID . (POINT MARK)) with positions of all cursors.
Real cursor has ID 0 and is the first element (car) of the list.
MARK is nil if cursor has no region."
  (let (alist)
    (when helix-multiple-cursors-mode
      (dolist (cursor (helix-all-fake-cursors))
        (let* ((id (overlay-get cursor 'id))
               (point (marker-position (overlay-get cursor 'point)))
               (mark (if-let* (((overlay-get cursor 'mark-active))
                               (mark (marker-position (overlay-get cursor 'mark)))
                               ((/= point mark)))
                         mark)))
          (unless (eql id 0)
            (push (list id point mark) alist)))))
    (push (list 0 (point) (if (use-region-p) (mark)))
          alist)
    alist))

;;; Executing commands for real and fake cursors

(defmacro helix-save-window-scroll (&rest body)
  "Save the window scroll position, evaluate BODY, restore it."
  (declare (indent 0) (debug t))
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
       (helix--store-point-state ,state (point) (mark t))
       (save-excursion ,@body)
       (helix--restore-point-state ,state))))

(defmacro helix-with-fake-cursor (cursor &rest body)
  "Move point to the fake CURSOR, restore the environment from it,
evaluate BODY, update fake CURSOR."
  (declare (indent defun) (debug (symbolp &rest form)))
  `(let ((helix--executing-command-for-fake-cursor t))
     (helix--restore-point-state ,cursor)
     (unwind-protect
         (progn ,@body)
       (helix-move-fake-cursor ,cursor (point) (mark t))
       (helix-update-fake-cursor-state ,cursor))))

(defmacro helix-with-each-cursor (&rest body)
  (declare (indent 0) (debug t))
  `(progn
     ;; First execute BODY for the fake cursors so that BODY can create new
     ;; fake cursors, but overall was only executed for the original ones.
     (when helix-multiple-cursors-mode
       (helix-save-window-scroll
         (helix-save-excursion
          (dolist (cursor (helix-all-fake-cursors))
            (helix-with-fake-cursor cursor
              ,@body)))))
     ;; Finaly execute for real cursor
     ,@body))

(defun helix-execute-command-for-all-cursors (command)
  "Call COMMAND interactively for all cursors: real and fake ones."
  (call-interactively command)
  (helix--execute-command-for-all-fake-cursors command)
  (when (helix-merge-regions-p command)
    (helix-merge-overlapping-regions))
  (setq helix--input-cache nil))

(defun helix--execute-command-for-all-fake-cursors (command)
  "Call COMMAND interactively for each fake cursor.

Two lists of commands are used: the `run-once' list and the `run-for-all'
list. If a command is in neither of these lists, the user will be prompted
for the proper action and then the choice will be saved.

Some commands are so unsupported that they are even prevented for
the original cursor, to inform about the lack of support."
  (when helix-multiple-cursors-mode
    (cond ((and (symbolp command)
                (get command 'helix-unsupported))
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
              (dolist (cursor (helix-all-fake-cursors))
                (helix-with-fake-cursor cursor
                  ;; (helix--call-interactively command)
                  (call-interactively command)))))))))

(defun helix--call-interactively (command)
  "Run COMMAND, simulating the parts of the command loop that
makes sense for fake cursor."
  (setq this-command command)
  ;; (ignore-errors)
  (run-hooks 'pre-command-hook)
  (unless (eq this-command 'ignore)
    (call-interactively command))
  (run-hooks 'post-command-hook)
  (when deactivate-mark (deactivate-mark)))

(defmacro helix-with-real-cursor-as-fake (&rest body)
  "Temporarily create a fake-cursor for real one with ID 0.
Restore it after BODY evaluation if it is still alive."
  (declare (indent 0) (debug t))
  (let ((real-cursor (make-symbol "real-cursor")))
    `(let ((,real-cursor (helix--create-fake-cursor-1 (point) (mark t) 0)))
       (prog1 (progn ,@body)
         (cond ((helix-overlay-live-p ,real-cursor)
                (helix-restore-point-from-fake-cursor ,real-cursor))
               ((helix-any-fake-cursors-p)
                (helix-restore-point-from-fake-cursor (helix-first-fake-cursor))))
         (helix-maybe-disable-multiple-cursors-mode)))))

;;; Minor mode

;;;###autoload
(define-minor-mode helix-multiple-cursors-mode
  "Minor mode, which is active when there are multiple cursors in the buffer.
No need activate it manually: it is activated automatically when you create
first fake cursor with `helix-create-fake-cursor', and disabled when you
delete last one with `helix-remove-fake-cursor'."
  :global nil
  :interactive nil
  :lighter helix-mc-mode-line
  :keymap helix-multiple-cursors-map
  (if helix-multiple-cursors-mode
      (helix-mc--disable-incompatible-minor-modes)
    (helix-mc--maybe-set-killed-rectangle)
    (helix--delete-fake-cursors)
    (helix-mc--enable-incompatible-minor-modes)))

(defun helix-maybe-enable-multiple-cursors-mode ()
  "Enable `helix-multiple-cursors-mode' if not already enabled
and fake cursors are present in the buffer."
  (when (and (not helix-multiple-cursors-mode)
             (helix-any-fake-cursors-p))
    (helix-multiple-cursors-mode 1)))

(defun helix-maybe-disable-multiple-cursors-mode ()
  "Disable `helix-multiple-cursors-mode' if no fake cursors remain
in current buffer."
  (unless (helix-any-fake-cursors-p)
    (helix-multiple-cursors-mode -1)))

(defun helix-mc--maybe-set-killed-rectangle ()
  "Add the latest `kill-ring' entry for each cursor to `killed-rectangle'.
So you can paste it in later with `yank-rectangle'."
  (when (helix-any-fake-cursors-p)
    (let ((entries (helix-with-real-cursor-as-fake
                     (-map #'(lambda (cursor)
                               (car-safe (overlay-get cursor 'kill-ring)))
                           (helix-all-fake-cursors :sort)))))
      (unless (helix-all-elements-are-equal-p entries)
        (setq killed-rectangle entries)))))

(defun helix-mc--disable-incompatible-minor-modes ()
  "Disable incompatible minor modes while there are multiple cursors
in the buffer."
  (dolist (mode helix-minor-modes-incompatible-with-multiple-cursors)
    (when (and (boundp mode) (symbol-value mode))
      (push mode helix--temporarily-disabled-minor-modes)
      (funcall mode -1))))

(defun helix-mc--enable-incompatible-minor-modes ()
  "Enable minor modes disabled by `helix-mc--disable-incompatible-minor-modes'."
  (dolist (mode helix--temporarily-disabled-minor-modes)
    (funcall mode 1))
  (setq helix--temporarily-disabled-minor-modes nil))

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

(defun helix-load-whitelists ()
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

;;; Merge overlapping regions

(defun helix-merge-regions-p (command)
  "Return non-nil if regions need to be merged after COMMAND."
  (and helix-multiple-cursors-mode
       mark-active
       (or (and helix--extend-selection
                (memq command helix--motion-command))
           (memq command helix--merge-regions-commands))))

(defun helix-merge-overlapping-regions ()
  "Merge overlapping regions."
  (let ((dir (helix-region-direction)))
    (dolist (group-or-overlapping-regions (helix--overlapping-regions))
      (let ((beg (point-max))
            (end (point-min))
            id delete real-cursor?)
        (dolist (region-data group-or-overlapping-regions)
          ;; rid - region id, b - region beginning, e - region end
          (pcase-let ((`(,rid ,b ,e) region-data))
            (when (< b beg)
              (setq beg b)
              (when (< dir 0)
                (if id (push id delete))
                (setq id rid)))
            (when (> e end)
              (setq end e)
              (when (< 0 dir)
                (if id (push id delete))
                (setq id rid)))
            (cond ((eql rid 0)
                   (setq real-cursor? t))
                  ((/= id rid)
                   (push rid delete)))))
        (let ((pnt (if (< dir 0) beg end))
              (mrk (if (< dir 0) end beg)))
          (pcase id
            (0 (goto-char pnt)
               (set-marker (mark-marker) mrk))
            (_ (when-let* ((cursor (gethash id helix--cursors-table)))
                 (cond (real-cursor?
                        (helix-restore-point-from-fake-cursor cursor)
                        (goto-char pnt)
                        (set-marker (mark-marker) mrk))
                       (t
                        (helix-move-fake-cursor cursor pnt mrk)))))))
        (dolist (id delete)
          (when-let* ((cursor (gethash id helix--cursors-table)))
            (helix--delete-fake-cursor cursor)))))
    (helix-maybe-disable-multiple-cursors-mode)))

(defun helix--overlapping-regions ()
  "Return the list of groups, where each group is a list of
cons cells (ID . (START END)) denoting fake cursor ID and its
region bounds. Inside each group, all regions are overlapping
and sorted by starting position. ID 0 coresponds to the real
cursor."
  (let ((alist (helix--regions-ranges))
        result
        current-group
        (current-end (point-min)))
    (dolist (item alist)
      (-let (((_ start end) item))
        (if (< start current-end)
            (push item current-group)
          ;; else
          (when (length> current-group 1)
            (push (nreverse current-group) result))
          (setq current-group (list item)))
        (setq current-end (max end current-end))))
    (when (length> current-group 1)
      (push (nreverse current-group) result))
    (nreverse result)))

(defun helix--regions-ranges ()
  "Return the alist with cons cells (ID . (START END)).
\(START END) are bounds of regions. Alist is sorted by START.
ID 0 coresponds to the real cursor."
  (let* ((alist (cons
                 ;; Append real cursor with ID 0
                 `(0 ,(region-beginning) ,(region-end))
                 (mapcar #'(lambda (cursor)
                             (let* ((id  (overlay-get cursor 'id))
                                    (pnt (overlay-get cursor 'point))
                                    (mrk (overlay-get cursor 'mark))
                                    (start (min pnt mrk))
                                    (end   (max pnt mrk)))
                               `(,id ,start ,end)))
                         (helix-all-fake-cursors)))))
    (sort alist #'(lambda (a b)
                    (< (-second-item a) (-second-item b))))))

;;; Integration with other Emacs functionality

(add-hook 'after-revert-hook 'helix-remove-all-fake-cursors)

(helix-define-advice execute-kbd-macro (:around (orig-fun &rest args))
  "`execute-kbd-macro' should never be run for fake cursors.
The real cursor will execute the keyboard macro, resulting in new commands
in the command loop, and the fake cursors can pick up on those instead."
  (unless helix--executing-command-for-fake-cursor
    (apply orig-fun args)))

;; Intercept some reading commands so you won't have to
;; answer them for every single cursor
(defvar helix--input-cache nil)

(defmacro helix-cache-input (fn-name)
  "Advice function to cache users input to use it for all cursors.

Should be used with interactive input command to create advice around it,
to cache users responses and use it for all cursors.

FN-NAME should be an interactive function taking PROMPT as first argument,
like `read-char' or `read-from-minibuffer'. This PROMPT will be used as
a hash key, to distinguish different calls of FN-NAME within one command.
Calls with equal PROMPT or without it would be undistinguishable."
  `(helix-define-advice ,fn-name (:around (orig-fun &rest args) helix)
     "Cache the users input to use it with multiple cursors."
     (if (not (bound-and-true-p helix-multiple-cursors-mode))
         (apply orig-fun args)
       (let* (;; Use PROMPT argument as a hash key to distinguish different
              ;; calls of `read-char' like functions within one command.
              (prompt (car-safe args))
              (key (list ,(symbol-name fn-name) prompt))
              (value (alist-get key helix--input-cache nil nil #'equal)))
         (unless value
           (setq value (apply orig-fun args))
           (push (cons key value) helix--input-cache))
         value))))

(helix-cache-input read-char)
(helix-cache-input read-quoted-char)
(helix-cache-input read-from-kill-ring)
(helix-cache-input read-char-from-minibuffer)
(helix-cache-input register-read-with-preview)  ; used by read-string

(defmacro helix-unsupported-command (command)
  "Adds command to list of unsupported commands and prevents it
from being executed if in `helix-multiple-cursors-mode'."
  `(progn
     (put ',command 'helix-unsupported t)
     (helix-define-advice ,command (:around (orig-fun &rest args)
                                            helix-unsupported)
       "Don't execute an unsupported command while multiple cursors are active."
       (unless (and helix-multiple-cursors-mode
                    (called-interactively-p 'any))
         (apply orig-fun args)))))

;; Commands that don't work with multiple-cursors
(helix-unsupported-command isearch-forward)
(helix-unsupported-command isearch-backward)

(helix-define-advice current-kill (:before (n &optional _do-not-move) helix)
  "Make sure pastes from other programs are added to `kill-ring's
of all cursors when yanking."
  (when-let* ((interprogram-paste (and (= n 0)
                                       interprogram-paste-function
                                       (funcall interprogram-paste-function))))
    (when (listp interprogram-paste)
      ;; Use `reverse' to avoid modifying external data.
      (setq interprogram-paste (reverse interprogram-paste)))
    ;; Add `interprogram-paste' to `kill-ring's of all cursors real and
    ;; fake. This is what `current-kill' do internally, but we have to do
    ;; it ourselves, because `interprogram-paste-function' is not a pure
    ;; function — it returns something only once.
    (let ((interprogram-cut-function nil)
          (interprogram-paste-function nil))
      ;; real cursor
      (if (listp interprogram-paste)
          (mapc 'kill-new interprogram-paste)
        (kill-new interprogram-paste))
      ;; fake cursors
      (dolist (cursor (helix-all-fake-cursors))
        (let ((kill-ring (overlay-get cursor 'kill-ring))
              (kill-ring-yank-pointer (overlay-get cursor 'kill-ring-yank-pointer)))
          (if (listp interprogram-paste)
              (mapc 'kill-new interprogram-paste)
            (kill-new interprogram-paste))
          (overlay-put cursor 'kill-ring kill-ring)
          (overlay-put cursor 'kill-ring-yank-pointer kill-ring-yank-pointer))))))

;; M-x
(helix-define-advice execute-extended-command (:after (&rest _) helix)
  "Execute selected command for all cursors."
  (setq helix-this-command this-command))

;;; Utils

(defun helix-fake-cursor-p (overlay)
  "Return t if an OVERLAY is a fake cursor."
  (eq (overlay-get overlay 'type) 'fake-cursor))

(defun helix-fake-region-p (overlay)
  (eq (overlay-get overlay 'type)
      'fake-region))

(defun helix--overlays-overlap-p (o1 o2)
  (< (overlay-start o2)
     (overlay-end o1)))

(defun helix--compare-by-overlay-start (o1 o2)
  (< (overlay-start o1)
     (overlay-start o2)))

(defun helix-overlay-live-p (overlay)
  "Return non-nil if OVERLAY is not deleted from buffer."
  (if-let* ((buffer (overlay-buffer overlay)))
      (buffer-live-p buffer)))

(provide 'helix-multiple-cursors-core)
;;; helix-multiple-cursors-core.el ends here
