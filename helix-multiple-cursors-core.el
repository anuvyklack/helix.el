;;; helix-multiple-cursors-core.el --- Multiple cursors for Helix -*- lexical-binding: t; -*-
;;
;; Authors: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The core functionality for multiple cursors. The code is inspired by
;; `multiple-cursors.el' package from Magnar Sveen.
;;
;; How multiple cursors works internally. Command is firstly executed for real
;; cursor by Emacs command loop. Then in `post-command-hook' it executed for all
;; fake cursors. Fake cursor is an overlay that emulates cursor and stores
;; inside point, mark, kill-ring and some other variables (full list is in
;; `helix-fake-cursor-specific-vars'). Executing command for fake cursor looks
;; as follows: set point and mark to positions saved in fake cursor overlay,
;; restore all variables from it, execute command in this environment, store
;; point, mark and new state into fake cursor overlay.
;;
;; Each command should has `multiple-cursors' symbol property. If it is `t' —
;; command will be execute for all cursors. Any other value except `nil' — it
;; will be executed only once for real (main) cursor. If `multiple-cursors'
;; property is `nil' i.e. absent user will be prompted how execute this command
;; and choosen value is permanently stored in `helix-whitelist-file' file.
;;
;; ID 0 is always corresponding to real cursor.

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
  (dolist (val cursors-positions)
    (-let [(id point mark line-selection?) val]
      (if (eql id 0)
          (progn (goto-char point)
                 (set-mark mark)
                 (setq helix-linewise-selection line-selection?))
        ;; else
        (let ((mark-active (not (null mark)))
              (helix-linewise-selection line-selection?))
          (if-let* ((cursor (gethash id helix--cursors-table)))
              (helix-move-fake-cursor cursor point mark :update)
            (helix--create-fake-cursor-1 id point mark))))))
  (helix-auto-multiple-cursors-mode)
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
can take on a new value. When `helix--delete-all-fake-cursors' is called,
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
  (when helix-max-cursors-number
    (when-let* ((num (helix-number-of-cursors))
                ((<= helix-max-cursors-number num)))
      (if (yes-or-no-p (format "%d active cursors. Continue? " num))
          (setq helix-max-cursors-number (read-number "Enter a new, temporary maximum: "))
        (helix--delete-all-fake-cursors)
        (error "Aborted: too many cursors"))))
  (prog1 (helix--create-fake-cursor-1 id point mark)
    (unless helix-multiple-cursors-mode
      (helix-multiple-cursors-mode 1))))

(defun helix--create-fake-cursor-1 (id point mark)
  (unless id (setq id (helix--new-fake-cursor-id)))
  (save-excursion
    (goto-char point)
    (let ((cursor (helix--set-cursor-overlay nil point)))
      (overlay-put cursor 'id id)
      (overlay-put cursor 'type 'fake-cursor)
      (overlay-put cursor 'priority 100)
      (helix--store-point-state cursor point mark)
      (when (and mark-active mark
                 (/= point mark))
        (helix--set-region-overlay cursor point mark))
      (puthash id cursor helix--cursors-table)
      cursor)))

(defun helix--delete-all-fake-cursors ()
  "Remove all fake cursors overlays form current buffer.
It is likely that you need `helix-delete-all-fake-cursors' function,
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
  (helix-delete-main-selection-overlay)
  (helix-create-fake-cursor (point) (mark t) id))

(defun helix-move-fake-cursor (cursor point &optional mark update)
  "Move fake CURSOR to new POINT.
If MARK is non-nil also set fake region.
Move fake CURSOR and its region according to new POINT and MARK.
Optionally UPDATE fake-cursors state."
  (set-marker (overlay-get cursor 'point) point)
  (set-marker (overlay-get cursor 'mark) (or mark point))
  (when update (helix-update-fake-cursor-state cursor))
  (helix--set-cursor-overlay cursor point)
  (if (and mark mark-active)
      (helix--set-region-overlay cursor point mark)
    (helix--delete-region-overlay cursor))
  cursor)

(defun helix-delete-fake-cursor (cursor)
  "Delete fake CURSOR and disable `helix-multiple-cursors-mode' if no
more fake cursors are remaining."
  (helix--delete-fake-cursor cursor)
  (helix-auto-multiple-cursors-mode))

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
             (overlay-put cursor 'before-string (propertize helix-bar-fake-cursor 'face face))
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

(defun helix--set-region-overlay (cursor beg end)
  "Set the overlay looking like active region between BEG and END
and bind it to CURSOR."
  (when (overlay-get cursor 'helix-linewise-selection)
    (when (< end beg) (cl-rotatef beg end))
    (cl-incf end))
  (if-let* ((region (overlay-get cursor 'fake-region)))
      (move-overlay region beg end)
    ;; else
    (setq region (-doto (make-overlay beg end nil nil t)
                   (overlay-put 'face 'region)
                   (overlay-put 'type 'fake-region)
                   (overlay-put 'id (overlay-get cursor 'id))))
    (overlay-put cursor 'fake-region region)))

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
  (or (-some-> (overlay-get overlay 'point)
        (set-marker point))
      (overlay-put overlay 'point (copy-marker point t)))
  (or (-some-> (overlay-get overlay 'mark)
        (set-marker mark))
      (overlay-put overlay 'mark (copy-marker mark)))
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
    (-let [(mark . point) (car ranges)]
      (set-mark mark)
      (goto-char point))
    (cl-loop for (mark . point) in (cdr ranges)
             do (helix-create-fake-cursor point mark))))

(defun helix-remove-fake-cursor-from-buffer (cursor)
  "Disable the fake-CURSOR display in the buffer without deleting it."
  (helix--delete-region-overlay cursor)
  (delete-overlay cursor)
  cursor)

(defun helix-restore-fake-cursor-in-buffer (cursor)
  (helix-set-cursor cursor
                    (overlay-get cursor 'point)
                    (overlay-get cursor 'mark)
                    (overlay-get cursor 'helix-linewise-selection)))

(defun helix-fake-cursor-p (overlay)
  "Return t if an OVERLAY is a fake cursor."
  (eq (overlay-get overlay 'type) 'fake-cursor))

(defun helix-fake-region-p (overlay)
  "Return t if an OVERLAY is a fake region."
  (eq (overlay-get overlay 'type) 'fake-region))

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
                               (mark (marker-position (overlay-get cursor 'mark))))
                         mark))
               (line-selection? (overlay-get cursor 'helix-linewise-selection)))
          (unless (eql id 0)
            (push (list id point mark line-selection?)
                  alist)))))
    (push (list 0 ;; id
                (point)
                (if mark-active (mark))
                helix-linewise-selection)
          alist)
    alist))

;;; Executing commands for real and fake cursors

(defmacro helix-save-window-scroll (&rest body)
  "Save the window scroll position, evaluate BODY, restore it."
  (declare (indent 0) (debug t))
  (let ((win-start (make-symbol "win-start"))
        (win-hscroll (make-symbol "win-hscroll")))
    `(let ((,win-start (copy-marker (window-start)))
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
  `(let ((helix-executing-command-for-fake-cursor t))
     (helix--restore-point-state ,cursor)
     (unwind-protect
         (progn ,@body)
       (helix-move-fake-cursor ,cursor (point) (mark t) :update))))

(defmacro helix-with-each-cursor (&rest body)
  "Evaluate BODY for all cursors: real and fake ones."
  (declare (indent 0) (debug t))
  ;; First collect fake cursors because BODY can create new cursors,
  ;; and we want it to be executed only for original ones.
  `(let ((cursors (if helix-multiple-cursors-mode
                      (helix-all-fake-cursors))))
     ;; Main cursor
     (helix-delete-main-selection-overlay)
     ,@body
     ;; Fake cursors
     (when cursors
       (helix-save-window-scroll
         (helix-save-excursion
          (dolist (cursor cursors)
            (helix-with-fake-cursor cursor
              ,@body)))))))

(defun helix-execute-command-for-all-cursors (command)
  "Call COMMAND interactively for all cursors: real and fake ones."
  (helix--call-interactively command)
  (helix--execute-command-for-all-fake-cursors command)
  (when (helix-merge-regions-p command)
    (helix-merge-overlapping-regions))
  (setq helix--input-cache nil))

(defun helix--execute-command-for-all-fake-cursors (command)
  "Call COMMAND interactively for each fake cursor."
  (when helix-multiple-cursors-mode
    (cond ((and (symbolp command)
                (get command 'helix-unsupported))
           (message "%S is not supported with multiple cursors" command))
          ((or
            ;; If it's a lambda, we can't know if it's supported or not -
            ;; so go ahead and assume it's ok.
            (not (symbolp command))
            (pcase (get command 'multiple-cursors)
              ('t t)
              ('nil (helix--prompt-for-unknown-command command))))
           (helix-save-window-scroll
             (helix-save-excursion
              (dolist (cursor (helix-all-fake-cursors))
                (helix-with-fake-cursor cursor
                  (helix--call-interactively command)))))))))

(defun helix--call-interactively (command)
  "Run COMMAND, simulating the parts of the command loop that
makes sense for fake cursor."
  (when (and (symbolp command)
             (get command 'helix-deactivate-mark))
    (deactivate-mark))
  (unless (eq command 'ignore)
    (let ((this-command command))
      (call-interactively command)))
  ;; (setq this-command command)
  ;; ;; (ignore-errors)
  ;; (run-hooks 'pre-command-hook)
  ;; (unless (eq this-command 'ignore)
  ;;   (call-interactively command))
  ;; (run-hooks 'post-command-hook)
  ;; (when deactivate-mark (deactivate-mark))
  )

(defmacro helix-with-real-cursor-as-fake (&rest body)
  "Temporarily create a fake-cursor for real one with ID 0.
Restore it after BODY evaluation if it is still alive."
  (declare (indent 0) (debug t))
  (let ((real-cursor (make-symbol "real-cursor")))
    `(let ((,real-cursor (helix--create-fake-cursor-1 0 (point) (mark t))))
       (helix-delete-main-selection-overlay)
       (unwind-protect (progn ,@body)
         (cond ((helix-overlay-live-p ,real-cursor)
                (helix-restore-point-from-fake-cursor ,real-cursor))
               ((helix-any-fake-cursors-p)
                (helix-restore-point-from-fake-cursor (helix-first-fake-cursor))))
         (helix-auto-multiple-cursors-mode)
         (helix-update-cursor)))))

;;; Multiple cursors minor mode

;;;###autoload
(define-minor-mode helix-multiple-cursors-mode
  "Minor mode, which is active when there are multiple cursors in the buffer.
No need to activate it manually: it is activated automatically when you create
first fake cursor with `helix-create-fake-cursor', and disabled when you
delete last one with `helix-delete-fake-cursor'."
  :global nil
  :interactive nil
  (if helix-multiple-cursors-mode
      (helix-mc--disable-incompatible-minor-modes)
    (helix--delete-all-fake-cursors)
    (helix-mc--enable-incompatible-minor-modes)))

(defun helix-auto-multiple-cursors-mode ()
  "Enable `helix-multiple-cursors' if there are multiple cursors,
disable if only one."
  (when (xor helix-multiple-cursors-mode
             (helix-any-fake-cursors-p))
    (helix-multiple-cursors-mode 'toggle)))

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

(defun helix-multiple-cursors--indicator ()
  (when helix-multiple-cursors-mode
    (format helix-multiple-cursors-mode-line-indicator
            (helix-number-of-cursors))))

;;; Whitelists

(defun helix--prompt-for-unknown-command (command)
  "Ask the user whether the COMMAND should be executed for all cursors or not,
and remember the choice.

Return t if COMMMAND should be executed for all cursors."
  (let ((for-all? (y-or-n-p (format "Do %S for all cursors?" command))))
    (cond (for-all?
           (put command 'multiple-cursors t)
           (push command helix-commands-to-run-for-all-cursors))
          (t
           (put command 'multiple-cursors 'false)
           (push command helix-commands-to-run-once)))
    (helix-save-whitelists-into-file)
    for-all?))

(defun helix-load-whitelists ()
  "Load `helix-whitelist-file' file if not yet."
  (unless helix--whitelist-file-loaded
    (load helix-whitelist-file 'noerror 'nomessage)
    (setq helix--whitelist-file-loaded t)
    (mapc #'(lambda (command)
              (put command 'multiple-cursors t))
          helix-commands-to-run-for-all-cursors)
    (mapc #'(lambda (command)
              (put command 'multiple-cursors 'false))
          helix-commands-to-run-once)))

(defun helix--dump-whitelist (list-symbol)
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

(defun helix-save-whitelists-into-file ()
  "Save users preferences which commands to execute for one cursor
and which for all to `helix-whitelist-file' file."
  (with-temp-file helix-whitelist-file
    (emacs-lisp-mode)
    (insert ";; -*- mode: emacs-lisp -*-")
    (newline)
    (insert ";; This file is automatically generated by the Helix.")
    (newline)
    (insert ";; It keeps track of your preferences for running commands with multiple cursors.")
    (newline)
    (newline)
    (helix--dump-whitelist 'helix-commands-to-run-for-all-cursors)
    (newline)
    (helix--dump-whitelist 'helix-commands-to-run-once)))

;;; Merge overlapping regions

(defun helix-merge-regions-p (command)
  "Return non-nil if regions need to be merged after COMMAND."
  (and helix-multiple-cursors-mode
       mark-active
       (cond ((symbolp command)
              (pcase (get command 'helix-merge-regions)
                ('extend-selection helix--extend-selection)
                (val val)))
             ((functionp command) ;; `command' is a lambda
              t))))

(defun helix-merge-overlapping-regions ()
  "Merge overlapping regions."
  (let ((dir (helix-region-direction)))
    (dolist (group-or-overlapping-regions (helix--overlapping-regions))
      (let ((beg (point-max))
            (end (point-min))
            id delete real-cursor?)
        (dolist (region-data group-or-overlapping-regions)
          ;; rid - region ID, b - region beginning, e - region end
          (-let [(rid b e) region-data]
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
    (helix-auto-multiple-cursors-mode)))

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
      (-let [(_ start end) item]
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

;;; Integration with other Emacs functionalities

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
     (if (bound-and-true-p helix-multiple-cursors-mode)
         (let* (;; Use PROMPT argument as a hash key to distinguish different
                ;; calls of `read-char' like functions within one command.
                (prompt (car-safe args))
                (key (list ,(symbol-name fn-name) prompt)))
           (with-memoization (alist-get key helix--input-cache nil nil #'equal)
             (apply orig-fun args)))
       ;; else
       (apply orig-fun args))))

(defmacro helix-unsupported-command (command)
  "Adds command to list of unsupported commands and prevents it
from being executed when `helix-multiple-cursors-mode' is active."
  `(progn
     (put ',command 'helix-unsupported t)
     (helix-define-advice ,command (:around (orig-fun &rest args)
                                            helix-unsupported)
       "Don't execute an unsupported command while multiple cursors are active."
       (unless (and helix-multiple-cursors-mode
                    (called-interactively-p 'any))
         (apply orig-fun args)))))

;; Execute following commands only for ALL cursor.
(mapc #'(lambda (command)
          (put command 'multiple-cursors t))
      '(comment-dwim         ;; gc
        fill-region          ;; gq
        indent-region        ;; =
        indent-rigidly-left  ;; >
        indent-rigidly-right ;; <
        tab-bar-mouse-down-1
        self-insert-command
        quoted-insert
        previous-line
        next-line
        newline
        newline-and-indent
        open-line
        delete-blank-lines
        transpose-chars
        transpose-lines
        transpose-paragraphs
        transpose-regions
        join-line
        right-char
        right-word
        forward-char
        forward-word
        left-char
        left-word
        backward-char
        backward-word
        forward-paragraph
        backward-paragraph
        forward-sexp
        backward-sexp
        upcase-word
        downcase-word
        capitalize-word
        forward-list
        backward-list
        hippie-expand
        hippie-expand-lines
        yank
        yank-pop
        append-next-kill
        kill-word
        kill-line
        kill-whole-line
        kill-region
        backward-kill-word
        backward-delete-char-untabify
        delete-char delete-forward-char
        delete-backward-char
        py-electric-backspace
        c-electric-backspace
        org-delete-backward-char
        cperl-electric-backspace
        python-indent-dedent-line-backspace
        paredit-backward-delete
        autopair-backspace
        just-one-space
        zap-to-char
        end-of-line
        set-mark-command
        exchange-point-and-mark
        cua-set-mark
        cua-replace-region
        cua-delete-region
        move-end-of-line
        beginning-of-line
        move-beginning-of-line
        kill-ring-save
        back-to-indentation
        subword-forward
        subword-backward
        subword-mark
        subword-kill
        subword-backward-kill
        subword-transpose
        subword-capitalize
        subword-upcase
        subword-downcase
        er/expand-region
        er/contract-region
        smart-forward
        smart-backward
        smart-up
        smart-down))

;; Execute following commands only for MAIN cursor.
(mapc #'(lambda (command)
          (put command 'multiple-cursors 'false))
      '(helix-normal-state  ;; ESC
        find-file-at-point  ;; gf
        browse-url-at-point ;; gx
        tab-next
        tab-previous
        save-buffer
        ido-exit-minibuffer
        ivy-done
        exit-minibuffer
        minibuffer-complete-and-exit
        eval-expression
        undo
        redo
        undo-tree-undo
        undo-tree-redo
        undo-fu-only-undo
        undo-fu-only-redo
        universal-argument
        universal-argument-more
        universal-argument-other-key
        negative-argument
        digit-argument
        top-level
        recenter-top-bottom
        describe-mode
        describe-key-1
        describe-function
        describe-bindings
        describe-prefix-bindings
        view-echo-area-messages
        other-window
        kill-buffer-and-window
        split-window-right
        split-window-below
        delete-other-windows
        toggle-window-split
        mwheel-scroll
        scroll-up-command
        scroll-down-command
        mouse-set-point
        mouse-drag-region
        quit-window
        toggle-read-only
        windmove-left
        windmove-right
        windmove-up
        windmove-down
        repeat-complex-command
        edebug-next-mode))

(provide 'helix-multiple-cursors-core)
;;; helix-multiple-cursors-core.el ends here
