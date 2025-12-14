;;; hel-multiple-cursors-core.el --- Multiple cursors for Hel -*- lexical-binding: t; -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Authors: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/anuvyklack/hel
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; The core functionality for multiple cursors. The code is inspired by
;; `multiple-cursors.el' package from Magnar Sveen.
;;
;; How multiple cursors works internally. Command is firstly executed for
;; real cursor by Emacs command loop. Then in `post-command-hook' it executed
;; for all fake cursors. Fake cursor is an overlay that emulates cursor and
;; stores inside point, mark, kill-ring and some other variables (full list
;; is in `hel-fake-cursor-specific-vars'). Executing command for fake cursor
;; looks as follows: set point and mark to positions saved in fake cursor
;; overlay,restore all variables from it, execute command in this environment,
;; store point, mark and new state into fake cursor overlay.
;;
;; Each command should has `multiple-cursors' symbol property. If it is
;; `t' — command will be execute for all cursors. Any other value except
;; `nil' — it will be executed only once for real (main) cursor. If
;; `multiple-cursors' property is `nil' i.e. absent user will be prompted
;; how execute this command and choosen value is permanently stored in
;; `hel-whitelist-file' file.
;;
;; ID 0 is always corresponding to real cursor.

;;; Code:

(require 'dash)
(require 'cl-lib)
(require 'subr-x)
(require 'rect)
(require 'hel-common)

;;; Undo

(defun hel--single-undo-step-beginning ()
  "Initiate atomic undo step.
All following buffer modifications are grouped together as a single
action. The step is terminated with `hel--single-undo-step-end'."
  (unless (or hel--in-single-undo-step
              (hel-undo-command-p this-command)
              (eq buffer-undo-list t))
    (setq hel--in-single-undo-step t)
    (unless (null (car-safe buffer-undo-list))
      (undo-boundary))
    (setq hel--undo-list-pointer buffer-undo-list)
    (hel--push-undo-boundary-1)))

(defvar hel-insert-state)

(defun hel--single-undo-step-end ()
  "Finalize atomic undo step started by `hel--single-undo-step-beginning'."
  (when (and hel--in-single-undo-step
             ;; Merged all changes in Insert state into one undo step.
             (not hel-insert-state))
    (hel--push-undo-boundary-2)
    (unless (eq buffer-undo-list hel--undo-list-pointer)
      (let ((undo-list buffer-undo-list))
        (while (and (consp undo-list)
                    (eq (car undo-list) nil))
          (setq undo-list (cdr undo-list)))
        (let ((equiv (gethash (car undo-list)
                              undo-equiv-table)))
          ;; Remove undo boundaries (nil elements) from `buffer-undo-list'
          ;; withing current undo step. Also remove number entries -- they
          ;; move point during undo, and we handle cursors positions manually
          ;; to synchronize real cursor with fake ones.
          (setq undo-list (hel-destructive-filter
                           (lambda (i) (or (numberp i) (null i)))
                           undo-list
                           hel--undo-list-pointer))
          ;; Restore "undo" status of the tip of `buffer-undo-list'.
          (when equiv
            (puthash (car undo-list) equiv
                     undo-equiv-table)))
        (setq buffer-undo-list undo-list)))
    (setq hel--in-single-undo-step nil
          hel--undo-list-pointer nil)))

(defun hel--push-undo-boundary-1 ()
  (setq hel--undo-boundary
        `(apply hel--undo-step-end ,(hel-cursors-positions)))
  (push hel--undo-boundary buffer-undo-list))

(defun hel--push-undo-boundary-2 ()
  (when hel--undo-boundary
    (let ((undo-list buffer-undo-list))
      (while (and (consp undo-list)
                  (eq (car undo-list) nil))
        (pop undo-list))
      (if (equal (car undo-list) hel--undo-boundary)
          (pop undo-list)
        ;; else
        (push `(apply hel--undo-step-start ,(hel-cursors-positions))
              undo-list))
      (setq hel--undo-boundary nil
            buffer-undo-list undo-list))))

(defun hel--undo-step-start (cursors-positions)
  "This function always called from `buffer-undo-list' during undo by
`primitive-undo' function. It is the first one from a pair of functions:
`hel--undo-step-start' and `hel--undo-step-end', which are executed
at beginning and end of a single undo step and restores real and fake
cursors positions and regions after undo/redo step.

CURSORS-POSITIONS is an alist returned by `hel-cursors-positions' function."
  (push `(apply hel--undo-step-end ,cursors-positions)
        buffer-undo-list))

(defun hel--undo-step-end (&optional cursors-positions)
  "This function always called from `buffer-undo-list' during undo by
`primitive-undo' function. It is the second one from a pair of functions:
`hel--undo-step-start' and `hel--undo-step-end', which are executed
at beginning and end of a single undo step and restores real and fake
cursors positions and regions after undo/redo step.

CURSORS-POSITIONS is an alist returned by `hel-cursors-positions' function."
  (maphash (lambda (id cursor)
             (unless (assoc id cursors-positions #'eql)
               (hel--delete-fake-cursor cursor)))
           hel--cursors-table)
  (dolist (val cursors-positions)
    (-let [(id point mark newline-at-eol) val]
      (pcase id
        (0 (hel-set-region mark point nil newline-at-eol))
        (_ (let ((mark-active (not (null mark)))
                 (hel--newline-at-eol newline-at-eol))
             (if-let* ((cursor (gethash id hel--cursors-table)))
                 (hel-move-fake-cursor cursor point mark :update)
               (hel--create-fake-cursor-1 id point mark)))))))
  (hel-auto-multiple-cursors-mode)
  (push `(apply hel--undo-step-start ,cursors-positions)
        buffer-undo-list))

;;; Fake cursor object

(defun hel--new-fake-cursor-id ()
  "Return new unique cursor id.
IDs' are used to keep track of cursors for undo."
  (cl-incf hel--fake-cursor-last-used-id))

(defvar hel--max-cursors-original nil
  "This variable maintains the original maximum number of cursors.
When `hel-create-fake-cursor' is called and `hel-max-cursors-number' is
overridden, this value serves as a backup so that `hel-max-cursors-number'
can take on a new value. When `hel--delete-all-fake-cursors' is called,
the values are reset.")

(defun hel-create-fake-cursor (point &optional mark id)
  "Create a fake cursor at POINT position.
If MARK is passed a fake active region overlay between POINT
and MARK will be created.
The ID, if specified, will be assigned to the new cursor.
Otherwise, the new unique ID will be created.
The current state is stored in the overlay for later retrieval."
  (unless hel--max-cursors-original
    (setq hel--max-cursors-original hel-max-cursors-number))
  (when hel-max-cursors-number
    (when-let* ((num (hel-number-of-cursors))
                ((<= hel-max-cursors-number num)))
      (if (yes-or-no-p (format "%d active cursors. Continue? " num))
          (setq hel-max-cursors-number (read-number "Enter a new, temporary maximum: "))
        (hel--delete-all-fake-cursors)
        (error "Aborted: too many cursors"))))
  (prog1 (hel--create-fake-cursor-1 id point mark)
    (unless hel-multiple-cursors-mode
      (hel-multiple-cursors-mode 1))))

(defun hel--create-fake-cursor-1 (id point mark)
  "Create a fake cursor with ID at POINT and fake region between POINT and MARK.
This function is the guts of the `hel-create-fake-cursor'."
  (or id (setq id (hel--new-fake-cursor-id)))
  (save-excursion
    (goto-char point)
    (let ((cursor (hel--set-cursor-overlay nil point)))
      (overlay-put cursor 'id id)
      (overlay-put cursor 'type 'fake-cursor)
      (overlay-put cursor 'priority 100)
      (hel--store-cursor-state cursor point mark)
      (hel--set-fake-region-overlay cursor)
      (puthash id cursor hel--cursors-table)
      cursor)))

(defun hel--delete-all-fake-cursors ()
  "Remove all fake cursors overlays form current buffer.
It is likely that you need `hel-delete-all-fake-cursors' function,
not this one."
  (when hel--max-cursors-original
    (setq hel-max-cursors-number hel--max-cursors-original
          hel--max-cursors-original nil))
  (mapc #'hel--delete-fake-cursor (hel-all-fake-cursors)))

(defun hel-create-fake-cursor-from-point (&optional id)
  "Create a fake cursor with an optional fake region based on point and mark.
Assign the ID to the new fake cursor, if specified.
The current state is stored in it for later retrieval."
  (hel-create-fake-cursor (point) (mark t) id))

(defun hel-move-fake-cursor (cursor point &optional mark update)
  "Move fake CURSOR to new POINT.
If MARK is non-nil also set fake region.
Move fake CURSOR and its region according to new POINT and MARK.
Optionally UPDATE fake-cursors state."
  (set-marker (overlay-get cursor 'point) point)
  (set-marker (overlay-get cursor 'mark) (or mark point))
  (when update (hel-update-fake-cursor-state cursor))
  (hel--set-cursor-overlay cursor point)
  (hel--set-fake-region-overlay cursor)
  cursor)

(defun hel-delete-fake-cursor (cursor)
  "Delete fake CURSOR and disable `hel-multiple-cursors-mode' if no
more fake cursors are remaining."
  (hel--delete-fake-cursor cursor)
  (hel-auto-multiple-cursors-mode))

(defun hel--set-cursor-overlay (cursor pos)
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
    (let ((face (cond (hel--extend-selection
                       'hel-extend-selection-cursor)
                      (hel-insert-state
                       'hel-insert-state-fake-cursor)
                      (t
                       'hel-normal-state-fake-cursor))))
      (cond ((and hel-match-fake-cursor-style
                  (hel-cursor-is-bar-p))
             (overlay-put cursor 'face nil)
             (overlay-put cursor 'before-string (propertize hel-bar-fake-cursor 'face face))
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

(defun hel--set-fake-region-overlay (cursor)
  "For fake CURSOR setup the overlay looking like active region when appropriate."
  (let ((beg (overlay-get cursor 'point))
        (end (overlay-get cursor 'mark))
        (newline-at-eol? (overlay-get cursor 'hel--newline-at-eol)))
    (if (and (overlay-get cursor 'mark-active)
             (or (/= beg end) newline-at-eol?))
        (progn
          (when newline-at-eol?
            (when (< end beg) (cl-rotatef beg end))
            (cl-incf end))
          (if-let ((region (overlay-get cursor 'fake-region)))
              (move-overlay region beg end)
            ;; else
            (setq region (-doto (make-overlay beg end nil nil t)
                           (overlay-put 'face 'region)
                           (overlay-put 'type 'fake-region)
                           (overlay-put 'id (overlay-get cursor 'id))
                           (overlay-put 'priority 1)))
            (overlay-put cursor 'fake-region region)))
      ;; else
      (hel--delete-fake-region-overlay cursor))))

(defun hel--delete-fake-cursor (cursor)
  "Delete CURSOR overlay."
  (remhash (overlay-get cursor 'id) hel--cursors-table)
  (set-marker (overlay-get cursor 'point) nil)
  (set-marker (overlay-get cursor 'mark) nil)
  (hel--delete-fake-region-overlay cursor)
  (delete-overlay cursor))

(defun hel--delete-fake-region-overlay (cursor)
  "Remove the dependent region overlay for a given CURSOR overlay."
  (-some-> (overlay-get cursor 'fake-region)
    (delete-overlay)))

(defun hel--store-cursor-state (overlay point mark)
  "Store POINT, MARK and variables relevant to fake cursor into OVERLAY."
  (or mark (setq mark point))
  (-if-let (pnt (overlay-get overlay 'point))
      (set-marker pnt point)
    (overlay-put overlay 'point (copy-marker point t)))
  (-if-let (mrk (overlay-get overlay 'mark))
      (set-marker mrk mark)
    (overlay-put overlay 'mark (copy-marker mark)))
  (dolist (var hel-fake-cursor-specific-vars)
    (if (boundp var)
        (overlay-put overlay var (symbol-value var))))
  overlay)

(defun hel-update-fake-cursor-state (cursor)
  "Update variables stored in fake CURSOR."
  (dolist (var hel-fake-cursor-specific-vars)
    (if (boundp var)
        (overlay-put cursor var (symbol-value var)))))

(defun hel-restore-point-from-fake-cursor (cursor)
  "Restore point, mark and variables from fake CURSOR overlay and delete it."
  (hel--restore-cursor-state cursor)
  (hel--delete-fake-cursor cursor)
  (if hel--newline-at-eol
      (hel--set-region-overlay (region-beginning) (1+ (region-end)))
    (hel--delete-region-overlay)))

(defun hel--restore-cursor-state (overlay)
  "Restore point, mark and cursor variables saved in OVERLAY."
  (goto-char (overlay-get overlay 'point))
  (set-marker (mark-marker) (overlay-get overlay 'mark))
  (dolist (var hel-fake-cursor-specific-vars)
    (if (boundp var)
        (set var (overlay-get overlay var))))
  (hel--delete-fake-region-overlay overlay)
  (delete-overlay overlay))

(defun hel-hide-fake-cursor (cursor)
  "Disable the fake-CURSOR visibility in the buffer without deleting it."
  (hel--delete-fake-region-overlay cursor)
  (delete-overlay cursor)
  cursor)

(defun hel-show-fake-cursor (cursor)
  "Restore fake-CURSOR visibility if it was previously hidden with
`hel-hide-fake-cursor'."
  (let ((point (overlay-get cursor 'point))
        (mark (overlay-get cursor 'mark)))
    (hel--set-cursor-overlay cursor point)
    (hel--set-fake-region-overlay cursor)))

(defun hel-fake-cursor-p (overlay)
  "Return t if an OVERLAY is a fake cursor."
  (eq (overlay-get overlay 'type) 'fake-cursor))

(defun hel-fake-region-p (overlay)
  "Return t if an OVERLAY is a fake region."
  (eq (overlay-get overlay 'type) 'fake-region))

;;; Access fake cursors

(defun hel-all-fake-cursors (&optional sort)
  "Return list with all fake cursors in current buffer.
If SORT is non-nil sort cursors in order they are located in buffer."
  (let ((cursors (hash-table-values hel--cursors-table)))
    (if sort
        (sort cursors (lambda (c1 c2)
                        (< (overlay-get c1 'point)
                           (overlay-get c2 'point))))
      cursors)))

(defun hel-fake-cursors-in (start end)
  "Return list of fake cursors within START...END buffer positions."
  (-filter #'hel-fake-cursor-p
           (overlays-in start end)))

(defun hel-cursor-with-id (id)
  "Return the cursor with the given ID if it is stil alive."
  (if-let* ((cursor (gethash id hel--cursors-table))
            ((hel-overlay-live-p cursor)))
      cursor))

(defun hel-fake-cursor-at (position)
  "Return the fake cursor at POSITION, or nil if no one."
  (--find (= position (overlay-get it 'point))
          (hel-fake-cursors-in position (1+ position))))

(defun hel-next-fake-cursor (&optional position)
  "Return the next fake cursor after the POSITION."
  ;; (unless position (setq position (point)))
  (cl-loop for pos = (next-overlay-change position)
           then (next-overlay-change pos)
           until (eql pos (point-max))
           thereis (hel-fake-cursor-at pos)))

(defun hel-previous-fake-cursor (position)
  "Return the first fake cursor before the POSITION."
  (cl-loop for pos = (previous-overlay-change position)
           then (previous-overlay-change pos)
           until (eql pos (point-min))
           thereis (hel-fake-cursor-at pos)))

(defun hel-first-fake-cursor ()
  "Return the first fake cursor in the buffer."
  (-min-by (lambda (a b)
             (> (overlay-get a 'point)
                (overlay-get b 'point)))
           (hel-all-fake-cursors)))

(defun hel-last-fake-cursor ()
  "Return the last fake cursor in the buffer."
  (-max-by (lambda (a b)
             (> (overlay-get a 'point)
                (overlay-get b 'point)))
           (hel-all-fake-cursors)))

(defun hel-number-of-cursors ()
  "The number of cursors (real and fake) in the buffer."
  (1+ (hash-table-count hel--cursors-table)))

(defun hel-any-fake-cursors-p ()
  "Return non-nil if there are fake cursors in the buffer."
  (not (hash-table-empty-p hel--cursors-table)))

(defun hel-cursors-positions ()
  "Return alist with positions data of all cursors.
Alist containes cons cells:

    (ID . (POINT MARK NEWLINE-AT-EOL?))

NEWLINE-AT-EOL? is the cursors value of the `hel--newline-at-eol' variable.
MARK is nil if cursor has no region.

Real cursor has ID 0 and is the first element (car) of the list."
  (let (alist)
    (when hel-multiple-cursors-mode
      (dolist (cursor (hel-all-fake-cursors))
        (push (list (overlay-get cursor 'id) ;; id
                    (marker-position (overlay-get cursor 'point)) ;; point
                    (if (overlay-get cursor 'mark-active)
                        (marker-position (overlay-get cursor 'mark))) ;; mark
                    (overlay-get cursor 'hel--newline-at-eol))
              alist)))
    (push (list 0 ;; id
                (point)
                (if mark-active (mark))
                hel--newline-at-eol)
          alist)
    alist))

;;; Executing commands for real and fake cursors

(defmacro hel-save-window-scroll (&rest body)
  "Save the window scroll position, evaluate BODY, restore it."
  (declare (indent 0) (debug t))
  (let ((win-start (make-symbol "win-start"))
        (win-hscroll (make-symbol "win-hscroll")))
    `(let ((,win-start (copy-marker (window-start)))
           (,win-hscroll (window-hscroll)))
       ,@body
       (set-window-start nil ,win-start :noforce)
       (set-window-hscroll nil ,win-hscroll)
       (set-marker ,win-start nil))))

(defmacro hel-save-excursion (&rest body)
  "Like `save-excursion' but additionally save and restore all
the data needed for multiple cursors functionality."
  (declare (indent 0) (debug t))
  (let ((state (make-symbol "point-state")))
    `(let ((,state (hel--conserve-main-cursor-state)))
       (save-excursion ,@body)
       (hel--restore-main-cursor-state ,state))))

(defun hel--conserve-main-cursor-state ()
  (let ((state (list :point (copy-marker (point) t)
                     :mark (copy-marker (mark-marker)))))
    (dolist (var hel-fake-cursor-specific-vars)
      (if (boundp var)
          (cl-callf plist-put state var (symbol-value var))))
    (hel--delete-region-overlay)
    state))

(defun hel--restore-main-cursor-state (state)
  (goto-char (let ((pnt (plist-get state :point)))
               (prog1 (marker-position pnt)
                 (set-marker pnt nil))))
  (set-marker (mark-marker)
              (let ((mrk (plist-get state :mark)))
                (prog1 (marker-position mrk)
                  (set-marker mrk nil))))
  (dolist (var hel-fake-cursor-specific-vars)
    (if (boundp var)
        (set var (plist-get state var))))
  (if (and hel--newline-at-eol mark-active)
      (hel--set-region-overlay (region-beginning) (1+ (region-end)))
    (hel--delete-region-overlay)))

(defmacro hel-with-fake-cursor (cursor &rest body)
  "Move point to the fake CURSOR, restore the environment from it,
evaluate BODY, update fake CURSOR."
  (declare (indent 1) (debug (symbolp &rest form)))
  `(let ((hel-executing-command-for-fake-cursor t))
     (hel--restore-cursor-state ,cursor)
     (unwind-protect
         (progn ,@body)
       (hel-move-fake-cursor ,cursor (point) (mark t) :update))))

(defmacro hel-with-each-cursor (&rest body)
  "Evaluate BODY for all cursors: real and fake ones."
  (declare (indent 0) (debug t))
  ;; First collect fake cursors because BODY can create new cursors,
  ;; and we want it to be executed only for original ones.
  `(let ((cursors (if hel-multiple-cursors-mode
                      (hel-all-fake-cursors))))
     ;; Main cursor
     ,@body
     ;; Fake cursors
     (when cursors
       (hel-save-window-scroll
         (hel-save-excursion
           (dolist (cursor cursors)
             (hel-with-fake-cursor cursor
               ,@body)))))))

(defun hel-execute-command-for-all-cursors (command)
  "Call COMMAND interactively for all cursors: real and fake ones."
  (hel--call-interactively command)
  (hel--execute-command-for-all-fake-cursors command)
  (when (hel-merge-regions-p command)
    (hel-merge-overlapping-regions))
  (setq hel--input-cache nil))

(defun hel--execute-command-for-all-fake-cursors (command)
  "Call COMMAND interactively for each fake cursor."
  (when hel-multiple-cursors-mode
    (cond ((and (symbolp command)
                (get command 'hel-unsupported))
           (message "%S is not supported with multiple cursors" command))
          ((or
            ;; If it's a lambda, we can't know if it's supported or not -
            ;; so go ahead and assume it's ok.
            (not (symbolp command))
            (pcase (get command 'multiple-cursors)
              ('t t)
              ('nil (hel--prompt-for-unknown-command command))))
           (hel-save-window-scroll
             (hel-save-excursion
               (dolist (cursor (hel-all-fake-cursors))
                 (hel-with-fake-cursor cursor
                   (hel--call-interactively command)))))))))

(defun hel--call-interactively (command)
  "Run COMMAND, simulating the parts of the command loop that
makes sense for fake cursor."
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

(defmacro hel-with-real-cursor-as-fake (&rest body)
  "Temporarily convert real cursor into fake-cursor one with ID 0.
Restore it after BODY evaluation if it is still alive."
  (declare (indent 0) (debug t))
  (let ((real-cursor (make-symbol "real-cursor")))
    `(let ((,real-cursor (hel--create-fake-cursor-1 0 (point) (mark t))))
       (hel--delete-region-overlay)
       (unwind-protect (progn ,@body)
         (cond ((hel-overlay-live-p ,real-cursor)
                (hel-restore-point-from-fake-cursor ,real-cursor))
               ((hel-any-fake-cursors-p)
                (hel-restore-point-from-fake-cursor (hel-first-fake-cursor))))
         (hel-auto-multiple-cursors-mode)
         (hel-update-cursor)))))

;;; Multiple cursors minor mode

(define-minor-mode hel-multiple-cursors-mode
  "Minor mode, which is active when there are multiple cursors in the buffer.
No need to activate it manually: it is activated automatically when you create
first fake cursor with `hel-create-fake-cursor', and disabled when you
delete last one with `hel-delete-fake-cursor'."
  :global nil
  :interactive nil
  :keymap (make-sparse-keymap)
  (if hel-multiple-cursors-mode
      (hel--disable-minor-modes-incompatible-with-multiple-cursors)
    (hel--delete-all-fake-cursors)
    (hel--enable-minor-modes-incompatible-with-multiple-cursors)))

(defun hel-auto-multiple-cursors-mode ()
  "Enable `hel-multiple-cursors' if there are multiple cursors,
disable if only one."
  (when (xor hel-multiple-cursors-mode
             (hel-any-fake-cursors-p))
    (hel-multiple-cursors-mode 'toggle)))

(defun hel--disable-minor-modes-incompatible-with-multiple-cursors ()
  "Disable incompatible minor modes while there are multiple cursors
in the buffer."
  (dolist (mode hel-minor-modes-incompatible-with-multiple-cursors)
    (when (and (boundp mode) (symbol-value mode))
      (push mode hel--temporarily-disabled-minor-modes)
      (funcall mode -1))))

(defun hel--enable-minor-modes-incompatible-with-multiple-cursors ()
  "Enable minor modes disabled by
`hel--disable-minor-modes-incompatible-with-multiple-cursors'."
  (when hel--temporarily-disabled-minor-modes
    (dolist (mode hel--temporarily-disabled-minor-modes)
      (funcall mode 1))
    (setq hel--temporarily-disabled-minor-modes nil)))

(defun hel-multiple-cursors--indicator ()
  (when hel-multiple-cursors-mode
    (format hel-multiple-cursors-mode-line-indicator
            (hel-number-of-cursors))))

;;; Whitelists

(defun hel--prompt-for-unknown-command (command)
  "Ask the user whether the COMMAND should be executed for all cursors or not,
and remember the choice.

Return t if COMMMAND should be executed for all cursors."
  (let ((for-all? (ignore-error quit ;; treat `C-g' as answer "no"
                    (y-or-n-p (format "Do %S for all cursors?" command)))))
    (if for-all?
        (progn
          (put command 'multiple-cursors t)
          (push command hel-commands-to-run-for-all-cursors))
      ;; else
      (put command 'multiple-cursors 'false)
      (push command hel-commands-to-run-once))
    (hel-save-whitelists-into-file)
    for-all?))

(defun hel-load-whitelists ()
  "Load `hel-whitelist-file' file if not yet."
  (unless hel--whitelist-file-loaded
    (load hel-whitelist-file 'noerror 'nomessage)
    (setq hel--whitelist-file-loaded t)
    (mapc (lambda (command)
            (put command 'multiple-cursors t))
          hel-commands-to-run-for-all-cursors)
    (mapc (lambda (command)
            (put command 'multiple-cursors 'false))
          hel-commands-to-run-once)))

(defun hel--dump-whitelist (list-symbol)
  "Insert (setq \\='LIST-SYMBOL LIST-VALUE) into current buffer."
  (cl-symbol-macrolet ((value (symbol-value list-symbol)))
    (insert "(setq " (symbol-name list-symbol) "\n"
            "      '(")
    (newline-and-indent)
    (set list-symbol (-> value
                         (sort (lambda (x y)
                                 (string-lessp (symbol-name x)
                                               (symbol-name y))))))
    (mapc (lambda (cmd)
            (insert (format "%S" cmd))
            (newline-and-indent))
          value)
    (insert "))")
    (newline)))

(defun hel-save-whitelists-into-file ()
  "Save users preferences which commands to execute for one cursor
and which for all to `hel-whitelist-file' file."
  (with-temp-file hel-whitelist-file
    (emacs-lisp-mode)
    (insert ";; -*- mode: emacs-lisp; lexical-binding: t -*-")
    (newline)
    (insert ";; This file is automatically generated by Hel.")
    (newline)
    (insert ";; It keeps track of your preferences for running commands with multiple cursors.")
    (newline)
    (newline)
    (hel--dump-whitelist 'hel-commands-to-run-for-all-cursors)
    (newline)
    (hel--dump-whitelist 'hel-commands-to-run-once)))

;;; Merge overlapping regions

(defun hel-merge-regions-p (command)
  "Return non-nil if regions need to be merged after COMMAND."
  (and hel-multiple-cursors-mode
       mark-active
       (cond ((symbolp command)
              (pcase (get command 'merge-selections)
                ('extend-selection hel--extend-selection)
                (val val)))
             ((functionp command) ;; COMMAND is a lambda
              t))))

(defun hel-merge-overlapping-regions ()
  "Merge overlapping regions."
  (let ((dir (hel-region-direction)))
    (dolist (group-or-overlapping-regions (hel--overlapping-regions))
      (let ((beg (point-max))
            (end (point-min))
            id delete real-cursor?)
        (dolist (val group-or-overlapping-regions)
          ;; rid - region ID, b - region beginning, e - region end
          (-let [(rid b e) val]
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
            (_ (when-let* ((cursor (gethash id hel--cursors-table)))
                 (cond (real-cursor?
                        (hel-restore-point-from-fake-cursor cursor)
                        (goto-char pnt)
                        (set-marker (mark-marker) mrk))
                       (t
                        (hel-move-fake-cursor cursor pnt mrk)))))))
        (dolist (id delete)
          (when-let* ((cursor (gethash id hel--cursors-table)))
            (hel--delete-fake-cursor cursor)))))
    (hel-auto-multiple-cursors-mode)))

(defun hel--overlapping-regions ()
  "Return the list of groups, where each group is a list of
cons cells (ID . (START END)) denoting fake cursor ID and its
region bounds. Inside each group, all regions are overlapping
and sorted by starting position. ID 0 coresponds to the real
cursor."
  (let ((alist (hel--regions-ranges))
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

(defun hel--regions-ranges ()
  "Return the alist with cons cells (ID . (START END)).
\(START END) are bounds of regions. Alist is sorted by START.
ID 0 coresponds to the real cursor."
  (let* ((alist (cons
                 ;; Append real cursor with ID 0
                 `(0 ,(region-beginning) ,(region-end))
                 (mapcar (lambda (cursor)
                           (let* ((id  (overlay-get cursor 'id))
                                  (pnt (overlay-get cursor 'point))
                                  (mrk (overlay-get cursor 'mark))
                                  (start (min pnt mrk))
                                  (end   (max pnt mrk)))
                             `(,id ,start ,end)))
                         (hel-all-fake-cursors)))))
    (sort alist (lambda (a b)
                  (< (-second-item a) (-second-item b))))))

;;; Integration with other Emacs functionalities

(defmacro hel-cache-input (fn-name)
  "Advice function to cache users input to use it with all cursors.

This macro wraps functions in around advice that caches the user's
response so it can be reused across all cursors.

FN-NAME must be an interactive function that takes PROMPT as its first argument,
like `read-char' or `read-from-minibuffer'. The PROMPT argument will be used as
a hash key to distinguish between different calls to FN-NAME within the same
command. Calls with equal PROMPT or without it would be indistinguishable."
  `(hel-define-advice ,fn-name (:around (orig-fun &rest args) hel-cache-input)
     "Cache the users input to use it with multiple cursors."
     (if hel-multiple-cursors-mode
         (let* (;; Use PROMPT argument as a hash key to distinguish different
                ;; calls of `read-char' like functions within one command.
                (prompt (car-safe args))
                (key (list ,(symbol-name fn-name) prompt)))
           (with-memoization (alist-get key hel--input-cache nil nil #'equal)
             (apply orig-fun args)))
       ;; else
       (apply orig-fun args))))

(defmacro hel-unsupported-command (command)
  "Adds command to list of unsupported commands and prevents it
from being executed when `hel-multiple-cursors-mode' is active."
  `(progn
     (put ',command 'hel-unsupported t)
     (hel-define-advice ,command (:around (orig-fun &rest args)
                                          hel-unsupported)
       "Don't execute an unsupported command while multiple cursors are active."
       (unless (and hel-multiple-cursors-mode
                    (called-interactively-p 'any))
         (apply orig-fun args)))))

;; Execute following commands for ALL cursor.
(mapc (lambda (command)
        (put command 'multiple-cursors t))
      '(keyboard-quit
        comment-dwim         ;; gc
        fill-region          ;; gq
        indent-region        ;; =
        indent-rigidly-left  ;; >
        indent-rigidly-right ;; <
        self-insert-command
        quoted-insert
        next-line
        previous-line
        newline
        newline-and-indent
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
        yank
        yank-pop
        append-next-kill
        kill-word
        kill-line
        kill-whole-line
        kill-region
        backward-kill-word
        backward-delete-char-untabify
        delete-char
        delete-forward-char
        delete-backward-char
        just-one-space
        zap-to-char
        end-of-line
        set-mark-command
        exchange-point-and-mark
        move-end-of-line
        beginning-of-line
        move-beginning-of-line
        kill-ring-save
        back-to-indentation))

;; Execute following commands only for MAIN cursor.
(mapc (lambda (command)
        (put command 'multiple-cursors 'false))
      '(hel-normal-state  ;; ESC
        find-file-at-point  ;; gf
        browse-url-at-point ;; gx
        save-buffer
        exit-minibuffer
        minibuffer-complete-and-exit
        eval-expression
        undo
        undo-redo
        undo-tree-undo
        undo-tree-redo
        undo-fu-only-undo
        undo-fu-only-redo
        universal-argument
        universal-argument-more
        negative-argument
        digit-argument
        tab-next
        tab-previous
        tab-bar-mouse-down-1
        top-level
        describe-mode
        describe-function
        describe-bindings
        describe-prefix-bindings
        view-echo-area-messages
        other-window
        kill-buffer-and-window
        split-window-right
        split-window-below
        delete-other-windows
        mwheel-scroll
        scroll-up-command
        scroll-down-command
        mouse-set-point
        mouse-drag-region
        quit-window
        windmove-left
        windmove-right
        windmove-up
        windmove-down
        repeat-complex-command
        edebug-next-mode
        undefined))

(provide 'hel-multiple-cursors-core)
;;; hel-multiple-cursors-core.el ends here
