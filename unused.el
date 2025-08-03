;;; -*- lexical-binding: t; -*-

(defmacro helix-define-motion (motion args &rest body)
  "Macros to define Helix motions.

\(fn MOTION (ARGS...) DOC BODY...)"
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-list
                           [&optional stringp]
                           def-body)))
  ;; collect docstring
  (let ((doc (when (< 1 (length body))
               (pcase (car body)
                 ((and 'format f)
                  (apply (car f) (cdr f)))
                 ((and (pred stringp) doc)
                  doc)))))
    (when doc (pop body))
    ;; macro expansion
    `(progn
       ;; the compiler does not recognize `defun' inside `let'
       ,(when (and motion body)
          `(defun ,motion (,@args)
             ,(or doc "")
             ,@body))
       (when ',motion
         (cl-pushnew ',motion helix--motion-command)
         (eval-after-load 'eldoc
           '(and (fboundp 'eldoc-add-command)
                 (eldoc-add-command ',motion)))))))

(defun helix-create-fake-cursors (regions)
  "Create set of fake active REGIONS.
REGIONS should be a list of cons cells (START . END) with bounds of regions."
  (when regions
    (--each regions
      (-let [(mark . point) it]
        (helix-create-fake-cursor point mark)))))

(defun helix-refresh-fake-cursor (cursor)
  (let ((pnt (overlay-get cursor 'point))
        (mrk (overlay-get cursor 'mark)))
    (helix-move-fake-cursor cursor pnt mrk)))

(defun helix--set-cursor-face (cursor &optional face)
  (unless face (setq face 'helix-fake-cursor))
  (cond ((overlay-get cursor 'after-string)
         (overlay-put cursor 'after-string (propertize " " 'face face)))
        (t
         (overlay-put cursor 'face face))))

(defvar helix-undo-commands '(helix-undo helix-redo undo-redo undo)
  "Commands that implementing undo/redo functionality.")

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

(defun helix-split-list-at-nil (lst)
  "Destructively split list
(1 2 3 4 nil 5 6) => ((1 2 3 4) (5 6)) "
  (let ((head lst)
        (it lst))
    (while (cadr it)
      (setq it (cdr it)))
    (setq lst (cddr it))
    (setcdr it nil)
    (list head lst)))

(defun helix-split-list-at-pointer (list pointer)
  "Destructively split LIST at POINTER.
(1 2 3 4 5) => ((1 2 3) (4 5))
       ^                 ^
       pointer           pointer

I.e.:
(1 . (2 . (3 . (4 5)))) => ((1 . (2 . (3 . nil))) (4 5))
                ^                                  ^
                pointer                            pointer"
  (let (head)
    (while (not (eq list pointer))
      (push (pop list) head))
    (setq head (nreverse head))
    (list head list)))

;; BUG: Hangs the Emacs
(defun helix-split-list-at-pointer-2 (list pointer)
  "Destructively split LIST at POINTER.
(1 2 3 4 5) => ((1 2 3) (4 5))
 ^     ^         ^       ^
 list  pointer   list    pointer

I.e.:
(1 . (2 . (3 . (4 5)))) => ((1 . (2 . (3 . nil))) (4 5))
 ^              ^            ^                     ^
 list           pointer      list                  pointer"
  (let ((it list))
    (while (not (eq (cdr it) pointer))
      (setq it (cdr it)))
    (setcdr it nil)
    (list list pointer)))

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
      (-each rest-cursors #'helix--delete-fake-cursor)
      ;; Swap BEG and END if backward direction.
      (when (< dir 0)
        (pcase-setq `(,beg . ,end) (cons end beg)))
      (cond ((eql id 0) ;; ID 0 denotes real cursors
             (helix-delete-fake-cursor cursor)
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
  "Return list with overlapping overlays grouped together."
  (setq overlays (sort overlays #'(lambda (o1 o2)
                                    (< (overlay-start o1)
                                       (overlay-start o2)))))
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

(defun helix-skip-gap (thing &optional direction)
  (unless direction (setq direction 1))
  (when-let* ((bounds (helix-bounds-of-complement-of-thing-at-point thing)))
    (goto-char (if (< direction 0)
                   (car bounds)
                 (cdr bounds)))))

(defmacro helix-save-goal-column (&rest body)
  "Restore the goal column after execution of BODY."
  (declare (indent defun) (debug t))
  `(let ((goal-column goal-column)
         (temporary-goal-column temporary-goal-column))
     ,@body))

(defun helix-skip-empty-lines (&optional direction)
  "Skip all empty lines toward direction.
If DIR is positive number move forward, else — backward."
  ;; (prog1
  ;;     (helix-skip-chars "\r\n" (or dir 1))
  ;;   (when (not helix-select-state-minor-mode)
  ;;     (set-mark (point))))
  (unless direction (setq direction 1))
  (let ((point-moved (helix-skip-chars "\r\n" direction)))
    (when (and point-moved
               (not helix-select-state-minor-mode))
      (set-mark (point)))
    point-moved))

(defun helix-state ()
  "Return current Helix state."
  helix-state)

(defun helix-set-cursor-type (type)
  "Set cursor TYPE."
  (if (display-graphic-p)
      (setq cursor-type type)
    (let* ((type (or (car-safe type) type))
           (code (pcase type
                   ('bar "6")
                   ('hbar "4")
                   (_ "2"))))
      (send-string-to-terminal (concat "\e[" code " q")))))

(defun helix-set-cursor-color (color)
  "Set the cursor color to COLOR."
  ;; `set-cursor-color' forces a redisplay, so only
  ;; call it when the color actually changes.
  (unless (equal (frame-parameter nil 'cursor-color) color)
    (set-cursor-color color)))

;;; Paste

(defun helix-copy-line ()
  "Copy selection as line into `kill-ring'."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (text (filter-buffer-substring beg end))
         (yank-handler (list #'helix-yank-line-handler)))
    ;; Ensure the text ends with a newline. This is required
    ;; if the deleted lines were the last lines in the buffer.
    (when (or (zerop (length text))
              (/= (aref text (1- (length text))) ?\n))
      (setq text (concat text "\n")))
    (put-text-property 0 (length text) 'yank-handler yank-handler text)
    (kill-new text)))

(defun helix-yank-line-handler (text)
  "Insert the TEXT linewise."
  (pcase helix-this-command ;; real-this-command
    ('helix-paste-before (helix-beginning-of-line)
                         (set-marker (mark-marker) (point))
                         (insert text))
    ((and 'helix-paste-after
          (guard (not (helix-linewise-selection-p))))
     (helix-end-of-line)
     (insert "\n")
     (set-mark (point))
     (insert text)
     (delete-char -1)) ; delete the last newline
    (_ (insert text))))

(helix-define-advice yank (:around (orig-fun &rest args))
  "Correctly set region after paste."
  (let ((old-point (point))
        (old-mark (or (mark t) (point)))
        (deactivate-mark nil))
    (apply orig-fun args)
    (when (= (mark-marker) old-mark)
      (set-mark old-point))))
