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
      (-let (((mark . point) it))
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
  "Return list with overlapping overlays grouped together."
  (setq overlays (sort overlays #'helix--compare-by-overlay-start))
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

(defun helix-comment-at-point-p ()
  "Return non-nil if point is inside a comment, or comment starts
right after the point."
  (ignore-errors
    (save-excursion
      ;; We cannot be in a comment if we are inside a string
      (unless (nth 3 (syntax-ppss))
        (let ((pnt (point)))
          (or (nth 4 (syntax-ppss))
              ;; this also test opening and closing comment delimiters... we
              ;; need to check that it is not newline, which is in "comment
              ;; ender" class in elisp-mode, but we just want it to be treated
              ;; as whitespace
              (and (< pnt (point-max))
                   (memq (char-syntax (char-after pnt)) '(?< ?>))
                   (not (eq (char-after pnt) ?\n)))
              ;; we also need to test the special syntax flag for comment
              ;; starters and enders, because `syntax-ppss' does not yet know if
              ;; we are inside a comment or not (e.g. / can be a division or
              ;; comment starter...).
              (when-let ((s (car (syntax-after pnt))))
                (or (and (/= 0 (logand (ash 1 16) s))
                         (nth 4 (syntax-ppss (+ pnt 2))))
                    (and (/= 0 (logand (ash 1 17) s))
                         (nth 4 (syntax-ppss (+ pnt 1))))
                    (and (/= 0 (logand (ash 1 18) s))
                         (nth 4 (syntax-ppss (- pnt 1))))
                    (and (/= 0 (logand (ash 1 19) s))
                         (nth 4 (syntax-ppss (- pnt 2))))))))))))

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
