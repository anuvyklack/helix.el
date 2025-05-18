;;; -*- lexical-binding: t; -*-

(defun helix-refresh-fake-cursor (cursor)
  (let ((pnt (overlay-get cursor 'point))
        (mrk (overlay-get cursor 'mark)))
    (helix-move-fake-cursor cursor pnt mrk)))

(defvar helix-undo-commands '(helix-undo helix-redo undo-redo undo)
  "Commands that implementing undo/redo functionality.")

(defun helix-undo-command-p (command)
  "Return non-nil if COMMAND is implementing undo/redo functionality."
  (memq command helix-undo-commands))

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
