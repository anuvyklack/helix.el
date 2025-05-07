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

(defun overlay-live-p (overlay)
  (if-let* ((buffer (overlay-buffer overlay)))
      (buffer-live-p buffer)))

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
