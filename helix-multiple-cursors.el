;;; helix-multiple-cursors.el --- Multiple cursors for Helix -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Multiple cursors for Helix.
;;
;;; Code:

(require 'dash)
(require 'helix-common)
(require 'helix-multiple-cursors-core)
(require 'helix-vars)

;; (keymap-lookup nil "M-<down-mouse-1>")

(defun helix-fake-cursor-at-pos (pos)
  "Return the fake cursor at POS, or nil if no one."
  (-find #'(lambda (overlay)
             (eq pos (overlay-get overlay 'point)))
         (helix-all-fake-cursors pos (1+ pos))))

;; M-right mouse
(defun helix-toggle-cursor-on-click (event)
  "Add a cursor where you click, or remove a fake cursor that is
already there."
  (interactive "e")
  (mouse-minibuffer-check event)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (let ((position (event-end event)))
    (when (not (windowp (posn-window position)))
      (error "Position not in text area of window"))
    (select-window (posn-window position))
    (when-let* ((pos (posn-point position))
                ((numberp pos)))
      ;; Is there a fake cursor with the actual *point* right where we are?
      (if-let* ((cursor (helix-fake-cursor-at-pos pos)))
          (helix--remove-fake-cursor cursor)
        (save-excursion
          (goto-char pos)
          (helix-create-fake-cursor-at-point nil t))))
    (helix-maybe-multiple-cursors-mode)))

(defun helix-copy-cursor-down (&optional count)
  (or count (setq count 1))
  (let ((dir (if (< count 0) -1 1))
        (count (abs count)))
    (when (use-region-p)
      (let ((region-dir   (helix-region-direction))
            (num-of-lines (count-lines (point) (mark)))
            (point-column (current-column))
            (mark-column  (progn
                            (helix-exchange-point-and-mark)
                            (current-column))))

        ;; (dotimes (i count))
        ))
    )
  )

(defun helix-mc-furthest-cursor-before-point ()
  (let ((beg (if mark-active
                 (min (mark) (point))
               (point)))
        furthest)
    (helix-for-each-fake-cursor
     (when (< (mc/cursor-beg cursor) beg)
       (setq beg (mc/cursor-beg cursor))
       (setq furthest cursor)))
    furthest))

(defun helix-mc-furthest-cursor-after-point ()
  (let ((end (if mark-active (max (mark) (point)) (point)))
        furthest)
    (helix-for-each-fake-cursor
     (when (> (mc/cursor-end cursor) end)
       (setq end (mc/cursor-end cursor))
       (setq furthest cursor)))
    furthest))

(defun helix-mc-first-cursor ())
(defun helix-mc-last-cursor ())

(defun helix-mc-bound-cursor (direction)
  "Return the first fake cursor if DIRECTION is positive number /last
If DIRECTION is negative number find the fake cursor with the minimal buffer
position. If its position is smaller than the positon of the point — return it.
Otherwise return nil.

If DIRECTION is positive number find the fake cursor with the maximum buffer
position. If its position is bigger than the positon of the point — return it.
Otherwise return nil."
  )


;; (current-column)
;; (move-to-column)
;; (count-lines)

;; (helix-copy-cursor-forward)

(provide 'helix-multiple-cursors)
;;; helix-multiple-cursors.el ends here
