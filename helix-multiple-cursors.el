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

(defun helix-mc-repeat-command ()
  "Run last command from variable `command-history' for every fake cursor."
  (interactive)
  (when (or helix-mc-always-repeat-command
            (y-or-n-p (format "[mc] repeat complex command: %s? " (caar command-history))))
    (helix-execute-command-for-all-fake-cursors
     #'(lambda () (interactive)
         (cl-letf (((symbol-function 'read-from-minibuffer)
                    (lambda (p &optional i k r h d m) (read i))))
           (repeat-complex-command 0))))))

;; (keymap-lookup nil "M-<down-mouse-1>")

;; M-right mouse
(defun helix-toggle-cursor-on-click (event)
  "Add a cursor where you click, or remove a fake cursor that is
already there."
  (interactive "e")
  (mouse-minibuffer-check event)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (let ((position (event-end event)))
    (unless (windowp (posn-window position))
      (error "Position not in text area of window"))
    (select-window (posn-window position))
    (when-let* ((pos (posn-point position))
                ((numberp pos)))
      (if-let* ((cursor (helix-fake-cursor-at pos)))
          (helix-remove-fake-cursor cursor)
        ;; (deactivate-mark)
        (helix-create-fake-cursor pos)))))

;; C
(defun helix-copy-selection-down ()
  "Copy each selection COUNT times down."
  (interactive)
  (helix-execute-command-for-all-cursors #'helix-copy-cursor)
  (when mark-active
    (helix-merge-overlapping-regions)))

;; M-c
(defun helix-copy-selection-up (count)
  "Copy each selection COUNT times up."
  (interactive "p")
  (let ((current-prefix-arg (* count -1)))
    (helix-execute-command-for-all-cursors #'helix-copy-cursor))
  (when mark-active
    (helix-merge-overlapping-regions)))

(defun helix-copy-cursor (count)
  "Copy point or region COUNT times up or down depending on
the sign of the COUNT."
  (interactive "p")
  (cond ((use-region-p)
         (helix-motion-loop (dir count)
           (helix--copy-region dir)))
        (t
         (helix-motion-loop (dir count)
           (helix--copy-cursor-1 dir)))))

(defun helix--copy-cursor-1 (direction)
  "Copy point toward the DIRECTION."
  (or direction (setq direction 1))
  (when-let* ((pos (save-excursion
                     (let ((column (current-column))
                           position)
                       (cl-block nil
                         (while (not position)
                           (unless (zerop (forward-line direction))
                             (cl-return))
                           (when (eql (move-to-column column) column)
                             (setq position (point)))))
                       position)))
              ((not (helix-fake-cursor-at pos))))
    (helix-create-fake-cursor-from-point)
    (goto-char pos)))

(defun helix--copy-region (&optional direction)
  "Copy region toward the DIRECTION."
  (or direction (setq direction 1))
  (pcase-let* ((region-dir (helix-region-direction))
               (`(,beg . ,end) (car (region-bounds)))
               (num-of-lines (count-lines beg end))
               (beg-column (save-excursion
                             (goto-char beg)
                             (current-column)))
               (end-column (save-excursion
                             (goto-char end)
                             (current-column))))
    (when-let* ((bounds (save-excursion
                          (goto-char (if (< direction 0) beg end))
                          (helix--bounds-of-following-region
                           beg-column end-column num-of-lines direction))))
      (let (pnt mrk)
        (if (< region-dir 0)
            (pcase-setq `(,pnt . ,mrk) bounds)
          (pcase-setq `(,mrk . ,pnt) bounds))
        (if-let* ((cursor (helix-fake-cursor-at pnt))
                  ((= mrk (overlay-get cursor 'mark))))
            nil ;; Do nothing, since fake cursor is already there.
          ;; else
          (helix-create-fake-cursor-from-point)
          (goto-char pnt)
          (set-marker (mark-marker) mrk))))))

(defun helix--bounds-of-following-region (start-column end-column number-of-lines direction)
  "Return bounds of following region that starts at START-COLUMN
ends at END-COLUMN spauns NUMBER-OF-LINES."
  (when (< direction 0)
    (pcase-setq `(,start-column . ,end-column)
                (cons end-column start-column)))
  (let (start end)
    (cl-block nil
      (while (not (and start end))
        (unless (zerop (forward-line direction))
          (cl-return))
        (when (eql (move-to-column start-column)
                   start-column)
          (setq start (point))
          (while (not end)
            (unless (zerop (forward-line (* (1- number-of-lines)
                                            direction)))
              (cl-return))
            (when (eql (move-to-column end-column)
                       end-column)
              (setq end (point)))))))
    (if (and start end)
        (if (< 0 direction)
            (cons start end)
          (cons end start)))))

;; (defun helix-mc-bound-cursor (direction)
;;   "Return the first fake cursor if DIRECTION is positive number /last
;; If DIRECTION is negative number find the fake cursor with the minimal buffer
;; position. If its position is smaller than the positon of the point — return it.
;; Otherwise return nil.
;;
;; If DIRECTION is positive number find the fake cursor with the maximum buffer
;; position. If its position is bigger than the positon of the point — return it.
;; Otherwise return nil."
;;   )



(provide 'helix-multiple-cursors)
;;; helix-multiple-cursors.el ends here
