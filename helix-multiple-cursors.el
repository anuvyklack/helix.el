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
(defun helix-copy-selection-down (count)
  "Copy point and region COUNT times down if COUNT is positive,
of up if negative."
  (interactive "p")
  (if (use-region-p)
      (helix-motion-loop (dir count)
        (helix--copy-region dir))
    ;; else
    (helix-motion-loop (dir count)
      (helix--copy-cursor dir))))

;; M-c
(defun helix-copy-selection-up (count)
  "Copy each selection COUNT times up."
  (interactive "p")
  (helix-copy-selection-down (- count)))

(defun helix--copy-cursor (direction)
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
            nil ;; Do nothing, since fake cursor is already at target position.
          ;; else
          (helix-create-fake-cursor-from-point)
          (goto-char pnt)
          (set-marker (mark-marker) mrk))))))

(defun helix--bounds-of-following-region
    (start-column end-column number-of-lines direction)
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

;; (
(defun helix-rotate-selections-backward (count)
  "Rotate main selection backward COUNT times."
  (interactive "p")
  (when helix-multiple-cursors-mode
    (dotimes (_ count)
      (let* ((cursor (or (helix-previous-fake-cursor (point))
                         (helix-last-fake-cursor)))
             (id (overlay-get cursor 'id)))
        (helix-create-fake-cursor-from-point id)
        (helix-restore-point-from-fake-cursor cursor)))))

;; )
(defun helix-rotate-selections-forward (count)
  "Rotate main selection forward COUNT times."
  (interactive "p")
  (when helix-multiple-cursors-mode
    (dotimes (_ count)
      (let* ((cursor (or (helix-next-fake-cursor (point))
                         (helix-first-fake-cursor)))
             (id (overlay-get cursor 'id)))
        (helix-create-fake-cursor-from-point id)
        (helix-restore-point-from-fake-cursor cursor)))))

;; M-(
(defun helix-rotate-selections-content-backward (count)
  "Rotate selections content backward COUNT times."
  (interactive "p")
  (when (and helix-multiple-cursors-mode
             (use-region-p))
    (dotimes (_ count)
      (helix-with-single-undo-step
        (helix-with-real-cursor-as-fake
          (let ((cursors (-> (helix-all-fake-cursors)
                             (sort #'helix--compare-by-overlay-start)
                             (nreverse))))
            (helix--rotate-selections-content cursors)))))))

;; M-)
(defun helix-rotate-selections-content-forward (count)
  "Rotate selections content forward COUNT times."
  (interactive "p")
  (when (and helix-multiple-cursors-mode
             (use-region-p))
    (dotimes (_ count)
      (helix-with-single-undo-step
        (helix-with-real-cursor-as-fake
          (let ((cursors (-> (helix-all-fake-cursors)
                             (sort #'helix--compare-by-overlay-start))))
            (helix--rotate-selections-content cursors)))))))

(defun helix--rotate-selections-content (cursors)
  "Rotate regions content for all CURSORS in the order they are in list."
  (let* ((first-cursor (car cursors))
         (content (buffer-substring (overlay-get first-cursor 'point)
                                    (overlay-get first-cursor 'mark))))
    (dolist (cursor (cdr cursors))
      (setq content (helix--replace-fake-region-content cursor content)))
    (helix--replace-fake-region-content first-cursor content)))

(defun helix--replace-fake-region-content (cursor content)
  "Replace the CURSORs region content with CONTENT.
Return the replaced substring."
  (helix--add-fake-cursor-to-undo-list cursor
    (helix--restore-point-state cursor)
    (let ((dir (helix-region-direction))
          (new-content (buffer-substring (point) (mark)))
          (deactivate-mark nil))
      (delete-region (point) (mark))
      (insert content)
      (when (< dir 0) (helix-exchange-point-and-mark))
      (helix-move-fake-cursor cursor (point) (mark t))
      new-content)))

(provide 'helix-multiple-cursors)
;;; helix-multiple-cursors.el ends here
