;;; helix-common.el --- Common functions -*- lexical-binding: t; -*-
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
;;  Common functions
;;
;;; Code:

(require 'cl-lib)
(require 'helix-vars)
(require 'thingatpt)

;;; Motions

(defun helix-forward-beginning (thing &optional count)
  "Move to the beginning of the COUNT THING."
  (setq count (or count 1))
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when (and bounds (< (point) (cdr bounds)))
      (goto-char (cdr bounds)))
    (ignore-errors
      (when (forward-thing thing count)
        (beginning-of-thing thing)))))

(defun helix-next-char (&optional dir)
  "Return the next after point char toward the direction.
If DIR is positive number get following char, negative — preceding char."
  (or dir (setq dir 1))
  (if (> dir 0) (following-char) (preceding-char)))

(defun helix-forward-chars (chars &optional dir)
  (or dir (setq dir 1))
  (not (zerop (if (> dir 0)
                  (skip-chars-forward chars)
                (skip-chars-backward chars)))))

;; (defun helix-skip-empty-lines (&optional dir)
;;   "Skip all empty lines toward direction.
;; If DIR is positive number move forward, else — backward."
;;   ;; (prog1
;;   ;;     (helix-forward-chars "\r\n" (or dir 1))
;;   ;;   (when (not helix-select-state-minor-mode)
;;   ;;     (set-mark (point))))
;;   (let ((point-moved (helix-forward-chars "\r\n" (or dir 1))))
;;     (when (and point-moved
;;                (not helix-select-state-minor-mode))
;;       (set-mark (point)))
;;     point-moved))

(defmacro helix-motion-loop (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY repeatedly COUNT times with DIRECTION bound to 1 or -1,
depending on the sign of COUNT. Each iteration must move point; if point
does not change, the loop immediately quits.

Returns the count of steps left to move.  If moving forward, that is
COUNT minus number of steps moved; if backward, COUNT plus number moved.

\(fn (DIRECTION COUNT) BODY...)"
  (declare (indent defun)
           (debug ((symbolp form) body)))
  (let ((direction (pop spec))
        (count (pop spec))
        (n (gensym "n")))
    `(let ((,direction (helix-sign ,count))
           (,n (abs ,count)))
       (while (and (/= ,n 0)
                   (/= (point) (progn ,@body (point))))
         (setq ,n (1- ,n)))
       (* ,n ,direction))))

;;; Things (`thingatpt.el')

(defun forward-helix-word (&optional count)
  "Returns the count of word left to move, positive or negative
depending on sign of COUNT."
  (or count (setq count 1))
  (helix-motion-loop (dir count)
    (helix-forward-chars "\r\n" dir)
    (helix-forward-chars " \t" dir)
    (or (memq (helix-next-char dir) '(?\r ?\n))
        (helix-forward-chars "^[:word:]\n\r\t\f " dir)
        (let ((word-separating-categories helix-cjk-word-separating-categories)
              (word-combining-categories  helix-cjk-word-combining-categories))
          (forward-word dir)))))

(defun forward-helix-WORD (&optional count)
  "Returns the count of word left to move, positive or negative
depending on sign of COUNT."
  (or count (setq count 1))
  (helix-motion-loop (dir count)
    (helix-forward-chars "\r\n" dir)
    (helix-forward-chars " \t" dir)
    (or (memq (helix-next-char dir) '(?\r ?\n))
        (helix-forward-chars "^\n\r\t\f " dir))))

;; (put 'visual-line 'beginning-op 'beginning-of-visual-line)
;; (put 'visual-line 'end-op       'end-of-visual-line)
(put 'visual-line 'forward-op #'(lambda (&optional count)
                                  (vertical-motion (or count 1))))

;;; Utils

(defun helix-exchange-point-and-mark ()
  "Exchange point and mark without activating the region."
  (let* ((point (point))
         (mark  (or (mark t) point)))
    (set-marker (mark-marker) point)
    (goto-char mark)))

(defsubst helix-forward-region-p ()
  "Return t if mark precedes point."
  (< (mark) (point)))

;; (defmacro helix-expand-selection-or (body)
;;   `(if helix-extend-selection
;;        (or (region-active-p) (set-mark (point)))
;;      ,@body))

(defsubst helix-empty-line-p ()
  (and (bolp) (eolp)))

(defun helix-bolp ()
  "Like 'bolp' but consider visual lines when 'visual-line-mode' is enabled."
  (if visual-line-mode
      (save-excursion
        (let ((p (point)))
          (beginning-of-visual-line)
          (= p (point))))
    (bolp)))

(defun helix-visual-bolp ()
  "Return t if point is at the beginning of visual line."
  (save-excursion
    (let ((p (point)))
      (beginning-of-visual-line)
      (= p (point)))))

(defun helix-eolp ()
  "Like 'eolp' but consider visual lines when 'visual-line-mode' is enabled."
  (if visual-line-mode
      (save-excursion
        (let ((p (point)))
          (end-of-visual-line)
          (= p (point))))
    (eolp)))

(defun helix-point-and-mark-at-bolp-p ()
  "Return symbol:
- `line' — if both point and mark are at the beginning of logical lines;
- `visual-line' — if point and mark are at the beginning of visual lines;
- nil — otherwise."
  (cond ((and (bolp)
              (save-mark-and-excursion
                (helix-exchange-point-and-mark)
                (bolp)))
         'line)
        ((and visual-line-mode
              (helix-visual-bolp)
              (save-mark-and-excursion
                (helix-exchange-point-and-mark)
                (helix-visual-bolp)))
         'visual-line)))

(defsubst helix-sign (&optional num)
  (cond ((< num 0) -1)
        ((zerop num) 0)
        (t 1)))

(provide 'helix-common)
;;; helix-common.el ends here
