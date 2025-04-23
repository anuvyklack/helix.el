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

(defun helix-forward-beginning (thing &optional count skip-empty-lines)
  "Move forward to beginning of COUNT THING.
When SKIP-EMPTY-LINES is non-nil skip all blank lines along the way.
This is needed, for example, for `helix-word': two `helix-word's
divided with empty lines, are considered adjoined when moving over them.

Works only with THINGs, that returns the count of steps left to move."
  (or count (setq count 1))
  (cond ((= count 0) 0)
        ((< count 0)
         (forward-thing thing count))
        (t (when skip-empty-lines (skip-chars-forward "\r\n"))
           (when-let* ((bnd (bounds-of-thing-at-point thing))
                       ((< (point) (cdr bnd))))
             (goto-char (cdr bnd))
             (when skip-empty-lines (skip-chars-forward "\r\n")))
           (let ((rest (forward-thing thing count)))
             (cond ((zerop rest)
                    (forward-thing thing -1)
                    (when skip-empty-lines (skip-chars-backward "\r\n")))
                   ((eobp) (backward-char))) ; assuming that buffer ends with newline
             rest))))

(defun helix-skip-chars (chars &optional direction)
  "Return t if moved any."
  (or direction (setq direction 1))
  (/= 0 (if (< 0 direction)
            (skip-chars-forward chars)
          (skip-chars-backward chars))))

(defun helix-next-char (&optional direction)
  "Return the next after point char toward the direction.
If DIRECTION is positive number get following char,
negative — preceding char."
  (or direction (setq direction 1))
  (if (> direction 0) (following-char) (preceding-char)))

;; (defun helix-skip-empty-lines (&optional direction)
;;   "Skip all empty lines toward direction.
;; If DIR is positive number move forward, else — backward."
;;   ;; (prog1
;;   ;;     (helix-skip-chars "\r\n" (or dir 1))
;;   ;;   (when (not helix-select-state-minor-mode)
;;   ;;     (set-mark (point))))
;;   (or direction (setq direction 1))
;;   (let ((point-moved (helix-skip-chars "\r\n" direction)))
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

(defun helix-bounds-of-complement-of-thing-at-point (thing &optional which)
  "Return the bounds of a complement of THING at point.
I.e., if there is a THING at point — returns nil, otherwise
the gap between two THINGs is returned.

Works only with THINGs, that returns the count of steps left to move,
like: `helix-word', `paragraph', `line'."
  (let ((orig-point (point)))
    (if-let* ((beg (save-excursion
                     (and (zerop (forward-thing thing -1))
                          (forward-thing thing))
                     (if (<= (point) orig-point)
                         (point))))
              (end (save-excursion
                     (and (zerop (forward-thing thing))
                          (forward-thing thing -1))
                     (if (<= orig-point (point))
                         (point))))
              ((and (<= beg (point) end)
                    (< beg end))))
        (pcase which
          (-1 beg)
          (1  end)
          (_ (cons beg end))))))

(defun forward-helix-word (&optional count)
  "Move point forward COUNT words (backward if COUNT is negative).
Returns the count of word left to move, positive or negative depending
on sign of COUNT.

Word is:
- sequence of characters matching `[[:word:]]'
- sequence non-word non-whitespace characters matching `[^[:word:]\\n\\r\\t\\f ]'
- indentation"
  (or count (setq count 1))
  (let* ((forward?  (natnump count))
         (backward? (not forward?)))
    (helix-motion-loop (dir count)
      (helix-skip-chars "\r\n" dir)
      (cond ((and forward?
                  (bolp)
                  (helix-skip-chars " \t" dir)))
            ((and backward?
                  (helix-skip-chars " \t" dir)
                  (bolp)))
            ((helix-skip-chars "^[:word:]\n\r\t\f " dir))
            ((let ((word-separating-categories helix-cjk-word-separating-categories)
                   (word-combining-categories  helix-cjk-word-combining-categories))
               (forward-word dir)))))))

(defun forward-helix-WORD (&optional count)
  "Move point forward COUNT WORDs (backward if COUNT is negative).
Returns the count of WORD left to move, positive or negative depending
on sign of COUNT.

WORD is:
- any space separated sequence of characters
- indentation"
  (or count (setq count 1))
  (let* ((forward?  (natnump count))
         (backward? (not forward?)))
    (helix-motion-loop (dir count)
      (helix-skip-chars "\r\n" dir)
      (cond ((and forward?
                  (bolp)
                  (helix-skip-chars " \t" dir)))
            ((and backward?
                  (helix-skip-chars " \t" dir)
                  (bolp)))
            (t (when forward?
                 (helix-skip-chars " \t" dir)
                 (helix-skip-chars "\r\n" dir))
               (helix-skip-chars "^\n\r\t\f " dir))))))

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
