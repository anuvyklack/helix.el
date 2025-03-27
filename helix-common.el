;;; helix-common.el --- Common functions -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
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

(defvar helix-select-state-minor-mode nil)

(defun helix-forward-beginning (thing &optional count)
  "Move to the beginning of the COUNT THING."
  (setq count (or count 1))
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when (and bounds (< (point) (cdr bounds)))
      (goto-char (cdr bounds)))
    (ignore-errors
      (when (forward-thing thing count)
        (beginning-of-thing thing))
      )))

(defun helix-get-next-char (&optional dir)
  "Get the next char toward the direction.
If DIR is positive number get following char, negative — preceding char."
  (setq dir (or dir 1))
  (if (> dir 0) (following-char) (preceding-char)))

(defun helix-forward-chars (chars &optional dir)
  (setq dir (or dir 1))
  (not (zerop (if (> dir 0)
                  (skip-chars-forward chars)
                (skip-chars-backward chars)))))

(defun helix-skip-empty-lines (&optional dir)
  "Skip all empty lines toward direction.
If DIR is positive number move forward, else — backward."
  ;; (prog1
  ;;     (helix-forward-chars "\r\n" (or dir 1))
  ;;   (when (not helix-select-state-minor-mode)
  ;;     (set-mark (point))))
  (let ((point-moved (helix-forward-chars "\r\n" (or dir 1))))
    (when (and point-moved
               (not helix-select-state-minor-mode))
      (set-mark (point)))
    point-moved))

(defmacro helix-motion-loop (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY repeatedly COUNT times with DIRECTION bound to 1 or -1,
depending on the sign of COUNT. Each iteration must move point; if point
does not change, the loop immediately quits and returns nil.
Else returns t.

\(fn (DIRECTION COUNT) BODY...)"
  (declare (indent defun)
           (debug ((symbolp form) body)))
  (pcase-let ((`(,direction ,count) spec))
    `(let* ((,count (or ,count 1))
            (,direction (if (< ,count 0) -1 1))
            (n (abs ,count)))
       (if (zerop n) t
         (while (and (not (zerop n))
                     (/= (point) (progn ,@body (point))))
           (setq n (1- n)))
         (zerop n)))))

(defun forward-helix-word (&optional count)
  (helix-motion-loop (dir count)
    (helix-forward-chars "\r\n" dir)
    (helix-forward-chars " \t" dir)
    (or (memq (helix-get-next-char dir) '(?\r ?\n))
        (helix-forward-chars "^[:word:]\n\r\t\f " dir)
        (let ((word-separating-categories helix-cjk-word-separating-categories)
              (word-combining-categories helix-cjk-word-combining-categories))
          (forward-word dir)))))

(defun forward-helix-WORD (&optional count)
  (helix-motion-loop (dir count)
    (helix-forward-chars "\r\n" dir)
    (helix-forward-chars " \t" dir)
    (or (memq (helix-get-next-char dir) '(?\r ?\n))
        (helix-forward-chars "^\n\r\t\f " dir))))

;;; Utils

(defun helix--ensure-list (x)
  "Return X unchanged if it is a list, or wrap it in list."
  (if (listp x) x (list x)))

(defun helix-sign (&optional num)
  (cond ((< num 0) -1)
        ((zerop num) 0)
        (t 1)))

(provide 'helix-common)
;;; helix-common.el ends here
