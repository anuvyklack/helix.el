;;; helix-common.el --- Common functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Yuriy Artemyev
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
(require 'dash)
(require 'thingatpt)
(require 'pcre2el)
(require 'helix-macros)
(require 'helix-vars)

;;; Motions

(defun helix-forward-next-thing (thing &optional count)
  "Move forward to the end of the COUNT-th next THING.
`forward-thing' first moves to the  boundary of the current THING, then to the
next THING. This function skips first step and always moves to the next THING."
  (unless count (setq count 1))
  (if (zerop count) 0
    (-when-let ((beg . end) (bounds-of-thing-at-point thing))
      (goto-char (if (natnump count) end beg)))
    (forward-thing thing count)))

(defun helix-forward-beginning-of-thing (thing &optional count)
  "Move to the beginning of COUNT-th next THING.
Move backward if COUNT is negative.
Returns the count of steps left to move.

Works only with THING, that returns the count of steps left to move,
such as `helix-word', `helix-sentence', `paragraph', `line'."
  (unless count (setq count 1))
  (if (eql count 0) 0
    (let ((rest (helix-forward-next-thing thing count)))
      (when (and (/= rest count)
                 (natnump count))
        (forward-thing thing -1))
      rest)))

(defun helix-forward-end-of-thing (thing &optional count)
  "Move to the end of COUNT-th next THING.
Move backward if COUNT is negative.
Returns the count of steps left to move.

Works only with THING, that returns the count of steps left to move,
such as `helix-word', `helix-sentence', `paragraph', `line'."
  (unless count (setq count 1))
  (if (eql count 0) 0
    (let ((rest (helix-forward-next-thing thing count)))
      (when (and (/= rest count)
                 (< count 0))
        (forward-thing thing))
      rest)))

(defun helix-skip-chars (chars &optional direction)
  "Move point toward the DIRECTION stopping after a char is not in CHARS string.
Move backward when DIRECTION is negative number, forward — otherwise.
Return t if point has moved."
  (/= 0 (if (natnump (or direction 1))
            (skip-chars-forward chars)
          (skip-chars-backward chars))))

(defun helix-skip-whitespaces (&optional direction)
  "Move point toward the DIRECTION across whitespace.
Move backward when DIRECTION is negative number, forward — otherwise.
Return the distance traveled positive or negative depending on DIRECTION."
  ;; Alternative: (helix-skip-chars " \t" dir)
  (if (natnump (or direction 1))
      (skip-syntax-forward " " (line-end-position))
    (skip-syntax-backward " " (line-beginning-position))
    ;; (let ((steps (skip-syntax-forward " " (line-end-position))))
    ;;   (if (/= steps 0)
    ;;       (let ((pnt (point)))
    ;;         (backward-prefix-chars)
    ;;         (+ steps (- (point) pnt)))
    ;;     steps))
    ))

(defun helix-next-char (&optional direction)
  "Return the next after point char toward the DIRECTION.
If DIRECTION is positive number — get following char, otherwise preceding char."
  (if (natnump (or direction 1))
      (following-char)
    (preceding-char)))

(defun helix-beginning-of-line (&optional count)
  "Move point to the beginning of current line.
Move over visual line when `visual-line-mode' is active."
  (if visual-line-mode
      (beginning-of-visual-line count)
    (helix--beginning-of-line count)))

(defun helix-end-of-line (&optional count)
  "Move point to the end of current line.
Move over visual line when `visual-line-mode' is active."
  (if visual-line-mode
      (end-of-visual-line count)
    (move-end-of-line count)))

(defun helix--forward-word-start (thing count)
  "Move to the COUNT-th next start of a word-like THING."
  (setq helix-linewise-selection nil)
  (setq count (abs count))
  (setq helix-linewise-selection nil)
  (when (zerop (forward-thing thing (1- count)))
    (unless helix--extend-selection
      (skip-chars-forward "\r\n")
      (set-mark (point)))
    (or (helilx-whitespace? (following-char))
        (forward-thing thing))
    (helix-skip-whitespaces)))

(defun helix--backward-word-start (thing count)
  "Move to the COUNT-th previous start of a word-like THING."
  (setq helix-linewise-selection nil)
  (setq count (- (abs count)))
  (setq helix-linewise-selection nil)
  (when (zerop (forward-thing thing (1+ count)))
    (unless helix--extend-selection
      (skip-chars-backward "\r\n")
      (set-mark (point)))
    (forward-thing thing -1)))

(defun helix--forward-word-end (thing count)
  "Move to the COUNT-th next word-like THING end."
  (interactive "p")
  (setq count (abs count))
  (setq helix-linewise-selection nil)
  (when (zerop (forward-thing thing (1- count)))
    (unless helix--extend-selection
      (skip-chars-forward "\r\n")
      (set-mark (point)))
    (forward-thing thing)))

(defmacro helix-motion-loop (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY repeatedly COUNT times with DIRECTION bound to 1 or -1,
depending on the sign of COUNT. Each iteration must move point; if point
does not change, the loop immediately quits.

Returns the count of steps left to move.  If moving forward, that is
COUNT minus number of steps moved; if backward, COUNT plus number moved.

\(fn (DIRECTION COUNT) BODY...)"
  (declare (indent 1)
           (debug ((symbolp form) body)))
  (let* ((dir (pop spec))
         (count (pop spec))
         (n (gensym "n")))
    `(let* ((,n ,count)
            (,dir (helix-sign ,n)))
       (while (and (/= ,n 0)
                   (/= (point) (progn ,@body (point))))
         (setq ,n (- ,n ,dir)))
       ,n)))

;;; Things

;; `helix-line' thing

;; The need for this thing arose from the requirement to select a folded section
;; of the buffer (in Org-mode or Outline-mode) using the `x' key command.

(put 'helix-line 'forward-op #'helix--forward-line)
(put 'helix-line 'bounds-of-thing-at-point
     #'(lambda ()
         (cons (save-excursion (helix--beginning-of-line 1) (point))
               (save-excursion (helix--forward-line 1) (point)))))

(defun helix--forward-line (&optional count)
  "Goto COUNT visible logical lines forward (backward if COUNT is negative).
The difference from `forward-line' is that this function ignores invisible parts
of the buffer (lines folded by `outline-minor-mode' for example) and always
moves only over visible lines."
  ;; Adopted from `move-end-of-line'.
  (or count (setq count 1))
  (let ((goal-column 0)
	(line-move-visual nil)
        (inhibit-field-text-motion (minibufferp))) ; bug#65980
    (and (line-move count t)
	 (not (bobp))
	 (while (and (not (bobp))
                     (invisible-p (1- (point))))
	   (goto-char (previous-single-char-property-change
                       (point) 'invisible))))))

(defun helix--beginning-of-line (&optional count)
  "Go to the beginning of the current visible logical line.
Adopted from `move-beginning-of-line'."
  (or count (setq count 1))
  (let ((orig-point (point)))
    ;; Move by lines, if COUNT is not 1 (the default).
    (when (/= count 1)
      (let ((line-move-visual nil))
	(line-move (1- count) t)))
    ;; Move to beginning-of-line, ignoring fields and invisible text.
    (let ((inhibit-field-text-motion t))
      (goto-char (line-beginning-position))
      (while (and (not (bobp)) (invisible-p (1- (point))))
        (goto-char (previous-char-property-change (point)))
        (goto-char (line-beginning-position))))
    ;; Now find first visible char in the line.
    (while (and (< (point) orig-point)
                (invisible-p (point)))
      (goto-char (next-char-property-change (point) orig-point)))
    ;; Obey field constraints.
    (goto-char (constrain-to-field (point) orig-point (/= count 1) t nil))))

;; `helix-visual-line' thing

(put 'helix-visual-line 'forward-op #'vertical-motion)
;; (put 'helix-visual-line 'beginning-op #'beginning-of-visual-line)
;; (put 'helix-visual-line 'end-op       #'end-of-visual-line)

;; `helix-word' thing

(defun forward-helix-word (&optional count)
  "Move point COUNT words forward (backward if COUNT is negative).
Returns the count of word left to move, positive or negative depending
on sign of COUNT.

Word is:
- sequence of characters matching `[[:word:]]'
- sequence non-word non-whitespace characters matching `[^[:word:]\\n\\r\\t\\f ]'"
  (helix-motion-loop (dir (or count 1))
    (helix-skip-chars "\r\n" dir)
    (helix-skip-whitespaces dir)
    (or (helix-line-boundary-p dir)
        (helix-skip-chars "^[:word:]\n\r\t\f " dir)
        (let ((word-separating-categories helix-cjk-word-separating-categories)
              (word-combining-categories  helix-cjk-word-combining-categories))
          (forward-word dir)))))

;; `helix-WORD' thing

(defun forward-helix-WORD (&optional count)
  "Move point COUNT WORDs forward (backward if COUNT is negative).
Returns the count of WORD left to move, positive or negative depending
on sign of COUNT.

WORD is any space separated sequence of characters."
  (helix-motion-loop (dir (or count 1))
    (helix-skip-chars "\r\n" dir)
    (helix-skip-whitespaces dir)
    (unless (helix-line-boundary-p dir)
      (helix-skip-chars "^\n\r\t\f " dir))))

;; `helix-sentence' thing

(defun forward-helix-sentence (&optional count)
  "Move point COUNT sentences forward (backward if COUNT is negative).
Returns then count of sentences left to move, positive of negative depending
on sign of COUNT.

What is sentence is defined by `forward-sentence-function'."
  (helix-motion-loop (dir (or count 1))
    (ignore-errors (forward-sentence dir))))

;; `helix-paragraph' thing

(defun forward-helix-paragraph (&optional count)
  "Move point COUNT paragraphs forward (backward if COUNT is negative).
Returns then count of sentences left to move, positive of negative depending
on sign of COUNT."
  (helix-motion-loop (dir (or count 1))
    (cond ((natnump dir) (forward-paragraph))
          ((not (bobp))
           (start-of-paragraph-text)
           (beginning-of-line)))))

;; `helix-function' thing

(defun forward-helix-function (&optional count)
  "Move point COUNT functions forward (backward if COUNT is negative).
Returns then count of sentences left to move, positive of negative depending
on sign of COUNT."
  (helix-motion-loop (dir (or count 1))
    (if (natnump dir) (end-of-defun) (beginning-of-defun))))

;; `helix-sexp' thing

(defun forward-helix-sexp (&optional count)
  (helix-motion-loop (dir (or count 1))
    (ignore-errors
      (forward-sexp dir))))

;; `helix-comment' thing

(put 'helix-comment 'bounds-of-thing-at-point #'helix-bounds-of-comment-at-point-ppss)

(defun helix-bounds-of-comment-at-point-ppss ()
  "Return the bounds of a comment at point using Parse-Partial-Sexp Scanner."
  (save-excursion
    (let ((state (syntax-ppss)))
      (when (nth 4 state)
        (cons (nth 8 state)
              (when (parse-partial-sexp
                     (point) (point-max) nil nil state 'syntax-table)
                (point)))))))

;;; Selection

(defun helix-mark-inner-thing (thing &optional count)
  (unless count (setq count 1))
  (when (zerop count)
    (error "Cannot mark zero %s" thing))
  (helix-carry-linewise-selection)
  (-if-let ((beg . end) (bounds-of-thing-at-point thing))
      (progn
        (set-mark beg)
        (goto-char end)
        (cl-decf count))
    ;; else
    (forward-thing thing)
    (forward-thing thing -1)
    (set-mark (point)))
  (forward-thing thing count)
  (helix-maybe-enable-linewise-selection))

(defun helix-mark-a-thing (thing)
  "Select a THING with spacing around.
Works only with THINGs, that returns the count of steps left to move,
such as `paragraph', `helix-function'."
  (helix-carry-linewise-selection)
  (-when-let ((thing-beg . thing-end) (bounds-of-thing-at-point thing))
    (-let [(beg . end)
           (or (progn
                 (goto-char thing-end)
                 (-if-let ((_ . space-end)
                           (helix-bounds-of-complement-of-thing-at-point thing))
                     (cons thing-beg space-end)))
               (progn
                 (goto-char thing-beg)
                 (-if-let ((space-beg . _)
                           (helix-bounds-of-complement-of-thing-at-point thing))
                     (cons space-beg thing-end)))
               (cons thing-beg thing-end))]
      (helix-set-region beg end)))
  (helix-maybe-enable-linewise-selection))

(defun helix-bounds-of-complement-of-thing-at-point (thing)
  "Return the bounds of a complement of THING at point.
I.e., if there is a THING at point — returns nil, otherwise
the gap between two THINGs is returned.

Works only with THINGs, that returns the count of steps left to move,
such as `helix-word', `helix-sentence', `paragraph', `line'."
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
        (cons beg end))))

(defun helix-mark-thing-forward (thing count)
  "Select from point to the end of the THING (or COUNT following THINGs).
If no THING at point select COUNT following THINGs."
  (helix-push-point)
  (helix-carry-linewise-selection)
  (unless helix--extend-selection
    (set-mark (point))
    (let ((dir (helix-sign count)))
      (when (-if-let ((thing-beg . thing-end) (bounds-of-thing-at-point thing))
                ;; At the boundary of the THING toward the motion direction.
                (= (point) (if (natnump dir)
                               thing-end
                             thing-beg))
              ;; No thing at point at all.
              t)
        (if (natnump dir)
            (helix-forward-beginning-of-thing thing dir)
          (helix-forward-end-of-thing thing dir))
        (set-mark (point)))))
  (forward-thing thing count)
  (helix-maybe-enable-linewise-selection)
  (helix-reveal-point-when-on-top))

(defun helix--mark-a-word (thing)
  "Inner implementation of `helix-mark-a-word' and `helix-mark-a-WORD' commands."
  (-when-let ((thing-beg . thing-end) (bounds-of-thing-at-point thing))
    (setq helix-linewise-selection nil)
    (-let [(beg . end)
           (or (progn
                 (goto-char thing-end)
                 (helix-with-restriction
                     (line-beginning-position) (line-end-position)
                   (-if-let ((_ . space-end)
                             (helix-bounds-of-complement-of-thing-at-point thing))
                       (cons thing-beg space-end))))
               (progn
                 (goto-char thing-beg)
                 (helix-with-restriction
                     (save-excursion (back-to-indentation) (point))
                     (line-end-position)
                   (-if-let ((space-beg . _)
                             (helix-bounds-of-complement-of-thing-at-point thing))
                       (cons space-beg thing-end))))
               (cons thing-beg thing-end))]
      (helix-set-region beg end))))

;;; Surround

(defun helix-bounds-of-quoted-at-point (quote-mark)
  "Return a cons cell (START . END) with bounds of text region
enclosed in QUOTE-MARKs."
  (if-let* ((limits (or (bounds-of-thing-at-point 'helix-comment)
                        (bounds-of-thing-at-point 'string))))
      (-if-let ((beg _ _ end)
                (helix-surround-4-bounds-at-point (char-to-string quote-mark)
                                                  (char-to-string quote-mark)
                                                  limits))
          (cons beg end))
    ;; else
    (helix--bounds-of-quoted-at-point-ppss quote-mark)))

(defun helix-surround--4-bounds (char)
  "For given CHAR according to `helix-surround-alist' return
the list (LEFT-BEG LEFT-END RIGHT-LEFT RIGHT-END) with 4 positions:
before/after left delimiter and before/after right delimiter,"
  (if-let* ((spec (alist-get char helix-surround-alist))
            (pair-or-list (pcase (plist-get spec :search)
                            ((and fn (pred functionp))
                             (funcall fn))
                            (val val))))
      (pcase pair-or-list
        ((and (pred -cons-pair-p) `(,left . ,right))
         (helix-surround-4-bounds-at-point left right
                                           (bounds-of-thing-at-point 'defun)
                                           (plist-get spec :regexp)
                                           (plist-get spec :balanced)))
        ((and list (pred proper-list-p) (guard (length= list 4)))
         list))
    ;; else
    (helix-surround-4-bounds-at-point (char-to-string char)
                                      (char-to-string char)
                                      (bounds-of-thing-at-point 'defun))))

(defun helix-surround-4-bounds-at-point
    (left right &optional limits regexp? balanced?)
  "Return the bounds of the text region enclosed in LEFT and RIGHT strings.

If LEFT and RIGHT are different, then point can be either: directly before
LEFT,directly after RIGHT, or somewhere between them. If LEFT and RIGHT are
equal — point should be between them.

The search can be bounded within the LIMITS: a cons cell with
\(LEFT-BOUND . RIGHT-BOUND) positions.

If REGEXP? is non-nil LEFT and RIGHT will be searched as regexp patterns
\(and clobber match data), otherwise they will be searched literally.

If BALANCED? is non-nil all nested LEFT RIGHT pairs will be skipped.

Return the list (LEFT-BEG LEFT-END RIGHT-LEFT RIGHT-END) with
4 positions: before/after LEFT and before/after RIGHT."
  (save-excursion
    (when (string-equal left right)
      (setq balanced? nil))
    (cond
     ;; Check if we can use Parse-Partial-Sexp Scanner
     ((and balanced?
           (length= left 1)
           (length= right 1)
           (eq (char-syntax (string-to-char left)) ?\( )
           (eq (char-syntax (string-to-char right)) ?\) ))
      (-if-let ((beg . end)
                (helix-bounds-of-brackets-at-point (string-to-char left)
                                                   (string-to-char right)))
          (list beg (1+ beg) (1- end) end)))
     (t
      (helix-surround--4-bounds-at-point-1 left right limits regexp? balanced?)))))

(defun helix-bounds-of-brackets-at-point (left right)
  "Return the bounds of the balanced expression at point enclosed
in LEFT and RIGHT brackets, for which the point is either: directly
before LEFT, directly after RIGHT, or between them. All nested balanced
expressions are skipped.

LEFT and RIGHT should be chars.

This function is intended to search balanced brackets in programming modes,
since internally uses Emacs built-in Parse-Partial-Sexp Scanner for balanced
expressions. For arbitrary delimiters use `helix-surround-4-bounds-at-point'.

Return the cons cell (START . END) with positions before LEFT and
after RIGHT."
  (when (eq left right)
    (user-error "Left and right brackets should not be equal"))
  (if-let* ((string-or-comment-bounds
             (or (bounds-of-thing-at-point 'helix-comment)
                 (bounds-of-thing-at-point 'string)))
            (bounds (helix-surround--4-bounds-at-point-1
                     (char-to-string left) (char-to-string right)
                     string-or-comment-bounds
                     nil t)))
      ;; If inside comment or string use manual algorithm.
      (-let [(beg _ _ end) bounds]
        (cons beg end))
    ;; Else if not or nothing have found — go out ...
    (when string-or-comment-bounds
      (goto-char (car string-or-comment-bounds)))
    ;; ... and try Parse-Partial-Sexp Scanner
    (save-excursion
      (let* ((pnt (point))
             (syntax-table (if (and (eq (char-syntax left) ?\()
                                    (eq (char-syntax right) ?\)))
                               (syntax-table)
                             (let ((table (copy-syntax-table (syntax-table))))
                               (modify-syntax-entry left  (format "(%c" right) table)
                               (modify-syntax-entry right (format ")%c" left) table)
                               table))))
        (with-syntax-table syntax-table
          (cond ((eq (following-char) left) ; point is before LEFT
                 (if-let* ((end (scan-lists pnt 1 0)))
                     (cons pnt end)))
                ((eq (preceding-char) right) ; point is after RIGHT
                 (if-let* ((beg (scan-lists pnt -1 0)))
                     (cons beg pnt)))
                (t
                 (ignore-errors
                   (while (progn (up-list -1 t)
                                 (/= (following-char) left)))
                   (if-let* ((end (scan-lists (point) 1 0)))
                       (cons (point) end))))))))))

(defun helix-4-bounds-of-brackets-at-point (left right)
  "Return 4 bounds of the balanced expression at point enclosed
in LEFT and RIGHT brackets, for which the point is either: directly
before LEFT, directly after RIGHT, or between them. All nested balanced
expressions are skipped.

LEFT and RIGHT should be chars.

This function is intended to search balanced brackets in programming modes,
since internally uses Emacs built-in Parse-Partial-Sexp Scanner for balanced
expressions. For arbitrary delimiters use `helix-surround-4-bounds-at-point'.

Return the list (LEFT-BEG LEFT-END RIGHT-LEFT RIGHT-END) with 4 positions:
1. Before LEFT bracket;
2. After LEFT bracket all following whitespaces and newlines;
3. Before RIGHT bracket all preceding whitespaces and newlines;
4. After RIGHT bracket."
  (-if-let ((left-beg . right-end) (helix-bounds-of-brackets-at-point left right))
      (save-excursion
        (let ((left-end (progn
                          (goto-char (1+ left-beg))
                          (skip-chars-forward " \t\r\n")
                          (point)))
              (right-beg (progn
                           (goto-char (1- right-end))
                           (skip-chars-backward " \t\r\n")
                           (point))))
          (list left-beg left-end right-beg right-end)))))

(defun helix-surround--4-bounds-at-point-1 (left right &optional limits regexp? balanced?)
  "The internal function for `helix-surround-4-bounds-at-point'."
  (save-excursion
    (let ((left-not-equal-right? (not (string-equal left right))))
      (cond
       ;; point is before LEFT
       ((and left-not-equal-right?
             (helix-looking-at left 1 regexp?))
        (let* ((left-beg (point))
               (left-end (if regexp? (match-end 0)
                           (+ left-beg (length left)))))
          (goto-char left-end)
          (if-let* ((right-end (helix-surround-search-outward
                                left right 1 limits regexp? balanced?))
                    (right-beg (if regexp? (match-beginning 0)
                                 (- right-end (length right)))))
              (list left-beg left-end right-beg right-end))))
       ;; point is after RIGHT
       ((and left-not-equal-right?
             (helix-looking-at right -1 regexp?))
        (let* ((right-end (point))
               (right-beg (if regexp? (match-beginning 0)
                            (- right-end (length right)))))
          (goto-char right-beg)
          (if-let* ((left-beg (helix-surround-search-outward
                               left right -1 limits regexp? balanced?))
                    (left-end (if regexp? (match-end 0)
                                (+ left-beg (length left)))))
              (list left-beg left-end right-beg right-end))))
       (t
        (if-let* ((left-beg (helix-surround-search-outward
                             left right -1 limits regexp? balanced?))
                  (left-end (if regexp? (match-end 0)
                              (+ left-beg (length left))))
                  (right-end (helix-surround-search-outward
                              left right 1 limits regexp? balanced?))
                  (right-beg (if regexp? (match-beginning 0)
                               (- right-end (length right)))))
            (list left-beg left-end right-beg right-end)))))))

(defun helix-surround-search-outward
    (left right &optional direction limits regexp? balanced?)
  "Return the position before LEFT or after RIGHT depending on DIRECTION.

This function assumes, that point is somewhere between LEFT RIGHT
delimiters, which should be strings.

DIRECTION should be either 1 — return the position after RIGHT,
or -1 — before LEFT.

The search is optionally bounded within LIMITS: a cons cell with
\(LEFT-BOUND . RIGHT-BOUND) positions.

If REGEXP? is non-nil LEFT and RIGHT will be searched as regexp patterns
\(and clobber match data), else they will be searched literally.

If BALANCED? is non-nil all nested LEFT RIGHT pairs on the way will
be skipped."
  (unless direction (setq direction 1))
  (save-excursion
    (if balanced?
        (helix-surround--search-outward-balanced left right direction limits regexp?)
      (let ((string (if (< direction 0) left right))
            (limit  (if (< direction 0) (car limits) (cdr limits))))
        (helix-search string direction limit regexp?)))))

(defun helix-surround--search-outward-balanced
    (left right &optional direction limits regexp?)
  "This is an internal function for `helix-surround-search-outward'
that is used when BALANCED? argument is non-nil."
  (save-excursion
    (let (open close limit)
      (if (> direction 0)
          (-setq open left
                 close right
                 (_ . limit) limits)
        (-setq open right
               close left
               (limit . _) limits))
      ;; The algorithm assume we are *inside* a pair: level of nesting is 1.
      (let ((level 1))
        (cl-block nil
          (while (> level 0)
            (let* ((pnt (point))
                   (open-pos (helix-search open direction limit regexp?))
                   (close-pos (progn
                                (goto-char pnt)
                                (helix-search close direction limit regexp?))))
              (cond ((and close-pos open-pos)
                     (let ((close-dist (helix-distance pnt close-pos))
                           (open-dist  (helix-distance pnt open-pos)))
                       (cond ((< open-dist close-dist)
                              (setq level (1+ level))
                              (goto-char open-pos))
                             (t
                              (setq level (1- level))
                              (goto-char close-pos)))))
                    (close-pos
                     (setq level (1- level))
                     (goto-char close-pos))
                    (t (cl-return))))))
        (if (eql level 0)
            (point))))))

(defun helix--bounds-of-quoted-at-point-ppss (quote-mark)
  "Return a cons cell (START . END) with bounds of region around
the point enclosed in QUOTE-MARK character.

Internally uses Emacs' built-in Parse-Partial-Sexp Scanner for
balanced expressions."
  (save-excursion
    (let ((syntax-table (if (eq (char-syntax quote-mark) ?\")
                            (syntax-table)
                          (let ((st (copy-syntax-table (syntax-table))))
                            (modify-syntax-entry quote-mark "\"" st)
                            st))))
      (with-syntax-table syntax-table
        (let* ((curpoint (point))
               (state (progn
                        (beginning-of-defun)
                        (parse-partial-sexp (point) curpoint nil nil (syntax-ppss)))))
          (if (nth 3 state)
              ;; Inside the string
              (ignore-errors
                (goto-char (nth 8 state))
                (cons (point)
                      (progn (forward-sexp) (point))))
            ;; At the beginning of the string
            (if-let* ((ca (char-after))
                      ;; ((eq (char-syntax ca) ?\"))
                      ((eq ca quote-mark))
                      (bounds (bounds-of-thing-at-point 'sexp))
                      ((<= (car bounds) (point)))
                      ((< (point) (cdr bounds))))
                bounds)))))))

;;; Copy/paste

(defun helix-push-point (&optional position)
  "Push POSITION (point by default) on the `mark-ring'."
  (let ((old (nth mark-ring-max mark-ring))
        (history-delete-duplicates nil))
    (add-to-history 'mark-ring
                    (copy-marker (or position (point)))
                    mark-ring-max t)
    (when old
      (set-marker old nil)))
  ;; Don't push the mark on the global mark ring if the last global
  ;; mark pushed was in this same buffer.
  (unless (and global-mark-ring
               (eq (marker-buffer (car global-mark-ring))
                   (current-buffer)))
    (let ((old (nth global-mark-ring-max global-mark-ring))
          (history-delete-duplicates nil))
      (add-to-history 'global-mark-ring
                      (copy-marker position)
                      global-mark-ring-max t)
      (when old
        (set-marker old nil))))
  nil)

(defun helix-push-mark (&optional position nomsg activate)
  "Set mark to the POSITION and push it on the `mark-ring'.
If NOMSG is nil show `Mark set' message in echo area."
  (helix-push-point position)
  (set-marker (mark-marker) (or position (point)) (current-buffer))
  (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
      (message "Mark set"))
  (when activate
    (set-mark (mark t)))
  nil)

(defun helix-yank (&optional arg)
  "Helix `yank' (paste) wrapper."
  (interactive)
  (cl-letf (((symbol-function 'push-mark) #'helix-push-mark)
            (deactivate-mark nil))
    ;; `yank' sets `this-command' to `yank' internally, so we don't have to
    (yank arg)))

(defun helix-paste (direction)
  "Paste before/after selection depending on DIRECTION."
  (let ((region-dir (if (use-region-p)
                        (helix-region-direction)
                      1)))
    (helix-ensure-region-direction direction)
    (when (helix-ends-with-newline (current-kill 0 :do-not-move))
      (if (natnump direction)
          (forward-line 1)
        (forward-line 0)))
    (helix-yank)
    (activate-mark)
    (helix-ensure-region-direction region-dir)
    (helix-maybe-enable-linewise-selection)))

;;; Utils

(defun helix--exchange-point-and-mark ()
  "Exchange point and mark."
  (goto-char (prog1 (marker-position (mark-marker))
               (set-marker (mark-marker) (point)))))

(defun helix-bolp ()
  "Like `bolp' but consider visual lines when `visual-line-mode' is enabled."
  (if visual-line-mode
      (helix-visual-bolp)
    (bolp)))

(defun helix-visual-bolp ()
  "Return t if point is at the beginning of visual line."
  (save-excursion
    (let ((p (point)))
      (beginning-of-visual-line)
      (eql p (point)))))

(defun helix-eolp ()
  "Like `eolp' but consider visual lines when `visual-line-mode' is enabled."
  (if visual-line-mode
      (helix-visual-eolp)
    (eolp)))

(defun helix-visual-eolp ()
  "Return t if point is at the end of visual line."
  (save-excursion
    (let ((p (point)))
      (end-of-visual-line)
      (eql p (point)))))

(defun helix-line-boundary-p (direction)
  "If DIRECTION is negative number, checks for beginning of line,
positive — end of line."
  (if (< direction 0) (bolp) (eolp)))

(defun helix-region-direction ()
  "Return the direction of region: -1 if point precedes mark, 1 otherwise."
  (if (< (point) (mark-marker)) -1 1))

(cl-defun helix-logical-lines-p (&optional (beg (region-beginning))
                                           (end (region-end)))
  "Return t if region between BEG and END spawn full logical lines."
  (save-excursion
    (and (progn (goto-char beg) (bolp))
         (progn (goto-char end) (bolp)))))

(cl-defun helix-visual-lines-p (&optional (beg (region-beginning))
                                          (end (region-end)))
  "Return t if region between BEG and END spawn visual lines."
  (save-excursion
    (and visual-line-mode
         (progn (goto-char beg) (helix-visual-bolp))
         (progn (goto-char end) (helix-visual-bolp)))))

(defun helilx-whitespace? (char)
  "Non-nil when CHAR belongs to whitespace syntax class."
  ;; FIXME: Space syntax class can be denoted with both " " and "-" chars.
  ;; Are we shore that `char-syntax' always returns " "?
  (and (eql (char-syntax char) ?\s)
       (not (memq char '(?\r ?\n))))
  ;; Alternative: (memq char '(?\s ?\t))
  )

(defun helix-sign (&optional num)
  (cond ((< num 0) -1)
        ((zerop num) 0)
        (t 1)))

(defun helix-distance (x y) (abs (- y x)))

(defun helix-search (string &optional direction limit regexp? visible?)
  "Search for STRING toward the DIRECTION.

DIRECTION can be either 1 — search forward, or -1 — search backward.

BOUND optionally bounds the search. It should be a position that
is *after* the point if DIRECTION is positive, and *before* the
point — if negative.

If REGEXP? is non-nil STRING will considered a regexp pattern,
otherwise — literally.

If VISIBLE? is non-nil skip invisible matches.

When REGEXP? is non-nil this function modifies the match data
that `match-beginning', `match-end' and `match-data' access."
  (unless direction (setq direction 1))
  (when-let* ((result (if regexp?
                          (re-search-forward string limit t direction)
                        (search-forward string limit t direction))))
    (if (and visible?
             (or (invisible-p (match-beginning 0))
                 (invisible-p (1- (match-end 0)))))
        (helix-search string limit regexp? visible?)
      result)))

(defun helix-re-search-with-wrap (regexp &optional direction)
  "Search REGEXP from the point toward the DIRECTION.
If nothing found, wrap around the buffer and search up to the point."
  (unless direction (setq direction 1))
  (when (and (use-region-p)
             (/= direction (helix-region-direction)))
    (goto-char (mark-marker)))
  (or (re-search-forward regexp nil t direction)
      ;; If nothing found — wrap around buffer end and try again.
      (let ((point (point)))
        (goto-char (if (< direction 0) (point-max) (point-min)))
        (if (re-search-forward regexp point t direction)
            (message "Wrapped around buffer")))))

(defun helix-looking-at (string &optional direction regexp?)
  "Return t if text directly after point toward the DIRECTION
matches STRING.

If REGEXP? is non-nil STRING will be searched as regexp pattern,
otherwise it will be searched literally.

When REGEXP? is non-nil this function modifies the match data
that `match-beginning', `match-end' and `match-data' access."
  (unless direction (setq direction 1))
  (cond ((and regexp? (< 0 direction))
         (looking-at string))
        ((and regexp? (< direction 0))
         (looking-back string (line-beginning-position)))
        ((< 0 direction)
         (let* ((pnt (point))
                (pos (+ pnt (length string))))
           (and (<= pos (point-max))
                (string-equal (buffer-substring-no-properties pnt pos) string))))
        ((< direction 0)
         (let* ((pnt (point))
                (pos (- pnt (length string))))
           (and (<= (point-min) pos)
                (string-equal (buffer-substring-no-properties pos pnt)
                              string))))))

(defun helix-ends-with-newline (string)
  "Return t if STRING ends with newline character."
  (eql (aref string (1- (length string)))
       ?\n))

(defun helix-all-elements-are-equal-p (list)
  "Return t if all elemetns in the LIST are `equal' each other."
  (let ((first (-first-item list)))
    (--all? (equal first it)
            (cdr list))))

(defun helix-cursor-is-bar-p ()
  "Return non-nil if `cursor-type' is bar."
  (let ((cursor-type (if (eq cursor-type t)
                         (frame-parameter nil 'cursor-type)
                       cursor-type)))
    (or (eq cursor-type 'bar)
        (and (listp cursor-type)
             (eq (car cursor-type) 'bar)))))

(defun helix-set-region (start end &optional direction)
  "Make active region.
If DIRECTION is nil the direction of the region will be form START to END.
If DIRECTION is positive number — make forward region, negative number —
backward region. If DIRECTION is supplied, START and END positions can be
passed in any order."
  (cond ((or (null direction)
             (natnump direction))
         (set-mark start)
         (goto-char end))
        (t
         (goto-char start)
         (set-mark end))))

;; (cl-defun helix-set-region (start end &optional direction)
;;   "Make active region.
;; If DIRECTION is nil the direction of the region will be form START to END.
;; If DIRECTION is positive number — make forward region, negative number —
;; backward region. If DIRECTION is supplied, START and END positions can be
;; passed in any order."
;;   (when (and (numberp direction)
;;              (xor (< 0 direction)
;;                   (<= start end)))
;;     (cl-rotatef start end))
;;   (set-mark start)
;;   (goto-char end))

(defun helix-maybe-set-mark (&optional position)
  "Disable linewise selection, and set mark at POSITION unless extending
selection is active."
  (setq helix-linewise-selection nil)
  (unless helix--extend-selection
    (set-mark (or position (point)))))

(defun helix-maybe-deactivate-mark ()
  "Disable linewise selection, and deactivate mark unless extending
selection is active."
  (setq helix-linewise-selection nil)
  (unless helix--extend-selection
    (deactivate-mark)))

(defun helix-ensure-region-direction (direction)
  "Exchange point and mark if region direction mismatch DIRECTION.
DIRECTION should be 1 or -1."
  (when (and (use-region-p)
             (/= direction (helix-region-direction)))
    (helix--exchange-point-and-mark)))

(defun helix-undo-command-p (command)
  "Return non-nil if COMMAND is implementing undo/redo functionality."
  (memq command helix-undo-commands))

(defun helix-destructive-filter (predicate list &optional pointer)
  "Destructively remove elements in LIST that satisfy PREDICATE
between start and POINTER.

Returns the modified list, which may have a new starting element
if removals occur at the beginning of the list, therefore, assign
the returned list to the original symbol like this:

  (setq foo (helix-destructive-filter #\\='predicate foo))"
  (let ((tail list)
        elem head)
    (while (and tail (not (eq tail pointer)))
      (setq elem (car tail))
      (cond ((funcall predicate elem)
             (setq tail (cdr tail))
             (if head
                 (setcdr head tail)
               (setq list tail)))
            (t
             (setq head tail
                   tail (cdr tail)))))
    list))

(defun helix-echo (string &optional face)
  "Show message in echo area."
  (put-text-property 0 (length string) 'face face string)
  (message "%s" string))

(defun helix-pcre-to-elisp (regexp)
  "Convert PCRE REGEXP into Elisp one if Helix configured to use PCRE syntax."
  (if helix-use-pcre-regex
      (condition-case err
          (pcre-to-elisp regexp)
        (rxt-invalid-regexp
         (helix-echo (error-message-string err) 'error)))
    regexp))

(defun helix-match-bounds ()
  "Return cons cell with bounds of the first match group in `match-data'.
If there were no match groups in the last used regexp — return the bounds
of the full regexp match."
  (cond ((match-beginning 1)
         (cons (match-beginning 1) (match-end 1)))
        (t
         (cons (match-beginning 0) (match-end 0)))))

(defun helix-collect-positions (fun &optional start end)
  "Consecutively call FUN and collect point positions after each invocation.
Finish as soon as point moves outside of START END buffer positions.
FUN on each invocation should move point."
  (unless start (setq start (window-start)))
  (unless end (setq end (window-end)))
  (save-excursion
    (cl-loop with win = (get-buffer-window)
             for old-point = (point)
             do (ignore-errors
                  ;; Bind `last-command' and `this-command' to the same value,
                  ;; to get uniform result in case `fun' behaves differently
                  ;; depending on their values.
                  (let ((last-command fun)
                        (this-command fun))
                    (call-interactively fun)))
             while (and (not (eql (point) old-point))
                        (<= start (point) end))
             collect (cons (point) win))))

(defun helix-invert-case-in-region (start end)
  "Invert case of characters within START...END buffer positions."
  (goto-char start)
  (while (< (point) end)
    (let ((char (following-char)))
      (delete-char 1)
      (insert-char (if (eq (upcase char) char)
                       (downcase char)
                     (upcase char))))))

(defun helix-letters-are-self-insert-p ()
  "Return t if any of the a-z keys are bound to self-insert command."
  ;; (mapcar #'char-to-string (number-sequence ?a ?z))
  (cl-dolist (key '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
    (if-let* ((cmd (key-binding key))
              ((symbolp cmd))
              ((string-match-p "\\`.*self-insert.*\\'"
                               (symbol-name cmd))))
        (cl-return t))))

(defun helix-comment-at-pos-p (pos)
  "Return non-nil if position POS is inside a comment, or comment starts
right after the point."
  (ignore-errors
    ;; We cannot be in a comment if we are inside a string
    (unless (nth 3 (syntax-ppss pos))
      (or (nth 4 (syntax-ppss pos))
          ;; This test opening and closing comment delimiters... We need
          ;; to check that it is not newline, which is in "comment ender"
          ;; class in elisp-mode, but we just want it to be treated as
          ;; whitespace.
          (and (< pos (point-max))
               (memq (char-syntax (char-after pos)) '(?< ?>))
               (not (eq (char-after pos) ?\n)))
          ;; We also need to test the special syntax flag for comment
          ;; starters and enders, because `syntax-ppss' does not yet know if
          ;; we are inside a comment or not (e.g. / can be a division or
          ;; comment starter...).
          (when-let ((s (car (syntax-after pos))))
            (or
             ;; First char of 2 chars comment opener
             (and (/= 0 (logand (ash 1 16) s))
                  (nth 4 (syntax-ppss (+ pos 2))))
             ;; Second char of 2 chars comment opener
             (and (/= 0 (logand (ash 1 17) s))
                  (nth 4 (syntax-ppss (+ pos 1))))
             ;; First char of 2 chars comment closer
             (and (/= 0 (logand (ash 1 18) s))
                  (nth 4 (syntax-ppss (- pos 1))))
             ;; Second char of 2 chars comment closer
             (and (/= 0 (logand (ash 1 19) s))
                  (nth 4 (syntax-ppss (- pos 2))))))))))

(defun helix-overlay-live-p (overlay)
  "Return non-nil if OVERLAY is not deleted from buffer."
  (-some-> (overlay-buffer overlay)
    (buffer-live-p)))

(defun helix-carry-linewise-selection ()
  "If linewise selection is active adjust active region to include
newline character and disable linewise selection.

See `helix-linewise-selection'"
  (when helix-linewise-selection
    (helix-set-region (region-beginning) (1+ (region-end))
                      (helix-region-direction))
    (helix-delete-main-selection-overlay)
    (setq helix-linewise-selection nil)
    t))

(defun helix-maybe-enable-linewise-selection ()
  (let ((beg (region-beginning))
        (end (region-end)))
    (setq helix-linewise-selection (helix-logical-lines-p beg end))
    (when (and helix-linewise-selection
               (/= beg end))
      (helix-set-region beg (1- end) (helix-region-direction)))))

(defun helix-set-main-selection-overlay ()
  "Set overlay for main selection between BEG and END positions."
  (let ((beg (region-beginning))
        (end (1+ (region-end))))
    (if helix-main-selection-overlay
        (move-overlay helix-main-selection-overlay beg end)
      (setq helix-main-selection-overlay (-doto (make-overlay beg end)
                                           (overlay-put 'face 'region)
                                           (overlay-put 'priority 1))))))

(defun helix-disable-linewise-selection ()
  (when helix-linewise-selection
    (setq helix-linewise-selection nil)
    (unless helix-executing-command-for-fake-cursor
      (helix-delete-main-selection-overlay))))

(defun helix-delete-main-selection-overlay ()
  (when helix-main-selection-overlay
    (delete-overlay helix-main-selection-overlay)))

(defun helix-reveal-point-when-on-top (&rest _)
  "Reveal point when its only partly visible.
Emacs somewhy becomes very slow when point is only partly visible.
Intended to be use as `:after' advice."
  (unless helix-executing-command-for-fake-cursor
    (redisplay)
    (when (zerop (cdr (posn-col-row (posn-at-point))))
      (recenter 0))))

(declare-function helix-extend-selection "helix-commands")

(defun helix-keep-selection-a (fun &rest args)
  "Keep region active, disable extending selection (`v' key)."
  (let ((deactivate-mark nil))
    (apply fun args))
  (helix-extend-selection -1))

(defun helix-deactivate-mark (&rest _)
  "Deactivate mark. This function can be used as advice."
  (deactivate-mark))

(defun helix-jump-command (command &rest args)
  "Aroung advice for COMMAND that moves point."
  (helix-delete-all-fake-cursors)
  (deactivate-mark)
  (helix-with-recenter-point-on-jump
    (apply command args))
  ;; We can land in another buffer, so deactivate mark there as well.
  (deactivate-mark))

(provide 'helix-common)
;;; helix-common.el ends here
