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
(require 'dash)
(require 'thingatpt)
(provide 'pcre2el)
(require 'helix-vars)

;;; Macros

(defmacro helix--add-to-alist (alist &rest elements)
  "Add the association of KEY and VAL to the value of ALIST.
If the list already contains an entry for KEY, update that entry;
otherwise prepend it to the list.

\(fn ALIST [KEY VAL]...)"
  (declare (indent defun))
  `(progn
     ,@(cl-loop for (key val) on elements by #'cddr
                collect `(setf (alist-get ,key ,alist nil nil #'equal)
                               ,val))
     ,alist))

(defmacro helix-define-advice (symbol args &rest body)
  "This macro wraps `define-advice' and extends it to automatically
add/remove advice when `helix-mode' is toggled on or off.

\(fn SYMBOL (HOW LAMBDA-LIST &optional NAME) &rest BODY)"
  (declare (indent 2) (doc-string 3) (debug (sexp sexp def-body)))
  (unless (listp args)
    (signal 'wrong-type-argument (list 'listp args)))
  (unless (<= 2 (length args) 4)
    (signal 'wrong-number-of-arguments (list 2 4 (length args))))
  (let* ((how (nth 0 args))
         (lambda-list (nth 1 args))
         (name (or (nth 2 args) 'helix))
         (advice (intern (format "%s@%s" symbol name))))
    `(prog1 (defun ,advice ,lambda-list ,@body)
       (cl-pushnew '(,symbol ,how ,advice) helix--advices
                   :test #'equal)
       (when helix-mode
         (advice-add ',symbol ,how #',advice)))))

(defmacro helix-with-restriction (start end &rest body)
  "Evaluate BODY with the buffer narrowed to START and END."
  (declare (indent 2) (debug t))
  (let ((beg1 (gensym "beg"))
        (end1 (gensym "end")))
    `(if-let* ((,beg1 ,start)
               (,end1 ,end))
         (save-restriction
           (narrow-to-region ,beg1 ,end1)
           ,@body)
       ;; else
       ,@body)))

(defmacro helix-with-deactivate-mark (&rest body)
  "Evaluate BODY with mark temporary deactivated."
  (declare (debug t))
  `(prog2
       (deactivate-mark)
       (progn ,@body)
     (activate-mark)))

;;; Motions

(defun helix-forward-beginning-of-thing (thing &optional count skip-empty-lines)
  "Move to the beginning of next COUNT-th THING.
Move backward if COUNT is negative.

When SKIP-EMPTY-LINES is non-nil skip all blank lines along the way.
This is needed, for example, for `helix-word': two `helix-word's divided
with empty lines, are considered adjoined when moving over them.

Works only with THINGs, that returns the count of steps left to move,
such as `helix-word', `helix-sentence', `paragraph', `line'."
  (unless count (setq count 1))
  (cond ((= count 0) 0)
        ((< count 0)
         (forward-thing thing count))
        (t (when skip-empty-lines (skip-chars-forward "\r\n"))
           (when-let* ((bounds (bounds-of-thing-at-point thing))
                       ((< (point) (cdr bounds))))
             (goto-char (cdr bounds))
             (when skip-empty-lines (skip-chars-forward "\r\n")))
           (let ((rest (forward-thing thing count)))
             (cond ((zerop rest)
                    (forward-thing thing -1)
                    (when skip-empty-lines (skip-chars-backward "\r\n")))
                   ((eobp) (backward-char))) ; assuming that buffer ends with newline
             rest))))

(defun helix-skip-chars (chars &optional direction)
  "Move point toward the DIRECTION stopping after a char is not in CHARS string.
Move backward when DIRECTION is negative number, forward — otherwise.
Return t if point has moved."
  (unless direction (setq direction 1))
  (/= 0 (if (< direction 0)
            (skip-chars-backward chars)
          (skip-chars-forward chars))))

(defun helix-skip-whitespaces (&optional direction)
  "Move point toward the DIRECTION across whitespace.
Move backward when DIRECTION is negative number, forward — otherwise.
Return the distance traveled positive or negative depending on DIRECTION."
  (unless direction (setq direction 1))
  ;; Alternative: (helix-skip-chars " \t" dir)
  (if (< direction 0)
      (skip-syntax-backward " " (line-beginning-position))
    (skip-syntax-forward " " (line-end-position))
    ;; (let ((steps (skip-syntax-forward " " (line-end-position))))
    ;;   (if (/= steps 0)
    ;;       (let ((pnt (point)))
    ;;         (backward-prefix-chars)
    ;;         (+ steps (- (point) pnt)))
    ;;     steps))
    ))

(defun helix-next-char (&optional direction)
  "Return the next after point char toward the direction.
If DIRECTION is positive number get following char,
negative — preceding char."
  (unless direction (setq direction 1))
  (if (< direction 0) (preceding-char) (following-char)))

(defun helix-beginning-of-line ()
  "Move point to beginning of current line.
Use visual line when `visual-line-mode' is active."
  (if visual-line-mode
      (beginning-of-visual-line)
    (move-beginning-of-line nil)))

(defun helix-end-of-line ()
  (if visual-line-mode
      (end-of-visual-line)
    (move-end-of-line 1)))

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
  (let ((dir (pop spec))
        (count (pop spec))
        (n (gensym "n")))
    `(let ((,dir (helix-sign ,count))
           (,n ,count))
       (while (and (/= ,n 0)
                   (/= (point) (progn ,@body (point))))
         (setq ,n (- ,n ,dir)))
       ,n)))

;;; Things

;; `visual-line' thing
(put 'visual-line 'forward-op #'(lambda (&optional count)
                                  (vertical-motion (or count 1))))
;; (put 'visual-line 'beginning-op 'beginning-of-visual-line)
;; (put 'visual-line 'end-op       'end-of-visual-line)

(defun forward-helix-word (&optional count)
  "Move point forward COUNT words (backward if COUNT is negative).
Returns the count of word left to move, positive or negative depending
on sign of COUNT.

Word is:
- sequence of characters matching `[[:word:]]'
- sequence non-word non-whitespace characters matching `[^[:word:]\\n\\r\\t\\f ]'"
  (unless count (setq count 1))
  (helix-motion-loop (dir count)
    (helix-skip-chars "\r\n" dir)
    (helix-skip-whitespaces dir)
    (or (helix-line-boundary-p dir)
        (helix-skip-chars "^[:word:]\n\r\t\f " dir)
        (let ((word-separating-categories helix-cjk-word-separating-categories)
              (word-combining-categories  helix-cjk-word-combining-categories))
          (forward-word dir)))))

(defun forward-helix-WORD (&optional count)
  "Move point forward COUNT WORDs (backward if COUNT is negative).
Returns the count of WORD left to move, positive or negative depending
on sign of COUNT.

WORD is any space separated sequence of characters."
  (unless count (setq count 1))
  (helix-motion-loop (dir count)
    (helix-skip-chars "\r\n" dir)
    (helix-skip-whitespaces dir)
    (unless (helix-line-boundary-p dir)
      (helix-skip-chars "^\n\r\t\f " dir))))

(defun forward-helix-sentence (&optional count)
  "Move point forward COUNT sentences (backward if COUNT is negative).
Returns then count of sentences left to move, positive of negative depending
on sign of COUNT.

What is sentence is defined by `forward-sentence-function'."
  (unless count (setq count 1))
  (helix-motion-loop (dir count)
    (ignore-errors (forward-sentence dir))))

(defun forward-helix-function (&optional count)
  "Move point forward COUNT functions (backward if COUNT is negative).
Returns then count of sentences left to move, positive of negative depending
on sign of COUNT.

What is function is defined by `beginning-of-defun' and `end-of-defun'
functions."
  (unless count (setq count 1))
  (helix-motion-loop (dir count)
    (if (< dir 0) (beginning-of-defun) (end-of-defun))))

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
  (let ((bounds (bounds-of-thing-at-point thing)))
    (cond (bounds
           (set-mark (car bounds))
           (goto-char (cdr bounds))
           (setq count (1- count)))
          (t
           (forward-thing thing)
           (forward-thing thing -1)
           (set-mark (point)))))
  (forward-thing thing count))

(defun helix-mark-a-thing (thing)
  "Select a THING with spacing around.
Works only with THINGs, that returns the count of steps left to move,
such as `paragraph', `helix-function'."
  (-when-let ((thing-beg . thing-end) (bounds-of-thing-at-point thing))
    (-let (((beg . end)
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
                (cons thing-beg thing-end))))
      (helix-set-region beg end))))

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

(defun helix-bounds-of-string-at-point (quote-mark)
  "Return a cons cell (START . END) with bounds of string
enclosed in QUOTE-MARKs."
  (if-let* ((bounds (or (bounds-of-thing-at-point 'helix-comment)
                        (bounds-of-thing-at-point 'string))))
      (-let (((l _ _ r) (helix-4-bounds-of-surrounded-at-point quote-mark bounds)))
        (cons l r))
    ;; else
    (helix--bounds-of-quoted-at-point-ppss quote-mark)))

(defun helix-4-bounds-of-surrounded-at-point (pair &optional scope regexp? balanced?)
  "Return the bounds of the text region enclosed in LEFT and RIGHT.

LEFT and RIGHT should be strings. If they are different, then point
can be either: directly before LEFT, directly after RIGHT, or somewhere
between them. If LEFT and RIGHT are equal — point should between them.

The search is optionally bounded within the SCOPE: a cons cell with
\(LEFT-BOUND . RIGHT-BOUND) positions.

If REGEXP? is non-nil LEFT and RIGHT will be searched as regexp patterns
\(and clobber match data), else they will be searched literally.

If BALANCED? is non-nil all nested LEFT RIGHT pairs will be skipped.

Return the list (LEFT-BEG LEFT-END RIGHT-LEFT RIGHT-END) with
4 positions: before/after LEFT and before/after RIGHT.

\(fn (LEFT . RIGHT) &optional SCOPE REGEXP? BALANCED?)"
  (save-excursion
    (-let* (((left . right) pair)
            (balanced? (if (string-equal left right)
                           nil
                         balanced?)))
      (cond ((and balanced?
                  ;; Check if we can use Parse-Partial-Sexp Scanner
                  (and (length= left 1)
                       (length= right 1)
                       (eq (char-syntax (string-to-char left)) ?\()
                       (eq (char-syntax (string-to-char right)) ?\))))
             (if-let* ((bounds (helix-bounds-of-sexp-at-point pair)))
                 (-let (((l . r) bounds))
                   (list l (1+ l) (1- r) r))))
            (t
             (helix--4-bounds-of-surrounded-at-point-1 pair scope regexp?
                                                       balanced?))))))

(defun helix-bounds-of-sexp-at-point (pair)
  "Return the bounds of the balanced expression at point enclosed
in LEFT and RIGHT, for which the point is either: directly before
LEFT, directly after RIGHT, or inside. All nested balanced expressions
are skipped.

LEFT and RIGHT should be strings of one character, typically brackets,
for example: (\"{\" . \"}\").

This function was created to search balanced brackets in programming
modes, since uses Emacs built-in Parse-Partial-Sexp Scanner inside.
For arbitrary delimeters use `helix-4-bounds-of-surrounded-at-point'.

Return the cons cell (START . END) with positions before LEFT and
after RIGHT.

\(fn (LEFT . RIGHT))"
  (-let (((left . right) pair))
    (when (string-equal left right)
      (user-error "Left and right delimiters should not be equal"))
    (if-let* ((bounds (or (bounds-of-thing-at-point 'helix-comment)
                          (bounds-of-thing-at-point 'string)))
              ;; If inside comment or string use manual algorithm.
              (sexp-bounds (helix--4-bounds-of-surrounded-at-point-1 pair bounds nil t)))
        (-let (((l _ _ r) sexp-bounds))
          (cons l r))
      ;; Else if not or nothing have found — go out ...
      (when bounds (goto-char (car bounds)))
      ;; ... and try Parse-Partial-Sexp Scanner
      (save-excursion
        (let* ((pnt (point))
               (left  (string-to-char left))
               (right (string-to-char right))
               (syntax-table (if (and (eq (char-syntax left) ?\()
                                      (eq (char-syntax right) ?\)))
                                 (syntax-table)
                               (let ((st (copy-syntax-table (syntax-table))))
                                 (modify-syntax-entry left (format "(%c" right) st)
                                 (modify-syntax-entry right (format ")%c" left) st)
                                 st)))
               ;; Always use the default `forward-sexp-function'. This is
               ;; important for modes that use a custom one like `python-mode'.
               (forward-sexp-function nil))
          (with-syntax-table syntax-table
            (cond ((eq (following-char) left) ; point is before LEFT
                   (cons pnt
                         (progn (forward-sexp) (point))))
                  ((eq (preceding-char) right) ; point is after RIGHT
                   (cons (progn (backward-sexp) (point))
                         pnt))
                  (t
                   (condition-case nil
                       (while (progn (up-list -1 t)
                                     (/= (following-char) left)))
                     (error (goto-char pnt)))
                   (if (/= (point) pnt)
                       (cons (point)
                             (progn (forward-sexp) (point))))))))))))

(defun helix-4-bounds-of-sexp-at-point (pair)
  (if-let* ((bounds (helix-bounds-of-sexp-at-point pair)))
      (save-excursion
        (-let* (((left-beg . right-end) bounds)
                (left-end (progn
                            (goto-char (1+ left-beg))
                            (skip-chars-forward " \t\n")
                            (point)))
                (right-beg (progn
                             (goto-char (1- right-end))
                             (skip-chars-backward " \t\n")
                             (point))))
          (list left-beg left-end right-beg right-end)))))

(defun helix-bounds-of-inner-part-of-sexp-at-point (pair)
  (if-let* ((bounds (helix-4-bounds-of-sexp-at-point pair)))
      (-let (((_ l r _) bounds))
        (cons l r))))

(defun helix--4-bounds-of-surrounded-at-point-1 (pair &optional scope regexp? balanced?)
  "The internal function for `helix-4-bounds-of-surrounded-at-point'."
  (save-excursion
    (-let* (((left . right) pair)
            (left-not-equal-right? (not (string-equal left right))))
      (cond (;; point is before LEFT
             (and left-not-equal-right?
                  (helix-looking-at left 1 regexp?))
             (let* ((left-beg (point))
                    (left-end (if regexp? (match-end 0)
                                (+ left-beg (length left)))))
               (goto-char left-end)
               (if-let* ((right-end (helix-search-out pair 1 scope regexp? balanced?))
                         (right-beg (if regexp? (match-beginning 0)
                                      (- right-end (length right)))))
                   (list left-beg left-end right-beg right-end))))
            (;; point is after RIGHT
             (and left-not-equal-right?
                  (helix-looking-at right -1 regexp?))
             (let* ((right-end (point))
                    (right-beg (if regexp? (match-beginning 0)
                                 (- right-end (length right)))))
               (goto-char right-beg)
               (if-let* ((left-beg (helix-search-out pair -1 scope regexp? balanced?))
                         (left-end (if regexp? (match-end 0)
                                     (+ left-beg (length left)))))
                   (list left-beg left-end right-beg right-end))))
            (t
             (if-let* ((left-beg (helix-search-out pair -1 scope regexp? balanced?))
                       (left-end (if regexp? (match-end 0)
                                   (+ left-beg (length left))))
                       (right-end (helix-search-out pair 1 scope regexp? balanced?))
                       (right-beg (if regexp? (match-beginning 0)
                                    (- right-end (length right)))))
                 (list left-beg left-end right-beg right-end)))))))

(defun helix-looking-at (str &optional direction regexp?)
  "Return t if text directly after point toward the DIRECTION
matches string STR.

If REGEXP? is non-nil STR will be searched as regexp pattern,
otherwise it will be searched literally.

When REGEXP? is non-nil this function modifies the match data
that `match-beginning', `match-end' and `match-data' access."
  (unless direction (setq direction 1))
  (cond ((and regexp? (< 0 direction))
         (looking-at str))
        ((and regexp? (< direction 0))
         (looking-back str (line-beginning-position)))
        ((< 0 direction)
         (let* ((pnt (point))
                (pos (+ pnt (length str))))
           (and (<= pos (point-max))
                (string-equal (buffer-substring-no-properties pnt pos) str))))
        ((< direction 0)
         (let* ((pnt (point))
                (pos (- pnt (length str))))
           (and (<= (point-min) pos)
                (string-equal (buffer-substring-no-properties pos pnt)
                              str))))))

(defun helix-search-out (pair &optional direction scope regexp? balanced?)
  "This function assumes, that point is somewhere inside LEFT RIGHT
enclosed text region, and return the position before LEFT or after
RIGHT depending on DIRECTION.

LEFT and RIGHT should be strings.
DIRECTION should be either 1 — return the position after RIGHT,
or -1 — before LEFT.

The search is optionally bounded within SCOPE: a cons cell with
\(LEFT-BOUND . RIGHT-BOUND) positions.

If REGEXP? is non-nil LEFT and RIGHT will be searched as regexp patterns
\(and clobber match data), else they will be searched literally.

If BALANCED? is non-nil all nested LEFT RIGHT pairs on the way will
be skipped.

\(fn (LEFT . RIGHT) &optional SCOPE REGEXP? BALANCED?)"
  (unless direction (setq direction 1))
  (save-excursion
    (if balanced?
        (helix--search-out-balanced pair direction scope regexp?)
      (let ((str   (if (< direction 0) (car pair) (car pair)))
            (bound (if (< direction 0) (car scope) (cdr scope))))
        (helix-search str direction bound regexp?)))))

(defun helix--search-out-balanced (pair &optional direction scope regexp?)
  "This is an internal function for `helix-search-out' that is used
when BALANCED? argument is non-nil."
  (save-excursion
    (let (open close bound)
      (if (> direction 0)
          (-setq (open . close) pair
                 (_ . bound) scope)
        (-setq (close . open) pair
               (bound . _) scope))
      ;; The algorithm assume we are *inside* a pair: level of nesting is 1.
      (let ((level 1))
        (cl-block nil
          (while (> level 0)
            (let* ((pnt (point))
                   (open-pos (helix-search open direction bound regexp?))
                   (close-pos (progn
                                (goto-char pnt)
                                (helix-search close direction bound regexp?))))
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

(defun helix--syntax-ppss-string-quote-mark (state)
  "If the position corresponding to Parse-Partial-Sexp Scanner STATE
is inside a string, return quote-mark character that bounds that string."
  (nth 3 state))

(defalias 'helix--syntax-ppss-inside-string-p #'helix--syntax-ppss-string-quote-mark)

;;; Paste

(defun helix-yank-line-handler (text)
  "Insert the TEXT linewise."
  (pcase helix-this-command
    ('helix-paste-before (helix-beginning-of-line)
                         (set-marker (mark-marker) (point))
                         (insert text))
    ('helix-paste-after (helix-end-of-line)
                        (insert "\n")
                        (set-marker (mark-marker) (point))
                        (insert text)
                        (delete-char -1)) ; delete the last newline
    (_ (insert text))))

(defun helix-copy-line ()
  "Copy selection as line into `kill-ring'."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (text (filter-buffer-substring beg end))
         (yank-handler (list #'helix-yank-line-handler)))
    ;; Ensure the text ends with a newline. This is required
    ;; if the deleted lines were the last lines in the buffer.
    (when (or (zerop (length text))
              (/= (aref text (1- (length text))) ?\n))
      (setq text (concat text "\n")))
    (put-text-property 0 (length text) 'yank-handler yank-handler text)
    (kill-new text)))

;;; Utils

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

(defun helix-empty-line-p ()
  "Return t if point is at blank line."
  (and (bolp) (eolp)))

(defun helix-line-boundary-p (direction)
  "If DIRECTION is negative number, checks for beginning of line,
positive — end of line."
  (if (< direction 0) (bolp) (eolp)))

(defun helix-region-direction ()
  "Return the direction of region: -1 if point precedes mark, 1 otherwise."
  (let* ((point (point))
         (mark (or (mark t) point)))
    (if (< point mark) -1 1)))

(defun helix-exchange-point-and-mark ()
  "Exchange point and mark without activating the region."
  ;; (goto-char (prog1 (mark t)
  ;;              (set-marker (mark-marker) (point) (current-buffer))))
  (let* ((point (point))
         (mark  (or (mark t) point)))
    (set-marker (mark-marker) point)
    (goto-char mark)))

(defun helix-linewise-selection-p ()
  "Return non-nil if active region exactly spans whole line(s).
Returns symbol:
- `line' — if logical lines are selected;
- `visual-line' — if visual lines are selected."
  (when (use-region-p)
    (save-mark-and-excursion
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char beg)
        ;; First check for logical line, because visual line can be logical line
        ;; and then should be treated as logical line.
        (cond ((and (bolp) (save-excursion
                             (goto-char end)
                             (bolp)))
               'line)
              ((and visual-line-mode
                    (helix-visual-bolp) (save-excursion
                                          (goto-char end)
                                          (helix-visual-bolp)
                                          ;; (helix-visual-eolp)
                                          ))
               'visual-line))))))

(defun helix-empty-line-selected-p ()
  (and (bolp) (save-excursion
                (backward-char)
                (helix-empty-line-p))))

(defun helilx-whitespace? (char)
  "Non-nil when CHAR belongs to whitespace syntax class."
  ;; FIXME: Space syntax class can be denoted with both " " and "-" chars.
  ;; Are we shore that `char-syntax' always returns " "?
  (and (eql (char-syntax char) ?\s)
       (not (memq char '(?\r ?\n))))
  ;; Alternative: (memq char '(?\s ?\t))
  )

(defsubst helix-sign (&optional num)
  (cond ((< num 0) -1)
        ((zerop num) 0)
        (t 1)))

(defun helix-distance (x y) (abs (- y x)))

(defun helix-search (string &optional direction bound regexp? visible?)
  "Search for string STR toward the DIRECTION.

DIRECTION can be either 1 — search forward, or -1 — search backward.

BOUND optionally bounds the search. It should be a position that
is *after* the point if DIRECTION is positive, and *before* the
point — if negative.

If REGEXP? is non-nil STR will be searched as regexp pattern,
else it will be searched literally.

If VISIBLE? is non-nil skip invisible matches.

When REGEXP? is non-nil this function modifies the match data
that `match-beginning', `match-end' and `match-data' access."
  (unless direction (setq direction 1))
  (when-let* ((result (if regexp?
                          (re-search-forward string bound t direction)
                        (search-forward string bound t direction))))
    (if (and visible?
             (or (invisible-p (match-beginning 0))
                 (invisible-p (1- (match-end 0)))))
        (helix-search string bound regexp? visible?)
      result)))

(defun helix-re-search-with-wrap (regexp &optional direction)
  "Search REGEXP from the point toward the DIRECTION.
If nothing found, wrap around the buffer and search up to the point."
  (unless direction (setq direction 1))
  (when (and (use-region-p)
             (not (eql direction (helix-region-direction))))
    (goto-char (mark-marker)))
  (or (re-search-forward regexp nil t direction)
      ;; If nothing found — wrap around buffer end and try again.
      (let ((point (point)))
        (goto-char (if (< direction 0) (point-max) (point-min)))
        (if (re-search-forward regexp point t direction)
            (message "Wrapped around buffer")))))

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

;; FIXME: Is it really faster?
(defun helix-line-number-at-pos (&optional pos absolute)
  "Faster implementation of `line-number-at-pos'."
  (if pos
      (save-excursion
        (if absolute
            (save-restriction
              (widen)
              (goto-char pos)
              (string-to-number (format-mode-line "%l")))
          (goto-char pos)
          (string-to-number (format-mode-line "%l"))))
    (string-to-number (format-mode-line "%l"))))

(defun helix-set-region (start end &optional direction)
  (unless direction (setq direction 1))
  (cond ((< direction 0)
         (goto-char start)
         (set-mark end))
        (t
         (goto-char end)
         (set-mark start))))

(defun helix-ensure-region-direction (direction)
  "Exchange point and mark if region direction mismatch DIRECTION.
DIRECTION should be 1 or -1."
  (when (use-region-p)
    (unless (eql (helix-region-direction)
                 direction)
      (helix-exchange-point-and-mark))))

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

(defun helix-pcre-to-elisp (regexp)
  "Convert PCRE REGEXP into Elisp one if Helix configured to use PCRE syntax."
  (if helix-use-pcre-regex
      (condition-case err
          (pcre-to-elisp regexp)
        (rxt-invalid-regexp
         (let ((string (error-message-string err)))
           (put-text-property 0 (length string) 'face 'error string)
           (message "%s" string))))
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
    ;; (save-excursion)
    ;; We cannot be in a comment if we are inside a string
    (unless (nth 3 (syntax-ppss pos))
      (or (nth 4 (syntax-ppss pos))
          ;; this also test opening and closing comment delimiters... we
          ;; need to check that it is not newline, which is in "comment
          ;; ender" class in elisp-mode, but we just want it to be treated
          ;; as whitespace
          (and (< pos (point-max))
               (memq (char-syntax (char-after pos)) '(?< ?>))
               (not (eq (char-after pos) ?\n)))
          ;; we also need to test the special syntax flag for comment
          ;; starters and enders, because `syntax-ppss' does not yet know if
          ;; we are inside a comment or not (e.g. / can be a division or
          ;; comment starter...).
          (when-let ((s (car (syntax-after pos))))
            (or (and (/= 0 (logand (ash 1 16) s))
                     (nth 4 (syntax-ppss (+ pos 2))))
                (and (/= 0 (logand (ash 1 17) s))
                     (nth 4 (syntax-ppss (+ pos 1))))
                (and (/= 0 (logand (ash 1 18) s))
                     (nth 4 (syntax-ppss (- pos 1))))
                (and (/= 0 (logand (ash 1 19) s))
                     (nth 4 (syntax-ppss (- pos 2))))))))))

(provide 'helix-common)
;;; helix-common.el ends here
