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

;;; Macros

(defmacro helix--add-to-alist (alist &rest elements)
  "Add the association of KEY and VAL to the value of ALIST.
If the list already contains an entry for KEY, update that entry;
otherwise prepend it to the list.

\(fn ALIST [KEY VAL]...)"
  `(progn
     ,@(cl-loop
        for (key val) on elements by #'cddr collect
        `(setf (alist-get ,key ,alist nil nil #'equal) ,val))
     ,alist))

(defmacro helix-with-restriction (restrictions &rest body)
  "Execute BODY with the buffer narrowed to BEG and END.

\(fn (BEG . END) BODY...)"
  (declare (indent defun) (debug t))
  (let ((beg (gensym "beg"))
        (end (gensym "end")))
    `(cl-destructuring-bind (,beg . ,end) ,restrictions
       (save-restriction
         (narrow-to-region ,beg ,end)
         ,@body))))

;;; Motions

(defun helix-forward-beginning-of-thing (thing &optional count skip-empty-lines)
  "Move to the beginning of next COUNT-th THING.
Move backward if COUNT is negative.

When SKIP-EMPTY-LINES is non-nil skip all blank lines along the way.
This is needed, for example, for `helix-word': two `helix-word's divided
with empty lines, are considered adjoined when moving over them.

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
  "Move point toward the DIRECTION stopping after a char is not in CHARS string.
Move backward when DIRECTION is negative number, forward — otherwise.
Return t if point has moved."
  (or direction (setq direction 1))
  (/= 0 (if (< direction 0)
            (skip-chars-backward chars)
          (skip-chars-forward chars))))

(defun helix-skip-whitespaces (&optional direction)
  "Move point toward the DIRECTION across whitespace.
Move backward when DIRECTION is negative number, forward — otherwise.
Return the distance traveled positive or negative depending on DIRECTION."
  (or direction (setq direction 1))
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
  (or direction (setq direction 1))
  (if (< direction 0) (preceding-char) (following-char)))

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
           (,n ,count))
       (while (and (/= ,n 0)
                   (/= (point) (progn ,@body (point))))
         (setq ,n (- ,n ,direction)))
       ,n)))

;;; Things

(defun forward-helix-word (&optional count)
  "Move point forward COUNT words (backward if COUNT is negative).
Returns the count of word left to move, positive or negative depending
on sign of COUNT.

Word is:
- sequence of characters matching `[[:word:]]'
- sequence non-word non-whitespace characters matching `[^[:word:]\\n\\r\\t\\f ]'"
  (or count (setq count 1))
  (helix-motion-loop (dir count)
    (helix-skip-chars "\r\n" dir)
    (helix-skip-whitespaces dir)
    (or (helix-beginning-or-end-of-line-p dir)
        (helix-skip-chars "^[:word:]\n\r\t\f " dir)
        (let ((word-separating-categories helix-cjk-word-separating-categories)
              (word-combining-categories  helix-cjk-word-combining-categories))
          (forward-word dir)))))

(defun forward-helix-WORD (&optional count)
  "Move point forward COUNT WORDs (backward if COUNT is negative).
Returns the count of WORD left to move, positive or negative depending
on sign of COUNT.

WORD is any space separated sequence of characters."
  (or count (setq count 1))
  (helix-motion-loop (dir count)
    (helix-skip-chars "\r\n" dir)
    (helix-skip-whitespaces dir)
    (unless (helix-beginning-or-end-of-line-p dir)
      (helix-skip-chars "^\n\r\t\f " dir))))

(put 'visual-line 'forward-op #'(lambda (&optional count)
                                  (vertical-motion (or count 1))))
;; (put 'visual-line 'beginning-op 'beginning-of-visual-line)
;; (put 'visual-line 'end-op       'end-of-visual-line)

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

;;; Selection

(defun helix-mark-inner-thing (thing &optional count)
  (or count (setq count 1))
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

(defun helix-bounds-of-string-at-point (quote-mark)
  "Return a cons cell (START . END) with bounds of string
enclosed in QUOTE-MARKs."
  (let (bounds)
    (cond ((setq bounds (bounds-of-thing-at-point 'helix-comment))
           (helix-bounds-of-enclosed-text-at-point quote-mark bounds))
          ((setq bounds (bounds-of-thing-at-point 'string))
           (if (eq (char-after (car bounds)) quote-mark)
               bounds
             (helix-bounds-of-enclosed-text-at-point quote-mark bounds)))
          (t
           (helix--bounds-of-quoted-at-point-ppss quote-mark)))))

(defun helix-bounds-of-enclosed-text-at-point (delimiter &optional limits)
  "Return the bounds of the text region enclosed in DELIMITERs.

The point should be inside this text region. DELIMITER should be
a string or a character. The search is bounded within LIMITS:
a cons cell with (LEFT . RIGHT) positions.

Return the cons cell (START . END) with positions before the openning
DELIMITER and after the closing one."
  (when (characterp delimiter) (setq delimiter (string delimiter)))
  (save-excursion
    (pcase-let ((pnt (point))
                (`(,left . ,right) limits))
      (if-let* ((beg (search-backward delimiter left t))
                (end (progn
                       (goto-char pnt)
                       (search-forward delimiter right t))))
          (cons beg end)))))

(defun helix-bounds-of-sexp-at-point (left right)
  "Return the bounds of the balanced expression at point enclosed
in LEFT and RIGHT, for which the point is in the position directly
before the LEFT delimiter, or inside. All nested balanced expressions
are skipped.

LEFT and RIGHT should be characters. Return the cons cell (START . END)
with positions before LEFT and after RIGHT."
  (if-let* ((bounds (or (bounds-of-thing-at-point 'helix-comment)
                        (bounds-of-thing-at-point 'string)))
            ;; If inside comment or string use manual algorithm.
            (sexp-bounds (helix--bounds-of-balanced-sexp-at-point left right bounds)))
      sexp-bounds
    ;; Else if not or nothing found go out ...
    (when bounds (goto-char (car bounds)))
    ;; ... and try Parse-Partial-Sexp Scanner
    (helix--bounds-of-balanced-sexp-at-point-ppss left right)))

(defun helix--bounds-of-balanced-sexp-at-point-ppss (left right)
  "Return the bounds of the balanced expression at point enclosed
in LEFT and RIGHT, for which the point is either: directly before
LEFT, directly after RIGHT, or inside.

All nested balanced expressions are skipped.
Internally uses Emacs built-in Parse-Partial-Sexp Scanner.

LEFT and RIGHT should be characters, typically parenthesis.
Return the cons cell (START . END) with positions before LEFT and
after RIGHT."
  (save-excursion
    (let ((syntax-table (if (and (eq (char-syntax left) ?\()
                                 (eq (char-syntax right) ?\)))
                            (syntax-table)
                          (let ((st (copy-syntax-table (syntax-table))))
                            (modify-syntax-entry left (format "(%c" right) st)
                            (modify-syntax-entry right (format ")%c" left) st)
                            st)))
          ;; Always use the default `forward-sexp-function'. This is important
          ;; for modes that use a custom one like `python-mode'.
          forward-sexp-function)
      (with-syntax-table syntax-table
        (cond ((= (following-char) left) ; point is before LEFT
               (cons (point)
                     (progn (forward-sexp) (point))))
              ((= (preceding-char) right) ; point is after RIGHT
               (let ((end (point)))
                 (cons (progn (backward-sexp) (point))
                       end)))
              (t
               (let ((pnt (point)))
                 (condition-case nil
                     (while (progn (up-list -1 t)
                                   (/= (following-char) left)))
                   (error (goto-char pnt)))
                 (if (/= (point) pnt)
                     (cons (point)
                           (progn (forward-sexp) (point)))))))))))

(defun helix--bounds-of-balanced-sexp-at-point (left right limits)
  "Return the bounds of the balanced expression at point enclosed
in LEFT and RIGHT, for which the point is either: directly before
LEFT, directly after RIGHT, or inside. All nested balanced expressions
are skipped.

LEFT and RIGHT can be strings or characters. The search is bounded
within LIMITS: a cons cell with \(LEFT-LIMIT . RIGHT-LIMIT) positions.

Return the cons cell (START . END) with positions before LEFT and
after RIGHT."
  (when (characterp left) (setq left (string left)))
  (when (characterp right) (setq right (string right)))
  (cl-destructuring-bind (left-limit . right-limit) limits
    (save-excursion
      (let ((pnt (point)))
        (cond ((helix-looking-at left) ; point is before LEFT
               (forward-char (length left))
               (if-let* ((end (helix--scan-balanced left right 1 right-limit)))
                   (cons pnt end)))
              ((helix-looking-back right) ; point is after RIGHT
               (backward-char (length right))
               (if-let* ((beg (helix--scan-balanced left right -1 left-limit)))
                   (cons beg pnt)))
              (t
               (if-let* ((beg (helix--scan-balanced left right -1 left-limit))
                         (end (helix--scan-balanced left right 1 right-limit)))
                   (cons beg end))))))))

(defun helix--scan-balanced (left right &optional direction limit)
  "Return the position after the closing delimiter toward the DIRECTION
skipping all balanced LEFT RIGHT pairs on the way.

LEFT and RIGHT should be strings. If DIRECTION is positive number,
return the position after RIGHT, negative — before LEFT."
  (save-excursion
    (let ((open  (if (< direction 0) right left))
          (close (if (< direction 0) left right)))
      ;; The algorithm assume we are *inside* a pair: level of nesting is 1.
      (let ((level 1))
        (cl-block nil
          (while (> level 0)
            (let* ((pnt (point))
                   (close-pos (search-forward close limit t direction))
                   (open-pos (progn
                               (goto-char pnt)
                               (search-forward open limit t direction))))
              (if (and close-pos open-pos)
                  (let ((close-dist (helix-distance pnt close-pos))
                        (open-dist  (helix-distance pnt open-pos)))
                    (cond ((< open-dist close-dist)
                           (setq level (1+ level))
                           (goto-char open-pos))
                          (t
                           (setq level (1- level))
                           (goto-char close-pos))))
                ;; else
                (when close-pos
                  (setq level (1- level))
                  (goto-char close-pos))
                (cl-return)))))
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


;;; Utils

(defun helix-exchange-point-and-mark ()
  "Exchange point and mark without activating the region."
  (let* ((point (point))
         (mark  (or (mark t) point)))
    (set-marker (mark-marker) point)
    (goto-char mark)))

(defun helix-region-direction ()
  "Return the direction of region: -1 if point precedes mark, 1 otherwise."
  (let* ((point (point))
         (mark (or (mark t) point)))
    (if (< point mark) -1 1)))

(defun helix-beginning-or-end-of-line-p (direction)
  "DIRECTION should be a number. If DIRECTION is negative,
checks for beginning of line, positive — end of line."
  (if (< direction 0) (bolp) (eolp)))

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

(defun helix-line-selected-p ()
  "Check that the selection is a multiple of whole lines.
Return symbol:
- `line' — if logical lines are selected;
- `visual-line' — if visual lines are selected;
- nil — otherwise."
  (if (use-region-p)
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
             'visual-line))))

(defun helilx-whitespace? (char)
  "Non-nil when CHAR belongs to whitespace syntax class."
  ;; FIXME: Space syntax class can be denoted with both " " and "-" chars.
  ;; Are we shore that `char-syntax' always returns " "?
  (eql (char-syntax char) ?\s)
  ;; Alternative: (memq char '(?\s ?\t))
  )

(defun helix-looking-at (str)
  "Return t if text after point literally matches string STR."
  (let* ((pnt (point))
         (pos (+ pnt (length str))))
    (and (<= pos (point-max))
         (string-equal (buffer-substring-no-properties pnt pos) str))))

(defun helix-looking-back (str)
  "Return t if text before point literally matches string STR."
  (let* ((pnt (point))
         (pos (- pnt (length str))))
    (and (<= (point-min) pos)
         (string-equal (buffer-substring-no-properties pos pnt)
                       str))))

(defun helix-comment-at-point-p ()
  "Return non-nil if point is inside a comment, or comment starts
right after the point."
  (ignore-errors
    (save-excursion
      ;; We cannot be in a comment if we are inside a string
      (unless (nth 3 (syntax-ppss))
        (let ((pnt (point)))
          (or (nth 4 (syntax-ppss))
              ;; this also test opening and closing comment delimiters... we
              ;; need to check that it is not newline, which is in "comment
              ;; ender" class in elisp-mode, but we just want it to be treated
              ;; as whitespace
              (and (< pnt (point-max))
                   (memq (char-syntax (char-after pnt)) '(?< ?>))
                   (not (eq (char-after pnt) ?\n)))
              ;; we also need to test the special syntax flag for comment
              ;; starters and enders, because `syntax-ppss' does not yet know if
              ;; we are inside a comment or not (e.g. / can be a division or
              ;; comment starter...).
              (when-let ((s (car (syntax-after pnt))))
                (or (and (/= 0 (logand (ash 1 16) s))
                         (nth 4 (syntax-ppss (+ pnt 2))))
                    (and (/= 0 (logand (ash 1 17) s))
                         (nth 4 (syntax-ppss (+ pnt 1))))
                    (and (/= 0 (logand (ash 1 18) s))
                         (nth 4 (syntax-ppss (- pnt 1))))
                    (and (/= 0 (logand (ash 1 19) s))
                         (nth 4 (syntax-ppss (- pnt 2))))))))))))

(defsubst helix-sign (&optional num)
  (cond ((< num 0) -1)
        ((zerop num) 0)
        (t 1)))

(defun helix-distance (x y) (abs (- y x)))

(defun helix-skip-gap (thing &optional direction)
  (or direction (setq direction 1))
  (when-let* ((bounds (helix-bounds-of-complement-of-thing-at-point thing)))
    (goto-char (if (< direction 0)
                   (car bounds)
                 (cdr bounds)))))

(provide 'helix-common)
;;; helix-common.el ends here
