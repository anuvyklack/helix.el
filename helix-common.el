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
  (cl-destructuring-bind (left . right) limits
    (save-excursion
      (let ((pnt (point)))
        (if-let* ((beg (search-forward delimiter left t))
                  (end (progn
                         (goto-char pnt)
                         (search-backward delimiter right t))))
            (cons beg end))))))

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

(defsubst helix-sign (&optional num)
  (cond ((< num 0) -1)
        ((zerop num) 0)
        (t 1)))

(defun helix-skip-gap (thing &optional direction)
  (or direction (setq direction 1))
  (when-let* ((bounds (helix-bounds-of-complement-of-thing-at-point thing)))
    (goto-char (if (< direction 0)
                   (car bounds)
                 (cdr bounds)))))

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

(provide 'helix-common)
;;; helix-common.el ends here
