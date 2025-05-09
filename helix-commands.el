;;; helix-commands.el --- Helix commands -*- lexical-binding: t; -*-
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
;;  Helix commands
;;
;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(require 's)
(require 'multiple-cursors-core)
(require 'helix-common)
(require 'helix-vars)

(defun helix-normal-state-escape ()
  "Command for ESC key in Helix Normal state."
  (interactive)
  (cond (helix--extend-selection
         (setq helix--extend-selection nil))
        (t
         (mc/execute-command-for-all-cursors #'helix-collapse-selection))))

;;; Movements

;; h
(defun helix-backward-char (count)
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (backward-char count))

;; l
(defun helix-forward-char (count)
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (forward-char count))

;; j
(defun helix-next-line (count)
  "Move to the next line."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  ;; Preserve the column: the behaviour is hard-coded and the column
  ;; is preserved if and only if the last command was `next-line' or
  ;; `previous-line'.
  (setq this-command 'next-line)
  (funcall-interactively 'next-line count))

;; k
(defun helix-previous-line (count)
  "Move to the COUNT-th previous line."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  ;; Preserve the column: the behaviour is hard-coded and the column
  ;; is preserved if and only if the last command was `next-line' or
  ;; `previous-line'.
  (setq this-command 'previous-line)
  (funcall-interactively 'previous-line count))

;; w
(defun helix-forward-word-start (count &optional bigword)
  "Move to the COUNT-th next word start.
If BIGWORD move over WORD-s."
  (interactive "p")
  (let ((thing (if bigword 'helix-WORD 'helix-word)))
    (when (zerop (forward-thing thing (1- count)))
      (if helix--extend-selection
          (or (region-active-p) (set-mark (point)))
        (skip-chars-forward "\r\n")
        (set-mark (point)))
      (or (helilx-whitespace? (following-char))
          (forward-thing thing))
      (helix-skip-whitespaces))))

;; W
(defun helix-forward-WORD-start (count)
  "Move to the COUNT-th next WORD start."
  (interactive "p")
  (helix-forward-word-start count :bigword))

;; b
(defun helix-backward-word-start (count &optional bigword)
  "Move to the COUNT-th previous word start.
If BIGWORD move over WORD-s."
  (interactive "p")
  (setq count (- count))
  (let ((thing (if bigword 'helix-WORD 'helix-word)))
    (when (zerop (forward-thing thing (1+ count)))
      (if helix--extend-selection
          (or (region-active-p) (set-mark (point)))
        (skip-chars-backward "\r\n")
        (set-mark (point)))
      (forward-thing thing -1))))

;; B
(defun helix-backward-WORD-start (count)
  "Move to the COUNT-th previous WORD start."
  (interactive "p")
  (helix-backward-word-start count :bigword))

;; e
(defun helix-forward-word-end (count &optional bigword)
  "Move to the COUNT-th next word end.
If BIGWORD move over WORD-s."
  (interactive "p")
  (let ((thing (if bigword 'helix-WORD 'helix-word)))
    (when (zerop (forward-thing thing (1- count)))
      (if helix--extend-selection
          (or (region-active-p) (set-mark (point)))
        (skip-chars-forward "\r\n")
        (set-mark (point)))
      (forward-thing thing))))

;; E
(defun helix-forward-WORD-end (count)
  "Move COUNT-th next WORD end."
  (interactive "p")
  (helix-forward-word-end count :bigword))

;; gg
(defun helix-goto-first-line (num)
  "Move point to the beginning of the buffer.
With numeric arg NUM, put point NUM/10 of the way from the beginning.
If the buffer is narrowed, this command uses the beginning of the
accessible part of the buffer.
Push mark at previous position, unless extending selection."
  (interactive "P")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    ;; else
    (set-marker (mark-marker) (point))
    (deactivate-mark))
  (push-mark)
  (goto-char (if num (+ (point-min)
                        (/ (* (- (point-max) (point-min))
                              (prefix-numeric-value num))
                           10))
               (point-min)))
  (if num (forward-line 1)
    (recenter 0)))

;; G
(defun helix-goto-last-line ()
  "Move point the end of the buffer."
  (interactive)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    ;; else
    (set-marker (mark-marker) (point))
    (deactivate-mark))
  (push-mark)
  (goto-char (point-max)))

;; gs
(defun helix-beginning-of-line ()
  "Move point to beginning of current line.
Use visual line when `visual-line-mode' is on."
  (interactive)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (if visual-line-mode
      (beginning-of-visual-line)
    (beginning-of-line)))

;; gh
(defun helix-first-non-blank ()
  "Move point to beginning of current line skipping indentation.
Use visual line when `visual-line-mode' is on."
  (interactive)
  (helix-beginning-of-line)
  (skip-syntax-forward " " (line-end-position))
  (backward-prefix-chars))

;; gl
(defun helix-end-of-line ()
  "Move point to end of current line.
Use visual line when `visual-line-mode' is on."
  (interactive)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (if visual-line-mode
      (end-of-visual-line)
    (move-end-of-line 1)))

;; ]p
(defun helix-forward-paragraph (count)
  "Move to the end of the COUNT next paragraph."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (forward-thing 'paragraph count))

;; [p
(defun helix-backward-paragraph (count)
  "Move to the beginning of the COUNT previous paragraph."
  (interactive "p")
  (setq count (- count))
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (forward-thing 'paragraph count))

;;; Changes

;; i
(defun helix-insert ()
  "Switch to Insert state before region."
  (interactive)
  (when (and (use-region-p)
             (< (mark) (point)))
    (helix-exchange-point-and-mark))
  (helix-insert-state 1))

;; a
(defun helix-append ()
  "Switch to Insert state after region."
  (interactive)
  (when (and (use-region-p)
             (< (point) (mark)))
    (helix-exchange-point-and-mark))
  (helix-insert-state 1))

;; c
(defun helix-change ()
  "Delete region and enter Insert state."
  (interactive)
  (if (use-region-p)
      (let ((line-selected? (helix-line-selected-p)))
        (kill-region nil nil t)
        (pcase line-selected?
          ('line (save-excursion (newline))
                 (indent-according-to-mode))
          ('visual-line (save-excursion (insert " ")))))
    ;; no region
    (delete-char -1))
  (helix-insert-state 1))

(defun helix-collapse-selection ()
  "Collapse region onto a single cursor."
  (interactive)
  (deactivate-mark))

;; d
(defun helix-delete ()
  "Delete text in region.
With no region delete char before point with next conditions:
- If point is surrounded by (balanced) whitespace and a brace delimiter
  ({} [] ()), delete a space on either side of the cursor.
- If point is at BOL and surrounded by braces on adjacent lines,
  collapse newlines:
  {
  |
  } => {|} "
  (interactive)
  (if (use-region-p)
      (kill-region nil nil t)
    (delete-char -1))
  (setq helix--extend-selection nil))

;; u
(defun helix-undo ()
  "Cancel current region then undo."
  (interactive)
  (deactivate-mark)
  (undo))

;;; Selection

;; v
(defun helix-extend-selection ()
  "Toggle extend selection."
  (interactive)
  (setq helix--extend-selection (not helix--extend-selection)))

;; x
(defun helix-mark-line (count)
  "Select COUNT lines.

Select visual lines when `visual-line-mode' is on.
Select upward if COUNT is negative.

If current selection already consists of whole lines and COUNT is positive,
expand the selection by COUNT lines upward if point is at the beginning
of the region, or downward if at the end."
  (interactive "p")
  (let* ((line (if visual-line-mode 'visual-line 'line))
         (dir (helix-sign count)))
    (cond ((use-region-p)
           (unless (helix-line-selected-p)
             (let ((b (region-beginning))
                   (e (region-end)))
               (goto-char b)
               (set-mark (car (bounds-of-thing-at-point line))) ; left end
               (goto-char e)
               (goto-char (cdr (bounds-of-thing-at-point line))) ; right end
               (setq count (- count dir))))
           (let ((rd (helix-region-direction)))
             (cond ((< rd 0 dir) (setq count (- count)))
                   ((< dir 0 rd) (helix-exchange-point-and-mark))))
           (forward-thing line count))
          ;; no region
          (t (let ((bounds (bounds-of-thing-at-point line)))
               (set-mark (car bounds))
               (goto-char (cdr bounds))
               (if (< dir 0) (helix-exchange-point-and-mark))
               (forward-thing line (- count dir)))))))

;; X
(defun helix-mark-line-upward (count)
  "Select COUNT lines upward.
Select visual lines when `visual-line-mode' is on."
  (interactive "p")
  (helix-mark-line (- count)))

;; _
(defun helix-trim-whitespaces-from-selection ()
  "Trim whitespaces and newlines from the both ends of the current selection."
  (interactive)
  (when (use-region-p)
    (let ((dir (if (< (point) (mark)) -1 1)))
      (helix-skip-chars " \t\r\n" (- dir))
      (helix-exchange-point-and-mark)
      (helix-skip-chars " \t\r\n" dir)
      (helix-exchange-point-and-mark))))

;;; Match

(defun helix-match-map-digit-argument (arg)
  "Like `digit-argument' but keep `helix-match-map' active."
  (interactive "P")
  (digit-argument arg)
  (set-transient-map helix-match-map))

;; Don't show `helix-digit-argument-for-match-map' in which-key popup.
(with-eval-after-load 'which-key
  (defvar which-key-replacement-alist)
  (cl-pushnew '((nil . "helix-digit-argument-for-match-map") . ignore)
              which-key-replacement-alist))

;; miw
(defun helix-mark-inner-word (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-word count))

;; miW
(defun helix-mark-inner-WORD (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-WORD count))

;; mip
(defun helix-mark-inner-paragraph (count)
  (interactive "p")
  (helix-mark-inner-thing 'paragraph count))

;; mi"
(defun helix-mark-inner-double-quoted ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-string-at-point ?\")))
    (set-mark (1+ (car bounds)))
    (goto-char (1- (cdr bounds)))))

;; ma"
(defun helix-mark-a-double-quoted ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-string-at-point ?\")))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; mi'
(defun helix-mark-inner-single-quoted ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-string-at-point ?')))
    (set-mark (1+ (car bounds)))
    (goto-char (1- (cdr bounds)))))

;; ma'
(defun helix-mark-a-single-quoted ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-string-at-point ?')))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; mi`
(defun helix-mark-inner-back-quoted ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-string-at-point ?`)))
    (set-mark (1+ (car bounds)))
    (goto-char (1- (cdr bounds)))))

;; ma`
(defun helix-mark-a-back-quoted ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-string-at-point ?`)))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; mi( mi)
(defun helix-mark-inner-paren ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-inner-part-of-sexp-at-point '("(" . ")"))))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; ma( ma)
(defun helix-mark-a-paren ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-sexp-at-point '("(" . ")"))))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; mi[ mi]
(defun helix-mark-inner-bracket ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-inner-part-of-sexp-at-point '("[" . "]"))))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; ma[ ma]
(defun helix-mark-a-bracket ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-sexp-at-point '("[" . "]"))))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; mi{ mi}
(defun helix-mark-inner-curly ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-inner-part-of-sexp-at-point '("{" . "}"))))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; ma{ ma}
(defun helix-mark-a-curly ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-sexp-at-point '("{" . "}"))))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; mi< mi>
(defun helix-mark-inner-angle ()
  (interactive)
  (let ((pair '("<" . ">"))
        (limits (bounds-of-thing-at-point 'defun)))
    (when-let* ((bounds (helix-4-bounds-of-surrounded-at-point pair limits nil t)))
      (pcase-let ((`(,_ ,l ,r ,_) bounds))
        (set-mark l)
        (goto-char r)))))

;; ma< ma>
(defun helix-mark-an-angle ()
  (interactive)
  (let ((pair '("<" . ">"))
        (limits (bounds-of-thing-at-point 'defun)))
    (when-let* ((bounds (helix-4-bounds-of-surrounded-at-point pair limits nil t)))
      (pcase-let ((`(,l ,_ ,_ ,r) bounds))
        (set-mark l)
        (goto-char r)))))

;;; Surround

(defun helix-surround-add-pair (key insert &optional search regexp? balanced?)
  "Add a new insert-delete pattern for Helix surround functionality.
KEY       - To what KEY this pattern should be binded?
INSERT    - Cons cell (LEFT . RIGHT) with strings, or function that returns such
            cons cell. The strigs that will be inserted by `helix-surround' and
            `helix-surround-change' functions.
SEARCH    - Any of:
            1. Cons cell with strings (LEFT . RIGHT). Should be patterns that
               will be used to search of two substrings to delete in
               `helix-surround-delete' and `helix-surround-change'functions.
               If not specified INSERT pair will be used.
            2. Function that return cons cell with strings (LEFT . RIGHT) like
               in 1.
            3. Function that returns list
                        (LEFT-START LEFT-END RIGHT-START RIGHT-END)
               with 4 positions of START/END of LEFT and RIGHT delimeters.
               Example:
                          left             right
                        |<tag> |Some text| </tag>|
                        ^      ^         ^       ^
                        ls     le        rs      re

Following parameters are taken into account only when SEARCH argument is a cons
cell with stirngs (LEFT . RIGHT) or a function, that returns such cons cell. If
SEARCH is a function that returns list with 4 positions, they will be ignored.

REGEXP?   - If non-nil than strings specified in SEARCH argument will be treated
            as regexp patterns, otherwise they will searched literally.
BALANCED? - When non-nil all nested balanced LEFT RIGHT pairs will be skipped,
            else the first found pattern will be accepted.

This function populates the buffer local `helix-surround-alist' variable,
and thus should be called from major-modes hooks.
See the defaul value of `helix-surround-alist' variable for examples."
  (push (cons key (list :insert insert
                        :search (or search insert)
                        :regexp regexp?
                        :balanced balanced?))
        helix-surround-alist))

(defun helix-surround--get-4-bounds (char)
  (let ((spec (alist-get char helix-surround-alist)))
    (if (not spec)
        (helix-4-bounds-of-surrounded-at-point (cons (char-to-string char)
                                                     (char-to-string char))
                                               (bounds-of-thing-at-point 'defun))
      ;; else
      (if-let* ((pair-or-fun (plist-get spec :search))
                (list-or-pair (if (functionp pair-or-fun)
                                  (funcall pair-or-fun)
                                pair-or-fun))
                ((consp list-or-pair)))
          (if (length= list-or-pair 4)
              list-or-pair
            (helix-4-bounds-of-surrounded-at-point list-or-pair
                                                   (bounds-of-thing-at-point 'defun)
                                                   (plist-get spec :regexp)
                                                   (plist-get spec :balanced)))))))

;; ms
(defun helix-surround ()
  "Enclose the selected region in chosen delimiters.
If the region consist of full lines, insert delimiters on separate
lines and reindent the region."
  (interactive)
  (when (use-region-p)
    (pcase-let*
        ((char (read-char "Insert pair: "))
         (forward? (< (mark) (point)))
         (beg (region-beginning))
         (end (region-end))
         (newline? (helix-line-selected-p))
         (pair-or-fun (if-let ((spec (alist-get char helix-surround-alist)))
                          (plist-get spec :insert)))
         (`(,left . ,right) (cond ((and pair-or-fun (functionp pair-or-fun))
                                   (funcall pair-or-fun))
                                  (pair-or-fun)
                                  (t (cons char char)))))
      (when newline?
        (setq left (s-trim left)
              right (s-trim right)))
      (let ((deactivate-mark nil))
        (goto-char end)
        (if newline?
            (progn (helix-skip-whitespaces)
                   (insert right)
                   (newline-and-indent))
          (insert right))
        (goto-char beg)
        (insert left)
        (when newline?
          (newline-and-indent)))
      (let* ((new-beg (point))
             (new-end (+ end (- new-beg beg))))
        (cond (forward?
               (set-mark new-beg)
               (goto-char new-end))
              (t
               (goto-char new-beg)
               (set-mark new-end)))
        (indent-region new-beg new-end)))
    (setq helix--extend-selection nil)))

;; md
(defun helix-surround-delete ()
  (interactive)
  (when-let* ((char (read-char "Delete pair: "))
              (bounds (helix-surround--get-4-bounds char)))
    (pcase-let ((`(,left-beg ,left-end ,right-beg ,right-end) bounds))
      (delete-region right-beg right-end)
      (delete-region left-beg left-end))))

;; mr mc
(defun helix-surround-change ()
  (interactive)
  (when-let* ((char (read-char "Delete pair: "))
              (bounds (helix-surround--get-4-bounds char)))
    (pcase-let*
        ((`(,left-beg ,left-end ,right-beg ,right-end) bounds)
         (char (read-char "Insert pair: "))
         (pair-or-fun (if-let ((spec (alist-get char helix-surround-alist)))
                          (plist-get spec :insert)))
         (`(,left . ,right) (cond ((and pair-or-fun (functionp pair-or-fun))
                                   (funcall pair-or-fun))
                                  (pair-or-fun)
                                  (t (cons char char)))))
      (save-mark-and-excursion
        (delete-region right-beg right-end)
        (goto-char right-beg)
        (insert right)
        (delete-region left-beg left-end)
        (goto-char left-beg)
        (insert left)))))

(defun helix-mark-inner-surround ()
  (interactive)
  (let ((char (if (integerp last-command-event)
                  last-command-event
                (get last-command-event 'ascii-character))))
    (when-let* ((bounds (helix-surround--get-4-bounds char)))
      (pcase-let ((`(,_ ,l ,r ,_) bounds))
        (set-mark l)
        (goto-char r)))))

(defun helix-mark-a-surround ()
  (interactive)
  (let ((char (if (integerp last-command-event)
                  last-command-event
                (get last-command-event 'ascii-character))))
    (when-let* ((bounds (helix-surround--get-4-bounds char)))
      (pcase-let ((`(,l ,_ ,_ ,r) bounds))
        (set-mark l)
        (goto-char r)))))

;;; Window navigation

(defalias 'helix-window-split #'split-window-below)

(defun helix-window-vsplit ()
  "Split the current window vertically.
The new window will be created to the right. All children of the
parent of the splitted window are rebalanced."
  (interactive)
  (let* ((window-to-split (selected-window))
         (new-window (split-window window-to-split nil 'right)))
    (select-window new-window)
    ;; Always copy quit-restore parameter in interactive use.
    (when-let* ((quit-restore (window-parameter window-to-split 'quit-restore)))
      (set-window-parameter new-window 'quit-restore quit-restore)))
  (balance-windows (window-parent)))

(defun helix-window-left (count)
  "Move the cursor to new COUNT-th window left of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-left)))

(defun helix-window-right (count)
  "Move the cursor to new COUNT-th window right of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-right)))

(defun helix-window-up (count)
  "Move the cursor to new COUNT-th window up of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-up)))

(defun helix-window-down (count)
  "Move the cursor to new COUNT-th window down of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-down)))

(defmacro helix-save-side-windows (&rest body)
  "Toggle side windows, evaluate BODY, restore side windows."
  (declare (indent defun) (debug (&rest form)))
  (let ((sides (make-symbol "sidesvar")))
    `(let ((,sides (window-with-parameter 'window-side)))
       (when ,sides (window-toggle-side-windows))
       (unwind-protect
           (progn ,@body)
         (when ,sides (window-toggle-side-windows))))))

(defun helix-move-window (side)
  "SIDE has the same meaning as in `split-window'."
  (helix-save-side-windows
    (unless (one-window-p)
      (save-excursion
        (let ((w (window-state-get (selected-window))))
          (delete-window)
          (let ((wtree (window-state-get)))
            (delete-other-windows)
            (let ((subwin (selected-window))
                  (newwin (split-window nil nil side)))
              (window-state-put wtree subwin)
              (window-state-put w newwin)
              (select-window newwin)))))
      (balance-windows))))

(defun helix-move-window-left ()
  "Swap window with one to the left."
  (interactive)
  (helix-move-window 'left))

(defun helix-move-window-right ()
  "Swap window with one to the right."
  (interactive)
  (helix-move-window 'right))

(defun helix-move-window-up ()
  "Swap window with one upwards."
  (interactive)
  (helix-move-window 'up))

(defun helix-move-window-down ()
  "Swap window with one downwards."
  (interactive)
  (helix-move-window 'down))

(defun helix-window-delete ()
  "Delete the current window or tab.
Rebalance all children of the deleted window's parent window."
  (interactive)
  (let ((parent (window-parent)))
    ;; If tabs are enabled and this is the only visible window, then attempt to
    ;; close this tab.
    (if (and (bound-and-true-p tab-bar-mode)
             (null parent))
        (tab-close)
      (delete-window)
      ;; balance-windows raises an error if the parent does not have
      ;; any further children (then rebalancing is not necessary anyway)
      (ignore-errors (balance-windows parent)))))

(provide 'helix-commands)
;;; helix-commands.el ends here
