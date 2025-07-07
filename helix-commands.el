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

(require 's)
(require 'dash)
(require 'pcre2el)
(require 'cl-lib)
(require 'thingatpt)
(require 'helix-common)
(require 'helix-core)
(require 'helix-multiple-cursors-core)
(require 'helix-states)
(require 'helix-search)
(provide 'pulse)
(require 'avy)

(defun helix-normal-state-escape ()
  "Command for ESC key in Helix Normal state."
  (interactive)
  (if helix--extend-selection
      (helix-extend-selection -1)
    (deactivate-mark)))

;;; Motions

;; h
(defun helix-backward-char (count)
  "Move backward COUNT chars."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (backward-char count))

;; l
(defun helix-forward-char (count)
  "Move forward COUNT chars."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (forward-char count))

;; j
(defun helix-next-line (count)
  "Move to the next COUNT line."
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
  "Move to the previous COUNT line."
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
(defun helix-forward-word-start (count &optional bigword?)
  "Move to the COUNT-th next word start.
If BIGWORD? move over WORD-s."
  (interactive "p")
  (let ((thing (if bigword? 'helix-WORD 'helix-word)))
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
(defun helix-backward-word-start (count &optional bigword?)
  "Move to the COUNT-th previous word start.
If BIGWORD? move over WORD-s."
  (interactive "p")
  (setq count (- count))
  (let ((thing (if bigword? 'helix-WORD 'helix-word)))
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
(defun helix-forward-word-end (count &optional bigword?)
  "Move to the COUNT-th next word end.
If BIGWORD? move over WORD-s."
  (interactive "p")
  (let ((thing (if bigword? 'helix-WORD 'helix-word)))
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
  (helix-remove-all-fake-cursors)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    ;; else
    (set-marker (mark-marker) (point))
    (deactivate-mark))
  (push-mark)
  (goto-char (if num
                 (+ (point-min)
                    (/ (* (- (point-max) (point-min))
                          (prefix-numeric-value num))
                       10))
               (point-min)))
  (if num
      (forward-line 1)
    (recenter 0)))

;; G
(defun helix-goto-last-line ()
  "Move point the end of the buffer."
  (interactive)
  (helix-remove-all-fake-cursors)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    ;; else
    (set-marker (mark-marker) (point))
    (deactivate-mark))
  (push-mark)
  (goto-char (point-max)))

;; gs
(defun helix-beginning-of-line-command ()
  "Move point to beginning of current line.
Use visual line when `visual-line-mode' is active."
  (declare (interactive-only helix-beginning-of-line))
  (interactive)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (helix-beginning-of-line))

;; gh
(defun helix-first-non-blank ()
  "Move point to beginning of current line skipping indentation.
Use visual line when `visual-line-mode' is active."
  (interactive)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (helix-beginning-of-line)
  (skip-syntax-forward " " (line-end-position))
  (backward-prefix-chars))

;; gl
(defun helix-end-of-line-command ()
  "Move point to end of current line.
Use visual line when `visual-line-mode' is active."
  (declare (interactive-only helix-end-of-line))
  (interactive)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (helix-end-of-line)
  ;; "Stick" cursor to the end of line after moving to it. Vertical
  ;; motions right after "gl" will place point at the end of each line.
  ;; Do this only when `visual-line-mode' is not active.
  (when (and (not visual-line-mode)
             (eolp))
    (setq temporary-goal-column most-positive-fixnum
          this-command 'next-line)))

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

;; mm
;; TODO: The most bare-boned version. Need upgrade.
(defun helix-jump-to-match-item ()
  "Jump between matching brackets."
  (interactive)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (ignore-errors
    (cond
     ;; before open bracket
     ((and (/= (point) (point-max))
           (eq 4 (syntax-class (syntax-after (point)))))
      (forward-list 1))
     ;; after close bracket
     ((and (/= (point) (point-min))
           (eq 5 (syntax-class (syntax-after (1- (point))))))
      (forward-list -1))
     (t
      (up-list -1)))))

;;; Easymotion / Avy

;; gw
(defun helix-avy-word-forward ()
  "Move to a word start after the point, choosing it with Avy."
  (interactive)
  (helix-remove-all-fake-cursors)
  (let ((mark (and helix--extend-selection
                   (not (region-active-p))
                   (point)))
        (avy-all-windows nil))
    (when (-> (avy--regex-candidates avy-goto-word-0-regexp
                                     (point) (window-end nil t))
              (avy-process))
      (cond (mark (set-mark mark))
            ((not helix--extend-selection)
             (set-mark (point))))
      (forward-thing 'helix-word))))

;; gb
(defun helix-avy-word-backward ()
  "Move to a word start before the point, choosing it with Avy."
  (interactive)
  (helix-remove-all-fake-cursors)
  (let ((mark (and helix--extend-selection
                   (not (region-active-p))
                   (point)))
        (avy-all-windows nil))
    (when (-> (avy--regex-candidates avy-goto-word-0-regexp
                                     (window-start) (point))
              (nreverse)
              (avy-process))
      (cond (mark (set-mark mark))
            ((not helix--extend-selection)
             (set-mark (point))
             (forward-thing 'helix-word))))))

;; gW
(defun helix-avy-WORD-forward ()
  "Move to a WORD start after the point, choosing it with Avy."
  (interactive)
  (helix-remove-all-fake-cursors)
  (let ((mark (and helix--extend-selection
                   (not (region-active-p))
                   (point)))
        (avy-all-windows nil))
    (when (-> (avy--regex-candidates "[^ \r\n\t]+"
                                     (point) (window-end nil t))
              (avy-process))
      (cond (mark (set-mark mark))
            ((not helix--extend-selection)
             (set-mark (point))))
      (forward-thing 'helix-WORD))))

;; gB
(defun helix-avy-WORD-backward ()
  "Move to a WORD start before the point, choosing it with Avy."
  (interactive)
  (helix-remove-all-fake-cursors)
  (let ((mark (and helix--extend-selection
                   (not (region-active-p))
                   (point)))
        (avy-all-windows nil))
    (when (-> (avy--regex-candidates "[^ \r\n\t]+"
                                     (window-start) (point))
              (nreverse)
              (avy-process))
      (cond (mark (set-mark mark))
            ((not helix--extend-selection)
             (set-mark (point))
             (forward-thing 'helix-WORD))))))

;; gj
(defun helix-avy-next-line ()
  "Move to a next line, choosing it with Avy."
  (interactive)
  (helix-remove-all-fake-cursors)
  (unless helix--extend-selection
    (deactivate-mark))
  (let ((temporary-goal-column (current-column)))
    (-> (helix-collect-positions #'next-line)
        (avy-process))))

;; gk
(defun helix-avy-previous-line ()
  "Move to a previous line, choosing it with Avy."
  (interactive)
  (helix-remove-all-fake-cursors)
  (unless helix--extend-selection
    (deactivate-mark))
  (let ((temporary-goal-column (current-column)))
    (-> (helix-collect-positions #'previous-line)
        (avy-process))))

;;; Changes

;; i
(defun helix-insert ()
  "Switch to Insert state before region."
  (interactive)
  (helix-with-each-cursor
    (helix-ensure-region-direction -1))
  (helix-insert-state 1))

;; a
(defun helix-append ()
  "Switch to Insert state after region."
  (interactive)
  (helix-with-each-cursor
    (helix-ensure-region-direction 1))
  (helix-insert-state 1))

;; I
(defun helix-insert-line ()
  "Switch to insert state at beginning of current line."
  (interactive)
  (helix-with-each-cursor
    (deactivate-mark)
    (helix-first-non-blank)
    (set-marker (mark-marker) (point)))
  (helix-insert-state 1))

;; A
(defun helix-append-line ()
  "Switch to Insert state at the end of the current line."
  (interactive)
  (helix-with-each-cursor
    (deactivate-mark)
    (helix-end-of-line)
    (set-marker (mark-marker) (point)))
  (helix-insert-state 1))

;; o
(defun helix-open-below ()
  "Insert a new line below point and switch to Insert state."
  (interactive)
  (helix-with-each-cursor
    (move-end-of-line nil)
    (newline-and-indent)
    (set-marker (mark-marker) (point)))
  (helix-insert-state 1))

;; O
(defun helix-open-above ()
  "Insert a new line above point and switch to Insert state."
  (interactive)
  (helix-with-each-cursor
    (move-beginning-of-line nil)
    (newline)
    (forward-line -1)
    (indent-according-to-mode)
    (set-marker (mark-marker) (point)))
  (helix-insert-state 1))

;; ] SPC
(defun helix-add-blank-line-below (count)
  "Add COUNT blank lines below."
  (interactive "p")
  (save-excursion
    (let (deactivate-mark)
      (move-end-of-line nil)
      (newline count))))

;; [ SPC
(defun helix-add-blank-line-above (count)
  "Add COUNT blank lines above."
  (interactive "p")
  (save-excursion
    (let (deactivate-mark)
      (move-beginning-of-line nil)
      (newline count))))

;; c
(defun helix-change ()
  "Delete region and enter Insert state."
  (interactive)
  (cond ((use-region-p)
         (let ((line-selected? (helix-linewise-selection-p)))
           (kill-region nil nil t)
           (pcase line-selected?
             ('line (newline)
                    (backward-char)
                    (indent-according-to-mode))
             ('visual-line (insert " ")
                           (backward-char)))))
        ((not (helix-bolp))
         (delete-char -1))
        ((bolp)
         (indent-according-to-mode)))
  (helix-insert-state 1))

;; TODO:
;; - If point is surrounded by (balanced) whitespace and a brace delimiter
;; ({} [] ()), delete a space on either side of the cursor.
;; - If point is at BOL and surrounded by braces on adjacent lines,
;; collapse newlines:
;; {
;; |
;; } => {|}
;; d
(defun helix-cut (count)
  "Kill (cut) text in region. I.e. delete text and put it in the `kill-ring'.
If no selection — delete COUNT chars before point."
  (interactive "p")
  (if (use-region-p)
      (pcase (helix-linewise-selection-p)
        ('line (helix-copy-line)
               (delete-region (region-beginning) (region-end)))
        (_
         (kill-region nil nil t)))
    ;; else
    (delete-char (- count)))
  (helix-extend-selection -1))

;; D
(defun helix-delete (count)
  "Delete text in region, without modifying the `kill-ring'.
If no selection — delete COUNT chars after point."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char count))
  (helix-extend-selection -1))

;; u
(defun helix-undo ()
  "Cancel current region then undo."
  (interactive)
  (deactivate-mark)
  (let (deactivate-mark)
    (undo-only)))

;; U
(defun helix-redo ()
  "Cancel current region then redo."
  (interactive)
  (deactivate-mark)
  (let (deactivate-mark)
    (undo-redo)))

;; y
(defun helix-copy ()
  "Copy selection into `kill-ring'."
  (interactive)
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end))
          (deactivate-mark nil))
      (pcase (helix-linewise-selection-p)
        ('line (helix-copy-line))
        (_ (copy-region-as-kill beg end)))
      (pulse-momentary-highlight-region beg end)
      (message "Copied into kill-ring"))))

;; p
(defun helix-paste-after ()
  "Paste after selection."
  (interactive)
  (helix-ensure-region-direction 1)
  ;; `yank' sets `this-command' to `yank' internally
  (yank))

;; P
(defun helix-paste-before ()
  "Paste before selection."
  (interactive)
  (helix-ensure-region-direction -1)
  ;; `yank' sets `this-command' to `yank' internally
  (yank))

;; C-p
(defun helix-paste-pop (count)
  "Replace just-pasted text with next COUNT element from `kill-ring'.
Wrapper around `yank-pop'."
  (interactive "p")
  (let ((yank-pop (command-remapping 'yank-pop))
        (deactivate-mark nil))
    (call-interactively yank-pop count)))

;; C-n
(defun helix-paste-undo-pop (count)
  "Replace just-pasted text with previous COUNT element from `kill-ring'.
Like `helix-paste-pop' but with negative COUNT argument."
  (interactive "p")
  (let ((yank-pop (command-remapping 'yank-pop))
        (deactivate-mark nil))
    (call-interactively yank-pop (- count))))

;; R
(defun helix-replace-with-kill-ring ()
  "Replace selection content with yanked text from `kill-ring'."
  (interactive)
  (when (use-region-p)
    (let (deactivate-mark)
      (delete-region (region-beginning) (region-end))
      (yank))))

;; J
(defun helix-join-line ()
  "Join the selected lines."
  (interactive)
  (let ((deactivate-mark nil)
        (region? (use-region-p))
        beg end dir)
    (when region?
      (setq beg (region-beginning)
            end (copy-marker (region-end))
            dir (helix-region-direction)))
    (let ((count (let ((count (if region? (count-lines beg end) 1)))
                   (if (> count 1) (1- count) count))))
      (when region? (goto-char beg))
      ;; All these `let' bindings are actually move point.
      (let ((in-comment? (helix-comment-at-pos-p
                          (progn (move-beginning-of-line nil)
                                 (skip-chars-forward " \t")
                                 (point))))
            (ubeg (progn (forward-line 1)
                         (line-beginning-position)))
            (uend (progn (forward-line (1- count))
                         (line-end-position))))
        (when in-comment?
          (uncomment-region ubeg uend)))
      (dotimes (_ count)
        (forward-line 0)
        (delete-char -1)
        (fixup-whitespace)))
    (when region?
      (helix-set-region beg end dir)
      (set-marker end nil))))

;; ~
(defun helix-invert-case ()
  "Invert case of characters."
  (interactive)
  (if (use-region-p)
      (let ((dir (helix-region-direction))
            (beg (region-beginning))
            (end (region-end))
            (deactivate-mark nil))
        (helix-invert-case-in-region beg end)
        (helix-set-region beg end dir))
    ;; else
    (helix-invert-case-in-region (point) (1+ (point)))))

;; ` or gu
(defun helix-downcase (start end)
  "Convert text in selection to lower case."
  (interactive "r")
  (when (use-region-p)
    (let (deactivate-mark)
      (downcase-region start end))))

;; M-` or gU
(defun helix-upcase (start end)
  "Convert text in selection to upper case."
  (interactive "r")
  (when (use-region-p)
    (let (deactivate-mark)
      (upcase-region start end))))

;;; Selections

;; v
(defun helix-extend-selection (&optional arg)
  "Toggle extending selections.
If ARG is nil — toggle extending selection.
If ARG positive number — enable, negative — disable."
  (interactive)
  (if helix--extend-selection
      (when (or (null arg) (< arg 0))
        (setq helix--extend-selection nil)
        (unless helix--executing-command-for-fake-cursor
          (helix-update-cursor)))
    ;; else
    (when (or (null arg) (< 0 arg))
      (setq helix--extend-selection t)
      (unless helix--executing-command-for-fake-cursor
        (set-cursor-color (face-attribute 'helix-extend-selection-cursor
                                          :background))))))

;; ;
(defun helix-collapse-selection ()
  "Collapse region onto a single cursor."
  (interactive)
  (deactivate-mark))

;; x
(defun helix-expand-line-selection (count)
  "Expand current selection linewise.
If region is forward — mark is before point — expand selection downwise,
if region is backward — point is before mark — expand upwise."
  (interactive "p")
  (when (helix-expand-selection-to-full-lines)
    (setq count (- count (helix-sign count))))
  (unless (zerop count)
    (let* ((line (if visual-line-mode 'visual-line 'line))
           (count (* count (helix-region-direction))))
      (forward-thing line count))))

;; X
(defun helix-contract-line-selection (count)
  "Contract current selection linewise.
Counterpart to `helix-expand-line-selection' that does the exact opposite."
  (interactive "p")
  (setq count (abs count))
  (when (helix-reduce-selection-to-full-lines)
    (setq count (- count (helix-sign count))))
  (unless (zerop count)
    (let* ((line (if visual-line-mode 'visual-line 'line))
           (beg (region-beginning))
           (end (region-end))
           (dir (helix-region-direction)))
      (cond ((< dir 0)
             (forward-thing line count)
             (when (<= end (point))
               (helix-set-region beg end dir)))
            (t ;; (< 0 dir)
             (setq count (- (abs count)))
             (forward-thing line count)
             (when(<= (point) beg)
               (helix-set-region beg end dir)))))))

(defun helix-expand-selection-to-full-lines ()
  "Create a line-wise selection, using visual or logical lines.
When region is active: expand selection to line boundaries to encompass full
line(s). Otherwise, select current line. Uses visual lines if `visual-line-mode'
is active, otherwise logical lines."
  (let ((line (if visual-line-mode 'visual-line 'line)))
    (cond ((use-region-p)
           (unless (helix-linewise-selection-p)
             (let ((beg (region-beginning))
                   (end (region-end)))
               (goto-char beg)
               (set-mark (car (bounds-of-thing-at-point line))) ; left end
               (goto-char end)
               (goto-char (cdr (bounds-of-thing-at-point line))) ; right end
               t)))
          (t ;; no region
           (-let (((beg . end) (bounds-of-thing-at-point line)))
             (set-mark beg)
             (goto-char end)
             t)))))

(defun helix-reduce-selection-to-full-lines ()
  "Remove non full line parts from both ends of the selection.
Return t if does anything, otherwise return nil."
  (when (and (use-region-p)
             (not (helix-linewise-selection-p)))
    (let ((line (if visual-line-mode 'visual-line 'line))
          (beg (region-beginning))
          (end (region-end))
          (dir (helix-region-direction))
          result)
      ;; new END
      (goto-char end)
      (unless (helix-bolp)
        (-let (((line-beg . _) (bounds-of-thing-at-point line)))
          (unless (< line-beg beg)
            (setq end line-beg
                  result t))))
      ;; new BEG
      (goto-char beg)
      (unless (helix-bolp)
        (-let (((_ . line-end) (bounds-of-thing-at-point line)))
          (unless (<= end line-end)
            (setq beg line-end
                  result t))))
      (helix-set-region beg end dir)
      result)))

;; %
(helix-define-advice mark-whole-buffer (:before ())
  (helix-remove-all-fake-cursors))

;; s
(defun helix-select-regex (&optional invert)
  "Create new selections for all matches to the regexp entered withing current
selections.

If INVERT is non-nil — create new selections for all regions that NOT match to
entered regexp withing current selections."
  (interactive)
  (cond (helix-multiple-cursors-mode
         (let* ((real-cursor (helix--create-fake-cursor-1 (point) (mark t) 0))
                (cursors (helix-all-fake-cursors))
                (ranges (-map #'(lambda (cursor)
                                  (if (overlay-get cursor 'mark-active)
                                      (let ((point (overlay-get cursor 'point))
                                            (mark  (overlay-get cursor 'mark)))
                                        (cons (min point mark)
                                              (max point mark)))))
                              cursors)))
           (-each cursors #'helix-remove-fake-cursor-from-buffer)
           (or (helix-select-interactively-in ranges invert)
               (progn ;; Restore original cursors
                 (-each cursors #'helix-restore-fake-cursor-in-buffer)
                 (helix-restore-point-from-fake-cursor real-cursor)))))
        (t
         (let ((dir (helix-region-direction))
               (beg (region-beginning))
               (end (region-end)))
           (or (helix-select-interactively-in (region-bounds) invert)
               ;; Restore original region
               (helix-set-region beg end dir))))))

;; S
(defun helix-split-region ()
  "Split each selection according to the regexp entered."
  (interactive)
  (helix-select-regex t))

;; M-s
(defun helix-split-region-on-newline ()
  "Split selections on line boundaries."
  (interactive)
  (let (any?)
    (helix-with-each-cursor
      (when-let* (((use-region-p))
                  (ranges (helix-regexp-match-ranges
                           ".+$" (region-beginning) (region-end))))
        (helix-create-cursors ranges)
        (setq any? t)))
    (when any?
      (helix-with-each-cursor
        (helix-extend-selection -1)))))

;; K
(defun helix-keep-selections ()
  "Keep selections that match to the regexp entered."
  (interactive)
  (helix-filter-selections))

;; M-K
(defun helix-remove-selections ()
  "Remove selections that match to the regexp entered."
  (interactive)
  (helix-filter-selections t))

;; _
(defun helix-trim-whitespaces-from-selection ()
  "Trim whitespaces and newlines from the both ends of selections."
  (interactive)
  (when (use-region-p)
    (let ((dir (helix-region-direction)))
      (helix-skip-chars " \t\r\n" (- dir))
      (helix-exchange-point-and-mark)
      (helix-skip-chars " \t\r\n" dir)
      (helix-exchange-point-and-mark))))

;; &
(defun helix-align-selections ()
  "Align selections."
  (interactive)
  (helix-with-real-cursor-as-fake
    (let* (;; Filter cursors to remain only the first one on each line.
           ;; Line numbers start from 1, so 0 is out of scope.
           (cursors (let ((current-line 0))
                      (-remove #'(lambda (cursor)
                                   (let* ((line (line-number-at-pos
                                                 (overlay-get cursor 'point))))
                                     (or (eql line current-line)
                                         (ignore (setq current-line line)))))
                               (helix-all-fake-cursors t))))
           (column (-reduce-from #'(lambda (column cursor)
                                     (goto-char (overlay-get cursor 'point))
                                     (max column (current-column)))
                                 0 cursors)))
      ;; Align
      (helix-save-window-scroll
        (dolist (cursor cursors)
          (helix-with-fake-cursor cursor
            (unless (eql (current-column) column)
              (let ((deactivate-mark) ;; Don't deactivate mark after insertion.
                    (str (s-repeat (- column (current-column)) " ")))
                (cond ((and (use-region-p)
                            (> (helix-region-direction) 0))
                       (helix-exchange-point-and-mark)
                       (insert str)
                       (helix-exchange-point-and-mark))
                      (t
                       (insert str)))))))))))

;; C
(defun helix-copy-selection (count)
  "Copy selections COUNT times down if COUNT is positive, or up if negative."
  (interactive "p")
  (helix-with-each-cursor
    (helix-motion-loop (dir count)
      (if (use-region-p)
          (helix--copy-region dir)
        (helix--copy-cursor dir)))))

;; M-c
(defun helix-copy-selection-up (count)
  "Copy each selection COUNT times up."
  (interactive "p")
  (helix-copy-selection (- count)))

(defun helix--copy-cursor (direction)
  "Copy point toward the DIRECTION."
  (when-let* ((pos (save-excursion
                     (cl-loop with column = (current-column)
                              while (zerop (forward-line direction))
                              when (eql (move-to-column column) column)
                              return (point))))
              ((not (helix-fake-cursor-at pos))))
    (helix-create-fake-cursor-from-point)
    (goto-char pos)))

(defun helix--copy-region (&optional direction)
  "Copy region toward the DIRECTION."
  (unless direction (setq direction 1))
  (let* ((region-dir (helix-region-direction))
         (beg (region-beginning))
         (end (region-end))
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
      (let (point mark)
        (if (< region-dir 0)
            (-setq (point . mark) bounds)
          (-setq (mark . point) bounds))
        (if-let* ((cursor (helix-fake-cursor-at point))
                  ((eql mark (overlay-get cursor 'mark))))
            nil ;; Do nothing — fake cursor is already at desired position.
          ;; else
          (helix-create-fake-cursor-from-point)
          (goto-char point)
          (set-marker (mark-marker) mark))))))

(defun helix--bounds-of-following-region
    (start-column end-column number-of-lines direction)
  "Return bounds of following region toward the DIRECTION that starts
at START-COLUMN, ends at END-COLUMN and consists of NUMBER-OF-LINES."
  (when (< direction 0)
    (cl-rotatef start-column end-column))
  (cl-loop with start and end
           while (zerop (forward-line direction))
           when (eql (move-to-column start-column)
                     start-column)
           do (setq start (point))
           when (cl-loop while (zerop (forward-line (* (1- number-of-lines)
                                                       direction)))
                         when (eql (move-to-column end-column)
                                   end-column)
                         return (point))
           do (setq end (point))
           return (if (< 0 direction)
                      (cons start end)
                    (cons end start))))

;; ,
(defun helix-remove-all-fake-cursors ()
  "Remove all fake cursors from current buffer."
  (interactive)
  (when helix-multiple-cursors-mode
    (helix-multiple-cursors-mode -1)))

;; M-,
(defun helix-remove-main-cursor ()
  "Delete main cursor and activate the next fake one."
  (interactive)
  (when helix-multiple-cursors-mode
    (helix-restore-point-from-fake-cursor (or (helix-next-fake-cursor)
                                              (helix-first-fake-cursor)))
    (helix-maybe-disable-multiple-cursors-mode)))

;; M-minus
(defun helix-merge-selections ()
  "Merge all cursors into single selection."
  (interactive)
  (when helix-multiple-cursors-mode
    (let ((beg (let ((cursor (helix-first-fake-cursor)))
                 (min (overlay-get cursor 'point)
                      (overlay-get cursor 'mark)
                      (point)
                      (if (use-region-p) (mark) most-positive-fixnum))))
          (end (let ((cursor (helix-last-fake-cursor)))
                 (max (overlay-get cursor 'point)
                      (overlay-get cursor 'mark)
                      (point)
                      (if (use-region-p) (mark) 0)))))
      (helix-remove-all-fake-cursors)
      (helix-set-region beg end 1))))

;; (
(defun helix-rotate-selections-backward (count)
  "Rotate main selection backward COUNT times."
  (interactive "p")
  (when helix-multiple-cursors-mode
    (let ((scroll-conservatively 0))
      (dotimes (_ count)
        (let ((cursor (or (helix-previous-fake-cursor (point))
                          (helix-last-fake-cursor))))
          (helix-create-fake-cursor-from-point)
          (helix-restore-point-from-fake-cursor cursor)))
      (redisplay))))

;; )
(defun helix-rotate-selections-forward (count)
  "Rotate main selection forward COUNT times."
  (interactive "p")
  (when helix-multiple-cursors-mode
    (let ((scroll-conservatively 0))
      (dotimes (_ count)
        (let ((cursor (or (helix-next-fake-cursor (point))
                          (helix-first-fake-cursor))))
          (helix-create-fake-cursor-from-point)
          (helix-restore-point-from-fake-cursor cursor))))
    (redisplay)))

;; M-(
(defun helix-rotate-selections-content-backward (count)
  "Rotate selections content backward COUNT times."
  (interactive "p")
  (when (and helix-multiple-cursors-mode
             (use-region-p))
    (dotimes (_ count)
      (helix-with-real-cursor-as-fake
        (-> (helix-all-fake-cursors t)
            (nreverse)
            (helix--rotate-selections-content))))))

;; M-)
(defun helix-rotate-selections-content-forward (count)
  "Rotate selections content forward COUNT times."
  (interactive "p")
  (when (and helix-multiple-cursors-mode
             (use-region-p))
    (dotimes (_ count)
      (helix-with-real-cursor-as-fake
        (-> (helix-all-fake-cursors t)
            (helix--rotate-selections-content))))))

(defun helix--rotate-selections-content (cursors)
  "Rotate regions content for all CURSORS in the order they are in list."
  (let* ((first-cursor (car cursors))
         (content (buffer-substring (overlay-get first-cursor 'point)
                                    (overlay-get first-cursor 'mark))))
    (dolist (cursor (cdr cursors))
      (setq content (helix-exchange-fake-region-content cursor content)))
    (helix-exchange-fake-region-content first-cursor content)))

(defun helix-exchange-fake-region-content (cursor content)
  "Exchange the CURSORs region content with CONTENT and return the old one."
  (helix-with-fake-cursor cursor
    (let ((dir (helix-region-direction))
          (deactivate-mark) ;; Do not deactivate mark after insertion.
          (new-content (buffer-substring (point) (mark))))
      (delete-region (point) (mark))
      (insert content)
      (when (< dir 0) (helix-exchange-point-and-mark))
      new-content)))

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
        (helix-create-fake-cursor pos pos)))))

;;;; Mark

(defun helix-mark-map-digit-argument (arg)
  "Like `digit-argument' but keep `helix-mark-map' active."
  (interactive "P")
  (digit-argument arg)
  (set-transient-map helix-mark-map))

;; Do not show `helix-mark-map-digit-argument' in which-key popup.
(with-eval-after-load 'which-key
  (defvar which-key-replacement-alist)
  (cl-pushnew '((nil . "helix-mark-map-digit-argument") . ignore)
              which-key-replacement-alist))

;; miw
(defun helix-mark-inner-word (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-word count))

;; miW
(defun helix-mark-inner-WORD (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-WORD count))

;; maw
(defun helix-mark-a-word ()
  (interactive)
  (helix--mark-a-word-1 nil))

;; maW
(defun helix-mark-a-WORD ()
  (interactive)
  (helix--mark-a-word-1 t))

(defun helix--mark-a-word-1 (bigword?)
  "Inner implementation of `helix-mark-a-word' and `helix-mark-a-WORD' commands."
  (let ((thing (if bigword? 'helix-WORD 'helix-word)))
    (-when-let ((thing-beg . thing-end) (bounds-of-thing-at-point thing))
      (-let (((beg . end)
              (or (progn
                    (goto-char thing-end)
                    (helix-with-restriction (line-beginning-position) (line-end-position)
                      (-if-let ((_ . space-end)
                                (helix-bounds-of-complement-of-thing-at-point thing))
                          (cons thing-beg space-end))))
                  (progn
                    (goto-char thing-beg)
                    (helix-with-restriction
                        (save-excursion
                          (back-to-indentation)
                          (point))
                        (line-end-position)
                      (-if-let ((space-beg . _)
                                (helix-bounds-of-complement-of-thing-at-point thing))
                          (cons space-beg thing-end))))
                  (cons thing-beg thing-end))))
        (helix-set-region beg end)))))

;; mis
(defun helix-mark-inner-sentence (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-sentence count))

;; mas
(defun helix-mark-a-sentence ()
  (interactive)
  (let ((thing 'helix-sentence))
    (-when-let ((thing-beg . thing-end) (bounds-of-thing-at-point thing))
      (-let (((beg . end)
              (or (progn
                    (goto-char thing-end)
                    (helix-with-restriction (line-beginning-position) (line-end-position)
                      (-if-let ((_ . space-end)
                                (helix-bounds-of-complement-of-thing-at-point thing))
                          (cons thing-beg space-end))))
                  (progn
                    (goto-char thing-beg)
                    (helix-with-restriction (line-beginning-position) (line-end-position)
                      (-if-let ((space-beg . _)
                                (helix-bounds-of-complement-of-thing-at-point thing))
                          (cons space-beg thing-end))))
                  (cons thing-beg thing-end))))
        (helix-set-region beg end)))))

;; mip
(defun helix-mark-inner-paragraph (count)
  (interactive "p")
  (push-mark nil t)
  (helix-mark-inner-thing 'paragraph count))

;; map
(defun helix-mark-a-paragraph ()
  (interactive)
  (push-mark nil t)
  (helix-mark-a-thing 'paragraph))

;; mif
(defun helix-mark-inner-function (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-function count)
  (helix-exchange-point-and-mark))

;; maf
(defun helix-mark-a-function ()
  (interactive)
  (helix-mark-a-thing 'helix-function))

;; mi"
(defun helix-mark-inner-double-quoted ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?\"))
    (helix-set-region (1+ beg) (1- end))))

;; ma"
(defun helix-mark-a-double-quoted ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?\"))
    (helix-set-region beg end)))

;; mi'
(defun helix-mark-inner-single-quoted ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?'))
    (helix-set-region (1+ beg) (1- end))))

;; ma'
(defun helix-mark-a-single-quoted ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?'))
    (helix-set-region beg end)))

;; mi`
(defun helix-mark-inner-back-quoted ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?`))
    (helix-set-region (1+ beg) (1- end))))

;; ma`
(defun helix-mark-a-back-quoted ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?`))
    (helix-set-region beg end)))

;; mi( mi)
(defun helix-mark-inner-paren ()
  (interactive)
  (-when-let ((_ beg end _) (helix-4-bounds-of-brackets-at-point ?\( ?\)))
    (helix-set-region beg end)))

;; ma( ma)
(defun helix-mark-a-paren ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-brackets-at-point ?\( ?\)))
    (helix-set-region beg end)))

;; mi[ mi]
(defun helix-mark-inner-bracket ()
  (interactive)
  (-when-let ((_ beg end _) (helix-4-bounds-of-brackets-at-point ?\[ ?\]))
    (helix-set-region beg end)))

;; ma[ ma]
(defun helix-mark-a-bracket ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-brackets-at-point ?\[ ?\]))
    (helix-set-region beg end)))

;; mi{ mi}
(defun helix-mark-inner-curly ()
  (interactive)
  (-when-let ((_ beg end _) (helix-4-bounds-of-brackets-at-point ?{ ?}))
    (helix-set-region beg end)))

;; ma{ ma}
(defun helix-mark-a-curly ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-brackets-at-point ?{ ?}))
    (helix-set-region beg end)))

;; mi< mi>
(defun helix-mark-inner-angle ()
  (interactive)
  (-when-let ((_ beg end _) (helix-4-bounds-of-brackets-at-point ?< ?>))
    (helix-set-region beg end)))

;; ma< ma>
(defun helix-mark-an-angle ()
  (interactive)
  (-when-let ((beg _ _ end) (helix-4-bounds-of-brackets-at-point ?< ?>))
    (helix-set-region beg end)))

;;; Search

;; f
(defun helix-find-char-forward (count)
  "Prompt user for CHAR and move to the next COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  (interactive "p")
  (unless (region-active-p) (set-mark (point)))
  (let ((char (read-char "f")))
    (helix-motion-loop (dir count)
      (helix-find-char char dir nil))))

;; F
(defun helix-find-char-backward (count)
  "Prompt user for CHAR and move to the previous COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  (interactive "p")
  (unless (region-active-p) (set-mark (point)))
  (setq count (- count))
  (let ((char (read-char "F")))
    (helix-motion-loop (dir count)
      (helix-find-char char dir nil))))

;; t
(defun helix-till-char-forward (count)
  "Prompt user for CHAR and move before the next COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  (interactive "p")
  (unless (region-active-p) (set-mark (point)))
  (let ((char (read-char "t")))
    (helix-motion-loop (dir count)
      (helix-find-char char dir t))))

;; T
(defun helix-till-char-backward (count)
  "Prompt user for CHAR and move before the prevous COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  (interactive "p")
  (unless (region-active-p) (set-mark (point)))
  (setq count (- count))
  (let ((char (read-char "T")))
    (helix-motion-loop (dir count)
      (helix-find-char char dir t))))

;; /
(defun helix-search-forward (count)
  (interactive "p")
  (when (helix-search-interactively)
    (setq helix-search--direction 1)
    (helix-search-next count)))

;; ?
(defun helix-search-backward (count)
  (interactive "p")
  (when (helix-search-interactively -1)
    (setq helix-search--direction -1)
    (helix-search-next count)))

;; n
(defun helix-search-next (count)
  "Select next COUNT search match."
  (interactive "p")
  (unless helix-search--direction (setq helix-search--direction 1))
  (when (and helix-search--direction
             (< helix-search--direction 0))
    (setq count (- count)))
  (let ((regexp (helix-search-pattern))
        (region-dir (if (use-region-p) (helix-region-direction) 1))
        ;; Center point after jump to search result if it is out of the screen.
        (scroll-conservatively 0))
    (helix-motion-loop (dir count)
      (when (save-excursion (helix-re-search-with-wrap regexp dir))
        (-let (((beg . end) (helix-match-bounds)))
          (cond ((and helix--extend-selection (use-region-p))
                 (helix-create-fake-cursor-from-point))
                ;; Push mark on first execution.
                ;; TODO: Take into account `helix-keep-search-highlight-commands'
                ;; commands.
                ((not (memq last-command '(helix-search-next helix-search-previous)))
                 (push-mark nil t)))
          (helix-set-region beg end region-dir))))
    ;; Update the screen so that the temporary value for
    ;; `scroll-conservatively' is taken into account.
    (redisplay)
    (helix-highlight-search-pattern regexp)))

;; N
(defun helix-search-previous (count)
  "Select previous COUNT search match."
  (interactive "p")
  (helix-search-next (- count)))

;; *
(defun helix-construct-search-pattern ()
  "Construct search pattern from all current selections and store it to / register.
Auto-detect word boundaries at the beginning and end of the search pattern."
  (interactive)
  (let ((quote (if helix-use-pcre-regex #'rxt-quote-pcre #'regexp-quote))
        patterns)
    (helix-with-each-cursor
      (when (use-region-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (open-word-boundary
                (cond ((eql beg (pos-bol))
                       (->> (buffer-substring-no-properties beg (1+ beg))
                            (string-match-p "[[:word:]]")))
                      (t
                       (->> (buffer-substring-no-properties (1- beg) (1+ beg))
                            (string-match-p "[^[:word:]][[:word:]]")))))
               (close-word-boundary
                (cond ((eql end (pos-eol))
                       (->> (buffer-substring-no-properties (1- end) end)
                            (string-match-p "[[:word:]]")))
                      (t
                       (->> (buffer-substring-no-properties (1- end) (1+ end))
                            (string-match-p "[[:word:]][^[:word:]]")))))
               (string (->> (buffer-substring-no-properties (point) (mark))
                            (funcall quote))))
          (push (concat (if open-word-boundary "\\b")
                        string
                        (if close-word-boundary "\\b"))
                patterns))))
    (setq patterns (nreverse (-uniq patterns)))
    (let* ((separator (if helix-use-pcre-regex "|" "\\|"))
           (regexp (apply #'concat (-interpose separator patterns))))
      (set-register '/ regexp)
      (message "Register / set: %s" regexp)
      (helix-highlight-search-pattern regexp))))

;; M-*
(defun helix-construct-search-pattern-no-bounds ()
  "Construct search pattern from all current selection and store it to / register.
Do not auto-detect word boundaries in the search pattern."
  (interactive)
  (let ((quote (if helix-use-pcre-regex #'rxt-quote-pcre #'regexp-quote))
        patterns)
    (helix-with-each-cursor
      (when (use-region-p)
        (push (funcall quote (buffer-substring-no-properties (point) (mark)))
              patterns)))
    (setq patterns (nreverse patterns))
    (let* ((separator (if helix-use-pcre-regex "|" "\\|"))
           (regexp (apply #'concat (-interpose separator patterns))))
      (set-register '/ regexp)
      (message "Register / set: %s" regexp)
      (helix-highlight-search-pattern regexp))))

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
                          LEFT                              RIGHT
                        |<tag> |Lorem ipsum dolor sit amet| </tag>|
                        ^      ^                          ^       ^
               LEFT-START      LEFT-END         RIGHT-START       RIGHT-END

Following parameters are taken into account only when SEARCH argument is a cons
cell with stirngs (LEFT . RIGHT) or a function, that returns such cons cell. If
SEARCH is a function that returns list with 4 positions, they will be ignored.

REGEXP?   - If non-nil then strings specified in SEARCH argument will be treated
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

(defun helix-surround--read-char ()
  "Read char from minibuffer and return (LEFT . RIGHT) pair with strings
to surround with."
  (let* ((char (read-char "surround: "))
         (pair-or-fun-or-nil (-some->
                                 (alist-get char helix-surround-alist)
                               (plist-get :insert))))
    (pcase pair-or-fun-or-nil
      ((and (pred functionp) fn)
       (funcall fn))
      ((and (pred consp) pair)
       pair)
      (_ (cons char char)))))

;; Cache the function output to use with all cursors.
(helix-cache-input helix-surround--read-char)

;; ms
(defun helix-surround ()
  "Enclose the selected region in chosen delimiters.
If the region consist of full lines, insert delimiters on separate
lines and reindent the region."
  (interactive)
  (when (use-region-p)
    (-let (((left . right) (helix-surround--read-char))
           (beg (region-beginning))
           (end (region-end))
           (dir (helix-region-direction))
           (lines? (helix-linewise-selection-p))
           (deactivate-mark nil))
      (when lines?
        (setq left  (s-trim left)
              right (s-trim right)))
      (goto-char end)
      (when lines?
        (helix-skip-whitespaces)
        (newline-and-indent))
      (insert right)
      (goto-char beg)
      (insert left)
      (when lines?
        (newline-and-indent))
      (let* ((new-beg (point))
             (new-end (+ end (- new-beg beg))))
        (helix-set-region new-beg new-end dir)
        (indent-region new-beg new-end)))
    (helix-extend-selection -1)))

;; md
(defun helix-surround-delete ()
  (interactive)
  (when-let* ((key (read-char "Delete pair: "))
              (bounds (helix-surround--4-bounds key)))
    (-let (((left-beg left-end right-beg right-end) bounds))
      (delete-region right-beg right-end)
      (delete-region left-beg left-end))))

;; mr mc
(defun helix-surround-change ()
  (interactive)
  (when-let* ((char (read-char "Delete pair: "))
              (bounds (helix-surround--4-bounds char)))
    (-let* (((left-beg left-end right-beg right-end) bounds)
            (char (read-char "Insert pair: "))
            (pair-or-fun (-some-> (alist-get char helix-surround-alist)
                           (plist-get :insert)))
            ((left . right) (pcase pair-or-fun
                              ((and (pred functionp) fun)
                               (funcall fun))
                              ((and pair (guard pair))
                               pair)
                              ('nil (cons char char))))
            (deactivate-mark nil))
      (save-mark-and-excursion
        (delete-region right-beg right-end)
        (goto-char right-beg)
        (insert right)
        (delete-region left-beg left-end)
        (goto-char left-beg)
        (insert left)))))

(defun helix-mark-inner-surround ()
  (interactive)
  (when-let* ((char (if (characterp last-command-event)
                        last-command-event
                      (get last-command-event 'ascii-character)))
              (bounds (helix-surround--4-bounds char)))
    (-let (((_ beg end _) bounds))
      (helix-set-region beg end))))

(defun helix-mark-a-surround ()
  (interactive)
  (when-let* ((char (if (characterp last-command-event)
                        last-command-event
                      (get last-command-event 'ascii-character)))
              (bounds (helix-surround--4-bounds char)))
    (-let (((beg _ _ end) bounds))
      (helix-set-region beg end))))

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
