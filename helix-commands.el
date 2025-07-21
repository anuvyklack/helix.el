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

(put 'helix-normal-state-escape 'multiple-cursors t)

;;; Motions

;; h
(defun helix-backward-char (count)
  "Move backward COUNT chars."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (backward-char count))

(put 'helix-backward-char 'multiple-cursors t)
(put 'helix-backward-char 'helix-merge-regions 'extend-selection)

;; l
(defun helix-forward-char (count)
  "Move forward COUNT chars."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (forward-char count))

(put 'helix-forward-char 'multiple-cursors t)
(put 'helix-forward-char 'helix-merge-regions 'extend-selection)

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

(put 'helix-next-line 'multiple-cursors t)
(put 'helix-next-line 'helix-merge-regions 'extend-selection)

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

(put 'helix-previous-line 'multiple-cursors t)
(put 'helix-previous-line 'helix-merge-regions 'extend-selection)

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

(put 'helix-forward-word-start 'multiple-cursors t)
(put 'helix-forward-word-start 'helix-merge-regions 'extend-selection)

;; W
(defun helix-forward-WORD-start (count)
  "Move to the COUNT-th next WORD start."
  (interactive "p")
  (helix-forward-word-start count :bigword))

(put 'helix-forward-WORD-start 'multiple-cursors t)
(put 'helix-forward-WORD-start 'helix-merge-regions 'extend-selection)

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

(put 'helix-backward-word-start 'multiple-cursors t)
(put 'helix-backward-word-start 'helix-merge-regions 'extend-selection)

;; B
(defun helix-backward-WORD-start (count)
  "Move to the COUNT-th previous WORD start."
  (interactive "p")
  (helix-backward-word-start count :bigword))

(put 'helix-backward-WORD-start 'multiple-cursors t)
(put 'helix-backward-WORD-start 'helix-merge-regions 'extend-selection)

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

(put 'helix-forward-word-end 'multiple-cursors t)
(put 'helix-forward-word-end 'helix-merge-regions 'extend-selection)

;; E
(defun helix-forward-WORD-end (count)
  "Move COUNT-th next WORD end."
  (interactive "p")
  (helix-forward-word-end count :bigword))

(put 'helix-forward-WORD-end 'multiple-cursors t)
(put 'helix-forward-WORD-end 'helix-merge-regions 'extend-selection)

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
  (helix-push-mark)
  (goto-char (if num
                 (+ (point-min)
                    (/ (* (- (point-max) (point-min))
                          (prefix-numeric-value num))
                       10))
               (point-min)))
  (if num
      (forward-line 1)
    (recenter 0)))

(put 'helix-goto-first-line 'multiple-cursors 'false)

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
  (helix-push-mark)
  (goto-char (point-max)))

(put 'helix-goto-last-line 'multiple-cursors 'false)

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

(put 'helix-beginning-of-line-command 'multiple-cursors t)
(put 'helix-beginning-of-line-command 'helix-merge-regions t)

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

(put 'helix-first-non-blank 'multiple-cursors t)
(put 'helix-first-non-blank 'helix-merge-regions t)

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

(put 'helix-end-of-line-command 'multiple-cursors t)
(put 'helix-end-of-line-command 'helix-merge-regions t)

;; ]p or }}
(defun helix-forward-paragraph (count)
  "Move to the end of the COUNT next paragraph."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (helix-forward-beginning-of-thing 'paragraph count))

(put 'helix-forward-paragraph 'multiple-cursors t)

;; [p or {{
(defun helix-backward-paragraph (count)
  "Move to the beginning of the COUNT previous paragraph."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (helix-forward-beginning-of-thing 'paragraph (- count)))

(put 'helix-backward-paragraph 'multiple-cursors t)

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

(put 'helix-jump-to-match-item 'multiple-cursors t)

;;; Avy (Easymotion)

;; gw
(defun helix-avy-word-forward ()
  "Move to a word start after the point, choosing it with Avy."
  (interactive)
  (helix-remove-all-fake-cursors)
  (helix-push-mark)
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

(put 'helix-avy-word-forward 'multiple-cursors 'false)

;; gb
(defun helix-avy-word-backward ()
  "Move to a word start before the point, choosing it with Avy."
  (interactive)
  (helix-remove-all-fake-cursors)
  (helix-push-mark)
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

(put 'helix-avy-word-backward 'multiple-cursors 'false)

;; gW
(defun helix-avy-WORD-forward ()
  "Move to a WORD start after the point, choosing it with Avy."
  (interactive)
  (helix-remove-all-fake-cursors)
  (helix-push-mark)
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

(put 'helix-avy-WORD-forward 'multiple-cursors 'false)

;; gB
(defun helix-avy-WORD-backward ()
  "Move to a WORD start before the point, choosing it with Avy."
  (interactive)
  (helix-remove-all-fake-cursors)
  (helix-push-mark)
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

(put 'helix-avy-WORD-backward 'multiple-cursors 'false)

;; gj
(defun helix-avy-next-line ()
  "Move to a next line, choosing it with Avy."
  (interactive)
  (helix-remove-all-fake-cursors)
  (helix-push-mark)
  (unless helix--extend-selection
    (deactivate-mark))
  (let ((temporary-goal-column (current-column)))
    (-> (helix-collect-positions #'next-line)
        (avy-process))))

(put 'helix-avy-next-line 'multiple-cursors 'false)

;; gk
(defun helix-avy-previous-line ()
  "Move to a previous line, choosing it with Avy."
  (interactive)
  (helix-remove-all-fake-cursors)
  (helix-push-mark)
  (unless helix--extend-selection
    (deactivate-mark))
  (let ((temporary-goal-column (current-column)))
    (-> (helix-collect-positions #'previous-line)
        (avy-process))))

(put 'helix-avy-previous-line 'multiple-cursors 'false)

;;; Changes

;; i
(defun helix-insert ()
  "Switch to Insert state before region."
  (interactive)
  (helix-with-each-cursor
    (helix-ensure-region-direction -1))
  (helix-insert-state 1))

(put 'helix-insert 'multiple-cursors 'false)

;; a
(defun helix-append ()
  "Switch to Insert state after region."
  (interactive)
  (helix-with-each-cursor
    (helix-ensure-region-direction 1))
  (helix-insert-state 1))

(put 'helix-append 'multiple-cursors 'false)

(defun helix--insert-or-append-on-line (direction)
  "Switch to insert state at beginning or end of current line
depending on DIRECTION."
  ;; Remain only one cursor on each line.
  (when helix-multiple-cursors-mode
    (helix-with-real-cursor-as-fake
      ;; Line numbers start from 1, so 0 as initial value is out of scope.
      (let ((current-line 0))
        (-each (helix-all-fake-cursors :sort)
          (lambda (cursor)
            (let ((line (line-number-at-pos
                         (overlay-get cursor 'point))))
              (if (eql line current-line)
                  (helix--delete-fake-cursor cursor)
                (setq current-line line))))))))
  (helix-with-each-cursor
    (if (natnump direction)
        (helix-end-of-line)
      (helix-first-non-blank))
    (set-marker (mark-marker) (point)))
  (helix-insert-state 1))

;; I
(defun helix-insert-line ()
  "Switch to insert state at beginning of current line."
  (interactive)
  (helix--insert-or-append-on-line -1))

(put 'helix-insert-line 'multiple-cursors 'false)

;; A
(defun helix-append-line ()
  "Switch to Insert state at the end of the current line."
  (interactive)
  (helix--insert-or-append-on-line 1))

(put 'helix-append-line 'multiple-cursors 'false)

;; o
(defun helix-open-below ()
  "Open new line below selection."
  (interactive)
  (helix-with-each-cursor
    (when (use-region-p)
      (helix-ensure-region-direction 1)
      (pcase (helix-linewise-selection-p)
        ('line (backward-char))))
    (move-end-of-line nil)
    (newline-and-indent)
    (set-marker (mark-marker) (point)))
  (helix-insert-state 1))

(put 'helix-open-below 'multiple-cursors 'false)

;; O
(defun helix-open-above ()
  "Open new line above selection."
  (interactive)
  (helix-with-each-cursor
    (when (use-region-p)
      (helix-ensure-region-direction -1))
    (move-beginning-of-line nil)
    (newline)
    (forward-line -1)
    (indent-according-to-mode)
    (set-marker (mark-marker) (point)))
  (helix-insert-state 1))

(put 'helix-open-above 'multiple-cursors 'false)

;; ] SPC
(defun helix-add-blank-line-below (count)
  "Add COUNT blank lines below selection."
  (interactive "p")
  (let ((deactivate-mark nil)
        (point (point))
        region)
    (when (use-region-p)
      (setq region (list (region-beginning) (region-end) (helix-region-direction)))
      (helix-ensure-region-direction 1)
      (pcase (helix-linewise-selection-p)
        ('line (backward-char))))
    (move-end-of-line nil)
    (newline count)
    (if region
        (apply #'helix-set-region region)
      (goto-char point))))

(put 'helix-add-blank-line-below 'multiple-cursors t)

;; [ SPC
(defun helix-add-blank-line-above (count)
  "Add COUNT blank lines above selection."
  (interactive "p")
  (helix-save-region
    (helix-ensure-region-direction -1)
    (move-beginning-of-line nil)
    (newline count)))

(put 'helix-add-blank-line-above 'multiple-cursors t)

;; c
(defun helix-change ()
  "Delete region and enter Insert state."
  (interactive)
  (helix-with-each-cursor
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
           (indent-according-to-mode))))
  (helix-insert-state 1))

(put 'helix-change 'multiple-cursors 'false)

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
      (kill-region nil nil t)
    (delete-char (- count)))
  (helix-extend-selection -1))

(put 'helix-cut 'multiple-cursors t)

;; D
(defun helix-delete (count)
  "Delete text in region, without modifying the `kill-ring'.
If no selection — delete COUNT chars after point."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char count))
  (helix-extend-selection -1))

(put 'helix-delete 'multiple-cursors t)

;; u
(defun helix-undo ()
  "Cancel current region then undo."
  (interactive)
  (deactivate-mark)
  (let (deactivate-mark)
    (undo-only)))

(put 'helix-undo 'multiple-cursors 'false)

;; U
(defun helix-redo ()
  "Cancel current region then redo."
  (interactive)
  (deactivate-mark)
  (let (deactivate-mark)
    (undo-redo)))

(put 'helix-redo 'multiple-cursors 'false)

;; y
(defun helix-copy ()
  "Copy selection into `kill-ring'."
  (interactive)
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end))
          (deactivate-mark nil))
      (copy-region-as-kill beg end)
      (pulse-momentary-highlight-region beg end)
      (message "Copied into kill-ring"))))

(put 'helix-copy 'multiple-cursors t)

;; p
(defun helix-paste-after ()
  "Paste after selection."
  (interactive)
  (helix-paste 1))

(put 'helix-paste-after 'multiple-cursors t)

;; P
(defun helix-paste-before ()
  "Paste before selection."
  (interactive)
  (helix-paste -1))

(put 'helix-paste-before 'multiple-cursors t)

;; C-p
(defun helix-paste-pop (count)
  "Replace just-pasted text with next COUNT element from `kill-ring'.
Wrapper around `yank-pop'."
  (interactive "p")
  (let ((yank-pop (or (command-remapping 'yank-pop)
                      #'yank-pop))
        (deactivate-mark nil))
    (call-interactively yank-pop count)))

(put 'helix-paste-pop 'multiple-cursors t)

;; C-n
(defun helix-paste-undo-pop (count)
  "Replace just-pasted text with previous COUNT element from `kill-ring'.
Like `helix-paste-pop' but with negative COUNT argument."
  (interactive "p")
  (let ((yank-pop (or (command-remapping 'yank-pop)
                      #'yank-pop))
        (deactivate-mark nil))
    (call-interactively yank-pop (- count))))

(put 'helix-paste-undo-pop 'multiple-cursors t)

;; R
(defun helix-replace-with-kill-ring ()
  "Replace selection content with yanked text from `kill-ring'."
  (interactive)
  (when (use-region-p)
    (let (deactivate-mark)
      (delete-region (region-beginning) (region-end))
      (helix-yank))))

(put 'helix-replace-with-kill-ring 'multiple-cursors t)

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

(put 'helix-join-line 'multiple-cursors t)

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

(put 'helix-invert-case 'multiple-cursors t)

;; ` or gu
(defun helix-downcase (start end)
  "Convert text in selection to lower case."
  (interactive "r")
  (when (use-region-p)
    (let (deactivate-mark)
      (downcase-region start end))))

(put 'helix-downcase 'multiple-cursors t)

;; M-` or gU
(defun helix-upcase (start end)
  "Convert text in selection to upper case."
  (interactive "r")
  (when (use-region-p)
    (let (deactivate-mark)
      (upcase-region start end))))

(put 'helix-upcase 'multiple-cursors t)

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

(put 'helix-extend-selection 'multiple-cursors t)

;; ;
(defun helix-collapse-selection ()
  "Collapse region onto a single cursor."
  (interactive)
  (deactivate-mark))

(put 'helix-collapse-selection 'multiple-cursors t)

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

(put 'helix-expand-line-selection 'multiple-cursors t)
(put 'helix-expand-line-selection 'helix-merge-regions t)

;; X
(defun helix-contract-line-selection (count)
  "Contract current selection linewise.
Counterpart to `helix-expand-line-selection' that does the exact opposite."
  (interactive "p")
  (setq count (abs count))
  (and (helix-reduce-selection-to-full-lines)
       (cl-decf count))
  (unless (zerop count)
    (let* ((line (if visual-line-mode 'visual-line 'line))
           (beg (region-beginning))
           (end (region-end))
           (dir (helix-region-direction)))
      (cond ((natnump dir)
             (cl-dotimes (_ count)
               (forward-thing line -1)
               (if (< beg (point))
                   (setq end (point))
                 (cl-return))))
            (t ;; (< dir 0)
             (cl-dotimes (_ count)
               (forward-thing line 1)
               (if (< (point) end)
                   (setq beg (point))
                 (cl-return)))))
      (helix-set-region beg end dir))))

(put 'helix-contract-line-selection 'multiple-cursors t)

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
           (-let [(beg . end) (bounds-of-thing-at-point line)]
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
        (-let [(line-beg . _) (bounds-of-thing-at-point line)]
          (unless (< line-beg beg)
            (setq end line-beg
                  result t))))
      ;; new BEG
      (goto-char beg)
      (unless (helix-bolp)
        (-let [(_ . line-end) (bounds-of-thing-at-point line)]
          (unless (<= end line-end)
            (setq beg line-end
                  result t))))
      (helix-set-region beg end dir)
      result)))

;; %
(helix-define-advice mark-whole-buffer (:before ())
  (helix-remove-all-fake-cursors))

(put 'mark-whole-buffer 'multiple-cursors 'false)

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
         (let ((region (list (region-beginning) (region-end) (helix-region-direction))))
           (or (helix-select-interactively-in (region-bounds) invert)
               ;; Restore original region
               (apply #'helix-set-region region))))))

(put 'helix-select-regex 'multiple-cursors 'false)

;; S
(defun helix-split-region ()
  "Split each selection according to the regexp entered."
  (interactive)
  (helix-select-regex t))

(put 'helix-split-region 'multiple-cursors 'false)

;; M-s
(defun helix-split-region-on-newline ()
  "Split selections on line boundaries."
  (interactive)
  (helix-with-each-cursor
    (helix-extend-selection -1)
    (when (use-region-p)
      (let ((end (region-end)))
        (helix-ensure-region-direction 1)
        (goto-char (mark-marker))
        (catch 'done
          (let (border)
            (while t
              (helix-end-of-line)
              (when (<= end (point))
                (goto-char end)
                (throw 'done nil))
              (setq border (point))
              (forward-char)
              (when (<= end (point))
                (goto-char border)
                (throw 'done nil))
              (helix-create-fake-cursor border (mark))
              (set-marker (mark-marker) (point)))))))))

(put 'helix-split-region-on-newline 'multiple-cursors 'false)

;; K
(defun helix-keep-selections ()
  "Keep selections that match to the regexp entered."
  (interactive)
  (helix-filter-selections))

(put 'helix-keep-selections 'multiple-cursors 'false)

;; M-K
(defun helix-remove-selections ()
  "Remove selections that match to the regexp entered."
  (interactive)
  (helix-filter-selections t))

(put 'helix-remove-selections 'multiple-cursors 'false)

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

(put 'helix-trim-whitespaces-from-selection 'multiple-cursors t)

;; &
(defun helix-align-selections ()
  "Align selections."
  (interactive)
  (helix-with-real-cursor-as-fake
    (let* (;; Filter cursors to remain only the first one on each line.
           ;; Line numbers start from 1, so 0 as initial value is out of scope.
           (cursors (let ((current-line 0))
                      (-remove #'(lambda (cursor)
                                   (let* ((line (line-number-at-pos
                                                 (overlay-get cursor 'point))))
                                     (or (eql line current-line)
                                         (ignore (setq current-line line)))))
                               (helix-all-fake-cursors :sort))))
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
                    (padding (s-repeat (- column (current-column)) " ")))
                (cond ((and (use-region-p)
                            (> (helix-region-direction) 0))
                       (helix-exchange-point-and-mark)
                       (insert padding)
                       (helix-exchange-point-and-mark))
                      (t
                       (insert padding)))))))))))

(put 'helix-align-selections 'multiple-cursors 'false)

;; C
(defun helix-copy-selection (count)
  "Copy selections COUNT times down if COUNT is positive, or up if negative."
  (interactive "p")
  (helix-with-each-cursor
    (helix-motion-loop (dir count)
      (if (use-region-p)
          (helix--copy-region dir)
        (helix--copy-cursor dir)))))

(put 'helix-copy-selection 'multiple-cursors 'false)
(put 'helix-copy-selection 'helix-merge-regions t)

;; M-c
(defun helix-copy-selection-up (count)
  "Copy each selection COUNT times up."
  (interactive "p")
  (helix-copy-selection (- count)))

(put 'helix-copy-selection-up 'multiple-cursors 'false)
(put 'helix-copy-selection-up 'helix-merge-regions t)

(defun helix--copy-cursor (direction)
  "Copy point toward the DIRECTION."
  (when-let* ((pos (save-excursion
                     (cl-loop with column = (current-column)
                              while (zerop (forward-line direction))
                              when (eql (move-to-column column) column)
                              return (point))))
              ((not (helix-fake-cursor-at pos))))
    (unless (helix-fake-cursor-at (point))
      (helix-create-fake-cursor-from-point))
    (goto-char pos)))

(defun helix--copy-region (direction)
  "Copy region toward the DIRECTION."
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
                  ((= mark (overlay-get cursor 'mark))))
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
  (let (start end)
    (cl-block nil
      (while (not (and start end))
        (unless (zerop (forward-line direction))
          (cl-return))
        (when (eql (move-to-column start-column)
                   start-column)
          (setq start (point))
          (unless (zerop (forward-line (* (1- number-of-lines)
                                          direction)))
            (cl-return))
          (when (eql (move-to-column end-column)
                     end-column)
            (setq end (point))))))
    (if (and start end)
        (if (< 0 direction)
            (cons start end)
          (cons end start)))))

;; ,
(defun helix-remove-all-fake-cursors ()
  "Remove all fake cursors from current buffer."
  (interactive)
  (when helix-multiple-cursors-mode
    (helix-multiple-cursors-mode -1)))

(put 'helix-remove-all-fake-cursors 'multiple-cursors 'false)

;; M-,
(defun helix-remove-main-cursor ()
  "Delete main cursor and activate the next fake one."
  (interactive)
  (when helix-multiple-cursors-mode
    (helix-restore-point-from-fake-cursor (or (helix-next-fake-cursor (point))
                                              (helix-first-fake-cursor)))
    (helix-maybe-disable-multiple-cursors-mode)))

(put 'helix-remove-main-cursor 'multiple-cursors 'false)

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

(put 'helix-merge-selections 'multiple-cursors 'false)

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

(put 'helix-rotate-selections-forward 'multiple-cursors 'false)

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

(put 'helix-rotate-selections-backward 'multiple-cursors 'false)

;; M-)
(defun helix-rotate-selections-content-forward (count)
  "Rotate selections content forward COUNT times."
  (interactive "p")
  (helix--rotate-selections-content count))

(put 'helix-rotate-selections-content-forward 'multiple-cursors 'false)

;; M-(
(defun helix-rotate-selections-content-backward (count)
  "Rotate selections content backward COUNT times."
  (interactive "p")
  (helix--rotate-selections-content count :backward))

(put 'helix-rotate-selections-content-backward 'multiple-cursors 'false)

(defun helix--rotate-selections-content (count &optional backward)
  (when (and helix-multiple-cursors-mode
             (use-region-p))
    (let ((dir (helix-region-direction)))
      ;; To correctly rotate the content of adjacent selections, we all
      ;; regions need to have negative direction.  This is due to marker
      ;; insertion type of point and mark markers of fake cursor (see
      ;; `set-marker-insertion-type'). Point-marker insertion type is t,
      ;; mark-marker — nil.  We want beginning of a region to be advanced
      ;; on insertion at its position, and end of a region — not.
      (when (natnump dir)
        (helix-with-each-cursor (helix-exchange-point-and-mark)))
      (helix-with-real-cursor-as-fake
        (let ((cursors (helix-all-fake-cursors :sort)))
          (when backward
            (setq cursors (nreverse cursors)))
          (dotimes (_ count)
            (helix--rotate-selections-content-1 cursors))))
      ;; Restore original regions direction.
      (unless (eql dir (helix-region-direction))
        (helix-with-each-cursor (helix-exchange-point-and-mark))))))

(defun helix--rotate-selections-content-1 (cursors)
  "Rotate regions content for all CURSORS."
  (let* ((first-cursor (car cursors))
         (content (buffer-substring (overlay-get first-cursor 'point)
                                    (overlay-get first-cursor 'mark))))
    (dolist (cursor (cdr cursors))
      (setq content (helix-exchange-fake-region-content cursor content)))
    (helix-exchange-fake-region-content first-cursor content)))

(defun helix-exchange-fake-region-content (cursor content)
  "Exchange the CURSORs region content with CONTENT and return the old one."
  (helix-with-fake-cursor cursor
    (let ((deactivate-mark nil) ;; Do not deactivate mark after insertion.
          (dir (helix-region-direction))
          (new-content (buffer-substring (point) (mark))))
      (delete-region (point) (mark))
      (insert content)
      (helix-ensure-region-direction dir)
      new-content)))

;; (keymap-lookup nil "M-<down-mouse-1>")

;; M-<right-mouse>
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

(put 'helix-toggle-cursor-on-click 'multiple-cursors 'false)

;;;; Mark

;; miw
(defun helix-mark-inner-word (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-word count))

(put 'helix-mark-inner-word 'multiple-cursors t)
(put 'helix-mark-inner-word 'helix-merge-regions t)

;; miW
(defun helix-mark-inner-WORD (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-WORD count))

(put 'helix-mark-inner-WORD 'multiple-cursors t)
(put 'helix-mark-inner-WORD 'helix-merge-regions t)

;; maw
(defun helix-mark-a-word ()
  (interactive)
  (helix--mark-a-word-1 nil))

(put 'helix-mark-a-word 'multiple-cursors t)
(put 'helix-mark-a-word 'helix-merge-regions t)

;; maW
(defun helix-mark-a-WORD ()
  (interactive)
  (helix--mark-a-word-1 t))

(put 'helix-mark-a-WORD 'multiple-cursors t)
(put 'helix-mark-a-WORD 'helix-merge-regions t)

(defun helix--mark-a-word-1 (bigword?)
  "Inner implementation of `helix-mark-a-word' and `helix-mark-a-WORD' commands."
  (let ((thing (if bigword? 'helix-WORD 'helix-word)))
    (-when-let ((thing-beg . thing-end) (bounds-of-thing-at-point thing))
      (-let [(beg . end)
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
                 (cons thing-beg thing-end))]
        (helix-set-region beg end)))))

;; mis
(defun helix-mark-inner-sentence (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-sentence count))

(put 'helix-mark-inner-sentence 'multiple-cursors t)
(put 'helix-mark-inner-sentence 'helix-merge-regions t)

;; mas
(defun helix-mark-a-sentence ()
  (interactive)
  (let ((thing 'helix-sentence))
    (-when-let ((thing-beg . thing-end) (bounds-of-thing-at-point thing))
      (-let [(beg . end)
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
                 (cons thing-beg thing-end))]
        (helix-set-region beg end)))))

(put 'helix-mark-a-sentence 'multiple-cursors t)
(put 'helix-mark-a-sentence 'helix-merge-regions t)

;; mip
(defun helix-mark-inner-paragraph (count)
  (interactive "p")
  (helix-push-mark)
  (helix-mark-inner-thing 'paragraph count))

(put 'helix-mark-inner-paragraph 'multiple-cursors t)
(put 'helix-mark-inner-paragraph 'helix-merge-regions t)

;; map
(defun helix-mark-a-paragraph ()
  (interactive)
  (helix-push-mark)
  (helix-mark-a-thing 'paragraph))

(put 'helix-mark-a-paragraph 'multiple-cursors t)
(put 'helix-mark-a-paragraph 'helix-merge-regions t)

;; mif
(defun helix-mark-inner-function (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-function count)
  (helix-exchange-point-and-mark))

(put 'helix-mark-inner-function 'multiple-cursors t)
(put 'helix-mark-inner-function 'helix-merge-regions t)

;; maf
(defun helix-mark-a-function ()
  (interactive)
  (helix-mark-a-thing 'helix-function))

(put 'helix-mark-a-function 'multiple-cursors t)
(put 'helix-mark-a-function 'helix-merge-regions t)

;; mi"
(defun helix-mark-inner-double-quoted ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?\"))
    (helix-set-region (1+ beg) (1- end))))

(put 'helix-mark-inner-double-quoted 'multiple-cursors t)
(put 'helix-mark-inner-double-quoted 'helix-merge-regions t)

;; ma"
(defun helix-mark-a-double-quoted ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?\"))
    (helix-set-region beg end)))

(put 'helix-mark-a-double-quoted 'multiple-cursors t)
(put 'helix-mark-a-double-quoted 'helix-merge-regions t)

;; mi'
(defun helix-mark-inner-single-quoted ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?'))
    (helix-set-region (1+ beg) (1- end))))

(put 'helix-mark-inner-single-quoted 'multiple-cursors t)
(put 'helix-mark-inner-single-quoted 'helix-merge-regions t)

;; ma'
(defun helix-mark-a-single-quoted ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?'))
    (helix-set-region beg end)))

(put 'helix-mark-a-single-quoted 'multiple-cursors t)
(put 'helix-mark-a-single-quoted 'helix-merge-regions t)

;; mi`
(defun helix-mark-inner-back-quoted ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?`))
    (helix-set-region (1+ beg) (1- end))))

(put 'helix-mark-inner-back-quoted 'multiple-cursors t)
(put 'helix-mark-inner-back-quoted 'helix-merge-regions t)

;; ma`
(defun helix-mark-a-back-quoted ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?`))
    (helix-set-region beg end)))

(put 'helix-mark-a-back-quoted 'multiple-cursors t)
(put 'helix-mark-a-back-quoted 'helix-merge-regions t)

;; mi( mi)
(defun helix-mark-inner-paren ()
  (interactive)
  (-when-let ((_ beg end _) (helix-4-bounds-of-brackets-at-point ?\( ?\)))
    (helix-set-region beg end)))

(put 'helix-mark-inner-paren 'multiple-cursors t)
(put 'helix-mark-inner-paren 'helix-merge-regions t)

;; ma( ma)
(defun helix-mark-a-paren ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-brackets-at-point ?\( ?\)))
    (helix-set-region beg end)))

(put 'helix-mark-a-paren 'multiple-cursors t)
(put 'helix-mark-a-paren 'helix-merge-regions t)

;; mi[ mi]
(defun helix-mark-inner-bracket ()
  (interactive)
  (-when-let ((_ beg end _) (helix-4-bounds-of-brackets-at-point ?\[ ?\]))
    (helix-set-region beg end)))

(put 'helix-mark-inner-bracket 'multiple-cursors t)
(put 'helix-mark-inner-bracket 'helix-merge-regions t)

;; ma[ ma]
(defun helix-mark-a-bracket ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-brackets-at-point ?\[ ?\]))
    (helix-set-region beg end)))

(put 'helix-mark-a-bracket 'multiple-cursors t)
(put 'helix-mark-a-bracket 'helix-merge-regions t)

;; mi{ mi}
(defun helix-mark-inner-curly ()
  (interactive)
  (-when-let ((_ beg end _) (helix-4-bounds-of-brackets-at-point ?{ ?}))
    (helix-set-region beg end)))

(put 'helix-mark-inner-curly 'multiple-cursors t)
(put 'helix-mark-inner-curly 'helix-merge-regions t)

;; ma{ ma}
(defun helix-mark-a-curly ()
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-brackets-at-point ?{ ?}))
    (helix-set-region beg end)))

(put 'helix-mark-a-curly 'multiple-cursors t)
(put 'helix-mark-a-curly 'helix-merge-regions t)

;; mi< mi>
(defun helix-mark-inner-angle ()
  (interactive)
  (-when-let ((_ beg end _) (helix-4-bounds-of-brackets-at-point ?< ?>))
    (helix-set-region beg end)))

(put 'helix-mark-inner-angle 'multiple-cursors t)
(put 'helix-mark-inner-angle 'helix-merge-regions t)

;; ma< ma>
(defun helix-mark-an-angle ()
  (interactive)
  (-when-let ((beg _ _ end) (helix-4-bounds-of-brackets-at-point ?< ?>))
    (helix-set-region beg end)))

(put 'helix-mark-an-angle 'multiple-cursors t)
(put 'helix-mark-an-angle 'helix-merge-regions t)

(defun helix-mark-inner-surround ()
  (interactive)
  (when-let* ((char (if (characterp last-command-event)
                        last-command-event
                      (get last-command-event 'ascii-character)))
              (bounds (helix-surround--4-bounds char)))
    (-let [(_ beg end _) bounds]
      (helix-set-region beg end))))

(put 'helix-mark-inner-surround 'multiple-cursors t)
(put 'helix-mark-inner-surround 'helix-merge-regions t)

(defun helix-mark-a-surround ()
  (interactive)
  (when-let* ((char (if (characterp last-command-event)
                        last-command-event
                      (get last-command-event 'ascii-character)))
              (bounds (helix-surround--4-bounds char)))
    (-let [(beg _ _ end) bounds]
      (helix-set-region beg end))))

(put 'helix-mark-a-surround 'multiple-cursors t)
(put 'helix-mark-a-surround 'helix-merge-regions t)

;;; Search

;; f
(defun helix-find-char-forward (count)
  "Prompt user for CHAR and move to the next COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (let ((char (read-char "f")))
    (helix-motion-loop (dir count)
      (helix-find-char char dir nil))))

(put 'helix-find-char-forward 'multiple-cursors t)
(put 'helix-find-char-forward 'helix-merge-regions 'extend-selection)

;; F
(defun helix-find-char-backward (count)
  "Prompt user for CHAR and move to the previous COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (setq count (- count))
  (let ((char (read-char "F")))
    (helix-motion-loop (dir count)
      (helix-find-char char dir nil))))

(put 'helix-find-char-backward 'multiple-cursors t)
(put 'helix-find-char-backward 'helix-merge-regions 'extend-selection)

;; t
(defun helix-till-char-forward (count)
  "Prompt user for CHAR and move before the next COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (let ((char (read-char "t")))
    (helix-motion-loop (dir count)
      (helix-find-char char dir t))))

(put 'helix-till-char-forward 'multiple-cursors t)
(put 'helix-till-char-forward 'helix-merge-regions 'extend-selection)

;; T
(defun helix-till-char-backward (count)
  "Prompt user for CHAR and move before the prevous COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (setq count (- count))
  (let ((char (read-char "T")))
    (helix-motion-loop (dir count)
      (helix-find-char char dir t))))

(put 'helix-till-char-backward 'multiple-cursors t)
(put 'helix-till-char-backward 'helix-merge-regions 'extend-selection)

;; /
(defun helix-search-forward (count)
  (interactive "p")
  (when (helix-search-interactively)
    (setq helix-search--direction 1)
    (helix-search-next count)))

(put 'helix-search-forward 'multiple-cursors 'false)
(put 'helix-search-forward 'helix-merge-regions t)

;; ?
(defun helix-search-backward (count)
  (interactive "p")
  (when (helix-search-interactively -1)
    (setq helix-search--direction -1)
    (helix-search-next count)))

(put 'helix-search-backward 'multiple-cursors 'false)
(put 'helix-search-backward 'helix-merge-regions t)

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
        ;; Recenter point after jump if it lands out of the screen.
        (scroll-conservatively 0))
    (helix-motion-loop (search-dir count)
      (-when-let ((beg . end) (helixf-search--search regexp search-dir))
        ;; Push mark on first invocation.
        (unless (or (memq last-command '(helix-search-next helix-search-previous))
                    (helix-search--keep-highlight last-command))
          (helix-push-mark))
        (when (and helix--extend-selection (use-region-p))
          (helix-create-fake-cursor-from-point))
        (helix-set-region beg end region-dir)))
    ;; Update the screen so that the temporary value for
    ;; `scroll-conservatively' is taken into account.
    (redisplay)
    (helix-highlight-search-pattern regexp)))

(put 'helix-search-next 'multiple-cursors 'false)
(put 'helix-search-next 'helix-merge-regions t)

;; N
(defun helix-search-previous (count)
  "Select previous COUNT search match."
  (interactive "p")
  (helix-search-next (- count)))

(put 'helix-search-previous 'multiple-cursors 'false)
(put 'helix-search-previous 'helix-merge-regions t)

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

(put 'helix-construct-search-pattern 'multiple-cursors 'false)

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

(put 'helix-construct-search-pattern-no-bounds 'multiple-cursors 'false)

;;; Surround

(defun helix-surround-add-pair (key insert &optional search regexp? balanced?)
  "Add a new insert-delete pattern for Helix surround functionality.
KEY       - A character that will activates this pattern.
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
            as regexp patterns, otherwise they will be searched literally.
BALANCED? - When non-nil all nested balanced LEFT RIGHT pairs will be skipped,
            else the first found pattern will be accepted.

This function populates the buffer local `helix-surround-alist' variable,
and thus should be called from major-modes hooks.

See the defaul value of `helix-surround-alist' variable and
`helix-integration.el' file for examples."
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
    (helix-save-region
      (-let (((left . right) (helix-surround--read-char))
             (beg (set-marker (make-marker) (region-beginning)))
             (end (-doto (make-marker)
                    (set-marker-insertion-type t)
                    (set-marker (region-end))))
             (linewise? (eq (helix-linewise-selection-p) 'line)))
        (when linewise?
          (setq left  (s-trim left)
                right (s-trim right)))
        (goto-char beg)
        (insert left)
        (when linewise? (newline))
        (goto-char end)
        (insert right)
        (when linewise? (newline))
        (indent-region beg end)
        (set-marker beg nil)
        (set-marker end nil)))
    (helix-extend-selection -1)))

(put 'helix-surround 'multiple-cursors t)

;; md
(defun helix-surround-delete ()
  (interactive)
  (when-let* ((key (read-char "Delete pair: "))
              (bounds (helix-surround--4-bounds key)))
    (-let [(left-beg left-end right-beg right-end) bounds]
      (delete-region right-beg right-end)
      (delete-region left-beg left-end))))

(put 'helix-surround-delete 'multiple-cursors t)

;; mr
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

(put 'helix-surround-change 'multiple-cursors t)

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

(put 'helix-window-vsplit 'multiple-cursors 'false)

(defun helix-window-left (count)
  "Move the cursor to new COUNT-th window left of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-left)))

(put 'helix-window-left 'multiple-cursors 'false)

(defun helix-window-right (count)
  "Move the cursor to new COUNT-th window right of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-right)))

(put 'helix-window-right 'multiple-cursors 'false)

(defun helix-window-up (count)
  "Move the cursor to new COUNT-th window up of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-up)))

(put 'helix-window-up 'multiple-cursors 'false)

(defun helix-window-down (count)
  "Move the cursor to new COUNT-th window down of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-down)))

(put 'helix-window-down 'multiple-cursors 'false)

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

(put 'helix-move-window-left 'multiple-cursors 'false)

(defun helix-move-window-right ()
  "Swap window with one to the right."
  (interactive)
  (helix-move-window 'right))

(put 'helix-move-window-right 'multiple-cursors 'false)

(defun helix-move-window-up ()
  "Swap window with one upwards."
  (interactive)
  (helix-move-window 'up))

(put 'helix-move-window-up 'multiple-cursors 'false)

(defun helix-move-window-down ()
  "Swap window with one downwards."
  (interactive)
  (helix-move-window 'down))

(put 'helix-move-window-down 'multiple-cursors 'false)

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

(put 'helix-window-delete 'multiple-cursors 'false)

(provide 'helix-commands)
;;; helix-commands.el ends here
