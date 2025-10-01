;;; helix-commands.el --- Helix commands -*- lexical-binding: t; -*-
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
(require 'helix-search)
(require 'avy)

;; ESC in normal state
(helix-define-command helix-normal-state-escape ()
  "Command for ESC key in Helix Normal state."
  :multiple-cursors t
  (interactive)
  (cond (helix--extend-selection
         (helix-extend-selection -1))
        (t
         (deactivate-mark))))

;;; Motions

;; h
(helix-define-command helix-backward-char (count)
  "Move backward COUNT chars."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (helix-maybe-deactivate-mark)
  (backward-char count))

;; l
(helix-define-command helix-forward-char (count)
  "Move forward COUNT chars."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (helix-maybe-deactivate-mark)
  (forward-char count))

;; j
(helix-define-command helix-next-line (count)
  "Move to the next COUNT line. Move upward if COUNT is negative.
If both linewise selection (`x' key) and extending selection (`v' key)
are active — works like `helix-expand-line-selection'."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (if (and helix--extend-selection (helix-logical-lines-p))
      (helix-expand-line-selection count)
    ;; else
    (helix-maybe-deactivate-mark)
    ;; Preserve the column: the behaviour is hard-coded and the column is
    ;; preserved if and only if the previous was `next-line' or `previous-line'.
    (setq this-command (if (natnump count) 'next-line 'previous-line))
    (funcall-interactively 'next-line count)))

;; k
(helix-define-command helix-previous-line (count)
  "Move to the previous COUNT line. Move downward if COUNT is negative.
If both linewise selection (`x' key) and extending selection (`v' key)
are active — works like `helix-expand-line-selection-backward'."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (helix-next-line (- count)))

;; w
(helix-define-command helix-forward-word-start (count)
  "Move to the COUNT-th next word start."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (helix--forward-word-start 'helix-word count))

;; W
(helix-define-command helix-forward-WORD-start (count)
  "Move to the COUNT-th next WORD start."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (helix--forward-word-start 'helix-WORD count))

;; b
(helix-define-command helix-backward-word-start (count)
  "Move to the COUNT-th previous word start."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (helix--backward-word-start 'helix-word count))

;; B
(helix-define-command helix-backward-WORD-start (count)
  "Move to the COUNT-th previous WORD start."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (helix--backward-word-start 'helix-WORD count))

;; e
(helix-define-command helix-forward-word-end (count)
  "Move to the COUNT-th next word end."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (helix--forward-word-end 'helix-word count))

;; E
(helix-define-command helix-forward-WORD-end (count)
  "Move COUNT-th next WORD end."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (helix--forward-word-end 'helix-WORD count))

;; gg
(helix-define-command helix-beginning-of-buffer (num)
  "Move point to the beginning of the buffer.
With numeric arg NUM, put point NUM/10 of the way from the beginning.
If the buffer is narrowed, this command uses the beginning of the
accessible part of the buffer.
Push mark at previous position, unless extending selection."
  :multiple-cursors nil
  (interactive "P")
  (helix-delete-all-fake-cursors)
  (helix-push-point)
  (helix-maybe-deactivate-mark)
  (if num
      (progn
        (goto-char (+ (point-min)
                      (/ (* (- (point-max) (point-min))
                            (prefix-numeric-value num))
                         10)))
        (forward-line 1))
    ;; else
    (goto-char (point-min))
    (recenter 0)))

;; G
(helix-define-command helix-end-of-buffer ()
  "Move point the end of the buffer."
  :multiple-cursors nil
  (interactive)
  (helix-delete-all-fake-cursors)
  (helix-push-point)
  (helix-maybe-deactivate-mark)
  (goto-char (point-max)))

;; gs
(helix-define-command helix-beginning-of-line-command ()
  "Move point to beginning of current line.
Use visual line when `visual-line-mode' is active."
  :multiple-cursors t
  :merge-selections t
  (declare (interactive-only helix-beginning-of-line-command))
  (interactive)
  (helix-set-region (if helix--extend-selection (mark) (point))
                    (helix-beginning-of-line)))

;; gh
(helix-define-command helix-first-non-blank ()
  "Move point to beginning of current line skipping indentation.
Use visual line when `visual-line-mode' is active."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (helix-set-region (if helix--extend-selection (mark) (point))
                    (progn (helix-beginning-of-line)
                           (skip-syntax-forward " " (line-end-position))
                           (backward-prefix-chars)
                           (point))))

;; gl
(helix-define-command helix-end-of-line-command ()
  "Move point to end of current line.
Use visual line when `visual-line-mode' is active."
  :multiple-cursors t
  :merge-selections t
  (declare (interactive-only helix-end-of-line-command))
  (interactive)
  (helix-set-region (if helix--extend-selection (mark) (point))
                    (helix-end-of-line))
  ;; "Stick" cursor to the end of line after moving to it. Vertical
  ;; motions right after "gl" will place point at the end of each line.
  (when (and (not visual-line-mode)
             (eolp))
    (setq temporary-goal-column most-positive-fixnum
          this-command 'next-line)))

;; ]p or } (Helix editor emulation)
(helix-define-command helix-mark-forward-to-beginning-of-paragraph (count)
  "Select from point to the start of the COUNT-th next paragraph.
This function behaves the same as `]p' in the Helix text editor."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-push-point)
  (helix-restore-newline-at-eol)
  (helix-set-region (if helix--extend-selection (mark) (point))
                    (progn
                      (helix-forward-beginning-of-thing 'helix-paragraph count)
                      (point))
                    nil :adjust))

;; [p or { (Helix editor emulation)
(helix-define-command helix-mark-backward-to-beginning-of-paragraph (count)
  "Select from point to the start of the COUNT-th previous paragraph.
This function behaves the same as `[p' in the Helix text editor."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-push-point)
  (helix-restore-newline-at-eol)
  (helix-set-region (if helix--extend-selection (mark) (point))
                    (progn (forward-thing 'helix-paragraph (- count))
                           (point))
                    nil :adjust))

;; ]p or } (alternative version)
(helix-define-command helix-mark-paragraph-forward (count)
  "Select from point to the end of the paragraph (or COUNT-th next paragraphs).
If no paragraph at point select COUNT next paragraphs."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-mark-thing-forward 'helix-paragraph count))

;; [p or { (alternative version)
(helix-define-command helix-mark-paragraph-backward (count)
  "Select from point to the start of the paragraph (or COUNT-th next paragraphs).
If no paragraph at point select COUNT previous paragraphs."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-mark-thing-forward 'helix-paragraph (- count)))

;; ]f (Helix editor emulation)
(helix-define-command helix-mark-forward-to-beginning-of-function (count)
  "Select from point to the start of the COUNT-th next function.
This function behaves the same as `]f' in the Helix text editor."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-push-point)
  (helix-restore-newline-at-eol)
  (helix-set-region (if helix--extend-selection (mark) (point))
                    (progn
                      (helix-forward-beginning-of-thing 'helix-function count)
                      (point))
                    nil :adjust))

;; [f (Helix editor emulation)
(helix-define-command helix-mark-backward-to-beginning-of-function (count)
  "Select from point to the start of the COUNT-th previous function.
This function behaves the same as `[f' in the Helix text editor."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-push-point)
  (helix-restore-newline-at-eol)
  (helix-set-region (if helix--extend-selection (mark) (point))
                    (progn (forward-thing 'helix-function (- count))
                           (point))
                    nil :adjust))

;; ]f (alternative version)
(helix-define-command helix-mark-function-forward (count)
  "Select from point to the end of the function (or COUNT-th next functions).
If no function at point select COUNT next functions."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-mark-thing-forward 'helix-function count))

;; [f (alternative version)
(helix-define-command helix-mark-function-backward (count)
  "Select from point to the end of the function (or COUNT-th next functions).
If no function at point select COUNT previous functions."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-mark-thing-forward 'helix-function (- count)))

;; ]s
(helix-define-command helix-mark-sentence-forward (count)
  "Select from point to the end of the sentence (or COUNT-th next sentences).
If no sentence at point select COUNT next sentences."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-mark-thing-forward 'helix-sentence count))

;; [s
(helix-define-command helix-mark-sentence-backward (count)
  "Select from point to the start of the sentence (or COUNT-th next sentences).
If no sentence at point select COUNT previous sentences."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-mark-thing-forward 'helix-sentence (- count)))

;; mm
;; TODO: The most bare-boned version. Need upgrade.
(helix-define-command helix-jump-to-match-item ()
  "Jump between matching brackets."
  :multiple-cursors t
  (interactive)
  (helix-maybe-deactivate-mark)
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
      (up-list 1))))
  (helix-reveal-point-when-on-top))

;; C-o
(helix-define-command helix-backward-mark-ring ()
  "Jump to the top position on `mark-ring'.
If point is already there, rotate `mark-ring' forward (like revolver cylinder)
and jump to new top position.

In Emacs, mark has two purposes: when active, it acts as a boundary for region;
when inactive, it can be used to store the previous significant position. The
Helix approach is based on selections, and mark is never used to store previous
position. So unlike the `pop-to-mark-command' which puts the value from `mark'
into `point' and value from `mark-ring' into `mark', this command puts the value
from `mark-ring' directly into `point' skipping `mark'.

\(Does not affect global mark ring)."
  :multiple-cursors t
  (interactive)
  (helix--jump-over-mark-ring))

;; C-i
(helix-define-command helix-forward-mark-ring ()
  "Jump to the top position on `mark-ring'.
If point is already there, rotate `mark-ring' backward and jump to new top
position. See `helix-backward-mark-ring'.

\(Does not affect global mark ring)."
  :multiple-cursors t
  (interactive)
  (helix--jump-over-mark-ring t))

;; C-S-o
(helix-define-command helix-backward-global-mark-ring ()
  "Jump to the top location on `global-mark-ring'.
If current buffer is the same as the target one, rotate `global-mark-ring'
forward (like revolver cylinder) and jump to new top location."
  :multiple-cursors t
  (interactive)
  (helix--jump-over-global-mark-ring))

;; C-S-i
(helix-define-command helix-forward-global-mark-ring ()
  "Jump to the top location on `global-mark-ring'.
If current buffer is the same as the target one, rotate `global-mark-ring'
backward and jump to new top location."
  :multiple-cursors t
  (interactive)
  (helix--jump-over-global-mark-ring t))

;;; Avy (Easymotion)

;; gw
(helix-define-command helix-avy-word-forward ()
  "Move to a word start after the point, choosing it with Avy."
  :multiple-cursors nil
  (interactive)
  (let ((orig-point (point)))
    (when (let ((avy-all-windows nil))
            (-> (avy--regex-candidates avy-goto-word-0-regexp
                                       (point) (window-end nil t))
                (avy-process)))
      (helix-delete-all-fake-cursors)
      (helix-push-point orig-point)
      (helix-set-region (if helix--extend-selection (mark) (point))
                        (progn (forward-thing 'helix-word)
                               (point))))))

;; gb
(helix-define-command helix-avy-word-backward ()
  "Move to a word start before the point, choosing it with Avy."
  :multiple-cursors nil
  (interactive)
  (let ((orig-point (point)))
    (when (let ((avy-all-windows nil))
            (-> (avy--regex-candidates avy-goto-word-0-regexp
                                       (window-start) (point))
                (nreverse)
                (avy-process)))
      (helix-delete-all-fake-cursors)
      (helix-push-point orig-point)
      (if helix--extend-selection
          (helix-set-region (mark) (point))
        (helix-set-region (point) (progn
                                    (forward-thing 'helix-word)
                                    (point)))))))

;; gW
(helix-define-command helix-avy-WORD-forward ()
  "Move to a WORD start after the point, choosing it with Avy."
  :multiple-cursors nil
  (interactive)
  (let ((orig-point (point)))
    (when (let ((avy-all-windows nil))
            (-> (avy--regex-candidates "[^ \r\n\t]+" (point) (window-end nil t))
                (avy-process)))
      (helix-delete-all-fake-cursors)
      (helix-push-point orig-point)
      (helix-set-region (if helix--extend-selection (mark) (point))
                        (progn (forward-thing 'helix-WORD)
                               (point))))))

;; gB
(helix-define-command helix-avy-WORD-backward ()
  "Move to a WORD start before the point, choosing it with Avy."
  :multiple-cursors nil
  (interactive)
  (let ((orig-point (point)))
    (when (let ((avy-all-windows nil))
            (-> (avy--regex-candidates "[^ \r\n\t]+" (window-start) (point))
                (nreverse)
                (avy-process)))
      (helix-delete-all-fake-cursors)
      (helix-push-point orig-point)
      (if helix--extend-selection
          (helix-set-region (mark) (point))
        (helix-set-region (point) (progn
                                    (forward-thing 'helix-WORD)
                                    (point)))))))

;; gj
(helix-define-command helix-avy-next-line ()
  "Move to a following line, selected using Avy.
When both linewise selection is active (via `x') and selection expansion
is enabled (via `v'), the selection will expand linewise to include all lines
to the chosen one."
  :multiple-cursors nil
  (interactive)
  (when-let* ((pos (save-excursion
                     (let ((temporary-goal-column (current-column)))
                       (-> (helix-collect-positions #'next-line)
                           (avy-process)))))
              ((natnump pos)))
    (helix-delete-all-fake-cursors)
    (helix-push-point)
    (if helix--extend-selection
        (let ((lines? (helix-logical-lines-p)))
          (helix-set-region (mark) pos)
          (when lines? (helix-expand-selection-to-full-lines)))
      ;; else
      (deactivate-mark)
      (goto-char pos))))

;; gk
(helix-define-command helix-avy-previous-line ()
  "Move to a preceding line, selected using Avy.
When both linewise selection is active (via `x') and selection expansion
is enabled (via `v'), the selection will expand linewise to include all lines
to the chosen one."
  :multiple-cursors nil
  (interactive)
  (when-let* ((pos (save-excursion
                     (let ((temporary-goal-column (current-column)))
                       (-> (helix-collect-positions #'previous-line)
                           (avy-process)))))
              ((natnump pos)))
    (helix-delete-all-fake-cursors)
    (helix-push-point)
    (if helix--extend-selection
        (let ((lines? (helix-logical-lines-p)))
          (helix-set-region (mark) pos)
          (when lines? (helix-expand-selection-to-full-lines)))
      ;; else
      (deactivate-mark)
      (goto-char pos))))

;;; Changes

;; i
(helix-define-command helix-insert ()
  "Switch to Insert state before region."
  :multiple-cursors nil
  (interactive)
  (helix-with-each-cursor
    (helix-ensure-region-direction -1))
  (helix-insert-state 1))

;; a
(helix-define-command helix-append ()
  "Switch to Insert state after region."
  :multiple-cursors nil
  (interactive)
  (helix-with-each-cursor
    (helix-ensure-region-direction 1))
  (helix-insert-state 1))

;; I
(helix-define-command helix-insert-line ()
  "Switch to insert state at beginning of current line."
  :multiple-cursors nil
  (interactive)
  (helix--insert-or-append-on-line -1))

;; A
(helix-define-command helix-append-line ()
  "Switch to Insert state at the end of the current line."
  :multiple-cursors nil
  (interactive)
  (helix--insert-or-append-on-line 1))

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

;; o
(helix-define-command helix-open-below ()
  "Open new line below selection."
  :multiple-cursors nil
  (interactive)
  (helix-with-each-cursor
    (when (use-region-p) (helix-ensure-region-direction 1))
    (move-end-of-line nil)
    (newline-and-indent)
    (set-marker (mark-marker) (point)))
  (helix-insert-state 1))

;; O
(helix-define-command helix-open-above ()
  "Open new line above selection."
  :multiple-cursors nil
  (interactive)
  (helix-with-each-cursor
    (when (use-region-p) (helix-ensure-region-direction -1))
    (move-beginning-of-line nil)
    (newline)
    (forward-line -1)
    (indent-according-to-mode)
    (set-marker (mark-marker) (point)))
  (helix-insert-state 1))

;; ] SPC
(helix-define-command helix-add-blank-line-below (count)
  "Add COUNT blank lines below selection."
  :multiple-cursors t
  (interactive "p")
  (let ((deactivate-mark nil))
    (helix-save-region ;; save-mark-and-excursion
      (helix-ensure-region-direction 1)
      (helix--forward-line 1)
      (newline count))))

;; [ SPC
(helix-define-command helix-add-blank-line-above (count)
  "Add COUNT blank lines above selection."
  :multiple-cursors t
  (interactive "p")
  (helix-save-region
    (helix-ensure-region-direction -1)
    (helix--beginning-of-line)
    (newline count)))

;; c
(helix-define-command helix-change ()
  "Delete region and enter Insert state."
  :multiple-cursors nil
  (interactive)
  (helix-with-each-cursor
    (cond ((use-region-p)
           (let ((logical-lines? (helix-logical-lines-p))
                 (visual-lines? (helix-visual-lines-p)))
             (kill-region nil nil t)
             (cond (logical-lines?
                    (indent-according-to-mode))
                   (visual-lines?
                    (insert " ")
                    (backward-char)))))
          ((not (helix-bolp))
           (delete-char -1))
          ((bolp)
           (indent-according-to-mode))))
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
(helix-define-command helix-cut (count)
  "Kill (cut) text in region. I.e. delete text and put it in the `kill-ring'.
If no selection — delete COUNT chars before point."
  :multiple-cursors t
  (interactive "p")
  (when (helix-logical-lines-p)
    (helix-restore-newline-at-eol))
  (cond ((use-region-p)
         (kill-region nil nil t))
        (t
         (delete-char (- count))))
  (helix-extend-selection -1))

;; D
(helix-define-command helix-delete (count)
  "Delete text in region, without modifying the `kill-ring'.
If no selection — delete COUNT chars after point."
  :multiple-cursors t
  (interactive "p")
  (when (helix-logical-lines-p)
    (helix-restore-newline-at-eol))
  (cond ((use-region-p)
         (delete-region (region-beginning) (region-end)))
        (t
         (delete-char count)))
  (helix-extend-selection -1))

;; C-w in insert state
(helix-define-command helix-delete-backward-word ()
  :multiple-cursors t
  (interactive)
  (delete-region (point) (progn
                           (helix-backward-word-start 1)
                           (point))))

;; u
(helix-define-command helix-undo ()
  "Cancel current region then undo."
  :multiple-cursors nil
  (interactive)
  ;; Deactivate mark to trigger global undo instead of region undo.
  (deactivate-mark)
  (let ((deactivate-mark nil))
    (undo-only)))

;; U
(helix-define-command helix-redo ()
  "Cancel current region then redo."
  :multiple-cursors nil
  (interactive)
  ;; Deactivate mark to trigger global undo instead of region undo.
  (deactivate-mark)
  (let ((deactivate-mark nil))
    (undo-redo)))

;; y
(helix-define-command helix-copy ()
  "Copy selection into `kill-ring'."
  :multiple-cursors nil
  (interactive)
  (let ((deactivate-mark nil)
        any?)
    (helix-with-each-cursor
      (when (use-region-p)
        (copy-region-as-kill (region-beginning) (if helix--newline-at-eol
                                                    (1+ (region-end))
                                                  (region-end)))
        (setq any? t))
      (helix-extend-selection -1))
    (when any? (message "Copied into kill-ring")))
  (helix-maybe-set-killed-rectangle)
  (helix-pulse-main-region))

(defun helix-maybe-set-killed-rectangle ()
  "Add the latest `kill-ring' entry for each cursor to `killed-rectangle',
unless they all are equal. You can paste them later with `yank-rectangle'."
  (when helix-multiple-cursors-mode
    (let ((entries (helix-with-real-cursor-as-fake
                     (-map (lambda (cursor)
                             (car-safe (overlay-get cursor 'kill-ring)))
                           (helix-all-fake-cursors :sort)))))
      (unless (helix-all-elements-are-equal-p entries)
        (setq killed-rectangle entries)))))

;; p
(helix-define-command helix-paste-after ()
  "Paste after selection."
  :multiple-cursors t
  (interactive)
  (helix-paste #'yank 1))

;; P
(helix-define-command helix-paste-before ()
  "Paste before selection."
  :multiple-cursors t
  (interactive)
  (helix-paste #'yank -1))

;; C-p
(helix-define-command helix-paste-pop (count)
  "Replace just-pasted text with next COUNT element from `kill-ring'."
  :multiple-cursors t
  (interactive "p")
  (helix-disable-newline-at-eol)
  (let ((deactivate-mark nil))
    (let ((yank-pop (or (command-remapping 'yank-pop)
                        #'yank-pop)))
      (funcall-interactively yank-pop count))
    (if (and (mark t)
             (/= (point) (mark t)))
        (activate-mark)
      (deactivate-mark))))

;; C-n
(helix-define-command helix-paste-undo-pop (count)
  "Replace just-pasted text with previous COUNT element from `kill-ring'."
  :multiple-cursors t
  (interactive "p")
  (helix-paste-pop (- count)))

;; R
(helix-define-command helix-replace-with-kill-ring ()
  "Replace selection content with yanked text from `kill-ring'."
  :multiple-cursors t
  (interactive)
  (when (use-region-p)
    (when (helix-string-ends-with-newline (current-kill 0 :do-not-move))
      (helix-restore-newline-at-eol))
    (let ((deactivate-mark nil)
          (dir (helix-region-direction)))
      (delete-region (region-beginning) (region-end))
      (cl-letf (((symbol-function 'push-mark) #'helix-push-mark))
        (yank))
      (helix-set-region (mark t) (point) dir :adjust)
      (helix-extend-selection -1))))

;; J
(helix-define-command helix-join-line ()
  "Join the selected lines."
  :multiple-cursors t
  (interactive)
  (helix-save-region
    (let* ((deactivate-mark nil)
           (region? (use-region-p))
           (count (let ((n (if region?
                               (count-lines (region-beginning) (region-end))
                             1)))
                    (if (> n 1) (1- n) n))))
      (when region? (goto-char (region-beginning)))
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
        (fixup-whitespace)))))

;; ~
(helix-define-command helix-invert-case ()
  "Invert case of characters."
  :multiple-cursors t
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
(helix-define-command helix-downcase ()
  "Convert text in selection to lower case.
With no selection downcase the character after point."
  :multiple-cursors t
  (interactive)
  (if (use-region-p)
      (let ((deactivate-mark nil))
        (downcase-region (region-beginning) (region-end)))
    (downcase-region (point) (progn (forward-char) (point)))))

;; M-` or gU
(helix-define-command helix-upcase ()
  "Convert text in selection to upper case.
With no selection upcase the character after point."
  :multiple-cursors t
  (interactive)
  (if (use-region-p)
      (let ((deactivate-mark nil))
        (upcase-region (region-beginning) (region-end)))
    (upcase-region (point) (progn (forward-char) (point)))))

;;; Selections

;; M-;
(helix-define-command helix-exchange-point-and-mark ()
  "Exchange point and mark."
  :multiple-cursors t
  (interactive)
  (when (use-region-p)
    (helix--exchange-point-and-mark)
    (when (and (not helix-executing-command-for-fake-cursor)
               (called-interactively-p 'any))
      (helix-reveal-point-when-on-top))))

;; v
(helix-define-command helix-extend-selection (arg)
  "Toggle extending selections.
If ARG is nil — toggle extending selection.
If ARG positive number — enable, negative — disable."
  :multiple-cursors t
  (interactive `(,(if helix--extend-selection -1 1)))
  (pcase arg
    (-1 (setq helix--extend-selection nil)
        (unless helix-executing-command-for-fake-cursor
          (helix-update-cursor)))
    (_ (setq helix--extend-selection t)
       (or (region-active-p)
           (set-mark (point)))
       (unless helix-executing-command-for-fake-cursor
         (set-cursor-color (face-attribute 'helix-extend-selection-cursor
                                           :background))))))

;; ;
(helix-define-command helix-collapse-selection ()
  "Deactivate selection."
  :multiple-cursors t
  (interactive)
  (if helix--extend-selection
      (progn
        (helix-disable-newline-at-eol)
        (set-mark (point)))
    (deactivate-mark)))

;; x
(helix-define-command helix-expand-line-selection (count)
  "Expand or contract current selection linewise downward COUNT times."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (let ((motion-dir (helix-sign count)))
    (and (helix-expand-selection-to-full-lines motion-dir)
         (setq count (- count motion-dir)))
    (unless (zerop count)
      (helix-restore-newline-at-eol)
      (let* ((line (if visual-line-mode 'helix-visual-line 'helix-line))
             (region-dir (helix-region-direction))
             (end (progn (forward-thing line count)
                         (when (= (point) (mark-marker))
                           (forward-thing line motion-dir))
                         (point)))
             (start (if (/= region-dir (helix-region-direction))
                        (save-excursion
                          (goto-char (mark-marker))
                          (forward-thing line (- motion-dir))
                          (point))
                      (mark))))
        (helix-set-region start end nil :adjust)))))

;; X
(helix-define-command helix-expand-line-selection-backward (count)
  "Expand or contract current selection linewise upward COUNT times."
  :multiple-cursors t
  (interactive "p")
  (helix-expand-line-selection (- count)))

;; %
(helix-define-command helix-mark-whole-buffer ()
  :multiple-cursors nil
  (interactive)
  (helix-delete-all-fake-cursors)
  (helix-push-point)
  ;; `minibuffer-prompt-end'is really `point-min' in most cases, but if we're
  ;; in the minibuffer, this is at the end of the prompt.
  (helix-set-region (minibuffer-prompt-end) (point-max) -1 :adjust))

;; s
(helix-define-command helix-select-regex (&optional invert)
  "Create new selections for all matches to the regexp entered withing current
selections.

If INVERT is non-nil — create new selections for all regions that NOT match to
entered regexp withing current selections."
  :multiple-cursors nil
  (interactive)
  (when (region-active-p)
    (helix-with-real-cursor-as-fake
      (let* ((cursors (helix-all-fake-cursors))
             (ranges (-map (lambda (cursor)
                             (if (overlay-get cursor 'mark-active)
                                 (let ((point (marker-position
                                               (overlay-get cursor 'point)))
                                       (mark (marker-position
                                              (overlay-get cursor 'mark))))
                                   (if (< point mark)
                                       (cons point mark)
                                     (cons mark point)))))
                           cursors)))
        (-each cursors #'helix-hide-fake-cursor)
        (if (helix-select-interactively-in ranges invert)
            (-each cursors #'helix--delete-fake-cursor)
          ;; Restore original cursors
          (-each cursors #'helix-show-fake-cursor))))))

;; S
(helix-define-command helix-split-region ()
  "Split each selection according to the regexp entered."
  :multiple-cursors nil
  (interactive)
  (helix-select-regex t))

;; M-s
(helix-define-command helix-split-region-on-newline ()
  "Split selections on line boundaries."
  :multiple-cursors nil
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

;; K
(helix-define-command helix-keep-selections ()
  "Keep selections that match to the regexp entered."
  :multiple-cursors nil
  (interactive)
  (helix-filter-selections))

;; M-K
(helix-define-command helix-remove-selections ()
  "Remove selections that match to the regexp entered."
  :multiple-cursors nil
  (interactive)
  (helix-filter-selections t))

;; _
(helix-define-command helix-trim-whitespaces-from-selection ()
  "Trim whitespaces and newlines from the both ends of selections."
  :multiple-cursors t
  (interactive)
  (when (use-region-p)
    (let* ((dir (helix-region-direction))
           (mark (progn (helix-skip-chars " \t\r\n" (- dir))
                        (point)))
           (point (progn (goto-char (mark-marker))
                         (helix-skip-chars " \t\r\n" dir)
                         (point))))
      (helix-set-region mark point))))

;; &
(helix-define-command helix-align-selections ()
  "Align selections."
  :multiple-cursors nil
  (interactive)
  (helix-with-real-cursor-as-fake
    (let ((rest (-partition-by (lambda (cursor)
                                 (line-number-at-pos (overlay-get cursor 'point)))
                               (helix-all-fake-cursors :sort)))
          cursors)
      (while (progn (setq cursors (mapcar #'car rest))
                    (length> cursors 1))
        (setq rest (->> rest
                        (mapcar #'cdr)
                        (delq nil)))
        (let ((column (-reduce-from (lambda (column cursor)
                                      (goto-char (overlay-get cursor 'point))
                                      (max column (current-column)))
                                    0 cursors)))
          ;; Align
          (helix-save-window-scroll
            (dolist (cursor cursors)
              (helix-with-fake-cursor cursor
                (unless (= (current-column) column)
                  (let ((deactivate-mark nil)
                        (padding (s-repeat (- column (current-column)) " ")))
                    (cond ((and (use-region-p)
                                (natnump (helix-region-direction)))
                           (helix--exchange-point-and-mark)
                           (insert padding)
                           (helix--exchange-point-and-mark))
                          (t
                           (insert padding)))))))))))))

;; C
(helix-define-command helix-copy-selection (count)
  "Copy selections COUNT times down if COUNT is positive, or up if negative."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (helix-with-each-cursor
    (helix-disable-newline-at-eol)
    (helix-motion-loop (dir count)
      (if (use-region-p)
          (helix--copy-region dir)
        (helix--copy-cursor dir)))))

;; M-c
(helix-define-command helix-copy-selection-up (count)
  "Copy each selection COUNT times up."
  :multiple-cursors nil
  :merge-selections t
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
    (unless (helix-fake-cursor-at (point))
      (helix-create-fake-cursor-from-point))
    (goto-char pos)))

(defun helix--copy-region (direction)
  "Copy region toward the DIRECTION."
  (let* ((region-dir (helix-region-direction))
         (beg (region-beginning))
         (end (region-end))
         (num-of-lines (count-lines beg end))
         (beg-column (save-excursion (goto-char beg) (current-column)))
         (end-column (save-excursion (goto-char end) (current-column))))
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

(defun helix--bounds-of-following-region ( start-column end-column
                                           number-of-lines direction)
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
(helix-define-command helix-delete-all-fake-cursors ()
  "Delete all fake cursors from current buffer."
  (interactive)
  (when helix-multiple-cursors-mode
    (helix-multiple-cursors-mode -1)))

;; M-,
(helix-define-command helix-remove-main-cursor ()
  "Delete main cursor and activate the next fake one."
  :multiple-cursors nil
  (interactive)
  (when helix-multiple-cursors-mode
    (helix-restore-point-from-fake-cursor (or (helix-next-fake-cursor (point))
                                              (helix-first-fake-cursor)))
    (helix-auto-multiple-cursors-mode)))

;; M-minus
(helix-define-command helix-merge-selections ()
  "Merge all cursors into single selection."
  :multiple-cursors nil
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
      (helix-delete-all-fake-cursors)
      (helix-set-region beg end 1))))

;; )
(helix-define-command helix-rotate-selections-forward (count)
  "Rotate main selection forward COUNT times."
  :multiple-cursors nil
  (interactive "p")
  (when helix-multiple-cursors-mode
    (helix-recenter-point-on-jump
      (dotimes (_ count)
        (let ((cursor (or (helix-next-fake-cursor (point))
                          (helix-first-fake-cursor))))
          (helix-create-fake-cursor-from-point)
          (helix-restore-point-from-fake-cursor cursor))))))

;; (
(helix-define-command helix-rotate-selections-backward (count)
  "Rotate main selection backward COUNT times."
  :multiple-cursors nil
  (interactive "p")
  (when helix-multiple-cursors-mode
    (helix-recenter-point-on-jump
      (dotimes (_ count)
        (let ((cursor (or (helix-previous-fake-cursor (point))
                          (helix-last-fake-cursor))))
          (helix-create-fake-cursor-from-point)
          (helix-restore-point-from-fake-cursor cursor))))))

;; M-)
(helix-define-command helix-rotate-selections-content-forward (count)
  "Rotate selections content forward COUNT times."
  :multiple-cursors nil
  (interactive "p")
  (helix--rotate-selections-content count))

;; M-(
(helix-define-command helix-rotate-selections-content-backward (count)
  "Rotate selections content backward COUNT times."
  :multiple-cursors nil
  (interactive "p")
  (helix--rotate-selections-content count :backward))

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
        (helix-with-each-cursor (helix--exchange-point-and-mark)))
      (helix-with-real-cursor-as-fake
        (let ((cursors (helix-all-fake-cursors :sort)))
          (when backward
            (setq cursors (nreverse cursors)))
          (dotimes (_ count)
            (helix--rotate-selections-content-1 cursors))))
      ;; Restore original regions direction.
      (unless (eql dir (helix-region-direction))
        (helix-with-each-cursor (helix--exchange-point-and-mark))))))

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
(helix-define-command helix-toggle-cursor-on-click (event)
  "Add a cursor where you click, or remove a fake cursor that is
already there."
  :multiple-cursors nil
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
          (helix-delete-fake-cursor cursor)
        (helix-create-fake-cursor pos)))))

;;;; Mark

;; miw
(helix-define-command helix-mark-inner-word (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-mark-inner-thing 'helix-word count))

;; miW
(helix-define-command helix-mark-inner-WORD (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-mark-inner-thing 'helix-WORD count))

;; maw
(helix-define-command helix-mark-a-word ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (helix--mark-a-word 'helix-word))

;; maW
(helix-define-command helix-mark-a-WORD ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (helix--mark-a-word 'helix-WORD))

;; mis
(helix-define-command helix-mark-inner-sentence (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-mark-inner-thing 'helix-sentence count))

;; mas
(helix-define-command helix-mark-a-sentence (&optional thing)
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (or thing (setq thing 'helix-sentence))
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
      (helix-set-region beg end))))

;; mip
(helix-define-command helix-mark-inner-paragraph (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-push-point)
  (helix-mark-inner-thing 'helix-paragraph count t)
  (helix-reveal-point-when-on-top))

;; map
(helix-define-command helix-mark-a-paragraph (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-push-point)
  (helix-mark-a-thing 'helix-paragraph count t)
  (helix-reveal-point-when-on-top))

;; mif
(helix-define-command helix-mark-inner-function (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-push-point)
  (helix-mark-inner-thing 'helix-function count t)
  (helix-ensure-region-direction -1)
  (helix-reveal-point-when-on-top))

;; maf
(helix-define-command helix-mark-a-function (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-push-point)
  (helix-mark-a-thing 'helix-function count t)
  (helix-reveal-point-when-on-top))

;; mi"
(helix-define-command helix-mark-inner-double-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?\"))
    (helix-set-region (1+ beg) (1- end))))

;; ma"
(helix-define-command helix-mark-a-double-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?\"))
    (helix-set-region beg end)))

;; mi'
(helix-define-command helix-mark-inner-single-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?'))
    (helix-set-region (1+ beg) (1- end))))

;; ma'
(helix-define-command helix-mark-a-single-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?'))
    (helix-set-region beg end)))

;; mi`
(helix-define-command helix-mark-inner-back-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?`))
    (helix-set-region (1+ beg) (1- end))))

;; ma`
(helix-define-command helix-mark-a-back-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-quoted-at-point ?`))
    (helix-set-region beg end)))

;; mi( mi)
(helix-define-command helix-mark-inner-paren ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((_ beg end _) (helix-4-bounds-of-brackets-at-point ?\( ?\)))
    (helix-set-region beg end)))

;; ma( ma)
(helix-define-command helix-mark-a-paren ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-brackets-at-point ?\( ?\)))
    (helix-set-region beg end)))

;; mi[ mi]
(helix-define-command helix-mark-inner-bracket ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((_ beg end _) (helix-4-bounds-of-brackets-at-point ?\[ ?\]))
    (helix-set-region beg end)))

;; ma[ ma]
(helix-define-command helix-mark-a-bracket ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-brackets-at-point ?\[ ?\]))
    (helix-set-region beg end)))

;; mi{ mi}
(helix-define-command helix-mark-inner-curly ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((_ beg end _) (helix-4-bounds-of-brackets-at-point ?{ ?}))
    (helix-set-region beg end)))

;; ma{ ma}
(helix-define-command helix-mark-a-curly ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg . end) (helix-bounds-of-brackets-at-point ?{ ?}))
    (helix-set-region beg end)))

;; mi< mi>
(helix-define-command helix-mark-inner-angle ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((_ beg end _) (helix-4-bounds-of-brackets-at-point ?< ?>))
    (helix-set-region beg end)))

;; ma< ma>
(helix-define-command helix-mark-an-angle ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg _ _ end) (helix-4-bounds-of-brackets-at-point ?< ?>))
    (helix-set-region beg end)))

(helix-define-command helix-mark-inner-surround ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when-let* ((char (if (characterp last-command-event)
                        last-command-event
                      (get last-command-event 'ascii-character)))
              (bounds (helix-surround--4-bounds char)))
    (-let [(_ beg end _) bounds]
      (helix-set-region beg end))))

(helix-define-command helix-mark-a-surround ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when-let* ((char (if (characterp last-command-event)
                        last-command-event
                      (get last-command-event 'ascii-character)))
              (bounds (helix-surround--4-bounds char)))
    (-let [(beg _ _ end) bounds]
      (helix-set-region beg end))))

;;; Search

;; f
(helix-define-command helix-find-char-forward (count)
  "Prompt user for CHAR and move to the next COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (let ((char (read-char "f")))
    (helix-maybe-set-mark)
    (helix-motion-loop (dir count)
      (helix-find-char char dir nil))))

;; F
(helix-define-command helix-find-char-backward (count)
  "Prompt user for CHAR and move to the previous COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (setq count (- count))
  (let ((char (read-char "F")))
    (helix-maybe-set-mark)
    (helix-motion-loop (dir count)
      (helix-find-char char dir nil))))

;; t
(helix-define-command helix-till-char-forward (count)
  "Prompt user for CHAR and move before the next COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (let ((char (read-char "t")))
    (helix-maybe-set-mark)
    (helix-motion-loop (dir count)
      (helix-find-char char dir t))))

;; T
(helix-define-command helix-till-char-backward (count)
  "Prompt user for CHAR and move before the prevous COUNT'th occurrence of it.
Right after this command while hints are active, you can use `n' and `N'
keys to repeat motion forward/backward."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (setq count (- count))
  (let ((char (read-char "T")))
    (helix-maybe-set-mark)
    (helix-motion-loop (dir count)
      (helix-find-char char dir t))))

;; /
(helix-define-command helix-search-forward (count)
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (when (helix-search-interactively)
    (setq helix-search--direction 1)
    (helix-search-next count)))

;; ?
(helix-define-command helix-search-backward (count)
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (when (helix-search-interactively -1)
    (setq helix-search--direction -1)
    (helix-search-next count)))

;; n
(helix-define-command helix-search-next (count)
  "Select next COUNT search match."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (helix-disable-newline-at-eol)
  (unless helix-search--direction (setq helix-search--direction 1))
  (when (< helix-search--direction 0)
    (setq count (- count)))
  (let ((regexp (helix-search-pattern))
        (region-dir (if (use-region-p) (helix-region-direction) 1)))
    (helix-recenter-point-on-jump
      (helix-motion-loop (search-dir count)
        (-when-let ((beg . end) (save-excursion
                                  (helixf-search--search regexp search-dir)))
          ;; Push mark on first invocation.
          (unless (or (memq last-command '(helix-search-next helix-search-previous))
                      (helix-search--keep-highlight-p last-command))
            (helix-push-point))
          (when (and helix--extend-selection (use-region-p))
            (helix-create-fake-cursor-from-point))
          (helix-set-region beg end region-dir))))
    (helix-highlight-search-pattern regexp)))

;; N
(helix-define-command helix-search-previous (count)
  "Select previous COUNT search match."
  :multiple-cursors nil
  :merge-selections t
  (interactive "p")
  (helix-search-next (- count)))

;; *
(helix-define-command helix-construct-search-pattern ()
  "Construct search pattern from all current selections and store it to / register.
Auto-detect word boundaries at the beginning and end of the search pattern."
  :multiple-cursors nil
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
      (helix-add-to-regex-history regexp)
      (helix-highlight-search-pattern regexp))))

;; M-*
(helix-define-command helix-construct-search-pattern-no-bounds ()
  "Construct search pattern from all current selection and store it to / register.
Do not auto-detect word boundaries in the search pattern."
  :multiple-cursors nil
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
      (helix-add-to-regex-history regexp)
      (helix-highlight-search-pattern regexp))))

;;; Surround

(cl-defun helix-surround-add-pair (key pair &key search regexp balanced)
  "Add a new insert-delete pattern for Helix surround functionality.

Positional arguments:

KEY        A character that will activates this pattern.

PAIR       Cons cell (LEFT . RIGHT) with strings, or function that returns such
           cons cell. The strigs that will be inserted by `helix-surround' and
           `helix-surround-change' functions.

Keyword arguments:

:SEARCH    Any of:
           1. Cons cell with strings (LEFT . RIGHT) of patterns that will be used
              to search of two substrings to delete by `helix-surround-delete'
              and `helix-surround-change' functions.
           2. nil — the value from PAIR argument will be used instead.
           3. Function that return cons cell with strings (LEFT . RIGHT) like
              in 1.
           4. Function that returns list
                       (LEFT-START LEFT-END RIGHT-START RIGHT-END)
              with 4 positions of START/END of LEFT and RIGHT delimeters.
              Example:
                         LEFT                              RIGHT
                       |<tag> |Lorem ipsum dolor sit amet| </tag>|
                       ^      ^                          ^       ^
              LEFT-START      LEFT-END         RIGHT-START       RIGHT-END

Following parameters are taken into account only when :SEARCH argument is a cons
cell with stirngs (LEFT . RIGHT) or a function, that returns such cons cell. If
:SEARCH is a function that returns list with 4 positions, they will be ignored.

:REGEXP    If non-nil then strings specified in :SEARCH argument will be treated
           as regexp patterns. Otherwise they will be searched literally.

:BALANCED  When non-nil all nested balanced LEFT RIGHT pairs will be skipped,
           else the first found pattern will be accepted.

This function populates the buffer local `helix-surround-alist' variable,
and thus should be called from major-modes hooks.

See the defaul value of `helix-surround-alist' variable and `helix-integration.el'
file for examples."
  (declare (indent 2))
  (push (cons key `(:insert ,pair
                    :search ,(or search pair)
                    :regexp ,regexp
                    :balanced ,balanced))
        helix-surround-alist))

(defun helix-surround--read-char ()
  "Read char from minibuffer and return (LEFT . RIGHT) pair with strings
to surround with."
  (let* ((char (read-char "surround: "))
         (pair-or-fun-or-nil (-some-> (alist-get char helix-surround-alist)
                               (plist-get :insert))))
    (pcase pair-or-fun-or-nil
      ((and (pred functionp) fn)
       (funcall fn))
      ((and (pred consp) pair)
       pair)
      (_ (cons (char-to-string char) (char-to-string char))))))

;; Cache the function output to use with all cursors.
(helix-cache-input helix-surround--read-char)

;; ms
(helix-define-command helix-surround ()
  "Enclose the selected region in chosen delimiters.
If the region consist of full lines, insert delimiters on separate
lines and reindent the region."
  :multiple-cursors t
  (interactive)
  (when (use-region-p)
    (helix-save-region
      (-let (((left . right) (helix-surround--read-char))
             (beg (copy-marker (region-beginning)))
             (end (copy-marker (region-end) t))
             (linewise-selection? (helix-logical-lines-p)))
        (when linewise-selection?
          (setq left (s-trim left)
                right (s-trim right)))
        (goto-char beg)
        (insert left)
        (when linewise-selection? (newline))
        (goto-char end)
        (when linewise-selection? (newline))
        (insert right)
        (indent-region beg end)
        (set-marker beg nil)
        (set-marker end nil)))
    (helix-extend-selection -1)))

;; md
(helix-define-command helix-surround-delete ()
  :multiple-cursors t
  (interactive)
  (when-let* ((key (read-char "Delete pair: "))
              (bounds (helix-surround--4-bounds key)))
    (-let (((left-beg left-end right-beg right-end) bounds)
           (deactivate-mark nil))
      (helix-disable-newline-at-eol)
      (delete-region right-beg right-end)
      (delete-region left-beg left-end))))

;; mr
(helix-define-command helix-surround-change ()
  :multiple-cursors t
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

;;; Window navigation

(helix-define-command helix-window-split (window-to-split)
  "Split current window horisontally.
All children of the parent of the splitted window will be rebalanced."
  :multiple-cursors nil
  (interactive `(,(selected-window)))
  (let ((new-window (split-window-below nil window-to-split)))
    (select-window new-window)
    (balance-windows (window-parent new-window))
    new-window))

(helix-define-command helix-root-window-split ()
  "Split root window of current frame horisontally and rebalance all windows."
  :multiple-cursors nil
  (interactive)
  (helix-window-split (frame-root-window)))

(helix-define-command helix-window-vsplit (window-to-split)
  "Split the current window vertically.
All children of the parent of the splitted window will be rebalanced."
  :multiple-cursors nil
  (interactive `(,(selected-window)))
  (let ((new-window (split-window-right nil window-to-split)))
    (select-window new-window)
    (balance-windows (window-parent new-window))
    new-window))

(helix-define-command helix-root-window-vsplit ()
  "Split root window of current frame vertically and rebalance all windows."
  :multiple-cursors nil
  (interactive)
  (helix-window-vsplit (frame-root-window)))

(helix-define-command helix-window-left (count)
  "Move the cursor to new COUNT-th window left of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-left)))

(helix-define-command helix-window-right (count)
  "Move the cursor to new COUNT-th window right of the current one."
  :multiple-cursors nil
  (interactive "p")
  (dotimes (_ count)
    (windmove-right)))

(helix-define-command helix-window-up (count)
  "Move the cursor to new COUNT-th window up of the current one."
  :multiple-cursors nil
  (interactive "p")
  (dotimes (_ count)
    (windmove-up)))

(helix-define-command helix-window-down (count)
  "Move the cursor to new COUNT-th window down of the current one."
  :multiple-cursors nil
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

(helix-define-command helix-move-window-left ()
  "Swap window with one to the left."
  :multiple-cursors nil
  (interactive)
  (helix-move-window 'left))

(helix-define-command helix-move-window-right ()
  "Swap window with one to the right."
  :multiple-cursors nil
  (interactive)
  (helix-move-window 'right))

(helix-define-command helix-move-window-up ()
  "Swap window with one upwards."
  :multiple-cursors nil
  (interactive)
  (helix-move-window 'up))

(helix-define-command helix-move-window-down ()
  "Swap window with one downwards."
  :multiple-cursors nil
  (interactive)
  (helix-move-window 'down))

(helix-define-command helix-window-delete ()
  "Delete the current window or tab.
Rebalance all children of the deleted window's parent window."
  :multiple-cursors nil
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

(helix-define-command helix-clone-indirect-buffer-same-window ()
  "Create indirect buffer and open it in the current window.
This command was written because `clone-indirect-buffer' calls `pop-to-buffer'
and opens a new window."
  :multiple-cursors nil
  (interactive)
  (-doto (clone-indirect-buffer nil nil)
    (switch-to-buffer)))

(helix-define-command helix-kill-current-buffer-and-window ()
  "Kill the current buffer and delete the current window or tab."
  :multiple-cursors nil
  (interactive)
  (let ((parent-win (window-parent)))
    (kill-buffer (current-buffer))
    ;; If tabs are enabled and this is the only visible window, then attempt to
    ;; close this tab.
    (if (and (bound-and-true-p tab-bar-mode)
             (null parent-win))
        (tab-close)
      ;; else
      (delete-window)
      ;; balance-windows raises an error if the parent does not have
      ;; any further children (then rebalancing is not necessary anyway)
      (ignore-errors (balance-windows parent-win)))))

;; zn
(helix-define-command helix-narrow-to-region-indirectly ()
  "Restrict editing in this buffer to the current region, indirectly.
This recursively creates indirect clones of the current buffer so that the
narrowing doesn't affect other windows displaying the same buffer. Call
`helix-widen-indirectly-narrowed' to undo it (incrementally)."
  :multiple-cursors t
  (interactive)
  (when (use-region-p)
    (helix-restore-newline-at-eol)
    (let ((orig-buffer (current-buffer))
          (beg (region-beginning))
          (end (region-end)))
      (deactivate-mark)
      (helix-clone-indirect-buffer-same-window)
      (narrow-to-region beg end)
      (setq helix--narrowed-base-buffer orig-buffer))))

;; zw
(helix-define-command helix-widen-indirectly-narrowed (&optional arg)
  "Widens narrowed buffers.
Incrementally kill indirect buffers (under the assumption they were created by
`helix-narrow-to-region-indirectly') and switch to their base buffer.

With `universal-argument' kill all indirect buffers, return the base buffer and
widen it.

If the current buffer is not an indirect buffer, works like `widen'."
  :multiple-cursors nil
  (interactive "P")
  (unless (buffer-narrowed-p)
    (user-error "Buffer isn't narrowed"))
  (let ((orig-buffer (current-buffer))
        (base-buffer helix--narrowed-base-buffer))
    (cond ((or (not base-buffer)
               (not (buffer-live-p base-buffer)))
           (widen))
          (arg
           (let ((buffer orig-buffer)
                 (buffers-to-kill (list orig-buffer)))
             (while (setq buffer (buffer-local-value 'helix--narrowed-base-buffer buffer))
               (push buffer buffers-to-kill))
             (switch-to-buffer (buffer-base-buffer))
             (->> buffers-to-kill
                  (-remove (current-buffer))
                  (-each #'kill-buffer))))
          ((switch-to-buffer base-buffer)
           (kill-buffer orig-buffer)))))

(provide 'helix-commands)
;;; helix-commands.el ends here
