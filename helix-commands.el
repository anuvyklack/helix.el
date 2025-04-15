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

(require 'thingatpt)
(require 'multiple-cursors-core)
(require 'helix-common)

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
  (next-line count))

;; k
(defun helix-previous-line (count)
  "Move to the COUNT-th previous line."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (previous-line count))

;; w
(defun helix-forward-word-start (count &optional bigword)
  "Move to the COUNT-th next word start.
If BIGWORD move over WORD-s."
  (interactive "p")
  (let ((thing (if bigword 'helix-WORD 'helix-word)))
    (when (forward-thing thing (1- count))
      (if helix--extend-selection
          (or (region-active-p) (set-mark (point)))
        (skip-chars-forward "\r\n")
        (set-mark (point)))
      (or (memq (following-char) '(?\s ?\t))
          (forward-thing thing))
      (skip-chars-forward " \t"))))

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
    (when (forward-thing thing (1+ count))
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
    (when (forward-thing thing (1- count))
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

;; x
(defun helix-line (count)
  (interactive "p")
  (if (region-active-p)
      (let ((b (region-beginning))
            (e (region-end)))
        (goto-char b)
        (set-mark (car (bounds-of-thing-at-point 'line))) ; left end
        (goto-char e)
        (when (eolp) (forward-char))
        (goto-char (cdr (bounds-of-thing-at-point 'line)))) ; right end
    ;; else
    (let ((bounds (bounds-of-thing-at-point 'line)))
      (set-mark (car bounds))
      (goto-char (cdr bounds)))) ; right end
  (helix-motion-loop (_ (1- count))
    (goto-char (cdr (bounds-of-thing-at-point 'line))))
  (backward-char))

;; i
(defun helix-insert ()
  "Switch to Insert state before region."
  (interactive)
  (when (and (region-active-p)
             (< (mark) (point)))
    (exchange-point-and-mark))
  (helix-insert-state 1))

;; a
(defun helix-append ()
  "Switch to Insert state after region."
  (interactive)
  (when (and (region-active-p)
             (< (point) (mark)))
    (exchange-point-and-mark))
  (helix-insert-state 1))

;; c
(defun helix-change ()
  "Delete region and enter Insert state."
  (interactive)
  (if (use-region-p)
      (kill-region nil nil :region)
    (delete-char (- 1)))
  (helix-insert))

(defun helix-collapse-selection ()
  "Collapse region onto a single cursor."
  (interactive)
  (deactivate-mark))

(defun helix-normal-state-escape ()
  "Command for ESC key in Helix Normal state."
  (interactive)
  (cond (helix--extend-selection
         (setq helix--extend-selection nil))
        (t
         (mc/execute-command-for-all-cursors #'helix-collapse-selection))))

;; v
(defun helix-extend-selection ()
  "Toggle extend selection."
  (interactive)
  (setq helix--extend-selection (not helix--extend-selection)))

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
  (cond ((use-region-p)
         (kill-region nil nil :region)
         (when (helix-empty-line-p)
           (delete-char 1)))
        (t (delete-char (- 1)))))

;; u
(defun helix-undo ()
  "Cancel current region then undo."
  (interactive)
  (deactivate-mark)
  (undo))

(provide 'helix-commands)
;;; helix-commands.el ends here
