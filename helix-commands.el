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
  (let ((line (if visual-line-mode 'visual-line 'line)))
    (if (region-active-p)
        (let ((b (region-beginning))
              (e (region-end)))
          (goto-char b)
          (set-mark (car (bounds-of-thing-at-point line))) ; left end
          (goto-char e)
          (when (eolp) (forward-char))
          (goto-char (cdr (bounds-of-thing-at-point line)))) ; right end
      ;; else
      (let ((bounds (bounds-of-thing-at-point line)))
        (set-mark (car bounds))
        (goto-char (cdr bounds))))      ; right end
    (helix-motion-loop (_ (1- count))
      (goto-char (cdr (bounds-of-thing-at-point line))))
    (backward-char)))

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

;;; Window navigation

(defalias 'helix-window-split #'split-window-below)

;; (split-window-right)
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
  (interactive)
  (helix-move-window 'left))

(defun helix-move-window-right ()
  (interactive)
  (helix-move-window 'right))

(defun helix-move-window-up ()
  (interactive)
  (helix-move-window 'up))

(defun helix-move-window-down ()
  (interactive)
  (helix-move-window 'down))

(provide 'helix-commands)
;;; helix-commands.el ends here
