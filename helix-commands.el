;;; helix-commands.el --- Helix commands -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Helix commands
;;
;;; Code:

(require 'thingatpt)
(require 'helix-common)

;; h
(defun helix-backward-char (count)
  (interactive "p")
  (if helix-extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (backward-char count))

;; l
(defun helix-forward-char (count)
  (interactive "p")
  (if helix-extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (forward-char count))

;; j
(defun helix-next-line (count)
  "Move to the next line."
  (interactive "p")
  (if helix-extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (next-line count))

;; k
(defun helix-previous-line (count)
  "Move to the previous line."
  (interactive "p")
  (if helix-extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (previous-line count))

;; w
(defun helix-forward-word-start (count &optional bigword)
  "Move next word start."
  (interactive "p")
  (let ((thing (if bigword 'helix-WORD 'helix-word)))
    (when (forward-thing thing (1- count))
      (if helix-extend-selection
          (or (region-active-p) (set-mark (point)))
        (skip-chars-forward "\r\n")
        (set-mark (point)))
      (or (memq (following-char) '(?\s ?\t))
          (forward-thing thing))
      (skip-chars-forward " \t"))))

;; W
(defun helix-forward-WORD-start (count)
  "Move next WORD start."
  (interactive "p")
  (helix-forward-word-start count :bigword))

;; b
(defun helix-backward-word-start (count &optional bigword)
  "Move previous word start."
  (interactive "p")
  (setq count (- count))
  (let ((thing (if bigword 'helix-WORD 'helix-word)))
    (when (forward-thing thing (1+ count))
      (if helix-extend-selection
          (or (region-active-p) (set-mark (point)))
        (skip-chars-backward "\r\n")
        (set-mark (point)))
      (forward-thing thing -1))))

;; B
(defun helix-backward-WORD-start (count)
  "Move previous WORD start."
  (interactive "p")
  (helix-backward-word-start count :bigword))

;; e
(defun helix-forward-word-end (count &optional bigword)
  "Move next word end."
  (interactive "p")
  (let ((thing (if bigword 'helix-WORD 'helix-word)))
    (when (forward-thing thing (1- count))
      (if helix-extend-selection
          (or (region-active-p) (set-mark (point)))
        (skip-chars-forward "\r\n")
        (set-mark (point)))
      (forward-thing thing))))

;; E
(defun helix-forward-WORD-end (count)
  "Move next WORD end."
  (interactive "p")
  (helix-forward-word-end count :bigword))

;; x
(defun helix-select-or-extend-line (count)
  (interactive "p")
  (if (not (region-active-p))
      (let ((bounds (bounds-of-thing-at-point 'line)))
        (set-mark (car bounds))
        (goto-char (cdr bounds)))
    ;; else
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (exchange-point-and-mark)
    (goto-char (car (bounds-of-thing-at-point 'line))) ; left end
    (exchange-point-and-mark)
    (goto-char (cdr (bounds-of-thing-at-point 'line)))) ; right end
  (helix-motion-loop (_ (1- count))
    (goto-char (cdr (bounds-of-thing-at-point 'line)))))

;; i
(defun helix-insert ()
  "Switch to Insert state before selection."
  (interactive)
  (when (< (mark) (point))
    (exchange-point-and-mark))
  (helix-insert-state 1))

;; a
(defun helix-append ()
  "Switch to Insert state after selection."
  (interactive)
  (when (< (point) (mark))
    (exchange-point-and-mark))
  (helix-insert-state 1))

(provide 'helix-commands)
;;; helix-commands.el ends here
