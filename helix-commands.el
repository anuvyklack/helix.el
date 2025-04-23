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
(require 'pixel-scroll)
(require 'multiple-cursors-core)
(require 'helix-common)

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
  "Move to the COUNT-th next word start."
  (interactive "p")
  (let ((thing (if bigword 'helix-WORD 'helix-word)))
    (when (zerop (helix-forward-beginning thing (1- count) t))
      (if helix--extend-selection
          (or (region-active-p) (set-mark (point)))
        (skip-chars-forward "\r\n")
        (set-mark (point)))
      (helix-forward-beginning thing 1 t))))

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
  ;; Move back over chars that have whitespace syntax but have the p flag.
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
      (let ((line? (helix-point-and-mark-at-bolp-p)))
        (kill-region nil nil t)
        (pcase line?
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
(defun helix-select-line (count)
  (interactive "p")
  (let ((line (if visual-line-mode 'visual-line 'line)))
    (if (region-active-p)
        (let ((b (region-beginning))
              (e (region-end)))
          (goto-char b)
          (set-mark (car (bounds-of-thing-at-point line))) ; left end
          (goto-char e)
          (goto-char (cdr (bounds-of-thing-at-point line)))) ; right end
      ;; no region
      (let ((bounds (bounds-of-thing-at-point line)))
        (set-mark (car bounds))
        (goto-char (cdr bounds))))
    (helix-motion-loop (_ (1- count))
      (goto-char (cdr (bounds-of-thing-at-point line))))))

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

(defun helix-mark-inner-thing (thing &optional count)
  (or count (setq count 1))
  (when (zerop count)
    (error "Cannot mark zero %ss'" thing))
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

;; miw
(defun helix-mark-inner-word (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-word count))

;; mip
(defun helix-mark-inner-paragraph (count)
  (interactive "p")
  (helix-mark-inner-thing 'paragraph count))

;;; Scrolling

;; (line-pixel-height)
;; (pixel-visible-pos-in-window)

(defun helix--get-scroll-count (count)
  "Given a user-supplied COUNT, return scroll count."
  (if (natnump count)
      (setq helix-scroll-count count)
    helix-scroll-count))

;; C-u
(defun helix-smooth-scroll-up (count)
  "Smoothly scroll the window and the cursor COUNT lines upwards.
If COUNT is not specified the function scrolls up `helix-scroll-count'
lines, which is the last used COUNT. If the scroll count is zero
the command scrolls half the screen."
  (interactive "P")
  (setq count (helix--get-scroll-count count))
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (delta (if (= 0 count)
                    (/ window-height 2)
                  (* count (default-line-height)))))
    ;; If point goes off the screen as the result of the scroll —
    ;; disable selection unless we want to extend it.
    (unless helix--extend-selection
      (let ((posn-y-at-point (cdr (posn-x-y (posn-at-point)))))
        (when (> delta (- window-height posn-y-at-point))
          (deactivate-mark))))
    (pixel-scroll-precision-interpolate delta nil 1)))
(put 'helix-scroll-line-up 'scroll-command t)

;; C-d
(defun helix-smooth-scroll-down (count)
  "Smoothly scroll the window and the cursor COUNT lines downwards.
If COUNT is not specified the function scrolls down `helix-scroll-count'
lines, which is the last used COUNT. If the scroll count is zero
the command scrolls half the screen."
  (interactive "P")
  (setq count (helix--get-scroll-count count))
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (delta (if (= 0 count)
                    (/ window-height 2)
                  (* count (default-line-height)))))
    ;; If point goes off the screen as the result of the scroll —
    ;; disable selection unless we want to extend it.
    (unless helix--extend-selection
      (let ((posn-y-at-point (cdr (posn-x-y (posn-at-point)))))
        (when (> delta posn-y-at-point)
          (deactivate-mark))))
    (pixel-scroll-precision-interpolate (- delta) nil 1)))
(put 'helix-scroll-line-down 'scroll-command t)

;; C-b
(defun helix-smooth-scroll-page-up (count)
  "Smoothly scroll the window COUNT pages upwards."
  (interactive "p")
  (unless helix--extend-selection (deactivate-mark))
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (delta (* count window-height)))
    (pixel-scroll-precision-interpolate delta nil 1)))
(put 'helix-smooth-scroll-page-up 'scroll-command t)

;; C-f
(defun helix-smooth-scroll-page-down (count)
  "Smoothly scroll the window COUNT pages downwards."
  (interactive "p")
  (unless helix--extend-selection (deactivate-mark))
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (delta (* count window-height)))
    (pixel-scroll-precision-interpolate (- delta) nil 1)))
(put 'helix-smooth-scroll-page-down 'scroll-command t)

;; C-e
(defun helix-mix-scroll-line-down (count)
  "Scroll the window COUNT lines downwards.
If COUNT > 1 scroll smoothly."
  (interactive "p")
  (if (= count 1)
      (helix-scroll-line-down count)
    (helix-smooth-scroll-line-down count)))
(put 'helix-mix-scroll-line-down 'scroll-command t)

(defun helix-scroll-line-down (count)
  "Scroll the window COUNT lines downwards."
  (interactive "p")
  (unless helix--extend-selection
    (let ((point-row (cdr (posn-col-row (posn-at-point)))))
      (when (> count point-row)
        (deactivate-mark))))
  (let ((scroll-preserve-screen-position nil))
    (scroll-up count)))
(put 'helix-scroll-line-down 'scroll-command t)

(defun helix-smooth-scroll-line-down (count)
  "Smoothly scroll the window COUNT lines downwards."
  (interactive "p")
  (let* ((pixel-scroll-precision-interpolation-total-time 0.1) ; duration
         (delta (* count (default-line-height))))
    ;; If point goes off the screen as the result of the scroll —
    ;; disable selection unless we want to extend it.
    (unless helix--extend-selection
      (let ((posn-y-at-point (cdr (posn-x-y (posn-at-point)))))
        (when (> delta posn-y-at-point)
          (deactivate-mark))))
    (pixel-scroll-precision-interpolate (- delta) nil 1)))
(put 'helix-smooth-scroll-line-down 'scroll-command t)

;; C-y
(defun helix-mix-scroll-line-up (count)
  "Scroll the window COUNT lines upwards.
If COUNT > 1 scroll smoothly."
  (interactive "p")
  (if (= count 1)
      (helix-scroll-line-up count)
    (helix-smooth-scroll-line-up count)))
(put 'helix-mix-scroll-line-up 'scroll-command t)

(defun helix-scroll-line-up (count)
  "Scroll the window COUNT lines upwards."
  (interactive "p")
  (unless helix--extend-selection
    (let (;; BUG: `window-text-height' claims that it doesn't count
          ;; modeline, headline, dividers, partially visible lines at bottom,
          ;; but it is not true.
          (num-of-lines (window-text-height))
          (point-row (-> (cdr (posn-col-row (posn-at-point)))
                         (1+)))) ; +1 because numbering starts from 0
      (when (> count (- num-of-lines point-row))
        (deactivate-mark))))
  (let ((scroll-preserve-screen-position nil))
    (scroll-down count)))
(put 'helix-scroll-line-up 'scroll-command t)

(defun helix-smooth-scroll-line-up (count)
  "Smoothly scroll the window COUNT lines upwards."
  (interactive "p")
  (let* ((pixel-scroll-precision-interpolation-total-time 0.1) ; duration
         (delta (* count (default-line-height))))
    ;; If point goes off the screen as the result of the scroll —
    ;; disable selection unless we want to extend it.
    (unless helix--extend-selection
      (let ((win-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
            (posn-y-at-point (cdr (posn-x-y (posn-at-point)))))
        (when (> delta (- win-height posn-y-at-point))
          (deactivate-mark))))
    (pixel-scroll-precision-interpolate delta nil 1)))
(put 'helix-smooth-scroll-line-up 'scroll-command t)

;; zz
(defun helix-smooth-scroll-line-to-center ()
  "Smoothly scroll current line to the center of the window."
  (interactive)
  (let* ((win-height (- (window-text-height nil t)
                        (window-mode-line-height)
                        (window-tab-line-height)))
         (posn-y-target (ceiling (/ win-height 2)))
         (posn-y-at-point (cdr (posn-x-y (posn-at-point))))
         (delta (- posn-y-target
                   posn-y-at-point)))
    (pixel-scroll-precision-interpolate delta nil 1)))

(defun helix-smooth-scroll-line-not-to-very-top ()
  "Smoothly scroll current line not to the very top of the window."
  (interactive)
  (let* ((win-height (- (window-text-height nil t)
                        (window-mode-line-height)
                        (window-tab-line-height)))
         (posn-y-target (ceiling (/ win-height 5)))
         (posn-y-at-point (cdr (posn-x-y (posn-at-point))))
         (delta (- posn-y-target
                   posn-y-at-point)))
    (pixel-scroll-precision-interpolate delta nil 1)))

;; zt
(defun helix-smooth-scroll-line-to-top ()
  "Smoothly scroll current line to the top of the window."
  (interactive)
  ;; Interpolation is imperfect: the line may be not on top, or point can move
  ;; to the next line. So we scroll a little bit before the top, and then finish
  ;; with `recenter' getting a clear result.
  (let* ((line-height (window-font-height))
         (posn-y-at-point (cdr (posn-x-y (posn-at-point))))
         (delta (- posn-y-at-point
                   (/ line-height 4))))
    (pixel-scroll-precision-interpolate (- delta) nil 1)
    (recenter 0)))

;; zb
(defun helix-smooth-scroll-line-to-bottom ()
  "Smoothly scroll current line to the bottom of the window."
  (interactive)
  ;; Interpolation is imperfect: the line may be not on top, or point can move
  ;; to the next line. So we scroll a little bit before the bottom, and then
  ;; finish with `recenter' getting a clear result.
  (let* ((win-height (- (window-text-height nil t)
                        (window-mode-line-height)
                        (window-tab-line-height)))
         (line-height (window-font-height))
         (posn-y-at-point (cdr (posn-x-y (posn-at-point))))
         (delta (- win-height
                   posn-y-at-point
                   (/ line-height 4))))
    (pixel-scroll-precision-interpolate delta nil 1)
    (recenter -1)))

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
