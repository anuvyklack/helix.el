;;; hel-scrolling.el --- Scrolling commands -*- lexical-binding: t; -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/anuvyklack/hel.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Hel commands for smooth and non-smooth scrolling.
;;
;;; Code:

(require 'pixel-scroll)
(require 'hel-vars)
(require 'hel-multiple-cursors-core)

;; (line-pixel-height)
;; (pixel-visible-pos-in-window)
;; (window-start)

;; XXX: Necessary for smooth scrolling to work.
(setq scroll-conservatively 101)

(defun hel--get-scroll-count (count)
  "Given a user-supplied COUNT, return scroll count."
  (if (natnump count)
      (setq hel-scroll-count count)
    hel-scroll-count))

(defun hel--smooth-scroll-up (count &optional restricted pages?)
  "Smoothly scroll window COUNT lines upwards.
If RESTRICTED is non-nil the scroll is restricted within current screen.
If PAGES is non-nil scroll over pages instead of lines."
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (line-height (default-line-height))
         (delta (cond ((= count 0) (/ window-height 2))
                      (pages? (* count window-height))
                      (t (* count line-height))))
         (point-data (posn-at-point))
         (row-at-point (cdr (posn-col-row point-data)))
         (y-at-point (cdr (posn-x-y point-data)))
         at-bottom?)
    ;; BUG: When jump lands at the top of the screen the point could be only
    ;; partially visible. If you try to scroll smoothly from this position the
    ;; point will jump unpredictably. Fix initial position in this case.
    (when (= row-at-point 0) (recenter 0))
    ;; `screen-space' is the height of the part of the screen toward the
    ;; scrolling direction.
    (let ((screen-space (- window-height y-at-point)))
      ;; If point goes off the screen as the result of the scroll
      (when (> delta (- screen-space line-height))
        (if restricted
            (setq delta (- screen-space (/ line-height 3))
                  at-bottom? t)
          ;; else — not restricted
          (hel-maybe-deactivate-mark))))
    (when (> delta line-height)
      (pixel-scroll-precision-interpolate delta nil 1))
    (when at-bottom? (recenter -1))))

(defun hel--smooth-scroll-down (count &optional restricted pages?)
  "Smoothly scroll window COUNT lines downwards.
If RESTRICTED in non-nil the scroll is restricted within current screen.
If PAGES is non-nil scroll over pages instead of lines."
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (line-height (default-line-height))
         (delta (cond ((= count 0) (/ window-height 2))
                      (pages? (* count window-height))
                      (t (* count line-height))))
         (point-data (posn-at-point))
         (row-at-point (cdr (posn-col-row point-data)))
         (y-at-point (cdr (posn-x-y point-data)))
         at-top?)
    ;; BUG: When jump lands at the top of the screen the point could be only
    ;; partially visible. If you try to scroll smoothly from this position the
    ;; point will jump unpredictably. Fix initial position in this case.
    (when (= row-at-point 0) (recenter 0))
    (when (> delta (- y-at-point
                      line-height))
      (if restricted
          (setq delta (- y-at-point
                         (/ line-height 3))
                at-top? t)
        ;; else
        (hel-maybe-deactivate-mark)))
    (when (> delta line-height)
      (pixel-scroll-precision-interpolate (- delta) nil 1))
    (when at-top? (recenter 0))))

;; C-u
(defun hel-smooth-scroll-up (count)
  "Smoothly scroll the window and the cursor COUNT lines upwards.
If COUNT is not specified the function scrolls up `hel-scroll-count'
lines, which is the last used COUNT. If the scroll count is zero
the command scrolls half the screen.

If multiple cursors are active, scroll is restricted only within
current screen to prevent desynchronization between main cursor
and fake ones."
  (interactive "P")
  (hel--smooth-scroll-up (hel--get-scroll-count count)
                         hel-multiple-cursors-mode))

(put 'hel-smooth-scroll-up 'scroll-command t)
(put 'hel-smooth-scroll-up 'multiple-cursors 'false)

;; C-d
(defun hel-smooth-scroll-down (count)
  "Smoothly scroll the window and the cursor COUNT lines downwards.
If COUNT is not specified the function scrolls down `hel-scroll-count'
lines, which is the last used COUNT. If the scroll count is zero
the command scrolls half the screen.

If multiple cursors are active, scroll is restricted only within
current screen to prevent desynchronization between main cursor
and fake ones."
  (interactive "P")
  (hel--smooth-scroll-down (hel--get-scroll-count count)
                           hel-multiple-cursors-mode))

(put 'hel-smooth-scroll-down 'scroll-command t)
(put 'hel-smooth-scroll-down 'multiple-cursors 'false)

;; C-b
(defun hel-smooth-scroll-page-up (count)
  "Smoothly scroll the window COUNT pages upwards.
If multiple cursors are active, rotate the main selection COUNT times
backward instead."
  (interactive "p")
  (hel--smooth-scroll-up count hel-multiple-cursors-mode :full-pages))

(put 'hel-smooth-scroll-page-up 'scroll-command t)
(put 'hel-smooth-scroll-page-up 'multiple-cursors 'false)

;; C-f
(defun hel-smooth-scroll-page-down (count)
  "Smoothly scroll the window COUNT pages downwards.
If multiple cursors are active, rotate the main selection forward COUNT times
instead."
  (interactive "p")
  (hel--smooth-scroll-down count hel-multiple-cursors-mode :full-pages))

(put 'hel-smooth-scroll-page-down 'scroll-command t)
(put 'hel-smooth-scroll-page-down 'multiple-cursors 'false)

;; C-e
(defun hel-mix-scroll-line-down (count)
  "Scroll the window COUNT lines downwards.
If COUNT > 1 scroll smoothly."
  (interactive "p")
  (if (= count 1)
      (hel-scroll-line-down count)
    (hel-smooth-scroll-line-down count)))

(put 'hel-mix-scroll-line-down 'scroll-command t)
(put 'hel-mix-scroll-line-down 'multiple-cursors 'false)

;; C-e
(defun hel-scroll-line-down (count)
  "Scroll the window COUNT lines downwards."
  (interactive "p")
  (let ((point-row (cdr (posn-col-row (posn-at-point)))))
    (when (> count point-row)
      (if hel-multiple-cursors-mode
          (setq count point-row)
        ;; else
        (hel-maybe-deactivate-mark)))
    (let ((scroll-preserve-screen-position nil))
      (scroll-up count))))

(put 'hel-scroll-line-down 'scroll-command t)
(put 'hel-scroll-line-down 'multiple-cursors 'false)

;; C-e
(defun hel-smooth-scroll-line-down (count)
  "Smoothly scroll the window COUNT lines downwards."
  (interactive "p")
  (let ((pixel-scroll-precision-interpolation-total-time 0.1))
    (hel--smooth-scroll-down count hel-multiple-cursors-mode)))

(put 'hel-smooth-scroll-line-down 'scroll-command t)
(put 'hel-smooth-scroll-line-down 'multiple-cursors 'false)

;; C-y
(defun hel-mix-scroll-line-up (count)
  "Scroll the window COUNT lines upwards.
If COUNT > 1 scroll smoothly."
  (interactive "p")
  (if (= count 1)
      (hel-scroll-line-up count)
    (hel-smooth-scroll-line-up count)))

(put 'hel-mix-scroll-line-up 'scroll-command t)
(put 'hel-mix-scroll-line-up 'multiple-cursors 'false)

;; C-y
(defun hel-scroll-line-up (count)
  "Non smoothly scroll the window COUNT lines upwards."
  (interactive "p")
  (let (;; BUG: `window-text-height' claims that it doesn't count modeline,
        ;; headline, dividers, partially visible lines at bottom, but it is
        ;; not true. That's why -2.
        (num-of-lines (- (window-text-height) 2))
        (point-row (1+ (cdr (posn-col-row (posn-at-point))))))
    (when (> count (- num-of-lines point-row))
      (if hel-multiple-cursors-mode
          (setq count (- num-of-lines point-row))
        ;; else
        (hel-maybe-deactivate-mark)))
    (let ((scroll-preserve-screen-position nil))
      (scroll-down count))))

(put 'hel-scroll-line-up 'scroll-command t)
(put 'hel-scroll-line-up 'multiple-cursors 'false)

;; C-y
(defun hel-smooth-scroll-line-up (count)
  "Smoothly scroll the window COUNT lines upwards."
  (interactive "p")
  (let ((pixel-scroll-precision-interpolation-total-time 0.1))
    (hel--smooth-scroll-up count hel-multiple-cursors-mode)))

(put 'hel-smooth-scroll-line-up 'scroll-command t)
(put 'hel-smooth-scroll-line-up 'multiple-cursors 'false)

;; zz
(defun hel-smooth-scroll-line-to-center ()
  "Smoothly scroll current line to the center of the window."
  (interactive)
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (posn-y-target (ceiling (/ window-height 2)))
         (point-data (posn-at-point))
         (row-at-point (cdr (posn-col-row point-data)))
         (y-at-point (cdr (posn-x-y point-data)))
         (delta (- posn-y-target
                   y-at-point)))
    (when (= row-at-point 0) (recenter 0))
    (pixel-scroll-precision-interpolate delta nil 1)))

(put 'hel-smooth-scroll-line-to-center 'scroll-command t)
(put 'hel-smooth-scroll-line-to-center 'multiple-cursors 'false)

;; zz (another version)
(defun hel-smooth-scroll-line-to-eye-level ()
  "Smoothly scroll current line not to the very top of the window."
  (interactive)
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (posn-y-target (ceiling (/ window-height 5)))
         (point-data (posn-at-point))
         (row-at-point (cdr (posn-col-row point-data)))
         (y-at-point (cdr (posn-x-y point-data)))
         (delta (- posn-y-target
                   y-at-point)))
    (when (= row-at-point 0) (recenter 0))
    (pixel-scroll-precision-interpolate delta nil 1)))

(put 'hel-smooth-scroll-line-to-eye-level 'scroll-command t)
(put 'hel-smooth-scroll-line-to-eye-level 'multiple-cursors 'false)

;; zt
(defun hel-smooth-scroll-line-to-top ()
  "Smoothly scroll current line to the top of the window."
  (interactive)
  ;; HACK: Interpolation is imperfect: the line may be not on top, or point can
  ;; move to the next line. So we scroll a little bit before the top, and then
  ;; finish with `recenter' getting a clear result.
  (let* ((line-height (default-line-height))
         (point-data (posn-at-point))
         (row-at-point (cdr (posn-col-row point-data)))
         (y-at-point (cdr (posn-x-y point-data)))
         (delta (- y-at-point
                   (/ line-height 4))))
    (when (= row-at-point 0) (recenter 0))
    (pixel-scroll-precision-interpolate (- delta) nil 1)
    (recenter 0)))

(put 'hel-smooth-scroll-line-to-top 'scroll-command t)
(put 'hel-smooth-scroll-line-to-top 'multiple-cursors 'false)

;; zb
(defun hel-smooth-scroll-line-to-bottom ()
  "Smoothly scroll current line to the bottom of the window."
  (interactive)
  ;; HACK: Interpolation is imperfect: the line may be not on top, or point can
  ;; move to the next line. So we scroll a little bit before the bottom, and
  ;; then finish with `recenter' getting a clear result.
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (line-height (default-line-height))
         (point-data (posn-at-point))
         (row-at-point (cdr (posn-col-row point-data)))
         (y-at-point (cdr (posn-x-y point-data)))
         (delta (- window-height
                   y-at-point
                   (/ line-height 4))))
    (when (= row-at-point 0) (recenter 0))
    (pixel-scroll-precision-interpolate delta nil 1)
    (recenter -1)))

(put 'hel-smooth-scroll-line-to-bottom 'scroll-command t)
(put 'hel-smooth-scroll-line-to-bottom 'multiple-cursors 'false)

(provide 'hel-scrolling)
;;; hel-scrolling.el ends here
