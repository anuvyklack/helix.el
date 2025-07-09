;;; helix-scrolling.el --- Scrolling commands -*- lexical-binding: t; -*-
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
;; Helix commands for smooth and non-smooth scrolling.
;;
;;; Code:

(require 'pixel-scroll)
(require 'helix-vars)
(require 'helix-multiple-cursors-core)

;; (line-pixel-height)
;; (pixel-visible-pos-in-window)
;; (window-start)

;; XXX: Necessary for smooth scrolling to work.
(setq scroll-conservatively 101)

(defun helix--get-scroll-count (count)
  "Given a user-supplied COUNT, return scroll count."
  (if (natnump count)
      (setq helix-scroll-count count)
    helix-scroll-count))

(defun helix--smooth-scroll-up (count &optional restricted pages?)
  "Smoothly scroll window COUNT lines upwards.
If RESTRICTED is non-nil the scroll is restricted within current screen.
If PAGES is non-nil scroll over pages instead of lines."
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (line-height (default-line-height))
         (delta (cond ((eql count 0) (/ window-height 2))
                      (pages? (* count window-height))
                      (t (* count line-height))))
         (posn-y-at-point (cdr (posn-x-y (posn-at-point))))
         at-bottom?)
    ;; BUG: When jump lands at the top of the screen the point could be only
    ;; partially visible. If you try to scroll smoothly from this position the
    ;; point will jump unpredictably. Fix initial position in this case.
    (when (eql posn-y-at-point 0) (recenter 0))
    ;; `screen-space' is the height of the part of the screen toward the
    ;; scrolling direction.
    (let ((screen-space (- window-height posn-y-at-point)))
      ;; If point goes off the screen as the result of the scroll
      (when (> delta (- screen-space line-height))
        (cond (restricted
               (setq delta (- screen-space (/ line-height 3))
                     at-bottom? t))
              ;; If not restricted, disable selection unless we want to extend it
              ((not helix--extend-selection)
               (deactivate-mark)))))
    (when (> delta line-height)
      (pixel-scroll-precision-interpolate delta nil 1))
    (when at-bottom? (recenter -1))))

(defun helix--smooth-scroll-down (count &optional restricted pages?)
  "Smoothly scroll window COUNT lines downwards.
If RESTRICTED in non-nil the scroll is restricted within current screen.
If PAGES is non-nil scroll over pages instead of lines."
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (line-height (default-line-height))
         (delta (cond ((eql count 0) (/ window-height 2))
                      (pages? (* count window-height))
                      (t (* count line-height))))
         (posn-y-at-point (cdr (posn-x-y (posn-at-point))))
         at-top?)
    ;; BUG: When jump lands at the top of the screen the point could be only
    ;; partially visible. If you try to scroll smoothly from this position the
    ;; point will jump unpredictably. Fix initial position in this case.
    (when (eql posn-y-at-point 0) (recenter 0))
    (when (> delta (- posn-y-at-point
                      line-height))
      (cond (restricted
             (setq delta (- posn-y-at-point
                            (/ line-height 3))
                   at-top? t))
            ((not helix--extend-selection)
             (deactivate-mark))))
    (when (> delta line-height)
      (pixel-scroll-precision-interpolate (- delta) nil 1))
    (when at-top? (recenter 0))))

;; C-u
(defun helix-smooth-scroll-up (count)
  "Smoothly scroll the window and the cursor COUNT lines upwards.
If COUNT is not specified the function scrolls up `helix-scroll-count'
lines, which is the last used COUNT. If the scroll count is zero
the command scrolls half the screen.

If multiple cursors are active, scroll is restricted only within
current screen to prevent desynchronization between main cursor
and fake ones."
  (interactive "P")
  (helix--smooth-scroll-up (helix--get-scroll-count count)
                           helix-multiple-cursors-mode))

(put 'helix-scroll-line-up 'scroll-command t)

;; C-d
(defun helix-smooth-scroll-down (count)
  "Smoothly scroll the window and the cursor COUNT lines downwards.
If COUNT is not specified the function scrolls down `helix-scroll-count'
lines, which is the last used COUNT. If the scroll count is zero
the command scrolls half the screen.

If multiple cursors are active, scroll is restricted only within
current screen to prevent desynchronization between main cursor
and fake ones."
  (interactive "P")
  (helix--smooth-scroll-down (helix--get-scroll-count count)
                             helix-multiple-cursors-mode))

(put 'helix-scroll-line-down 'scroll-command t)

;; C-b
(defun helix-smooth-scroll-page-up (count)
  "Smoothly scroll the window COUNT pages upwards.
If multiple cursors are active, rotate the main selection COUNT times
backward instead."
  (interactive "p")
  (helix--smooth-scroll-up count helix-multiple-cursors-mode t))

(put 'helix-smooth-scroll-page-up 'scroll-command t)

;; C-f
(defun helix-smooth-scroll-page-down (count)
  "Smoothly scroll the window COUNT pages downwards.
If multiple cursors are active, rotate the main selection forward COUNT times
instead."
  (interactive "p")
  (helix--smooth-scroll-down count helix-multiple-cursors-mode t))

(put 'helix-smooth-scroll-page-down 'scroll-command t)

;; C-e
(defun helix-mix-scroll-line-down (count)
  "Scroll the window COUNT lines downwards.
If COUNT > 1 scroll smoothly."
  (interactive "p")
  (if (eql count 1)
      (helix-scroll-line-down count)
    (helix-smooth-scroll-line-down count)))

(put 'helix-mix-scroll-line-down 'scroll-command t)

;; C-e
(defun helix-scroll-line-down (count)
  "Scroll the window COUNT lines downwards."
  (interactive "p")
  (let ((point-row (cdr (posn-col-row (posn-at-point)))))
    (when (> count point-row)
      (cond (helix-multiple-cursors-mode
             (setq count point-row))
            ((not helix--extend-selection)
             (deactivate-mark))))
    (let ((scroll-preserve-screen-position nil))
      (scroll-up count))))

(put 'helix-scroll-line-down 'scroll-command t)

;; C-e
(defun helix-smooth-scroll-line-down (count)
  "Smoothly scroll the window COUNT lines downwards."
  (interactive "p")
  (let ((pixel-scroll-precision-interpolation-total-time 0.1))
    (helix--smooth-scroll-down count helix-multiple-cursors-mode)))

(put 'helix-smooth-scroll-line-down 'scroll-command t)

;; C-y
(defun helix-mix-scroll-line-up (count)
  "Scroll the window COUNT lines upwards.
If COUNT > 1 scroll smoothly."
  (interactive "p")
  (if (eql count 1)
      (helix-scroll-line-up count)
    (helix-smooth-scroll-line-up count)))

(put 'helix-mix-scroll-line-up 'scroll-command t)

;; C-y
(defun helix-scroll-line-up (count)
  "Non smoothly scroll the window COUNT lines upwards."
  (interactive "p")
  (let (;; BUG: `window-text-height' claims that it doesn't count modeline,
        ;; headline, dividers, partially visible lines at bottom, but it is
        ;; not true. That's why -2.
        (num-of-lines (- (window-text-height) 2))
        (point-row (1+ (cdr (posn-col-row (posn-at-point))))))
    (when (> count (- num-of-lines point-row))
      (cond (helix-multiple-cursors-mode
             (setq count (- num-of-lines point-row)))
            ((not helix--extend-selection)
             (deactivate-mark))))
    (let ((scroll-preserve-screen-position nil))
      (scroll-down count))))

(put 'helix-scroll-line-up 'scroll-command t)

;; C-y
(defun helix-smooth-scroll-line-up (count)
  "Smoothly scroll the window COUNT lines upwards."
  (interactive "p")
  (let ((pixel-scroll-precision-interpolation-total-time 0.1))
    (helix--smooth-scroll-up count helix-multiple-cursors-mode)))

(put 'helix-smooth-scroll-line-up 'scroll-command t)

;; zz
(defun helix-smooth-scroll-line-to-center ()
  "Smoothly scroll current line to the center of the window."
  (interactive)
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (posn-y-target (ceiling (/ window-height 2)))
         (posn-y-at-point (cdr (posn-x-y (posn-at-point))))
         (delta (- posn-y-target
                   posn-y-at-point)))
    (when (eql posn-y-at-point 0) (recenter 0))
    (pixel-scroll-precision-interpolate delta nil 1)))

(put 'helix-smooth-scroll-line-to-center 'scroll-command t)

;; zz (another version)
(defun helix-smooth-scroll-line-not-to-very-top ()
  "Smoothly scroll current line not to the very top of the window."
  (interactive)
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (posn-y-target (ceiling (/ window-height 5)))
         (posn-y-at-point (cdr (posn-x-y (posn-at-point))))
         (delta (- posn-y-target
                   posn-y-at-point)))
    (when (eql posn-y-at-point 0) (recenter 0))
    (pixel-scroll-precision-interpolate delta nil 1)))

(put 'helix-smooth-scroll-line-not-to-very-top 'scroll-command t)

;; zt
(defun helix-smooth-scroll-line-to-top ()
  "Smoothly scroll current line to the top of the window."
  (interactive)
  ;; HACK: Interpolation is imperfect: the line may be not on top, or point can
  ;; move to the next line. So we scroll a little bit before the top, and then
  ;; finish with `recenter' getting a clear result.
  (let* ((line-height (default-line-height))
         (posn-y-at-point (cdr (posn-x-y (posn-at-point))))
         (delta (- posn-y-at-point
                   (/ line-height 4))))
    (when (eql posn-y-at-point 0) (recenter 0))
    (pixel-scroll-precision-interpolate (- delta) nil 1)
    (recenter 0)))

(put 'helix-smooth-scroll-line-to-top 'scroll-command t)

;; zb
(defun helix-smooth-scroll-line-to-bottom ()
  "Smoothly scroll current line to the bottom of the window."
  (interactive)
  ;; HACK: Interpolation is imperfect: the line may be not on top, or point can
  ;; move to the next line. So we scroll a little bit before the bottom, and
  ;; then finish with `recenter' getting a clear result.
  (let* ((window-height (- (window-text-height nil t)
                           (window-mode-line-height)
                           (window-tab-line-height)))
         (line-height (default-line-height))
         (posn-y-at-point (cdr (posn-x-y (posn-at-point))))
         (delta (- window-height
                   posn-y-at-point
                   (/ line-height 4))))
    (when (eql posn-y-at-point 0) (recenter 0))
    (pixel-scroll-precision-interpolate delta nil 1)
    (recenter -1)))

(put 'helix-smooth-scroll-line-to-bottom 'scroll-command t)

(provide 'helix-scrolling)
;;; helix-scrolling.el ends here
