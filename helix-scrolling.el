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
  "Non smoothly scroll the window COUNT lines upwards."
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

(provide 'helix-scrolling)
;;; helix-scrolling.el ends here
