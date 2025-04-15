;;; helix-multiple-cursors.el --- Multiple cursors for Helix -*- lexical-binding: t; -*-
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
;;  Multiple cursors for Helix.
;;
;;; Code:

(require 'multiple-cursors-core)
(require 'mc-mark-more)

(setopt mc/match-cursor-style nil)

;; (defun mc/make-cursor-overlay (pos)
;;   "Create overlay to look like cursor at POS.
;; Special case for end of line, because overlay over a newline
;; highlights the entire width of the window."
;;   (if (memq (char-after pos) '(?\r ?\n))
;;       (mc/make-cursor-overlay-at-eol pos)
;;     (mc/make-cursor-overlay-inline pos)))

;; (keymap-lookup nil "M-<down-mouse-1>")

(defun helix-keep-primary-selection ()
  "Disable multiple-cursors-mode and run the corresponding hook."
  (interactive)
  (multiple-cursors-mode 0)
  (run-hooks 'multiple-cursors-mode-disabled-hook))

(defun helix-create-fake-cursor-at-point (&optional id ignore-region)
  " Add a fake cursor and possibly a fake active region overlay
based on point and mark. Saves the current state in the overlay
to be restored later."
  (unless mc--max-cursors-original
    (setq mc--max-cursors-original mc/max-cursors))
  (when mc/max-cursors
    (unless (< (mc/num-cursors) mc/max-cursors)
      (if (yes-or-no-p (format "%d active cursors. Continue? " (mc/num-cursors)))
          (setq mc/max-cursors (read-number "Enter a new, temporary maximum: "))
        (mc/remove-fake-cursors)
        (error "Aborted: too many cursors"))))
  (let ((overlay (mc/make-cursor-overlay-at-point)))
    (overlay-put overlay 'mc-id (or id (mc/create-cursor-id)))
    (overlay-put overlay 'type 'fake-cursor)
    (overlay-put overlay 'priority 100)
    (mc/store-current-state-in-overlay overlay)
    (when (and (not ignore-region)
               (use-region-p))
      (overlay-put overlay 'region-overlay
                   (mc/make-region-overlay-between-point-and-mark)))
    overlay))

(defun helix-toggle-cursor-on-click (event)
  "Add a cursor where you click, or remove a fake cursor that is
already there."
  (interactive "e")
  (mouse-minibuffer-check event)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (let ((position (event-end event)))
    (when (not (windowp (posn-window position)))
      (error "Position not in text area of window"))
    (select-window (posn-window position))
    (let ((pt (posn-point position)))
      (when (numberp pt)
        ;; is there a fake cursor with the actual *point* right where we are?
        (if-let* ((cursor (mc/fake-cursor-at-point pt)))
            (mc/remove-fake-cursor cursor)
          (save-excursion ; save-mark-and-excursion
            (goto-char pt)
            (helix-create-fake-cursor-at-point nil :ignore-region)))))
    (mc/maybe-multiple-cursors-mode)))

(setq mc--default-cmds-to-run-for-all
      (append mc--default-cmds-to-run-for-all
              '(helix-insert ; i
                helix-append ; a
                helix-backward-char ;
                helix-next-line     ; j
                helix-previous-line ; k
                helix-forward-char  ; l
                helix-forward-word-start    ; w
                helix-backward-word-start   ; b
                helix-forward-word-end      ; e
                helix-forward-WORD-start    ; W
                helix-backward-WORD-start   ; B
                helix-forward-WORD-end      ; E
                helix-line ; x
                helix-delete ; d
                helix-collapse-selection ; ;
                )))

(setq mc--default-cmds-to-run-once
      (append mc--default-cmds-to-run-once
              '(helix-normal-state-escape ; esc in normal state
                helix-normal-state
                helix-keep-primary-selection
                helix-extend-selection ; v
                helix-undo
                undo-redo
                tab-next
                tab-previous
                keypad
                )))

(provide 'helix-multiple-cursors)
;;; helix-multiple-cursors.el ends here
