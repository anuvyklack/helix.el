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

(require 'dash)
(require 's)
(require 'helix-common)
(require 'helix-multiple-cursors-core)

;; (keymap-lookup nil "M-<down-mouse-1>")

;; M-right mouse
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
        (helix-create-fake-cursor pos)))))

(provide 'helix-multiple-cursors)
;;; helix-multiple-cursors.el ends here
