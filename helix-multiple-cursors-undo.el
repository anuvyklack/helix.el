;;; helix-multiple-cursors-undo.el --- Undo for multiple cursors -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Undo for multiple cursors.
;;
;;; Code:

(defun helix--undoable-p ()
  (and helix--undo-list-pointer
       (not (eq helix--undo-list-pointer
                buffer-undo-list))))

(defvar helix-undo--point-state-during-redo nil)

(defun helix-undo--store-point-3 ()
  (when (helix--undoable-p)
    (push '(apply helix-undo--store-point-2)
          buffer-undo-list)))

(defun helix-undo--store-point-2 ()
  (when (helix--undoable-p)
    (push '(apply helix-undo--store-point)
          buffer-undo-list)))

(defun helix-undo--store-point ()
  (when (helix--undoable-p)
    (let ((state (make-overlay (point) (point) nil nil t)))
      (overlay-put state 'type 'original-cursor)
      (helix--store-point-state-in-overlay state)
      (setq helix-undo--point-state-during-redo state))))

(defun helix-undo--restore-point-3 ()
  (when (helix--undoable-p)
    (push '(apply helix-undo--restore-point-2)
          buffer-undo-list)))

(defun helix-undo--restore-point-2 ()
  (when (helix--undoable-p)
    (push '(apply helix-undo--restore-point)
          buffer-undo-list)))

(defun helix-undo--restore-point ()
  (when (helix--undoable-p)
    (let ((state helix-undo--point-state-during-redo))
      (helix--restore-point-state-from-overlay state)
      (delete-overlay state))
    (setq helix-undo--point-state-during-redo nil)))

(provide 'helix-multiple-cursors-undo)
;;; helix-multiple-cursors-undo.el ends here
