;;; helix-search.el --- Search -*- lexical-binding: t; -*-
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
;;  Helix search related functionality
;;
;;; Code:

(require 'dash)
(require 'helix-common)
(require 'helix-multiple-cursors-core)
(require 'pcre)

;; S
(defun helix-split-region ()
  (interactive)
  )

;; M-s
(defun helix-split-region-on-newline ()
  (interactive)
  )

;;;-----------------------------------------------------------------------------

(defvar helix--search-buffer nil)
(defvar helix--search-start nil)
(defvar helix--search-end nil)
(defvar helix--search-regions nil)
(defvar helix--search-regions-overlays nil
  "List of overlays of regions that match current search pattern.")

(defun helix-regex-history-add (regex)
  (let ((history-delete-duplicates t))
    (add-to-history helix-regex-history regex helix-regex-history-max)))

;; s
(defun helix-select-regex (start end)
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active selection"))
  (let ((dir (helix-region-direction)))
    (setq helix--search-buffer (current-buffer)
          helix--search-start start
          helix--search-end   end)
    (add-hook 'minibuffer-setup-hook #'helix--start-search-session)
    (add-hook 'minibuffer-exit-hook #'helix--end-search-session)
    (if (string-empty-p (helix-read-pcre-regex "select: "))
        ;; Restore region if search string is empty.
        (helix-set-region start end dir)
      ;; Create cursors for matches
      (pcase-let ((`(,beg . ,end) (car helix--search-regions)))
        (set-mark beg)
        (goto-char end))
      (dolist (bounds (cdr helix--search-regions))
        (pcase-let ((`(,beg . ,end) bounds))
          (helix-create-fake-cursor end beg))))
    (remove-hook 'minibuffer-setup-hook #'helix--start-search-session)
    (remove-hook 'minibuffer-exit-hook #'helix--end-search-session)))

(defun helix-read-pcre-regex (prompt)
  (let* ((history-add-new-input nil)
         (regex (read-string prompt nil 'helix-regex-history)))
    (add-to-history 'helix-regex-history regex helix-regex-history-max)
    regex))

(defun helix--start-search-session ()
  "Start a search session."
  (with-minibuffer-selected-window
    (deactivate-mark)
    (let ((ov (make-overlay helix--search-start helix--search-end)))
      (overlay-put ov 'face 'helix-mc-region-face)
      (setq helix--search-regions-overlays (list ov))))
  (add-hook 'after-change-functions #'helix--search-highlight nil t))

(defun helix--end-search-session ()
  "End search session."
  (mapc #'delete-overlay helix--search-regions-overlays)
  (remove-hook 'after-change-functions #'helix--search-highlight))

(defun helix--search-highlight (_ _ _)
  "Highlight current matches during search session."
  (mapc #'delete-overlay helix--search-regions-overlays)
  (let* ((regex (minibuffer-contents))
         (beg helix--search-start)
         (end helix--search-end)
         result)
    (with-minibuffer-selected-window
      (setq helix--search-regions (helix-pcre-all-matches regex beg end))
      (setq helix--search-regions-overlays
            (dolist (bounds helix--search-regions (nreverse result))
              (pcase-let* ((`(,beg . ,end) bounds)
                           (ov (make-overlay beg end)))
                (overlay-put ov 'face 'helix-mc-region-face)
                (push ov result)))))))

(defun helix-pcre-all-matches (regexp start end)
  "Return list with bounds of regions that match REGEXP within START...END.
If REGEXP contains error return the list with one element: (START . END)
i.e. bounds of original region."
  (save-excursion
    (goto-char start)
    (condition-case nil
        (let (result)
          (while (pcre-re-search-forward regexp end t)
            ;; Take the first match group content, if any, or the whole
            ;; match string.
            (let ((bounds (cons (or (match-beginning 1) (match-beginning 0))
                                (or (match-end 1) (match-end 0)))))
              ;; Signal if we stack in infinite loop.
              ;; This can happen when regexp consists only of "^" or "$".
              (when (equal bounds (car-safe result))
                (signal 'error nil))
              (push bounds result)))
          (nreverse result))
      (error
       (list (cons start end))))))

(provide 'helix-search)
;;; helix-search.el ends here
