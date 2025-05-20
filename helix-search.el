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

(declare-function helix-extend-selection "helix-commands")

;;; Search

(defvar helix-search--buffer nil)

(defvar helix-search--start nil
  "The low bound of region within which the search is performed.")

(defvar helix-search--end nil
  "The low bound of region within which the search is performed.")

(defvar helix-search--function nil
  "Function that takes 3 arguments: REGEXP, START, END, and return
list of cons cells with bounds of regions that match the REGEXP withing
START...END. Tipical examples are: `helix-pcre-all-matches' and
`helix-pcre-all-inverted-matches'")

(defvar helix-search--regions-bounds nil
  "List of cons cells (START . END) with bounds of regions, that
`helix-search--function' returns for current REGEXP.")

(defvar helix-search--regions-overlays nil
  "List of regions overlays.")

(defun helix-search-and-select (search-fun start end)
  (unless (use-region-p)
    (user-error "No active selection"))
  (let ((dir (helix-region-direction)))
    (setq helix-search--buffer (current-buffer)
          helix-search--start start
          helix-search--end   end
          helix-search--function #'(lambda (regex)
                                     (funcall search-fun regex start end)))
    (add-hook 'minibuffer-setup-hook #'helix--start-search-session)
    (add-hook 'minibuffer-exit-hook #'helix--end-search-session)
    (deactivate-mark)
    (condition-case nil
        (let ((regexp (helix-read-pcre-regex "select: ")))
          (if (string-empty-p regexp)
              ;; Restore region if search string is empty.
              (helix-set-region start end dir)
            ;; else
            (set-register "/" regexp)
            (helix-create-cursors helix-search--regions-bounds)
            (helix-extend-selection -1)))
      (quit ;; Restore region if quit from minibuffer.
       (helix-set-region start end dir)))
    (remove-hook 'minibuffer-setup-hook #'helix--start-search-session)
    (remove-hook 'minibuffer-exit-hook #'helix--end-search-session)))

(defun helix-read-pcre-regex (prompt)
  (let ((history-add-new-input nil)
        (history-delete-duplicates t))
    (let ((regex (read-string prompt nil 'helix-regex-history)))
      (add-to-history 'helix-regex-history regex helix-regex-history-max)
      regex)))

(defun helix--start-search-session ()
  "Start a search session."
  (with-minibuffer-selected-window
    (let ((ov (make-overlay helix-search--start helix-search--end)))
      (overlay-put ov 'face 'helix-region-face)
      (setq helix-search--regions-overlays (list ov))))
  (add-hook 'after-change-functions #'helix--search-highlight nil t))

(defun helix--end-search-session ()
  "End search session."
  (mapc #'delete-overlay helix-search--regions-overlays)
  (remove-hook 'after-change-functions #'helix--search-highlight))

(defun helix--search-highlight (_ _ _)
  "Highlight current matches during search session."
  (mapc #'delete-overlay helix-search--regions-overlays)
  (let ((regex (minibuffer-contents))
        result)
    (with-minibuffer-selected-window
      (setq helix-search--regions-bounds (funcall helix-search--function regex))
      (setq helix-search--regions-overlays
            (dolist (bounds helix-search--regions-bounds (nreverse result))
              (pcase-let* ((`(,beg . ,end) bounds)
                           (ov (make-overlay beg end)))
                (overlay-put ov 'face 'helix-region-face)
                (push ov result)))))))

(defun helix-pcre-all-matches (regexp start end)
  "Return list with bounds of regions that match REGEXP within START...END.
If REGEXP contains an error return the list with one element: (START . END)
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

(defun helix-pcre-all-inverted-matches (regex beg end)
  (-> (helix-pcre-all-matches regex beg end)
      (helix-search--invert-matches beg end)))

(defun helix-search--invert-matches (regions start end)
  (if (and (length= regions 1)
           (equal (car regions)
                  (cons start end)))
      regions
    ;; else
    (let ((bounds (car regions))
          (result))
      (unless (eql (car bounds) start)
        (push (cons start (car bounds))
              result)
        (setq start (cdr bounds)))
      (dolist (bounds (cdr regions))
        (push (cons start (car bounds))
              result)
        (setq start (cdr bounds)))
      (unless (eql start end)
        (push (cons start end)
              result))
      (nreverse result))))

;;; Filter

(defvar helix-filter--regions nil
  "List of fake regions overlays.")

(defvar helix-filter--regions-strings nil
  "List of fake regions content.")

(defvar helix-filter--invert nil
  "If non-nil — remove selections that match regexp,
otherwise remove those that non match.")

(defun helix-filter-selections (&optional invert)
  "Keep selections that match regexp.
If INVERT is non-nil — remove selections that match regexp"
  (unless (use-region-p) (user-error "No active selection"))
  (when helix-multiple-cursors-mode
    (helix-with-real-cursor-as-fake
      (let* ((cursors (helix-all-fake-cursors))
             (regions (mapcar #'(lambda (cursor)
                                  (overlay-get cursor 'fake-region))
                              cursors))
             (strings (mapcar #'(lambda (cursor)
                                  (buffer-substring-no-properties
                                   (overlay-get cursor 'point)
                                   (overlay-get cursor 'mark)))
                              cursors)))
        (setq helix-filter--regions regions
              helix-filter--regions-strings strings
              helix-filter--invert invert)
        (add-hook 'minibuffer-setup-hook #'helix--start-filter-session)
        (add-hook 'minibuffer-exit-hook #'helix--end-filter-session)
        (deactivate-mark)
        (mapc #'delete-overlay cursors)
        (condition-case nil
            (let* ((regexp (helix-read-pcre-regex (if invert "remove: " "keep: ")))
                   flags)
              (cond ((or (string-empty-p regexp)
                         (progn
                           (setq flags (dolist (str strings (nreverse flags))
                                         (push (if (pcre-string-match-p regexp str) t)
                                               flags)))
                           (when invert
                             (setq flags (mapcar #'not flags)))
                           (not (-contains? flags t))))
                     (signal 'quit nil))
                    (t
                     (set-register "/" regexp)
                     (--each (-zip-lists cursors flags)
                       (pcase-let ((`(,cursor ,flag) it))
                         (if flag
                             (helix--set-cursor-overlay cursor (overlay-get cursor 'point))
                           (helix--delete-fake-cursor cursor)))))))
          ;; Restore if error occurs or quit from minibuffer.
          (t (dolist (cursor cursors)
               (helix--set-cursor-overlay cursor (overlay-get cursor 'point))
               (overlay-put (overlay-get cursor 'fake-region)
                            'face 'helix-region-face))))
        (remove-hook 'minibuffer-setup-hook #'helix--start-filter-session)
        (remove-hook 'minibuffer-exit-hook #'helix--end-filter-session)
        (setq helix-filter--regions nil
              helix-filter--regions-strings nil)
        (activate-mark)))))

(defun helix--start-filter-session ()
  (add-hook 'after-change-functions #'helix--filter-highlight nil t))

(defun helix--end-filter-session ()
  (remove-hook 'after-change-functions #'helix--filter-highlight))

(defun helix--filter-highlight (_ _ _)
  "Highlight current matches during filter session."
  (let ((regexp (minibuffer-contents))
        (regions helix-filter--regions)
        (strings helix-filter--regions-strings)
        flags)
    (condition-case nil
        (cond ((or (string-empty-p regexp)
                   (progn
                     (setq flags (dolist (str strings (nreverse flags))
                                   (push (if (pcre-string-match-p regexp str) t)
                                         flags)))
                     (when helix-filter--invert
                       (setq flags (mapcar #'not flags)))
                     (not (-contains? flags t))))
               (signal 'error nil))
              (t
               (--each (-zip-lists regions flags)
                 (pcase-let ((`(,region ,flag) it))
                   (if flag
                       (overlay-put region 'face 'helix-region-face)
                     (overlay-put region 'face nil))))))
      (error (dolist (region regions)
               (overlay-put region 'face 'helix-region-face))))))

(provide 'helix-search)
;;; helix-search.el ends here
