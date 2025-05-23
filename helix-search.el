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

(declare-function helix-extend-selection "helix-commands")

;;; Highlight

(cl-defstruct (helix-highlight (:constructor helix-highlight-create)
                               (:type vector) (:copier nil) (:predicate nil))
  (regexp nil :type string)
  (face nil :read-only t)
  (regions nil :documentation "List with bounds of regions in which the highlighting is performed.")
  (direction nil :documentation "1 — forward, -1 — backward, nil — both ways.")
  (invert nil :documentation "Invert overlays."
          :type bool :read-only t)
  (buffer nil :read-only t)
  (overlays nil :documentation "The active overlays of the highlight."))

(defun helix-highlight-delete (hl)
  "Destruct `helix-highlight' object."
  (mapc #'delete-overlay (helix-highlight-overlays hl)))

(defun helix-highlight-update (hl)
  (let ((buffer (or (helix-highlight-buffer hl)
                    (current-buffer)))
        (direction (helix-highlight-direction hl)))
    (with-current-buffer buffer
      (if-let* ((regexp (helix-highlight-regexp hl))
                ((not (string-empty-p regexp)))
                (search-ranges
                 (or (helix-highlight-regions hl)
                     (let (ranges)
                       (dolist (win (get-buffer-window-list buffer))
                         (when-let* (((window-live-p win))
                                     (beg (pcase direction
                                            (1 (max (window-start) (point)))
                                            (_ (window-start))))
                                     (end (pcase direction
                                            (-1 (min (point) (window-end)))
                                            (_ (window-end))))
                                     ((< beg end)))
                           (push (cons beg end) ranges)))
                       (nreverse ranges))))
                (regions (let ((invert (helix-highlight-invert hl)))
                           (-mapcat (-lambda ((beg . end))
                                      (helix-regexp-match-regions
                                       regexp beg end invert))
                                    search-ranges))))
          ;; do
          (let ((face (helix-highlight-face hl))
                (old-overlays (helix-highlight-overlays hl))
                new-overlays)
            (mapc #'delete-overlay old-overlays)
            (--each regions
              (-let (((beg . end) it)
                     ov)
                (cond ((setq ov (pop old-overlays))
                       (move-overlay ov beg end))
                      ((setq ov (make-overlay beg end))
                       (overlay-put ov 'face face)))
                (push ov new-overlays)))
            (setf (helix-highlight-overlays hl) new-overlays)
            :success)
        ;; else
        (mapc #'delete-overlay (helix-highlight-overlays hl))
        nil))))

(defun helix-regexp-match-regions (regexp start end &optional invert)
  "Return list with bounds of regions that match REGEXP within START...END.
If INVERT is non-nil return list with complements of regions that match REGEXP."
  (save-excursion
    (goto-char start)
    (ignore-errors
      (let (regions)
        (condition-case nil
            (while (re-search-forward regexp end t)
              (let ((bounds (helix-match-bounds)))
                ;; Signal if we stack in infinite loop. This happens, for
                ;; example, when regexp consists only of "^" or "$".
                (when (equal bounds (car-safe regions))
                  (signal 'error nil))
                (push bounds regions)))
          (error
           (setq regions nil)))
        (when regions
          (setq regions (nreverse regions))
          (if invert
              (helix--invert-regions regions start end)
            regions))))))

(defun helix--invert-regions (regions start end)
  "Return the list with complements to REGIONS withing START...END.
REGIONS is a list of cons cells with positions (START . END)."
  (when regions
    (let (result)
      (-let (((r-start . r-end) (car regions)))
        (unless (eql r-start start)
          (push (cons start r-start) result)
          (setq start r-end)))
      (dolist (bounds (cdr regions))
        (-let (((r-start . r-end) bounds))
          (push (cons start r-start) result)
          (setq start r-end)))
      (unless (eql start end)
        (push (cons start end) result))
      (nreverse result))))

(defun helix-highlight-entire-regions (hl)
  (--each (helix-highlight-regions hl)
    (-let* (((beg . end) it)
            (ov (make-overlay beg end)))
      (overlay-put ov 'face (helix-highlight-face hl))
      (push ov (helix-highlight-overlays hl)))))

;;; Search

(defun helix-read-regexp (prompt)
  (let ((history-add-new-input nil)
        (history-delete-duplicates t))
    (let ((regex (read-string prompt nil 'helix-regex-history)))
      (add-to-history 'helix-regex-history regex helix-regex-history-max)
      regex)))

(defun helix-search-pattern ()
  "Return regexp from register \"/\", or nil."
  (if-let* ((pattern (get-register '/))
            ((stringp pattern))
            ((not (string-empty-p pattern))))
      (helix-pcre-to-elisp pattern)
    (user-error "Register / is empty")))

(defun helix-re-search-with-wrap (regexp &optional direction)
  "Search REGEXP from the point toward the DIRECTION.
If nothing found, wrap around the buffer and search up to the point."
  (when (and (use-region-p)
             (not (eql direction (helix-region-direction))))
    (goto-char (mark-marker)))
  (or (re-search-forward regexp nil t direction)
      ;; If nothing found — wrap around buffer end and try again.
      (let ((point (point)))
        (goto-char (if (< direction 0) (point-max) (point-min)))
        (if (re-search-forward regexp point t direction)
            (message "Wrapped around buffer")))))

(defvar helix--minibuffer-session-timer nil)

(defvar helix-search--buffer nil)
(defvar helix-search--point nil)
(defvar helix-search--overlay nil)
(defvar helix-search--direction nil "1 or -1.")
(defvar helix-search--timer nil)
(helix-defvar-local helix-search--hl nil "The `helix-highlight' object for interactive search sessions.")

(defun helix-search-interactively (&optional direction)
  "DIRECTION should be either 1 or -1."
  (unless direction (setq direction 1))
  (setq helix-search--buffer (current-buffer)
        helix-search--point (point)
        helix-search--direction direction
        helix-search--hl (helix-highlight-create :buffer (current-buffer)
                                                 :face 'helix-lazy-highlight))
  (let ((region-dir (if (use-region-p) (helix-region-direction) 1)))
    (when-let*
        ((pattern (condition-case nil
                      (minibuffer-with-setup-hook #'helix-search--start-session
                        (helix-read-regexp (if (< 0 direction) "/ " "? ")))
                    (quit)))
         ((not (string-empty-p pattern)))
         (regexp (helix-pcre-to-elisp pattern)))
      (set-register '/ pattern)
      :success)))

(defun helix-search--start-session ()
  "Start interactive search."
  (add-hook 'after-change-functions #'helix-search--update-hook nil t)
  (add-hook 'minibuffer-exit-hook #'helix-search--stop-session nil t))

(defun helix-search--update-hook (&optional _ _ _)
  (when helix-search--timer
    (cancel-timer helix-search--timer))
  (setq helix-search--timer
        (run-at-time helix-update-highlight-delay nil
                     #'helix-search--do-update)))

(defun helix-search--do-update ()
  (let ((regexp (-some-> (minibuffer-contents-no-properties)
                  (helix-pcre-to-elisp))))
    (with-current-buffer helix-search--buffer
      (let ((dir helix-search--direction)
            (hl helix-search--hl)
            (scroll-conservatively 0))
        (goto-char helix-search--point)
        (if (helix-re-search-with-wrap regexp dir)
            (-let* (((beg . end) (helix-match-bounds)))
              (goto-char (if (< dir 0) beg end))
              (if helix-search--overlay
                  (move-overlay helix-search--overlay beg end)
                (setq helix-search--overlay
                      (let ((ov (make-overlay beg end nil t nil)))
                        (overlay-put ov 'face 'helix-region-face)
                        (overlay-put ov 'priority 99)
                        ov)))
              (setf (helix-highlight-regexp hl) regexp)
              (helix-highlight-update hl))
          ;; else
          (helix-highlight-delete hl))))))

(defun helix-search--stop-session ()
  "Stop interactive select."
  (with-current-buffer helix-search--buffer
    (when helix-search--timer
      (cancel-timer helix-search--timer)
      (setq helix-search--timer nil))
    (when helix-search--overlay
      (delete-overlay helix-search--overlay)
      (setq helix-search--overlay nil))
    (when helix-search--hl
      (helix-highlight-delete helix-search--hl)
      (setq helix-search--hl nil))))

;;; Flash search pattern

(defun helix-flash-search-pattern ()
  "Highlight all mathings to the regexp from \"/\" register."
  ;; Get regexp from "/" register before we delete currently active overlays,
  ;; because `helix-search-pattern' can throw.
  (let ((regexp (helix-search-pattern)))
    (when helix-search--hl (helix-highlight-delete helix-search--hl))
    (setq helix-search--hl (helix-highlight-create :buffer (current-buffer)
                                                   :regexp regexp
                                                   :face 'helix-lazy-highlight))
    (helix-highlight-update helix-search--hl)
    (add-hook 'pre-command-hook  #'helix-flash-search-pattern--cleanup-hook nil t)
    (add-hook 'post-command-hook #'helix-flash-search-pattern--update-hook nil t)))

(defun helix-flash-search-pattern--cleanup-hook ()
  (unless (memq this-command helix-keep-search-highlight-commands)
    (when helix-search--timer
      (cancel-timer helix-search--timer))
    (when helix-search--hl
      (helix-highlight-delete helix-search--hl)
      (setq helix-search--hl nil))
    (remove-hook 'pre-command-hook  #'helix-flash-search-pattern--cleanup-hook t)
    (remove-hook 'post-command-hook #'helix-flash-search-pattern--update-hook t)))

(defun helix-flash-search-pattern--update-hook (&optional _ _ _)
  (when helix-search--timer
    (cancel-timer helix-search--timer))
  (setq helix-search--timer
        (run-at-time helix-update-highlight-delay nil
                     #'helix-flash-search-pattern--do-update)))

(defun helix-flash-search-pattern--do-update ()
  (if helix-search--hl
      (helix-highlight-update helix-search--hl)
    (remove-hook 'post-command-hook #'helix-flash-search-pattern--update-hook t)))

;;; Select

(defvar helix-select--hl nil
  "`helix-highlight' object for interactive select sessions.")

(defun helix-select-in-regions (regions &optional invert)
  (unless (use-region-p)
    (user-error "No active selection"))
  (setq helix-select--hl (helix-highlight-create :buffer (current-buffer)
                                                 :face 'helix-region-face
                                                 :regions regions
                                                 :invert invert))
  (helix-with-deactivate-mark
   (when-let* ((pattern (condition-case nil
                            (minibuffer-with-setup-hook #'helix-select--start-session
                              (helix-read-regexp "select: "))
                          (quit)))
               ((string pattern))
               ((not (string-empty-p pattern)))
               (regexp (helix-pcre-to-elisp pattern))
               (new-regions (-mapcat (-lambda ((beg . end))
                                       (helix-regexp-match-regions
                                        regexp beg end invert))
                                     regions)))
     (set-register '/ pattern)
     (helix-create-cursors new-regions)
     :success)))

(defun helix-select--start-session ()
  "Start interactive select minibuffer session."
  (with-minibuffer-selected-window
    (helix-highlight-entire-regions helix-select--hl))
  (add-hook 'after-change-functions #'helix-select--update-highlight nil t)
  (add-hook 'minibuffer-exit-hook #'helix-select--stop-session nil t))

(defun helix-select--stop-session ()
  "Stop interactive select minibuffer session."
  (when helix--minibuffer-session-timer
    (cancel-timer helix--minibuffer-session-timer)
    (setq helix--minibuffer-session-timer nil))
  (when helix-select--hl
    (helix-highlight-delete helix-select--hl)
    (setq helix-select--hl nil)))

(defun helix-select--update-highlight (_ _ _)
  (when helix--minibuffer-session-timer
    (cancel-timer helix--minibuffer-session-timer))
  (setq helix--minibuffer-session-timer
        (run-at-time helix-update-highlight-delay nil
                     #'helix-select--do-update-highlight)))

(defun helix-select--do-update-highlight ()
  (let ((hl helix-select--hl))
    (setf (helix-highlight-regexp hl)
          (-some-> (minibuffer-contents-no-properties)
            (helix-pcre-to-elisp)))
    (with-minibuffer-selected-window
      (or (helix-highlight-update hl)
          (helix-highlight-entire-regions hl)))))

;;; Filter

(defvar helix-filter-selections--regions-overlays nil "List of fake regions overlays.")
(defvar helix-filter-selections--regions-contents nil "List of fake regions content.")
(defvar helix-filter-selections--invert nil)

(defun helix-filter-selections (&optional invert)
  "Keep selections that match regexp.
If INVERT is non-nil — remove selections that match regexp"
  (unless helix-multiple-cursors-mode
    (user-error "No multiple selections"))
  (helix-with-real-cursor-as-fake
    (let* ((cursors (helix-all-fake-cursors))
           (regions-overlays (-map #'(lambda (cursor)
                                       (overlay-get cursor 'fake-region))
                                   cursors))
           (regions-contents (-map #'(lambda (cursor)
                                       (buffer-substring-no-properties
                                        (overlay-get cursor 'point)
                                        (overlay-get cursor 'mark)))
                                   cursors)))
      (setq helix-filter-selections--regions-overlays regions-overlays
            helix-filter-selections--regions-contents regions-contents
            helix-filter-selections--invert invert)
      (deactivate-mark)
      (mapc #'delete-overlay cursors)
      (if-let* ((pattern (condition-case nil
                             (minibuffer-with-setup-hook #'helix-filter-selections--start-session
                               (helix-read-regexp (if invert "remove: " "keep: ")))
                           (quit)))
                ((not (string-empty-p pattern)))
                (regexp (helix-pcre-to-elisp pattern))
                (flags (unless (string-empty-p regexp)
                         (let ((flags (-map #'(lambda (str)
                                                (if (string-match regexp str) t))
                                            helix-filter-selections--regions-contents)))
                           (if helix-filter-selections--invert
                               (-map #'not flags)
                             flags))))
                ((-contains? flags t)))
          (--each (-zip-lists cursors flags)
            (-let (((cursor flag) it))
              (cond (flag
                     (helix--set-cursor-overlay cursor (overlay-get cursor 'point))
                     (overlay-put (overlay-get cursor 'fake-region)
                                  'face 'helix-region-face))
                    (t (helix--delete-fake-cursor cursor)))))
        ;; Else restore all cursors
        (dolist (cursor cursors)
          (helix--set-cursor-overlay cursor (overlay-get cursor 'point))
          (overlay-put (overlay-get cursor 'fake-region)
                       'face 'helix-region-face))))))

(defun helix-filter-selections--start-session ()
  (add-hook 'after-change-functions #'helix-filter-selections--update-hook nil t)
  (add-hook 'minibuffer-exit-hook #'helix-filter-selections--stop-session nil t))

(defun helix-filter-selections--stop-session ()
  (when helix--minibuffer-session-timer
    (cancel-timer helix--minibuffer-session-timer)
    (setq helix--minibuffer-session-timer nil)))

(defun helix-filter-selections--update-hook (_ _ _)
  (when helix--minibuffer-session-timer
    (cancel-timer helix--minibuffer-session-timer))
  (setq helix--minibuffer-session-timer
        (run-at-time helix--minibuffer-session-timer nil
                     #'helix-filter-selections--do-update)))

(defun helix-filter-selections--do-update ()
  "Highlight current matches during filtering selections session."
  (let* ((regexp (-some-> (minibuffer-contents-no-properties)
                   (helix-pcre-to-elisp)))
         (regions-overlays helix-filter-selections--regions-overlays)
         (flags (unless (string-empty-p regexp)
                  (let ((flags (-map #'(lambda (str)
                                         (if (string-match regexp str) t))
                                     helix-filter-selections--regions-contents)))
                    (if helix-filter-selections--invert
                        (-map #'not flags)
                      flags)))))
    (cond ((and flags (-contains? flags t))
           (--each (-zip-lists regions-overlays flags)
             (-let (((overlay flag) it))
               (if flag
                   (overlay-put overlay 'face 'helix-region-face)
                 (overlay-put overlay 'face nil)))))
          (t (--each regions-overlays
               (overlay-put it 'face 'helix-region-face))))))

(provide 'helix-search)
;;; helix-search.el ends here
