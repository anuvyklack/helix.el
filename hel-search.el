;;; hel-search.el --- Search -*- lexical-binding: t; -*-
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
;;  Hel search related functionality
;;
;;; Code:

(require 'dash)
(require 'hel-common)
(require 'hel-multiple-cursors-core)

(declare-function hel-update-cursor "hel-core")

(defvar hel-search--timer nil)
(defvar hel-search--buffer nil)
(defvar hel-search--point nil)
(defvar hel-search--window-start nil)
(defvar hel-search--window-end nil)
(defvar hel-search--overlay nil "Main overlay that will become next search.")
(defvar hel-search--direction nil "1 or -1.")
(hel-defvar-local hel-search--hl nil "The `hel-highlight' object for interactive search sessions.")

;;; Highlight search pattern

(defun hel-highlight-search-pattern (regexp &optional direction)
  "Highlight all mathings to the REGEXP toward the DIRECTION.
DIRECTION must be either 1 or -1."
  (let ((hl (hel-highlight-create :buffer (current-buffer)
                                  :regexp regexp
                                  :direction direction
                                  :face 'hel-search-highlight)))
    (unless (hel-highlight-equal hl hel-search--hl)
      (when hel-search--hl (hel-highlight-delete hel-search--hl))
      (setq hel-search--hl hl)))
  (add-hook 'pre-command-hook  #'hel-highlight-search-pattern--cleanup-hook nil t)
  ;; Update highlighting after commands for which
  ;; `hel-search--keep-highlight-p' returns t.
  (add-hook 'post-command-hook #'hel-highlight-search-pattern--update-hook nil t))

(defun hel-highlight-search-pattern--cleanup-hook ()
  (unless (hel-search--keep-highlight-p this-command)
    (setq hel-search--direction nil)
    (when hel-search--timer
      (cancel-timer hel-search--timer)
      (setq hel-search--timer nil))
    (when hel-search--hl
      (hel-highlight-delete hel-search--hl)
      (setq hel-search--hl nil))
    (remove-hook 'pre-command-hook  #'hel-highlight-search-pattern--cleanup-hook t)
    (remove-hook 'post-command-hook #'hel-highlight-search-pattern--update-hook t)))

(defun hel-search--keep-highlight-p (command)
  "Return t if highlight overlays shouldn't be removed on COMMAND execution."
  (and (symbolp command) ;; COMMAND is not lambda
       (or (get command 'scroll-command)
           (memq command hel-keep-search-highlight-commands))))

(defun hel-highlight-search-pattern--update-hook (&optional _ _ _)
  (when hel-search--timer
    (cancel-timer hel-search--timer))
  (setq hel-search--timer
        (run-at-time hel-update-highlight-delay nil
                     (lambda () (hel-highlight-update hel-search--hl)))))

;;; Highlighting class

(cl-defstruct (hel-highlight (:constructor hel-highlight-create)
                             (:type vector) (:copier nil) (:predicate nil))
  (regexp nil :type string)
  (buffer nil :read-only t)
  (face nil :read-only t)
  (ranges nil :documentation "List of cons cells (START . END) in which the highlighting is performed.")
  (direction nil :type number
             :documentation "DIRECTION relative to the point: 1 or -1. Overridden by RANGES.")
  (invert nil :type bool :read-only t :documentation "INVERT overlays.")
  (overlays nil :documentation "Active OVERLAYS."))

(defun hel-highlight-equal (h1 h2)
  "Return t if two `hel-highlight' objects are equal to each other."
  (and h1 h2
       (equal (hel-highlight-regexp h1) (hel-highlight-regexp h1))
       (equal (hel-highlight-buffer h1) (hel-highlight-buffer h2))
       (equal (hel-highlight-face h1) (hel-highlight-face h2))
       (or (equal (hel-highlight-ranges h1) (hel-highlight-ranges h2))
           (equal (hel-highlight-direction h1) (hel-highlight-direction h2)))
       (equal (hel-highlight-invert h1) (hel-highlight-invert h2))))

(defun hel-highlight-delete (hl)
  "Destruct `hel-highlight' object."
  (mapc #'delete-overlay (hel-highlight-overlays hl)))

(defun hel-highlight-update (hl)
  (let ((buffer (or (hel-highlight-buffer hl)
                    (current-buffer)))
        (dir (hel-highlight-direction hl))
        (invert (hel-highlight-invert hl)))
    (with-current-buffer buffer
      (if-let* ((regexp (hel-highlight-regexp hl))
                ((not (string-empty-p regexp)))
                (search-ranges
                 (or (hel-highlight-ranges hl)
                     (->> (get-buffer-window-list buffer)
                          (-map (lambda (win)
                                  (when (window-live-p win)
                                    (cond ((null dir)
                                           (cons (window-start win) (window-end win)))
                                          ((and (< dir 0)
                                                (< (window-start win) (point)))
                                           (cons (window-start win)
                                                 (min (point) (window-end win))))
                                          ((and (< 0 dir)
                                                (< (point) (window-end win)))
                                           (cons (max (window-start win) (point))
                                                 (window-end win)))))))
                          (-remove #'null))))
                (ranges (-mapcat (-lambda ((beg . end))
                                   (hel-regexp-match-ranges regexp beg end invert))
                                 search-ranges)))
          ;; do
          (let ((face (hel-highlight-face hl))
                (old-overlays (hel-highlight-overlays hl)))
            (setf (hel-highlight-overlays hl)
                  (-map (-lambda ((beg . end))
                          (let (ov)
                            (cond ((setq ov (pop old-overlays))
                                   (move-overlay ov beg end))
                                  ((setq ov (make-overlay beg end))
                                   (overlay-put ov 'face face)))
                            ov))
                        ranges))
            (-each old-overlays #'delete-overlay)
            :success)
        ;; else
        (-each (hel-highlight-overlays hl) #'delete-overlay)
        nil))))

(defun hel-regexp-match-ranges (regexp start end &optional invert)
  "Return list of ranges that match REGEXP within START...END positions.
If INVERT is non-nil return list with complements of ranges that match REGEXP."
  (save-excursion
    (goto-char start)
    (ignore-errors
      (let (ranges)
        (condition-case nil
            (while (re-search-forward regexp end t)
              (let ((bounds (hel-match-bounds)))
                ;; Signal if we stack in infinite loop. This happens, for
                ;; example, when regexp consists only of "^" or "$".
                (when (equal bounds (car-safe ranges))
                  (signal 'error nil))
                (unless (or (invisible-p (car bounds))
                            (invisible-p (1- (cdr bounds))))
                  (push bounds ranges))))
          (error
           (setq ranges nil)))
        (when ranges
          (setq ranges (nreverse ranges))
          (if invert
              (hel--invert-ranges ranges start end)
            ranges))))))

(defun hel--invert-ranges (ranges start end)
  "Return the list with complements to RANGES withing START...END positions.
RANGES is a list of cons cells with positions (START . END)."
  (when ranges
    (let (result)
      (-let [(r-start . r-end) (car ranges)]
        (unless (eql r-start start)
          (push (cons start r-start) result)
          (setq start r-end)))
      (cl-loop for (r-start . r-end) in (cdr ranges) do
               (push (cons start r-start) result)
               (setq start r-end))
      (unless (eql start end)
        (push (cons start end) result))
      (nreverse result))))

(defun hel-highlight-entire-ranges (hl)
  (cl-loop for (beg . end) in (hel-highlight-ranges hl)
           do (push (-doto (make-overlay beg end)
                      (overlay-put 'face (hel-highlight-face hl)))
                    (hel-highlight-overlays hl))))

;;; Search

(defun hel-read-regexp (prompt)
  (let ((history-add-new-input nil))
    (read-string prompt nil 'hel-regex-history)))

(defun hel-add-to-regex-history (regex)
  (when (length> regex 2) ;; Reduce the noise level.
    (let ((history-delete-duplicates t))
      (add-to-history 'hel-regex-history regex hel-regex-history-max)))
  (set-register '/ regex)
  (message "Register / set: %s" regex))

(defun hel-search-pattern ()
  "Return regexp from \"/\" register."
  (if-let* ((pattern (get-register '/))
            ((stringp pattern))
            ((not (string-empty-p pattern))))
      (hel-pcre-to-elisp pattern)
    (user-error "Register / is empty")))

;; FIXME: Cursor in the minibuffer blinks on each input.
(defun hel-search-interactively (&optional direction)
  "DIRECTION should be either 1 or -1."
  (unless direction (setq direction 1))
  (setq hel-search--buffer (current-buffer)
        hel-search--point (point)
        hel-search--window-start (window-start)
        hel-search--window-end (window-end)
        hel-search--direction direction
        hel-search--hl (hel-highlight-create :buffer (current-buffer)
                                             :face 'hel-search-highlight))
  (save-excursion
    (let ((region (hel-region)))
      (deactivate-mark)
      (when-let* ((pattern (condition-case nil
                               (minibuffer-with-setup-hook #'hel-search--start-session
                                 (hel-read-regexp (if (< 0 direction) "/" "?")))
                             (quit (when region
                                     (apply #'hel-set-region region)))))
                  ((not (string-empty-p pattern))))
        (hel-add-to-regex-history pattern)
        pattern))))

(defun hel-search--start-session ()
  "Start interactive search."
  (add-hook 'after-change-functions #'hel-search--update-hook nil :local)
  (add-hook 'minibuffer-exit-hook #'hel-search--stop-session nil :local))

(defun hel-search--update-hook (&optional _ _ _)
  (when hel-search--timer
    (cancel-timer hel-search--timer))
  (setq hel-search--timer
        (run-at-time hel-update-highlight-delay nil
                     #'hel-search--do-update)))

(defun hel-search--do-update ()
  (let ((pattern (minibuffer-contents-no-properties)))
    (with-selected-window (minibuffer-selected-window)
      (hel-recenter-point-on-jump
        (let ((dir hel-search--direction)
              (hl hel-search--hl))
          (goto-char hel-search--point)
          (if-let* (((not (string-empty-p pattern)))
                    (regexp (hel-pcre-to-elisp pattern))
                    (match-range (helf-search--search regexp dir)))
              (-let [(beg . end) match-range]
                (goto-char (if (< dir 0) beg end))
                (hel-search--set-target-overlay beg end)
                (setf (hel-highlight-regexp hl) regexp)
                (hel-highlight-update hl))
            ;; else
            (when hel-search--overlay
              (delete-overlay hel-search--overlay))
            (hel-highlight-delete hl)
            (hel-echo "Search failed" 'error))
          (when (and (<= hel-search--window-start (point) hel-search--window-end)
                     (/= (window-start) hel-search--window-start))
            (set-window-start nil hel-search--window-start :noforce)))))))

;; TODO: skip or open invisible matches.
(defun helf-search--search (regexp dir)
  (if (hel-re-search-with-wrap regexp dir)
      (hel-match-bounds)
      ;; (let ((match (hel-match-bounds)))
      ;;   (if (or (eq search-invisible t)
      ;;           (not (isearch-range-invisible (car match) (cdr match))))
      ;;       match))
    ))

(defun hel-search--set-target-overlay (beg end)
  (if hel-search--overlay
      (move-overlay hel-search--overlay beg end)
    (setq hel-search--overlay (-doto (make-overlay beg end nil t nil)
                                (overlay-put 'face 'region)
                                (overlay-put 'priority 99)))))

(defun hel-search--stop-session ()
  "Stop interactive select."
  (with-current-buffer hel-search--buffer
    (when hel-search--timer
      (cancel-timer hel-search--timer)
      (setq hel-search--timer nil))
    (when hel-search--overlay
      (delete-overlay hel-search--overlay)
      (setq hel-search--overlay nil))
    (when hel-search--hl
      (hel-highlight-delete hel-search--hl)
      (setq hel-search--hl nil))))

;;; Select

(defvar hel-select--hl nil
  "`hel-highlight' object for interactive select sessions.")

(defun hel-select-interactively-in (ranges &optional invert)
  "Inner function for `hel-select-regex' command."
  (setq hel-select--hl (hel-highlight-create :buffer (current-buffer)
                                             :face 'region
                                             :ranges ranges
                                             :invert invert))
  (when-let* ((pattern (condition-case nil
                           (minibuffer-with-setup-hook #'hel-select--start-session
                             (hel-read-regexp (if invert "split: " "select: ")))
                         (quit)))
              ((stringp pattern))
              ((not (string-empty-p pattern)))
              (regexp (hel-pcre-to-elisp pattern))
              (regions (-mapcat (-lambda ((beg . end))
                                  (hel-regexp-match-ranges
                                   regexp beg end invert))
                                ranges)))
    (hel-add-to-regex-history pattern)
    (setq mark-active t
          hel--extend-selection nil
          hel--newline-at-eol nil)
    (cl-loop for (mark . point) in regions
             do (hel-create-fake-cursor point mark))
    :success))

(defun hel-select--start-session ()
  "Start interactive select minibuffer session."
  (with-minibuffer-selected-window
    (hel-highlight-entire-ranges hel-select--hl)
    (setq mark-active nil))
  (add-hook 'after-change-functions #'hel-select--update nil t)
  (add-hook 'minibuffer-exit-hook #'hel-select--stop-session nil t))

(defun hel-select--stop-session ()
  "Stop interactive select minibuffer session."
  (when hel-search--timer
    (cancel-timer hel-search--timer)
    (setq hel-search--timer nil))
  (when hel-select--hl
    (hel-highlight-delete hel-select--hl)
    (setq hel-select--hl nil)))

(defun hel-select--update (_ _ _)
  (when hel-search--timer
    (cancel-timer hel-search--timer))
  (setq hel-search--timer
        (run-at-time hel-update-highlight-delay nil
                     #'hel-select--do-update)))

(defun hel-select--do-update ()
  (let ((hl hel-select--hl))
    (setf (hel-highlight-regexp hl)
          (-some-> (minibuffer-contents-no-properties)
            (hel-pcre-to-elisp)))
    (with-minibuffer-selected-window
      (or (hel-highlight-update hl)
          (hel-highlight-entire-ranges hl)))))

;;; Filter

(defvar hel-filter--regions-overlays nil "List of fake regions overlays.")
(defvar hel-filter--regions-contents nil "List of fake regions content.")
(defvar hel-filter--invert nil)

(defun hel-filter-selections (&optional invert)
  "Keep selections that match regexp entered.
If INVERT is non-nil — remove selections that match regexp."
  (unless hel-multiple-cursors-mode
    (user-error "No multiple selections"))
  (hel-with-real-cursor-as-fake
    (let* ((cursors (hel-all-fake-cursors))
           (regions-overlays (-map (lambda (cursor)
                                     (overlay-get cursor 'fake-region))
                                   cursors))
           (regions-contents (-map (lambda (cursor)
                                     (buffer-substring-no-properties
                                      (overlay-get cursor 'point)
                                      (overlay-get cursor 'mark)))
                                   cursors)))
      (setq hel-filter--regions-overlays regions-overlays
            hel-filter--regions-contents regions-contents
            hel-filter--invert invert)
      (deactivate-mark)
      (-each cursors #'delete-overlay)
      (if-let* ((pattern (condition-case nil
                             (minibuffer-with-setup-hook #'hel-filter--start-session
                               (hel-read-regexp (if invert "remove: " "keep: ")))
                           (quit)))
                ((not (string-empty-p pattern)))
                (regexp (hel-pcre-to-elisp pattern))
                (flags (let ((flags (-map (lambda (str)
                                            (if (string-match regexp str) t))
                                          hel-filter--regions-contents)))
                         (if hel-filter--invert
                             (-map #'not flags)
                           flags)))
                ((-contains? flags t)))
          (cl-loop for cursor in cursors
                   for flag in flags
                   do (if flag
                          (progn
                            (hel--set-cursor-overlay cursor (overlay-get cursor 'point))
                            (overlay-put (overlay-get cursor 'fake-region)
                                         'face 'region))
                        (hel--delete-fake-cursor cursor)))
        ;; Else restore all cursors
        (dolist (cursor cursors)
          (hel--set-cursor-overlay cursor (overlay-get cursor 'point))
          (overlay-put (overlay-get cursor 'fake-region)
                       'face 'region))))))

(defun hel-filter--start-session ()
  (add-hook 'after-change-functions #'hel-filter--update-hook nil t)
  (add-hook 'minibuffer-exit-hook #'hel-filter--stop-session nil t))

(defun hel-filter--stop-session ()
  (when hel-search--timer
    (cancel-timer hel-search--timer)
    (setq hel-search--timer nil)))

(defun hel-filter--update-hook (_ _ _)
  (when hel-search--timer
    (cancel-timer hel-search--timer))
  (setq hel-search--timer
        (run-at-time hel-update-highlight-delay nil
                     #'hel-filter--do-update)))

(defun hel-filter--do-update ()
  "Highlight current matches during filtering selections session."
  (if-let* ((regions-overlays hel-filter--regions-overlays)
            (pattern (minibuffer-contents-no-properties))
            ((not (string-empty-p pattern)))
            (regexp (hel-pcre-to-elisp pattern))
            (flags (let ((flags (-map (lambda (str)
                                        (if (string-match regexp str) t))
                                      hel-filter--regions-contents)))
                     (if hel-filter--invert
                         (-map #'not flags)
                       flags)))
            ((-contains? flags t)))
      (cl-loop for overlay in regions-overlays
               for flag in flags
               do (overlay-put overlay 'face (if flag 'region)))
    ;; Else highlight all regions
    (dolist (ov regions-overlays)
      (overlay-put ov 'face 'region))))

;;; Find char

(defun hel-find-char (char direction exclusive?)
  (let* ((case (let (case-fold-search)
                 (not (string-match-p "[A-Z]" (char-to-string char)))))
         (pattern (pcase char
                    (?\t "\t") ;; TAB
                    ((or ?\r ?\n) "\n") ;; RET
                    ;; (?\e) ;; ESC
                    ;; (?\d) ;; DEL (backspace)
                    ;; (_ (char-fold-to-regexp (char-to-string char)))
                    (_ (regexp-quote (char-to-string char)))))
         (hl (hel-highlight-create :buffer (current-buffer)
                                   :regexp pattern
                                   :face 'hel-search-highlight))
         (case-fold-search case)
         (deactivate-mark nil))
    (cl-flet ((search (dir)
                (let ((case-fold-search case))
                  (if exclusive?
                      (cond ((<= 0 dir direction) ;; t n
                             (forward-char))
                            ((<= dir direction 0) ;; T n
                             (backward-char)))
                    ;; else
                    (cond ((< dir 0 direction) ;; f N
                           (backward-char))
                          ((< direction 0 dir) ;; F N
                           (forward-char))))
                  (if (hel-search pattern dir nil t t)
                      (prog1 t
                        (setf (hel-highlight-direction hl) dir)
                        (save-match-data
                          (hel-highlight-update hl))
                        (if exclusive?
                            (cond ((<= 0 dir direction) ;; t n
                                   (backward-char))
                                  ((<= dir direction 0) ;; T n
                                   (forward-char)))
                          ;; not exclusive?
                          (cond ((< dir 0 direction) ;; f N
                                 (forward-char))
                                ((< direction 0 dir) ;; F N
                                 (backward-char)))))
                    ;; else
                    (prog1 nil
                      (hel-highlight-delete hl))))))
      (when (search direction)
        (let ((next (lambda () (interactive) (search direction)))
              (prev (lambda () (interactive) (search (- direction))))
              (on-exit (lambda () (hel-highlight-delete hl))))
          (set-transient-map (define-keymap
                               "n" next
                               "N" prev)
                             t on-exit))))))

(provide 'hel-search)
;;; hel-search.el ends here
