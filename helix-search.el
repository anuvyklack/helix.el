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

(defvar helix-search--timer nil)
(defvar helix-search--buffer nil)
(defvar helix-search--point nil)
(defvar helix-search--overlay nil "Main overlay that will become next search.")
(defvar helix-search--direction nil "1 or -1.")
(helix-defvar-local helix-search--hl nil "The `helix-highlight' object for interactive search sessions.")

;;; Highlight search pattern

(defun helix-highlight-search-pattern (regexp &optional direction)
  "Highlight all mathings to the REGEXP toward the DIRECTION.
DIRECTION must be either 1 or -1."
  (let ((hl (helix-highlight-create :buffer (current-buffer)
                                    :regexp regexp
                                    :direction direction
                                    :face 'helix-lazy-highlight)))
    (unless (helix-highlight-equal hl helix-search--hl)
      (when helix-search--hl (helix-highlight-delete helix-search--hl))
      (setq helix-search--hl hl)))
  (add-hook 'pre-command-hook  #'helix-highlight-search-pattern--cleanup-hook nil t)
  ;; Update highlighting after `helix-keep-search-highlight-commands'.
  (add-hook 'post-command-hook #'helix-highlight-search-pattern--update-hook nil t))

(defun helix-highlight-search-pattern--cleanup-hook ()
  (unless (memq this-command helix-keep-search-highlight-commands)
    (setq helix-search--direction nil)
    (when helix-search--timer
      (cancel-timer helix-search--timer)
      (setq helix-search--timer nil))
    (when helix-search--hl
      (helix-highlight-delete helix-search--hl)
      (setq helix-search--hl nil))
    (remove-hook 'pre-command-hook  #'helix-highlight-search-pattern--cleanup-hook t)
    (remove-hook 'post-command-hook #'helix-highlight-search-pattern--update-hook t)))

(defun helix-highlight-search-pattern--update-hook (&optional _ _ _)
  (when helix-search--timer
    (cancel-timer helix-search--timer))
  (setq helix-search--timer
        (run-at-time helix-update-highlight-delay nil
                     #'(lambda () (helix-highlight-update helix-search--hl)))))

;;; Highlighting class

(cl-defstruct (helix-highlight (:constructor helix-highlight-create)
                               (:type vector) (:copier nil) (:predicate nil))
  (regexp nil :type string)
  (buffer nil :read-only t)
  (face nil :read-only t)
  (ranges nil :documentation "List of cons cells (START . END) in which the highlighting is performed.")
  (direction nil :type number
             :documentation "DIRECTION relative to the point: 1 or -1. Overridden by RANGES.")
  (invert nil :type bool :read-only t :documentation "INVERT overlays.")
  (overlays nil :documentation "Active OVERLAYS."))

(defun helix-highlight-equal (h1 h2)
  "Return t if two `helix-highlight' objects are equal to each other."
  (and h1 h2
       (equal (helix-highlight-regexp h1) (helix-highlight-regexp h1))
       (equal (helix-highlight-buffer h1) (helix-highlight-buffer h2))
       (equal (helix-highlight-face h1) (helix-highlight-face h2))
       (or (equal (helix-highlight-ranges h1) (helix-highlight-ranges h2))
           (equal (helix-highlight-direction h1) (helix-highlight-direction h2)))
       (equal (helix-highlight-invert h1) (helix-highlight-invert h2))))

(defun helix-highlight-delete (hl)
  "Destruct `helix-highlight' object."
  (mapc #'delete-overlay (helix-highlight-overlays hl)))

(defun helix-highlight-update (hl)
  (let ((buffer (or (helix-highlight-buffer hl)
                    (current-buffer)))
        (dir (helix-highlight-direction hl))
        (invert (helix-highlight-invert hl)))
    (with-current-buffer buffer
      (if-let* ((regexp (helix-highlight-regexp hl))
                ((not (string-empty-p regexp)))
                (search-ranges
                 (or (helix-highlight-ranges hl)
                     (->> (get-buffer-window-list buffer)
                          (-map #'(lambda (win)
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
                                   (helix-regexp-match-ranges regexp beg end invert))
                                 search-ranges)))
          ;; do
          (let ((face (helix-highlight-face hl))
                (old-overlays (helix-highlight-overlays hl)))
            (setf (helix-highlight-overlays hl)
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
        (-each (helix-highlight-overlays hl) #'delete-overlay)
        nil))))

(defun helix-regexp-match-ranges (regexp start end &optional invert)
  "Return list of ranges that match REGEXP within START...END positions.
If INVERT is non-nil return list with complements of ranges that match REGEXP."
  (save-excursion
    (goto-char start)
    (ignore-errors
      (let (ranges)
        (condition-case nil
            (while (re-search-forward regexp end t)
              (let ((bounds (helix-match-bounds)))
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
              (helix--invert-ranges ranges start end)
            ranges))))))

(defun helix--invert-ranges (ranges start end)
  "Return the list with complements to RANGES withing START...END positions.
RANGES is a list of cons cells with positions (START . END)."
  (when ranges
    (let (result)
      (pcase-let ((`(,r-start . ,r-end) (car ranges)))
        (unless (eql r-start start)
          (push (cons start r-start) result)
          (setq start r-end)))
      (pcase-dolist (`(,r-start . ,r-end) (cdr ranges))
        (push (cons start r-start) result)
        (setq start r-end))
      (unless (eql start end)
        (push (cons start end) result))
      (nreverse result))))

(defun helix-highlight-entire-ranges (hl)
  (dolist (range (helix-highlight-ranges hl))
    (-let* (((beg . end) range)
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

(defun helix-search-interactively (&optional direction)
  "DIRECTION should be either 1 or -1."
  (unless direction (setq direction 1))
  (setq helix-search--buffer (current-buffer)
        helix-search--point (point)
        helix-search--direction direction
        helix-search--hl (helix-highlight-create :buffer (current-buffer)
                                                 :face 'helix-lazy-highlight))
  (save-excursion
    (if-let* ((pattern (condition-case nil
                           (minibuffer-with-setup-hook #'helix-search--start-session
                             (helix-read-regexp (if (< 0 direction) "/ " "? ")))
                         (quit)))
              ((not (string-empty-p pattern))))
        (set-register '/ pattern))))

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
  (let ((pattern (minibuffer-contents-no-properties)))
    (with-current-buffer helix-search--buffer
      (let ((dir helix-search--direction)
            (hl helix-search--hl)
            (scroll-conservatively 0))
        (goto-char helix-search--point)
        (if-let* (((not (string-empty-p pattern)))
                  (regexp (helix-pcre-to-elisp pattern))
                  ((helix-re-search-with-wrap regexp dir)))
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
          (when helix-search--overlay
            (delete-overlay helix-search--overlay))
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

;;; Select

(defvar helix-select--hl nil
  "`helix-highlight' object for interactive select sessions.")

(defun helix-select-interactively-in (ranges &optional invert)
  (unless (use-region-p)
    (user-error "No active selection"))
  (setq helix-select--hl (helix-highlight-create :buffer (current-buffer)
                                                 :face 'helix-region-face
                                                 :ranges ranges
                                                 :invert invert))
  (helix-with-deactivate-mark
   (when-let* ((pattern (condition-case nil
                            (minibuffer-with-setup-hook #'helix-select--start-session
                              (helix-read-regexp "select: "))
                          (quit)))
               ((stringp pattern))
               ((not (string-empty-p pattern)))
               (regexp (helix-pcre-to-elisp pattern))
               (region-ranges (-mapcat (-lambda ((beg . end))
                                         (helix-regexp-match-ranges
                                          regexp beg end invert))
                                       ranges)))
     (set-register '/ pattern)
     (setq helix--extend-selection nil)
     (helix-create-cursors region-ranges)
     :success)))

(defun helix-select--start-session ()
  "Start interactive select minibuffer session."
  (with-minibuffer-selected-window
    (helix-highlight-entire-ranges helix-select--hl))
  (add-hook 'after-change-functions #'helix-select--update nil t)
  (add-hook 'minibuffer-exit-hook #'helix-select--stop-session nil t))

(defun helix-select--stop-session ()
  "Stop interactive select minibuffer session."
  (when helix-search--timer
    (cancel-timer helix-search--timer)
    (setq helix-search--timer nil))
  (when helix-select--hl
    (helix-highlight-delete helix-select--hl)
    (setq helix-select--hl nil)))

(defun helix-select--update (_ _ _)
  (when helix-search--timer
    (cancel-timer helix-search--timer))
  (setq helix-search--timer
        (run-at-time helix-update-highlight-delay nil
                     #'helix-select--do-update)))

(defun helix-select--do-update ()
  (let ((hl helix-select--hl))
    (setf (helix-highlight-regexp hl)
          (-some-> (minibuffer-contents-no-properties)
            (helix-pcre-to-elisp)))
    (with-minibuffer-selected-window
      (or (helix-highlight-update hl)
          (helix-highlight-entire-ranges hl)))))

;;; Filter

(defvar helix-filter--regions-overlays nil "List of fake regions overlays.")
(defvar helix-filter--regions-contents nil "List of fake regions content.")
(defvar helix-filter--invert nil)

(defun helix-filter-selections (&optional invert)
  "Keep selections that match regexp.
If INVERT is non-nil — remove selections that match regexp."
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
      (setq helix-filter--regions-overlays regions-overlays
            helix-filter--regions-contents regions-contents
            helix-filter--invert invert)
      (deactivate-mark)
      (-each cursors #'delete-overlay)
      (if-let* ((pattern (condition-case nil
                             (minibuffer-with-setup-hook #'helix-filter--start-session
                               (helix-read-regexp (if invert "remove: " "keep: ")))
                           (quit)))
                ((not (string-empty-p pattern)))
                (regexp (helix-pcre-to-elisp pattern))
                (flags (let ((flags (-map #'(lambda (str)
                                              (if (string-match regexp str) t))
                                          helix-filter--regions-contents)))
                         (if helix-filter--invert
                             (-map #'not flags)
                           flags)))
                ((-contains? flags t)))
          (cl-loop for cursor in cursors
                   for flag in flags
                   do (cond (flag
                             (helix--set-cursor-overlay cursor (overlay-get cursor 'point))
                             (overlay-put (overlay-get cursor 'fake-region)
                                          'face 'helix-region-face))
                            (t (helix--delete-fake-cursor cursor))))
        ;; Else restore all cursors
        (dolist (cursor cursors)
          (helix--set-cursor-overlay cursor (overlay-get cursor 'point))
          (overlay-put (overlay-get cursor 'fake-region)
                       'face 'helix-region-face))))))

(defun helix-filter--start-session ()
  (add-hook 'after-change-functions #'helix-filter--update-hook nil t)
  (add-hook 'minibuffer-exit-hook #'helix-filter--stop-session nil t))

(defun helix-filter--stop-session ()
  (when helix-search--timer
    (cancel-timer helix-search--timer)
    (setq helix-search--timer nil)))

(defun helix-filter--update-hook (_ _ _)
  (when helix-search--timer
    (cancel-timer helix-search--timer))
  (setq helix-search--timer
        (run-at-time helix-update-highlight-delay nil
                     #'helix-filter--do-update)))

(defun helix-filter--do-update ()
  "Highlight current matches during filtering selections session."
  (if-let* ((regions-overlays helix-filter--regions-overlays)
            (pattern (minibuffer-contents-no-properties))
            ((not (string-empty-p pattern)))
            (regexp (helix-pcre-to-elisp pattern))
            (flags (let ((flags (-map #'(lambda (str)
                                          (if (string-match regexp str) t))
                                      helix-filter--regions-contents)))
                     (if helix-filter--invert
                         (-map #'not flags)
                       flags)))
            ((-contains? flags t)))
      (cl-loop for overlay in regions-overlays
               for flag in flags
               do (overlay-put overlay 'face (if flag 'helix-region-face)))
    ;; Else highlight all regions
    (dolist (ov regions-overlays)
      (overlay-put ov 'face 'helix-region-face))))

;;; Find char

(defun helix-find-char (char direction exclusive?)
  (let* ((case (let (case-fold-search)
                 (not (string-match-p "[A-Z]" (char-to-string char)))))
         (pattern (pcase char
                    (?\t "\t") ;; TAB
                    ((or ?\r ?\n) "\n") ;; RET
                    ;; (?\e) ;; ESC
                    ;; (?\d) ;; DEL (backspace)
                    (_ (char-fold-to-regexp (char-to-string char)))
                    ;; (_ (regexp-quote (char-to-string key)))
                    ))
         (hl (helix-highlight-create :buffer (current-buffer)
                                     :regexp pattern
                                     :face 'helix-lazy-highlight))
         (case-fold-search case)
         (deactivate-mark nil))
    (let ((search #'(lambda (dir)
                      (let ((case-fold-search case))
                        (if exclusive?
                            (cond (;; t n
                                   (<= 0 dir direction) (forward-char))
                                  (;; T n
                                   (<= dir direction 0) (backward-char)))
                          ;; not exclusive?
                          (cond (;; f N
                                 (< dir 0 direction) (backward-char))
                                (;; F N
                                 (< direction 0 dir) (forward-char))))
                        (if (helix-search pattern dir nil t t)
                            (prog1 t
                              (setf (helix-highlight-direction hl) dir)
                              (save-match-data
                                (helix-highlight-update hl))
                              (if exclusive?
                                  (cond (;; t n
                                         (<= 0 dir direction) (backward-char))
                                        (;; T n
                                         (<= dir direction 0) (forward-char)))
                                ;; not exclusive?
                                (cond (;; f N
                                       (< dir 0 direction) (forward-char))
                                      (;; F N
                                       (< direction 0 dir) (backward-char)))))
                          ;; else
                          (prog1 nil
                            (helix-highlight-delete hl)))))))
      (when (funcall search direction)
        (let* ((next #'(lambda () (interactive) (funcall search direction)))
               (prev #'(lambda () (interactive) (funcall search (- direction))))
               (on-exit #'(lambda () (helix-highlight-delete hl))))
          (set-transient-map (define-keymap
                               "n" next
                               "N" prev)
                             t on-exit))))))

(provide 'helix-search)
;;; helix-search.el ends here
