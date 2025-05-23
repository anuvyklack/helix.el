;;; helix-commands.el --- Helix commands -*- lexical-binding: t; -*-
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
;;  Helix commands
;;
;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(require 's)
(require 'dash)
(require 'pcre2el)
(require 'helix-common)
(require 'helix-multiple-cursors-core)
(require 'helix-search)

(defun helix-normal-state-escape ()
  "Command for ESC key in Helix Normal state."
  (interactive)
  (cond (helix--extend-selection
         (setq helix--extend-selection nil))
        (t
         (helix-execute-command-for-all-cursors #'helix-collapse-selection))))

;;; Movements

;; h
(helix-define-motion helix-backward-char (count)
  "Move backward."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (backward-char count))

;; l
(helix-define-motion helix-forward-char (count)
  "Move forward."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  (forward-char count))

;; j
(helix-define-motion helix-next-line (count)
  "Move to the next line."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  ;; Preserve the column: the behaviour is hard-coded and the column
  ;; is preserved if and only if the last command was `next-line' or
  ;; `previous-line'.
  (setq this-command 'next-line)
  (funcall-interactively 'next-line count))

;; k
(helix-define-motion helix-previous-line (count)
  "Move to the COUNT-th previous line."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (deactivate-mark))
  ;; Preserve the column: the behaviour is hard-coded and the column
  ;; is preserved if and only if the last command was `next-line' or
  ;; `previous-line'.
  (setq this-command 'previous-line)
  (funcall-interactively 'previous-line count))

;; w
(helix-define-motion helix-forward-word-start (count &optional bigword)
  "Move to the COUNT-th next word start.
If BIGWORD move over WORD-s."
  (interactive "p")
  (let ((thing (if bigword 'helix-WORD 'helix-word)))
    (when (zerop (forward-thing thing (1- count)))
      (if helix--extend-selection
          (or (region-active-p) (set-mark (point)))
        (skip-chars-forward "\r\n")
        (set-mark (point)))
      (or (helilx-whitespace? (following-char))
          (forward-thing thing))
      (helix-skip-whitespaces))))

;; W
(helix-define-motion helix-forward-WORD-start (count)
  "Move to the COUNT-th next WORD start."
  (interactive "p")
  (helix-forward-word-start count :bigword))

;; b
(helix-define-motion helix-backward-word-start (count &optional bigword)
  "Move to the COUNT-th previous word start.
If BIGWORD move over WORD-s."
  (interactive "p")
  (setq count (- count))
  (let ((thing (if bigword 'helix-WORD 'helix-word)))
    (when (zerop (forward-thing thing (1+ count)))
      (if helix--extend-selection
          (or (region-active-p) (set-mark (point)))
        (skip-chars-backward "\r\n")
        (set-mark (point)))
      (forward-thing thing -1))))

;; B
(helix-define-motion helix-backward-WORD-start (count)
  "Move to the COUNT-th previous WORD start."
  (interactive "p")
  (helix-backward-word-start count :bigword))

;; e
(helix-define-motion helix-forward-word-end (count &optional bigword)
  "Move to the COUNT-th next word end.
If BIGWORD move over WORD-s."
  (interactive "p")
  (let ((thing (if bigword 'helix-WORD 'helix-word)))
    (when (zerop (forward-thing thing (1- count)))
      (if helix--extend-selection
          (or (region-active-p) (set-mark (point)))
        (skip-chars-forward "\r\n")
        (set-mark (point)))
      (forward-thing thing))))

;; E
(helix-define-motion helix-forward-WORD-end (count)
  "Move COUNT-th next WORD end."
  (interactive "p")
  (helix-forward-word-end count :bigword))

;; gg
(defun helix-goto-first-line (num)
  "Move point to the beginning of the buffer.
With numeric arg NUM, put point NUM/10 of the way from the beginning.
If the buffer is narrowed, this command uses the beginning of the
accessible part of the buffer.
Push mark at previous position, unless extending selection."
  (interactive "P")
  (helix-disable-multiple-cursors-mode)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    ;; else
    (set-marker (mark-marker) (point))
    (deactivate-mark))
  (push-mark)
  (goto-char (if num (+ (point-min)
                        (/ (* (- (point-max) (point-min))
                              (prefix-numeric-value num))
                           10))
               (point-min)))
  (if num (forward-line 1)
    (recenter 0)))

;; G
(defun helix-goto-last-line ()
  "Move point the end of the buffer."
  (interactive)
  (helix-disable-multiple-cursors-mode)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    ;; else
    (set-marker (mark-marker) (point))
    (deactivate-mark))
  (push-mark)
  (goto-char (point-max)))

;; gs
(helix-define-motion helix-beginning-of-line ()
  "Move point to beginning of current line.
Use visual line when `visual-line-mode' is on."
  (interactive)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (if visual-line-mode
      (beginning-of-visual-line)
    (beginning-of-line)))

;; gh
(helix-define-motion helix-first-non-blank ()
  "Move point to beginning of current line skipping indentation.
Use visual line when `visual-line-mode' is on."
  (interactive)
  (helix-beginning-of-line)
  (skip-syntax-forward " " (line-end-position))
  (backward-prefix-chars))

;; gl
(helix-define-motion helix-end-of-line ()
  "Move point to end of current line.
Use visual line when `visual-line-mode' is on."
  (interactive)
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (if visual-line-mode
      (end-of-visual-line)
    (move-end-of-line 1)))

;; ]p
(helix-define-motion helix-forward-paragraph (count)
  "Move to the end of the COUNT next paragraph."
  (interactive "p")
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (forward-thing 'paragraph count))

;; [p
(helix-define-motion helix-backward-paragraph (count)
  "Move to the beginning of the COUNT previous paragraph."
  (interactive "p")
  (setq count (- count))
  (if helix--extend-selection
      (or (region-active-p) (set-mark (point)))
    (set-mark (point)))
  (forward-thing 'paragraph count))

;;; Changes

;; i
(defun helix-insert ()
  "Switch to Insert state before region."
  (interactive)
  (when (and (use-region-p)
             (< (mark) (point)))
    (helix-exchange-point-and-mark))
  (helix-insert-state 1))

;; a
(defun helix-append ()
  "Switch to Insert state after region."
  (interactive)
  (when (and (use-region-p)
             (< (point) (mark)))
    (helix-exchange-point-and-mark))
  (helix-insert-state 1))

;; c
(defun helix-change ()
  "Delete region and enter Insert state."
  (interactive)
  (if (use-region-p)
      (let ((line-selected? (helix-line-selected-p)))
        (kill-region nil nil t)
        (pcase line-selected?
          ;; ('line (save-excursion (newline))
          ;;        (indent-according-to-mode))
          ('visual-line (insert " ")
                        (backward-char))))
    ;; no region
    (delete-char -1))
  (helix-insert-state 1))

;; - If point is surrounded by (balanced) whitespace and a brace delimiter
;; ({} [] ()), delete a space on either side of the cursor.
;; - If point is at BOL and surrounded by braces on adjacent lines,
;; collapse newlines:
;; {
;; |
;; } => {|}
;; d
(defun helix-delete ()
  "Delete text in region.
With no region delete char before point."
  (interactive)
  (cond ((use-region-p)
         ;; If line selected — add newline symbol after it into active region.
         (when (helix-line-selected-p)
           (when (< (helix-region-direction) 0)
             (helix-exchange-point-and-mark))
           (forward-char))
         (kill-region nil nil t))
        (t
         (delete-char -1)))
  (helix-extend-selection -1))

;; u
(defun helix-undo ()
  "Cancel current region then undo."
  (interactive)
  (deactivate-mark)
  (undo-only))

(defvar helix--point-marker (make-marker))

;; U
(defun helix-redo ()
  "Cancel current region then redo."
  (interactive)
  (deactivate-mark)
  (undo-redo))

;;; Selections

;; v
(defun helix-extend-selection (&optional arg)
  "Toggle extending selections.
If ARG is nil or `toggle' — toggle extending selection.
If ARG positive number — enable, negative — disable.
Manages the internal `helix--extend-selection' flag."
  (interactive)
  (setq helix--extend-selection (cond ((or (null arg)
                                           (eq arg 'toggle))
                                       (not helix--extend-selection))
                                      ((> arg 0) t)
                                      (t nil))))

;; ;
(defun helix-collapse-selection ()
  "Collapse region onto a single cursor."
  (interactive)
  (deactivate-mark))

;; x
(defun helix-mark-line (count)
  "Mark COUNT lines directionally, extending the active region.
Uses visual lines if `visual-line-mode' is active, otherwise logical lines.
Positive COUNT extends forward, negative extends backward.
When no region exists, marks the current line first.
Handles edge cases and direction transitions (forward<->backward)
automatically."
  (interactive "p")
  (let* ((line (if visual-line-mode 'visual-line 'line))
         (motion-dir (helix-sign count))
         (region-dir (helix-region-direction)))
    (when (helix-mark-current-line)
      (setq count (- count motion-dir)
            region-dir 1))
    (unless (zerop count)
      (cond ((<= 0 region-dir motion-dir)
             (forward-char)
             (forward-thing line count)
             (backward-char))
            ((<= region-dir motion-dir 0)
             (forward-thing line count))
            ((< region-dir 0 motion-dir)
             (forward-thing line count)
             (helix-mark-current-line))
            ((< motion-dir 0 region-dir)
             (forward-thing line (1+ count))
             (backward-char)
             (and (helix-mark-current-line)
                  (helix-exchange-point-and-mark)))))))

(defun helix-mark-current-line ()
  "Extend or create a line-wise selection, using visual/logical lines.
When active region exists: expand selection to encompass full line(s),
converting partial-line selections to whole lines.
Without region: select current line (excludes trailing newline).
Uses visual lines if `visual-line-mode' is active, otherwise logical lines."
  (let ((line (if visual-line-mode 'visual-line 'line)))
    (cond ((use-region-p)
           (unless (helix-line-selected-p)
             (let ((b (region-beginning))
                   (e (region-end)))
               (goto-char b)
               (set-mark (car (bounds-of-thing-at-point line))) ; left end
               (goto-char e)
               (goto-char (cdr (bounds-of-thing-at-point line))) ; right end
               (backward-char)
               1)))
          (t ;; no region
           (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point line)))
             (set-mark beg)
             (goto-char end)
             (backward-char)
             1)))))

;; X
(defun helix-mark-line-upward (count)
  "Select COUNT lines upward.
Select visual lines when `visual-line-mode' is on."
  (interactive "p")
  (helix-mark-line (- count)))

;; %
(defun helix-select-all ()
  (interactive)
  (helix-disable-multiple-cursors-mode)
  (goto-char (point-min))
  (set-mark (point-max)))

;; s
(defun helix-select-regex (start end)
  "Enter PCRE regexp and create cursors for all matching regions in START...END."
  (interactive "r")
  (helix-search-and-select #'helix-regexp-matches-bounds start end))

;; S
(defun helix-split-region (start end)
  (interactive "r")
  (helix-search-and-select #'helix-regexp-inverted-matches-bounds start end))

;; M-s
(defun helix-split-region-on-newline (start end)
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active selection"))
  ;; ".+$" is equal in pcre and elisp regexp syntax.
  (-> (helix-regexp-matches-bounds ".+$" start end)
      (helix-create-cursors))
  (helix-extend-selection -1))

;; K
(defun helix-keep-selections ()
  "Keep selections matching the regexp."
  (interactive)
  (helix-filter-selections))

;; M-K
(defun helix-remove-selections ()
  "Remove selections matching the regexp."
  (interactive)
  (helix-filter-selections t))

;; _
(defun helix-trim-whitespaces-from-selection ()
  "Trim whitespaces and newlines from the both ends of the current selection."
  (interactive)
  (when (use-region-p)
    (let ((dir (if (< (point) (mark)) -1 1)))
      (helix-skip-chars " \t\r\n" (- dir))
      (helix-exchange-point-and-mark)
      (helix-skip-chars " \t\r\n" dir)
      (helix-exchange-point-and-mark))))

;; &
(defun helix-align-selections ()
  "Align selections."
  (interactive)
  (helix-with-real-cursor-as-fake
    ;; Filter cursors to remain only the first on each line.
    (let ((cursors (let ((current-line 0)) ;; Line numbers start from 1
                     (-remove #'(lambda (cursor)
                                  (let* ((pnt (overlay-get cursor 'point))
                                         (line (line-number-at-pos pnt)))
                                    (or (eql line current-line)
                                        (ignore
                                         (setq current-line line)))))
                              (helix-all-fake-cursors t))))
          (column 0))
      ;; Define the column
      (dolist (cursor cursors)
        (goto-char (overlay-get cursor 'point))
        (setq column (max column (current-column))))
      ;; Align
      (dolist (cursor cursors)
        (goto-char (overlay-get cursor 'point))
        (unless (eql (current-column) column)
          (helix--restore-point-state cursor)
          (let ((deactivate-mark nil) ;; To not deactivate-mark after insertion
                (str (s-repeat (- column (current-column)) " ")))
            (cond ((and (use-region-p)
                        (> (helix-region-direction) 0))
                   (helix-exchange-point-and-mark)
                   (insert str)
                   (helix-exchange-point-and-mark))
                  (t
                   (insert str))))
          (helix-move-fake-cursor cursor (point) (mark t)))))))

;; C
(defun helix-copy-selection (count)
  "Copy point and region COUNT times down if COUNT is positive,
of up if negative."
  (interactive "p")
  (if (use-region-p)
      (helix-motion-loop (dir count)
        (helix--copy-region dir))
    ;; else
    (helix-motion-loop (dir count)
      (helix--copy-cursor dir))))

;; M-c
(defun helix-copy-selection-up (count)
  "Copy each selection COUNT times up."
  (interactive "p")
  (helix-copy-selection (- count)))

(defun helix--copy-cursor (direction)
  "Copy point toward the DIRECTION."
  (or direction (setq direction 1))
  (when-let* ((pos (save-excursion
                     (let ((column (current-column))
                           position)
                       (cl-block nil
                         (while (not position)
                           (unless (zerop (forward-line direction))
                             (cl-return))
                           (when (eql (move-to-column column) column)
                             (setq position (point)))))
                       position)))
              ((not (helix-fake-cursor-at pos))))
    (helix-create-fake-cursor-from-point)
    (goto-char pos)))

(defun helix--copy-region (&optional direction)
  "Copy region toward the DIRECTION."
  (or direction (setq direction 1))
  (let* ((region-dir (helix-region-direction))
         (beg (region-beginning))
         (end (region-end))
         (num-of-lines (count-lines beg end))
         (beg-column (save-excursion
                       (goto-char beg)
                       (current-column)))
         (end-column (save-excursion
                       (goto-char end)
                       (current-column))))
    (when-let* ((bounds (save-excursion
                          (goto-char (if (< direction 0) beg end))
                          (helix--bounds-of-following-region
                           beg-column end-column num-of-lines direction))))
      (let (pnt mrk)
        (if (< region-dir 0)
            (pcase-setq `(,pnt . ,mrk) bounds)
          (pcase-setq `(,mrk . ,pnt) bounds))
        (if-let* ((cursor (helix-fake-cursor-at pnt))
                  ((= mrk (overlay-get cursor 'mark))))
            nil ;; Do nothing, since fake cursor is already at target position.
          ;; else
          (helix-create-fake-cursor-from-point)
          (goto-char pnt)
          (set-marker (mark-marker) mrk))))))

(defun helix--bounds-of-following-region
    (start-column end-column number-of-lines direction)
  "Return bounds of following region that starts at START-COLUMN
ends at END-COLUMN spauns NUMBER-OF-LINES."
  (when (< direction 0)
    (pcase-setq `(,start-column . ,end-column)
                (cons end-column start-column)))
  (let (start end)
    (cl-block nil
      (while (not (and start end))
        (unless (zerop (forward-line direction))
          (cl-return))
        (when (eql (move-to-column start-column)
                   start-column)
          (setq start (point))
          (unless (zerop (forward-line (* (1- number-of-lines)
                                          direction)))
            (cl-return))
          (when (eql (move-to-column end-column)
                     end-column)
            (setq end (point))))))
    (if (and start end)
        (if (< 0 direction)
            (cons start end)
          (cons end start)))))

;; (
(defun helix-rotate-selections-backward (count)
  "Rotate main selection backward COUNT times."
  (interactive "p")
  (when helix-multiple-cursors-mode
    (dotimes (_ count)
      (let* ((cursor (or (helix-previous-fake-cursor (point))
                         (helix-last-fake-cursor)))
             (id (overlay-get cursor 'id)))
        (helix-create-fake-cursor-from-point id)
        (helix-restore-point-from-fake-cursor cursor)))))

;; )
(defun helix-rotate-selections-forward (count)
  "Rotate main selection forward COUNT times."
  (interactive "p")
  (when helix-multiple-cursors-mode
    (dotimes (_ count)
      (let* ((cursor (or (helix-next-fake-cursor (point))
                         (helix-first-fake-cursor)))
             (id (overlay-get cursor 'id)))
        (helix-create-fake-cursor-from-point id)
        (helix-restore-point-from-fake-cursor cursor)))))

;; M-(
(defun helix-rotate-selections-content-backward (count)
  "Rotate selections content backward COUNT times."
  (interactive "p")
  (when (and helix-multiple-cursors-mode
             (use-region-p))
    (dotimes (_ count)
      (helix-with-real-cursor-as-fake
        (let ((cursors (-> (helix-all-fake-cursors t)
                           (nreverse))))
          (helix--rotate-selections-content cursors))))))

;; M-)
(defun helix-rotate-selections-content-forward (count)
  "Rotate selections content forward COUNT times."
  (interactive "p")
  (when (and helix-multiple-cursors-mode
             (use-region-p))
    (dotimes (_ count)
      (helix-with-real-cursor-as-fake
        (let ((cursors (helix-all-fake-cursors t)))
          (helix--rotate-selections-content cursors))))))

(defun helix--rotate-selections-content (cursors)
  "Rotate regions content for all CURSORS in the order they are in list."
  (let* ((first-cursor (car cursors))
         (content (buffer-substring (overlay-get first-cursor 'point)
                                    (overlay-get first-cursor 'mark))))
    (dolist (cursor (cdr cursors))
      (setq content (helix--replace-fake-region-content cursor content)))
    (helix--replace-fake-region-content first-cursor content)))

(defun helix--replace-fake-region-content (cursor content)
  "Replace the CURSORs region content with CONTENT.
Return the replaced substring."
  (helix--add-fake-cursor-to-undo-list cursor
    (helix--restore-point-state cursor)
    (let ((dir (helix-region-direction))
          (new-content (buffer-substring (point) (mark)))
          (deactivate-mark nil)) ;; To not deactivate-mark after insertion
      (delete-region (point) (mark))
      (insert content)
      (when (< dir 0) (helix-exchange-point-and-mark))
      (helix-move-fake-cursor cursor (point) (mark t))
      new-content)))

;;; Search

(defvar helix--asterisk-string nil
  "Inner variable for `helix-asterisk' command.")

;; *
(defun helix-asterisk ()
  (interactive)
  (setq helix--asterisk-string nil)
  (helix-execute-command-for-all-cursors #'helix--asterisk-1)
  (set-register '/ helix--asterisk-string)
  (message "register / set to %s" helix--asterisk-string))

;; M-*
(defun helix-meta-asterisk ()
  (interactive)
  (setq helix--asterisk-string nil)
  (helix-execute-command-for-all-cursors #'helix--meta-asterisk-1)
  (set-register "/" helix--asterisk-string)
  (message "register / set to %s" helix--asterisk-string))

(defun helix--asterisk-1 (beg end)
  (interactive "r")
  (when (use-region-p)
    (let* ((quote (if helix-use-pcre-regex #'rxt-quote-pcre #'regexp-quote))
           (open-word-boundary
            (cond ((eql beg (pos-bol))
                   (->> (buffer-substring-no-properties beg (1+ beg))
                        (string-match-p "[[:word:]]")))
                  (t
                   (->> (buffer-substring-no-properties (1- beg) (1+ beg))
                        (string-match-p "[^[:word:]][[:word:]]")))))
           (close-word-boundary
            (cond ((eql end (pos-eol))
                   (->> (buffer-substring-no-properties (1- end) end)
                        (string-match-p "[[:word:]]")))
                  (t
                   (->> (buffer-substring-no-properties (1- end) (1+ end))
                        (string-match-p "[[:word:]][^[:word:]]")))))
           (string (->> (buffer-substring-no-properties (point) (mark))
                        (funcall quote))))
      (setq helix--asterisk-string
            (concat helix--asterisk-string
                    (if helix--asterisk-string "|")
                    (if open-word-boundary "\\b")
                    string
                    (if close-word-boundary "\\b"))))))

(defun helix--meta-asterisk-1 ()
  (interactive)
  (when (use-region-p)
    (let ((quote (if helix-use-pcre-regex #'rxt-quote-pcre #'regexp-quote)))
      (setq helix--asterisk-string
            (concat helix--asterisk-string
                    (if helix--asterisk-string
                        (if helix-use-pcre-regex "|" "\\|"))
                    (->> (buffer-substring-no-properties (point) (mark))
                         (funcall quote)))))))

;; n
(defun helix-search-next (count)
  (interactive "p")
  (let ((region-dir (if (use-region-p) (helix-region-direction) 1))
        (cursor (when helix--extend-selection
                  (helix-create-fake-cursor-from-point))))
    (condition-case nil
        (if-let* ((regexp (get-register '/)))
            (let ((regexp (if helix-use-pcre-regex
                              (pcre-to-elisp regexp)
                            regexp)))
              (unless (eql (helix-sign count)
                           region-dir)
                (helix-exchange-point-and-mark))
              (re-search-forward regexp nil nil count)
              (let ((bounds (cons (or (match-beginning 1) (match-beginning 0))
                                  (or (match-end 1) (match-end 0))))
                    pnt mrk)
                (if (< region-dir 0)
                    (pcase-setq `(,pnt . , mrk) bounds)
                  (pcase-setq `(,mrk . , pnt) bounds))
                (goto-char pnt)
                (set-mark mrk)))
          ;; else
          (message "register / is empty")
          (signal 'error nil))
      (error (when cursor
               (helix-restore-point-from-fake-cursor cursor))
             (helix-ensure-region-direction region-dir)))))

;; N
(defun helix-search-previous (count)
  (interactive "p")
  (helix-search-next (- count)))

;;; Match

(defun helix-match-map-digit-argument (arg)
  "Like `digit-argument' but keep `helix-match-map' active."
  (interactive "P")
  (digit-argument arg)
  (set-transient-map helix-match-map))

;; Don't show `helix-digit-argument-for-match-map' in which-key popup.
(with-eval-after-load 'which-key
  (defvar which-key-replacement-alist)
  (cl-pushnew '((nil . "helix-digit-argument-for-match-map") . ignore)
              which-key-replacement-alist))

;; miw
(defun helix-mark-inner-word (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-word count))

;; miW
(defun helix-mark-inner-WORD (count)
  (interactive "p")
  (helix-mark-inner-thing 'helix-WORD count))

;; mip
(defun helix-mark-inner-paragraph (count)
  (interactive "p")
  (helix-mark-inner-thing 'paragraph count)
  (helix-trim-whitespaces-from-selection))

;; map
(defun helix-mark-a-paragraph (count)
  (interactive "p")
  (helix-mark-inner-thing 'paragraph count))

;; mi"
(defun helix-mark-inner-double-quoted ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-string-at-point ?\")))
    (set-mark (1+ (car bounds)))
    (goto-char (1- (cdr bounds)))))

;; ma"
(defun helix-mark-a-double-quoted ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-string-at-point ?\")))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; mi'
(defun helix-mark-inner-single-quoted ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-string-at-point ?')))
    (set-mark (1+ (car bounds)))
    (goto-char (1- (cdr bounds)))))

;; ma'
(defun helix-mark-a-single-quoted ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-string-at-point ?')))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; mi`
(defun helix-mark-inner-back-quoted ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-string-at-point ?`)))
    (set-mark (1+ (car bounds)))
    (goto-char (1- (cdr bounds)))))

;; ma`
(defun helix-mark-a-back-quoted ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-string-at-point ?`)))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; mi( mi)
(defun helix-mark-inner-paren ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-inner-part-of-sexp-at-point '("(" . ")"))))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; ma( ma)
(defun helix-mark-a-paren ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-sexp-at-point '("(" . ")"))))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; mi[ mi]
(defun helix-mark-inner-bracket ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-inner-part-of-sexp-at-point '("[" . "]"))))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; ma[ ma]
(defun helix-mark-a-bracket ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-sexp-at-point '("[" . "]"))))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; mi{ mi}
(defun helix-mark-inner-curly ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-inner-part-of-sexp-at-point '("{" . "}"))))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; ma{ ma}
(defun helix-mark-a-curly ()
  (interactive)
  (when-let* ((bounds (helix-bounds-of-sexp-at-point '("{" . "}"))))
    (set-mark (car bounds))
    (goto-char (cdr bounds))))

;; mi< mi>
(defun helix-mark-inner-angle ()
  (interactive)
  (let ((pair '("<" . ">"))
        (limits (bounds-of-thing-at-point 'defun)))
    (when-let* ((bounds (helix-4-bounds-of-surrounded-at-point pair limits nil t)))
      (pcase-let ((`(,_ ,l ,r ,_) bounds))
        (set-mark l)
        (goto-char r)))))

;; ma< ma>
(defun helix-mark-an-angle ()
  (interactive)
  (let ((pair '("<" . ">"))
        (limits (bounds-of-thing-at-point 'defun)))
    (when-let* ((bounds (helix-4-bounds-of-surrounded-at-point pair limits nil t)))
      (pcase-let ((`(,l ,_ ,_ ,r) bounds))
        (set-mark l)
        (goto-char r)))))

;;; Surround

(defun helix-surround-add-pair (key insert &optional search regexp? balanced?)
  "Add a new insert-delete pattern for Helix surround functionality.
KEY       - To what KEY this pattern should be binded?
INSERT    - Cons cell (LEFT . RIGHT) with strings, or function that returns such
            cons cell. The strigs that will be inserted by `helix-surround' and
            `helix-surround-change' functions.
SEARCH    - Any of:
            1. Cons cell with strings (LEFT . RIGHT). Should be patterns that
               will be used to search of two substrings to delete in
               `helix-surround-delete' and `helix-surround-change'functions.
               If not specified INSERT pair will be used.
            2. Function that return cons cell with strings (LEFT . RIGHT) like
               in 1.
            3. Function that returns list
                        (LEFT-START LEFT-END RIGHT-START RIGHT-END)
               with 4 positions of START/END of LEFT and RIGHT delimeters.
               Example:
                          left             right
                        |<tag> |Some text| </tag>|
                        ^      ^         ^       ^
                        ls     le        rs      re

Following parameters are taken into account only when SEARCH argument is a cons
cell with stirngs (LEFT . RIGHT) or a function, that returns such cons cell. If
SEARCH is a function that returns list with 4 positions, they will be ignored.

REGEXP?   - If non-nil then strings specified in SEARCH argument will be treated
            as regexp patterns, otherwise they will searched literally.
BALANCED? - When non-nil all nested balanced LEFT RIGHT pairs will be skipped,
            else the first found pattern will be accepted.

This function populates the buffer local `helix-surround-alist' variable,
and thus should be called from major-modes hooks.
See the defaul value of `helix-surround-alist' variable for examples."
  (push (cons key (list :insert insert
                        :search (or search insert)
                        :regexp regexp?
                        :balanced balanced?))
        helix-surround-alist))

(defun helix-surround-insert--read-char ()
  "Read char from minibuffer and return (LEFT . RIGHT) pair to surround with."
  (let* ((char (read-char "surround: "))
         (pair-or-fun-or-nil (-some->
                                 (alist-get char helix-surround-alist)
                               (plist-get :insert))))
    (pcase pair-or-fun-or-nil
      ((and (pred functionp) fn)
       (funcall fn))
      ((and (pred consp) pair)
       pair)
      (_ (cons char char)))))

(helix--advice-to-cache-input helix-surround-insert--read-char)

(defun helix-surround--get-4-bounds (char)
  (let ((spec (alist-get char helix-surround-alist)))
    (if (not spec)
        (helix-4-bounds-of-surrounded-at-point (cons (char-to-string char)
                                                     (char-to-string char))
                                               (bounds-of-thing-at-point 'defun))
      ;; else
      (when-let* ((list-or-pair (pcase (plist-get spec :search)
                                  ((and (pred functionp) fn)
                                   (funcall fn))
                                  ((and (pred consp) lop)
                                   lop))))
        (pcase list-or-pair
          ((and (pred -cons-pair?) pair)
           (helix-4-bounds-of-surrounded-at-point pair
                                                  (bounds-of-thing-at-point 'defun)
                                                  (plist-get spec :regexp)
                                                  (plist-get spec :balanced)))
          ((and list (guard (length= list 4)))
           list))))))

;; ms
(defun helix-surround ()
  "Enclose the selected region in chosen delimiters.
If the region consist of full lines, insert delimiters on separate
lines and reindent the region."
  (interactive)
  (when (use-region-p)
    (pcase-let* ((`(,left . ,right) (helix-surround-insert--read-char))
                 (beg (region-beginning))
                 (end (region-end))
                 (dir (helix-region-direction))
                 (lines? (helix-line-selected-p)))
      (when lines?
        (setq left  (s-trim left)
              right (s-trim right)))
      (let ((deactivate-mark nil)) ;; To not deactivate-mark after insertion
        (goto-char end)
        (when lines?
          (helix-skip-whitespaces)
          (newline-and-indent))
        (insert right)
        (goto-char beg)
        (insert left)
        (when lines?
          (newline-and-indent)))
      (let* ((new-beg (point))
             (new-end (+ end (- new-beg beg))))
        (helix-set-region new-beg new-end dir)
        (indent-region new-beg new-end)))
    (helix-extend-selection -1)))

;; md
(defun helix-surround-delete ()
  (interactive)
  (when-let* ((char (read-char "Delete pair: "))
              (bounds (helix-surround--get-4-bounds char)))
    (pcase-let ((`(,left-beg ,left-end ,right-beg ,right-end) bounds))
      (delete-region right-beg right-end)
      (delete-region left-beg left-end))))

;; mr mc
(defun helix-surround-change ()
  (interactive)
  (when-let* ((char (read-char "Delete pair: "))
              (bounds (helix-surround--get-4-bounds char)))
    (pcase-let*
        ((`(,left-beg ,left-end ,right-beg ,right-end) bounds)
         (char (read-char "Insert pair: "))
         (pair-or-fun (if-let ((spec (alist-get char helix-surround-alist)))
                          (plist-get spec :insert)))
         (`(,left . ,right) (cond ((and pair-or-fun (functionp pair-or-fun))
                                   (funcall pair-or-fun))
                                  (pair-or-fun)
                                  (t (cons char char)))))
      (save-mark-and-excursion
        (delete-region right-beg right-end)
        (goto-char right-beg)
        (insert right)
        (delete-region left-beg left-end)
        (goto-char left-beg)
        (insert left)))))

(defun helix-mark-inner-surround ()
  (interactive)
  (let ((char (if (integerp last-command-event)
                  last-command-event
                (get last-command-event 'ascii-character))))
    (when-let* ((bounds (helix-surround--get-4-bounds char)))
      (pcase-let ((`(,_ ,l ,r ,_) bounds))
        (set-mark l)
        (goto-char r)))))

(defun helix-mark-a-surround ()
  (interactive)
  (let ((char (if (integerp last-command-event)
                  last-command-event
                (get last-command-event 'ascii-character))))
    (when-let* ((bounds (helix-surround--get-4-bounds char)))
      (pcase-let ((`(,l ,_ ,_ ,r) bounds))
        (set-mark l)
        (goto-char r)))))

;;; Window navigation

(defalias 'helix-window-split #'split-window-below)

(defun helix-window-vsplit ()
  "Split the current window vertically.
The new window will be created to the right. All children of the
parent of the splitted window are rebalanced."
  (interactive)
  (let* ((window-to-split (selected-window))
         (new-window (split-window window-to-split nil 'right)))
    (select-window new-window)
    ;; Always copy quit-restore parameter in interactive use.
    (when-let* ((quit-restore (window-parameter window-to-split 'quit-restore)))
      (set-window-parameter new-window 'quit-restore quit-restore)))
  (balance-windows (window-parent)))

(defun helix-window-left (count)
  "Move the cursor to new COUNT-th window left of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-left)))

(defun helix-window-right (count)
  "Move the cursor to new COUNT-th window right of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-right)))

(defun helix-window-up (count)
  "Move the cursor to new COUNT-th window up of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-up)))

(defun helix-window-down (count)
  "Move the cursor to new COUNT-th window down of the current one."
  (interactive "p")
  (dotimes (_ count)
    (windmove-down)))

(defmacro helix-save-side-windows (&rest body)
  "Toggle side windows, evaluate BODY, restore side windows."
  (declare (indent defun) (debug (&rest form)))
  (let ((sides (make-symbol "sidesvar")))
    `(let ((,sides (window-with-parameter 'window-side)))
       (when ,sides (window-toggle-side-windows))
       (unwind-protect
           (progn ,@body)
         (when ,sides (window-toggle-side-windows))))))

(defun helix-move-window (side)
  "SIDE has the same meaning as in `split-window'."
  (helix-save-side-windows
    (unless (one-window-p)
      (save-excursion
        (let ((w (window-state-get (selected-window))))
          (delete-window)
          (let ((wtree (window-state-get)))
            (delete-other-windows)
            (let ((subwin (selected-window))
                  (newwin (split-window nil nil side)))
              (window-state-put wtree subwin)
              (window-state-put w newwin)
              (select-window newwin)))))
      (balance-windows))))

(defun helix-move-window-left ()
  "Swap window with one to the left."
  (interactive)
  (helix-move-window 'left))

(defun helix-move-window-right ()
  "Swap window with one to the right."
  (interactive)
  (helix-move-window 'right))

(defun helix-move-window-up ()
  "Swap window with one upwards."
  (interactive)
  (helix-move-window 'up))

(defun helix-move-window-down ()
  "Swap window with one downwards."
  (interactive)
  (helix-move-window 'down))

(defun helix-window-delete ()
  "Delete the current window or tab.
Rebalance all children of the deleted window's parent window."
  (interactive)
  (let ((parent (window-parent)))
    ;; If tabs are enabled and this is the only visible window, then attempt to
    ;; close this tab.
    (if (and (bound-and-true-p tab-bar-mode)
             (null parent))
        (tab-close)
      (delete-window)
      ;; balance-windows raises an error if the parent does not have
      ;; any further children (then rebalancing is not necessary anyway)
      (ignore-errors (balance-windows parent)))))

(provide 'helix-commands)
;;; helix-commands.el ends here
