;;; helix-paredit.el --- Helix with Paredit -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/anuvyklack/helix.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Integration Helix with Paredit.
;;
;;; Code:

(require 'helix)
(require 'paredit)

(defun helix-paredit-string-start-pos (&optional state)
  ;; 8. character address of start of comment or string; nil if not in one
  (nth 8 (or state (paredit-current-parse-state))))

;; `helix-paredit-sexp' thing
(defun forward-helix-paredit-sexp (&optional count)
  "Move forward across COUNT S-expressions (sexp).
If COUNT is negative — move backward."
  (helix-motion-loop (dir (or count 1))
    (cond ((paredit-in-string-p)
           ;; `forward-sexp' may move into the next string in the buffer.
           (goto-char (helix-paredit-string-start-pos))
           (if (natnump dir)
               (forward-sexp 1)))
          ((paredit-in-char-p)
           ;;++ Corner case: a buffer of `\|x'.  What to do?
           (if (natnump dir)
               (forward-char)
             (backward-char 2)))
          (t
           (ignore-errors (forward-sexp dir))))))

;; `helix-paredit-WORD' thing
(defun forward-helix-paredit-WORD (&optional count)
  (helix-motion-loop (dir (or count 1))
    (helix-skip-chars "\r\n" dir)
    (helix-skip-chars " \t()[]" dir)
    (unless (helix-line-boundary-p dir)
      (helix-skip-chars "^()[]\n\r\t\f " dir))))

;;; Commands

;; W
(helix-define-command helix-paredit-forward-WORD-start (count)
  "Move to the COUNT-th next WORD start.
Like `helix-forward-WORD-end' but additionally skips all parentheses
and brackets."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (skip-chars-forward "()[]")
  (helix--forward-word-start 'helix-paredit-WORD count))

;; B
(helix-define-command helix-paredit-backward-WORD-start (count)
  "Move to the COUNT-th previous WORD start.
Like `helix-backward-WORD-end' but additionally skips all parentheses
and brackets."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (cl-assert (< 0 count))
  (skip-chars-backward "\r\n")
  (skip-chars-backward "()[]")
  (helix-set-region (if helix--extend-selection (mark) (point))
                    (progn
                      (forward-thing 'helix-paredit-WORD (- count))
                      (point))))

;; E
(helix-define-command helix-paredit-forward-WORD-end (count)
  "Move to the COUNT-th next WORD end.
Like `helix-forward-WORD-end' but additionally skips all parentheses
and brackets."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (skip-chars-forward "()[]")
  (skip-chars-forward "\r\n")
  (helix-set-region (if helix--extend-selection (mark) (point))
                    (progn
                      (forward-thing 'helix-paredit-WORD count)
                      (point))))

;; miW
(helix-define-command helix-paredit-mark-inner-WORD (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-mark-inner-thing 'helix-paredit-WORD count))

;; maW
(helix-define-command helix-paredit-mark-a-WORD ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (helix--mark-a-word 'helix-paredit-WORD))

;; M-n or C-l
(helix-define-command helix-paredit-forward-sexp (count)
  "Mark COUNT sexp forward."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (unless (zerop count)
    (let* ((sexp 'helix-paredit-sexp)
           (dir (helix-sign count))
           (region-dir (if (use-region-p) (helix-region-direction) dir)))
      ;; +1 because `forward-thing' moves point first to the boundary of the
      ;; current thing, and only than to the next thing.
      (when (/= dir region-dir)
        (cl-callf + count dir))
      (forward-thing sexp count)
      (if helix--extend-selection
          (let ((new-region-dir (if (use-region-p) (helix-region-direction) region-dir)))
            (when (and (= new-region-dir region-dir)
                       (/= dir region-dir))
              (forward-thing sexp region-dir))
            ;; If region reversed -- adjust mark position.
            (when (or (/= new-region-dir region-dir)
                      (= (point) (mark)))
              (save-excursion
                (goto-char (mark-marker))
                (-when-let ((beg . end) (bounds-of-thing-at-point sexp))
                  (set-marker (mark-marker) (if (natnump dir) beg end))))))
        ;; else
        (-if-let ((beg . end) (bounds-of-thing-at-point sexp))
            (helix-set-region beg end region-dir)
          (deactivate-mark))))
    (helix-reveal-point-when-on-top)))

;; M-p or C-h
(helix-define-command helix-paredit-backward-sexp (count)
  "Mark COUNT sexp backward."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (helix-paredit-forward-sexp (- count)))

;; L
(helix-define-command helix-paredit-up-sexp-forward (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (unless (memq last-command '(helix-paredit-up-sexp-forward
                               helix-paredit-up-sexp-backward))
    (helix-push-point))
  (let ((sexp 'helix-paredit-sexp)
        (dir (helix-sign count)))
    (-if-let ((sexp-beg . sexp-end) (bounds-of-thing-at-point sexp))
        (if (use-region-p)
            (let ((beg (region-beginning))
                  (end (region-end)))
              (when (and (<= sexp-beg beg end sexp-end)
                         (or (/= sexp-beg beg)
                             (/= sexp-end end)))
                (helix-set-region sexp-beg sexp-end count)
                (cl-callf - count dir)))
          ;; else
          (helix-set-region sexp-beg sexp-end count)
          (cl-callf - count dir))
      ;; else
      (forward-thing sexp dir)
      (-when-let ((beg . end) (bounds-of-thing-at-point sexp))
        (helix-set-region beg end count))
      (cl-callf - count dir))
    (unless (zerop count)
      (unwind-protect
          (helix-motion-loop (dir count)
            (condition-case nil
                (goto-char (paredit-next-up/down-point dir +1))
              (scan-error (user-error "No sexp up"))))
        (-when-let ((beg . end) (bounds-of-thing-at-point sexp))
          (helix-set-region beg end count))))
    (helix-reveal-point-when-on-top)))

;; M-o or C-k or H
(helix-define-command helix-paredit-up-sexp-backward (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-paredit-up-sexp-forward (- count)))

;; M-i or C-j
(helix-define-command helix-paredit-down-sexp (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (let ((region-dir (if (use-region-p)
                        (prog1 (helix-region-direction)
                          (helix-ensure-region-direction -1))
                      1))
        (sexp 'helix-paredit-sexp))
    (ignore-errors
      (helix-motion-loop (dir count)
        (goto-char (paredit-next-up/down-point dir -1))
        (-when-let ((beg . end) (or (bounds-of-thing-at-point sexp)
                                    (progn (forward-sexp)
                                           (bounds-of-thing-at-point sexp))))
          (helix-set-region beg end region-dir))))))

;; d
(helix-define-command helix-paredit-cut (count)
  "Kill (cut) text in region. I.e. delete text and put it in the `kill-ring'.
If no selection — delete COUNT chars before point."
  :multiple-cursors t
  (interactive "p")
  (when (helix-logical-lines-p)
    (helix-restore-newline-at-eol))
  (cond ((use-region-p)
         (condition-case err
             (paredit-kill-region (region-beginning) (region-end))
           (error
            (helix-echo (error-message-string err) 'error)
            (helix-set-region (mark t) (point) nil t))))
        (t
         (paredit-backward-delete count)))
  (helix-extend-selection -1))

;; D
(helix-define-command helix-paredit-delete (count)
  "Delete text in region, without modifying the `kill-ring'.
If no selection — delete COUNT chars after point."
  :multiple-cursors t
  (interactive "p")
  (when (helix-logical-lines-p)
    (helix-restore-newline-at-eol))
  (cond ((use-region-p)
         (condition-case err
             (paredit-delete-region (region-beginning) (region-end))
           (error
            (helix-echo (error-message-string err) 'error)
            (helix-set-region (mark t) (point) nil t))))
        (t
         (paredit-forward-delete count)))
  (helix-extend-selection -1))

;; M-r
(helix-define-advice paredit-raise-sexp (:around (orig-fun &rest args))
  "Don't deactivate region."
  (helix-save-region
    (apply orig-fun args)))

;; gc
(helix-advice-add 'paredit-comment-dwim :around #'helix-keep-selection-a)

(dolist (cmd '(paredit-comment-dwim     ; M-; or gc
               paredit-raise-sexp       ; M-r
               paredit-newline          ; RET
               paredit-semicolon        ; ;
               paredit-doublequote      ; "
               paredit-meta-doublequote ; M-"
               paredit-backslash        ; \
               paredit-open-round       ; (
               paredit-close-round      ; )
               paredit-open-square      ; [
               paredit-close-square))   ; ]
  (put cmd 'multiple-cursors t))

;;; Slurpage & Barfage

;; <
(helix-define-command helix-paredit-< (count)
  "Slurp/barf COUNT times the end of the list the point is located at."
  :multiple-cursors t
  (interactive "p")
  (let (deactivate-mark)
    (cond ((memql (following-char) '(?\( ?\[ ?{))
           (helix-paredit-opening-slurp/barf (- count)))
          ((memql (preceding-char) '(?\) ?\] ?}))
           (helix-paredit-closing-slurp/barf (- count))))))

;; >
(helix-define-command helix-paredit-> (count)
  "Slurp/barf COUNT times the end of the list the point is located at."
  :multiple-cursors t
  (interactive "p")
  (let (deactivate-mark)
    (cond ((memql (following-char) '(?\( ?\[ ?{))
           (helix-paredit-opening-slurp/barf count))
          ((memql (preceding-char) '(?\) ?\] ?}))
           (helix-paredit-closing-slurp/barf count)))))

(defun helix-paredit-opening-slurp/barf (count)
  "Slurp/barf beginning of the list COUNT times.
The point must located right before the opening bracket."
  (if (or (paredit-in-comment-p)
          (paredit-in-string-p)
          (paredit-in-char-p))
      (user-error "Invalid context for slurping or barfing S-expression")
    ;; else
    (let ((open (char-after))
          (pos (save-excursion
                 (when (natnump count) (forward-char))
                 (if (/= (helix-forward-beginning-of-thing 'helix-sexp count)
                         count)
                     (point-marker)))))
      (when pos
        (delete-char +1)
        (goto-char pos)
        (insert open)
        (backward-char)
        (set-marker pos nil)
        (save-excursion
          (ignore-errors (backward-up-list))
          (lisp-indent-line)
          (indent-sexp))))))

(defun helix-paredit-closing-slurp/barf (count)
  "Slurp/barf end of the list COUNT times.
The point must located right after the closing bracket."
  (if (or (paredit-in-comment-p)
          (paredit-in-string-p)
          (save-excursion (backward-char)
                          (paredit-in-char-p)))
      (user-error "Invalid context for slurping or barfing S-expression")
    ;; else
    (let ((close (char-before))
          (pos (save-excursion
                 (when (< count 0) (backward-char))
                 (if (/= (helix-forward-end-of-thing 'helix-sexp count)
                         count)
                     (point-marker)))))
      (when pos
        (delete-char -1)
        (goto-char pos)
        (insert close)
        (set-marker pos nil)
        (save-excursion
          (ignore-errors (backward-up-list))
          (lisp-indent-line)
          (indent-sexp))))))

;;; Keybindings

(defvar helix-paredit-mode-map (make-sparse-keymap))

(helix-keymap-set helix-paredit-mode-map
  "C-M-f"       'paredit-forward       ; forward-sexp
  "C-M-b"       'paredit-backward      ; backward-sexp
  "C-M-u"       'paredit-backward-up   ; backward-up-list
  "C-M-d"       'paredit-forward-down  ; down-list
  "C-M-p"       'paredit-backward-down ; backward-list
  "C-M-n"       'paredit-forward-up    ; forward-list
  "C-<right>"   'paredit-forward-slurp-sexp
  "C-<left>"    'paredit-forward-barf-sexp
  "C-M-<left>"  'paredit-backward-slurp-sexp
  "C-M-<right>" 'paredit-backward-slurp-sexp)

(helix-keymap-set helix-paredit-mode-map :state 'normal
  "W" 'helix-paredit-forward-WORD-start
  "B" 'helix-paredit-backward-WORD-start
  "E" 'helix-paredit-forward-WORD-end

  "m W"   'helix-paredit-mark-inner-WORD
  "m i W" 'helix-paredit-mark-inner-WORD
  "m a W" 'helix-paredit-mark-a-WORD

  "d" 'helix-paredit-cut
  "D" 'helix-paredit-delete

  ;; "M-y" 'paredit-copy-as-kill
  "M-d" 'paredit-kill

  "M-i" 'helix-paredit-down-sexp
  "M-o" 'helix-paredit-up-sexp-backward
  "M-n" 'helix-paredit-forward-sexp
  "M-p" 'helix-paredit-backward-sexp

  "M-h" 'helix-paredit-backward-sexp
  "M-j" 'helix-paredit-down-sexp
  "M-k" 'helix-paredit-up-sexp-backward
  "M-l" 'helix-paredit-forward-sexp

  "H"   'helix-paredit-up-sexp-backward
  "L"   'helix-paredit-up-sexp-forward

  "M-r" 'paredit-raise-sexp
  "M-?" 'paredit-convolute-sexp
  "<"   'helix-paredit-<
  ">"   'helix-paredit->
  "g c" 'paredit-comment-dwim
  ;; "g q" 'paredit-reindent-defun
  )

(keymap-set helix-paredit-mode-map "RET" #'paredit-newline)

(helix-keymap-set helix-paredit-mode-map :state 'insert
  ;; "RET"  'paredit-newline
  ";"    'paredit-semicolon
  "\""   'paredit-doublequote
  "M-\"" 'paredit-meta-doublequote
  "\\"   'paredit-backslash
  "M-;"  'paredit-comment-dwim
  "("    'paredit-open-round
  ")"    'paredit-close-round
  "["    'paredit-open-square
  "]"    'paredit-close-square)

;;; Minor mode

;;;###autoload
(define-minor-mode helix-paredit-mode
  "Helix integration with Paredit."
  :keymap helix-paredit-mode-map
  (helix-update-active-keymaps))

;;; Eldoc integration

;; Add motion commands to the `eldoc-message-commands' obarray.
(eldoc-add-command 'helix-paredit-backward-sexp       ; M-h
                   'helix-paredit-down-sexp           ; M-l
                   'helix-paredit-up-sexp-backward    ; M-j
                   'helix-paredit-forward-sexp        ; M-k
                   'helix-paredit-forward-WORD-start  ; W
                   'helix-paredit-backward-WORD-start ; B
                   'helix-paredit-forward-WORD-end)   ; E

(provide 'helix-paredit)
;;; helix-paredit.el ends here
