;;; hel-paredit.el --- Hel with Paredit -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Yuriy Artemyev
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
;; Integration Hel with Paredit.
;;
;;; Code:

(require 'hel)
(require 'paredit)

;;; Minor mode

;;;###autoload (autoload 'hel-paredit-mode "hel-paredit" nil t)
(define-minor-mode hel-paredit-mode
  "Hel integration with Paredit."
  :keymap (make-sparse-keymap)
  (hel-update-active-keymaps))

;;;; Keybindings

(hel-keymap-set hel-paredit-mode-map :state 'normal
  "W" 'hel-paredit-forward-WORD-start
  "B" 'hel-paredit-backward-WORD-start
  "E" 'hel-paredit-forward-WORD-end

  "m W"   'hel-paredit-mark-inner-WORD
  "m i W" 'hel-paredit-mark-inner-WORD
  "m a W" 'hel-paredit-mark-a-WORD

  "d" 'hel-paredit-cut
  "D" 'hel-paredit-delete

  "M-i" 'hel-paredit-down-sexp
  "M-o" 'hel-paredit-up-sexp-backward
  "M-n" 'hel-paredit-forward-sexp
  "M-p" 'hel-paredit-backward-sexp

  "H"   'hel-paredit-up-sexp-backward
  "L"   'hel-paredit-up-sexp-forward

  "M-r" 'paredit-raise-sexp
  "M-?" 'hel-paredit-convolute-sexp
  "<"   'hel-paredit-<
  ">"   'hel-paredit->
  "g c" 'paredit-comment-dwim
  ;; "g q" 'paredit-reindent-defun
  )

(when hel-want-C-hjkl-keys
  (hel-keymap-set hel-paredit-mode-map :state 'normal
    "C-h" 'hel-paredit-backward-sexp
    "C-j" 'hel-paredit-down-sexp
    "C-k" 'hel-paredit-up-sexp-backward
    "C-l" 'hel-paredit-forward-sexp))

(hel-keymap-set hel-paredit-mode-map
  "RET"         'paredit-newline
  "C-M-f"       'paredit-forward       ; `forward-sexp'
  "C-M-b"       'paredit-backward      ; `backward-sexp'
  "C-M-u"       'paredit-backward-up   ; `backward-up-list'
  "C-M-d"       'paredit-forward-down  ; `down-list'
  "C-M-p"       'paredit-backward-down ; `backward-list'
  "C-M-n"       'paredit-forward-up    ; `forward-list'
  "C-<right>"   'paredit-forward-slurp-sexp
  "C-<left>"    'paredit-forward-barf-sexp
  "C-M-<left>"  'paredit-backward-slurp-sexp
  "C-M-<right>" 'paredit-backward-slurp-sexp)

(hel-keymap-set hel-paredit-mode-map :state 'insert
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

;;; Common

(defun hel-paredit-string-start-pos (&optional state)
  ;; 8. character address of start of comment or string; nil if not in one
  (nth 8 (or state (paredit-current-parse-state))))

;; `hel-paredit-sexp' thing
(defun forward-hel-paredit-sexp (&optional count)
  "Move forward across COUNT S-expressions (sexp).
If COUNT is negative — move backward."
  (hel-motion-loop (dir (or count 1))
    (cond ((paredit-in-string-p)
           ;; `forward-sexp' may move into the next string in the buffer.
           (goto-char (hel-paredit-string-start-pos))
           (if (natnump dir)
               (forward-sexp 1)))
          ((paredit-in-char-p)
           ;;++ Corner case: a buffer of `\|x'.  What to do?
           (if (natnump dir)
               (forward-char)
             (backward-char 2)))
          (t
           (ignore-errors (forward-sexp dir))))))

;; `hel-paredit-WORD' thing
(defun forward-hel-paredit-WORD (&optional count)
  (hel-motion-loop (dir (or count 1))
    (hel-skip-chars "\r\n" dir)
    (hel-skip-chars " \t()[]" dir)
    (unless (hel-line-boundary-p dir)
      (hel-skip-chars "^()[]\n\r\t\f " dir))))

;;; Commands

;; W
(hel-define-command hel-paredit-forward-WORD-start (count)
  "Move to the COUNT-th next WORD start.
Like `hel-forward-WORD-end' but additionally skips all parentheses
and brackets."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (skip-chars-forward "()[]")
  (hel--forward-word-start 'hel-paredit-WORD count))

;; B
(hel-define-command hel-paredit-backward-WORD-start (count)
  "Move to the COUNT-th previous WORD start.
Like `hel-backward-WORD-end' but additionally skips all parentheses
and brackets."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (cl-assert (< 0 count))
  (skip-chars-backward "\r\n")
  (skip-chars-backward "()[]")
  (hel-set-region (if hel--extend-selection (mark) (point))
                  (progn
                    (forward-thing 'hel-paredit-WORD (- count))
                    (point))))

;; E
(hel-define-command hel-paredit-forward-WORD-end (count)
  "Move to the COUNT-th next WORD end.
Like `hel-forward-WORD-end' but additionally skips all parentheses
and brackets."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (skip-chars-forward "()[]")
  (skip-chars-forward "\r\n")
  (hel-set-region (if hel--extend-selection (mark) (point))
                  (progn
                    (forward-thing 'hel-paredit-WORD count)
                    (point))))

;; miW
(hel-define-command hel-paredit-mark-inner-WORD (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-inner-thing 'hel-paredit-WORD count))

;; maW
(hel-define-command hel-paredit-mark-a-WORD ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (hel--mark-a-word 'hel-paredit-WORD))

;; M-n or C-l
(hel-define-command hel-paredit-forward-sexp (count)
  "Mark COUNT sexp forward."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (unless (zerop count)
    (let* ((sexp 'hel-paredit-sexp)
           (dir (hel-sign count))
           (region-dir (if (use-region-p) (hel-region-direction) dir)))
      ;; +1 because `forward-thing' moves point first to the boundary of the
      ;; current thing, and only than to the next thing.
      (when (/= dir region-dir)
        (cl-callf + count dir))
      (forward-thing sexp count)
      (if hel--extend-selection
          (let* ((new-region-dir (if (use-region-p) (hel-region-direction) region-dir))
                 (end (progn
                        (when (and (= new-region-dir region-dir)
                                   (/= dir region-dir))
                          (forward-thing sexp region-dir))
                        (point)))
                 (beg (or (when (or (/= new-region-dir region-dir)
                                    (= (point) (mark)))
                            ;; If region reversed -- adjust mark position.
                            (goto-char (mark))
                            (-if-let ((beg . end) (bounds-of-thing-at-point sexp))
                                (if (natnump dir) beg end)))
                          (mark))))
            (hel-set-region beg end))
        ;; else
        (-if-let ((beg . end) (bounds-of-thing-at-point sexp))
            (hel-set-region beg end region-dir)
          (deactivate-mark))))
    (hel-reveal-point-when-on-top)))

;; M-p or C-h
(hel-define-command hel-paredit-backward-sexp (count)
  "Mark COUNT sexp backward."
  :multiple-cursors t
  :merge-selections 'extend-selection
  (interactive "p")
  (hel-paredit-forward-sexp (- count)))

;; L
(hel-define-command hel-paredit-up-sexp-forward (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (unless (memq last-command '(hel-paredit-up-sexp-forward
                               hel-paredit-up-sexp-backward))
    (hel-push-point))
  (let ((sexp 'hel-paredit-sexp)
        (dir (hel-sign count)))
    (-if-let ((sexp-beg . sexp-end) (bounds-of-thing-at-point sexp))
        (if (use-region-p)
            (let ((beg (region-beginning))
                  (end (region-end)))
              (when (and (<= sexp-beg beg end sexp-end)
                         (or (/= sexp-beg beg)
                             (/= sexp-end end)))
                (hel-set-region sexp-beg sexp-end count)
                (cl-callf - count dir)))
          ;; else
          (hel-set-region sexp-beg sexp-end count)
          (cl-callf - count dir))
      ;; else
      (forward-thing sexp dir)
      (-when-let ((beg . end) (bounds-of-thing-at-point sexp))
        (hel-set-region beg end count))
      (cl-callf - count dir))
    (unless (zerop count)
      (unwind-protect
          (hel-motion-loop (dir count)
            (condition-case nil
                (goto-char (paredit-next-up/down-point dir +1))
              (scan-error (user-error "No sexp up"))))
        (-when-let ((beg . end) (bounds-of-thing-at-point sexp))
          (hel-set-region beg end count))))
    (hel-reveal-point-when-on-top)))

;; M-o or C-k or H
(hel-define-command hel-paredit-up-sexp-backward (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-paredit-up-sexp-forward (- count)))

;; M-i or C-j
(hel-define-command hel-paredit-down-sexp (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (let ((region-dir (if (use-region-p)
                        (prog1 (hel-region-direction)
                          (hel-ensure-region-direction -1))
                      1))
        (sexp 'hel-paredit-sexp))
    (ignore-errors
      (hel-motion-loop (dir count)
        (goto-char (paredit-next-up/down-point dir -1))
        (-when-let ((beg . end) (or (bounds-of-thing-at-point sexp)
                                    (progn (forward-sexp)
                                           (bounds-of-thing-at-point sexp))))
          (hel-set-region beg end region-dir))))))

;; d
(hel-define-command hel-paredit-cut (count)
  "Kill (cut) text in region. I.e. delete text and put it in the `kill-ring'.
If no selection — delete COUNT chars before point."
  :multiple-cursors t
  (interactive "p")
  (when (hel-logical-lines-p)
    (hel-restore-newline-at-eol))
  (cond ((use-region-p)
         (condition-case err
             (paredit-kill-region (region-beginning) (region-end))
           (error
            (hel-echo (error-message-string err) 'error)
            (hel-set-region (mark t) (point) nil t))))
        (t
         (paredit-backward-delete count)))
  (hel-extend-selection -1))

;; D
(hel-define-command hel-paredit-delete (count)
  "Delete text in region, without modifying the `kill-ring'.
If no selection — delete COUNT chars after point."
  :multiple-cursors t
  (interactive "p")
  (when (hel-logical-lines-p)
    (hel-restore-newline-at-eol))
  (cond ((use-region-p)
         (condition-case err
             (paredit-delete-region (region-beginning) (region-end))
           (error
            (hel-echo (error-message-string err) 'error)
            (hel-set-region (mark t) (point) nil t))))
        (t
         (paredit-forward-delete count)))
  (hel-extend-selection -1))

;; M-r
(hel-define-advice paredit-raise-sexp (:around (orig-fun &rest args))
  "Don't deactivate region."
  (hel-save-region
    (apply orig-fun args)))

;; M-?
(defalias 'hel-paredit-convolute-sexp #'paredit-convolute-sexp
  "Convolute S-expression.

Save the S-expressions preceding point and delete them. Splice the S-expressions
following point. Wrap the enclosing list in a new list prefixed by the saved
text. With a prefix argument COUNT, move up COUNT lists before wrapping.

  (let ((x 5)             (frob
        (y 3))       ->    |(let ((x 5)
    (frob |(zwonk))               (y 3))
    (wibblethwop))            (zwonk)
                              (wibblethwop)))

\(fn &optional COUNT)")

;; gc
(hel-advice-add 'paredit-comment-dwim :around #'hel-keep-selection-a)

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
(hel-define-command hel-paredit-< (count)
  "Slurp/barf COUNT times the end of the list the point is located at."
  :multiple-cursors t
  (interactive "p")
  (let (deactivate-mark)
    (cond ((memql (following-char) '(?\( ?\[ ?{))
           (hel-paredit-opening-slurp/barf (- count)))
          ((memql (preceding-char) '(?\) ?\] ?}))
           (hel-paredit-closing-slurp/barf (- count))))))

;; >
(hel-define-command hel-paredit-> (count)
  "Slurp/barf COUNT times the end of the list the point is located at."
  :multiple-cursors t
  (interactive "p")
  (let (deactivate-mark)
    (cond ((memql (following-char) '(?\( ?\[ ?{))
           (hel-paredit-opening-slurp/barf count))
          ((memql (preceding-char) '(?\) ?\] ?}))
           (hel-paredit-closing-slurp/barf count)))))

(defun hel-paredit-opening-slurp/barf (count)
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
                 (if (/= (hel-forward-beginning-of-thing 'hel-sexp count)
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

(defun hel-paredit-closing-slurp/barf (count)
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
                 (if (/= (hel-forward-end-of-thing 'hel-sexp count)
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

;;; Eldoc integration

;; Add motion commands to the `eldoc-message-commands' obarray.
(eldoc-add-command 'hel-paredit-backward-sexp       ; M-h
                   'hel-paredit-down-sexp           ; M-l
                   'hel-paredit-up-sexp-backward    ; M-j
                   'hel-paredit-forward-sexp        ; M-k
                   'hel-paredit-forward-WORD-start  ; W
                   'hel-paredit-backward-WORD-start ; B
                   'hel-paredit-forward-WORD-end)   ; E

(provide 'hel-paredit)
;;; hel-paredit.el ends here
