;;; helix-integration.el --- Integration with other packages -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Yuriy Artemyev
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
;;  Helix integration with other packages.
;;
;;; Code:

(require 'helix-core)
(require 'helix-multiple-cursors-core)
(require 'helix-commands)
(require 'keypad)

;;; Integration multiple cursors with Emacs functionality

;; M-x
(helix-define-advice execute-extended-command (:after (&rest _) helix)
  "Execute selected command for all cursors."
  (setq helix-this-command this-command))

(put 'execute-extended-command 'multiple-cursors 'false)

(helix-define-advice current-kill (:before (n &optional _do-not-move) helix)
  "Make sure pastes from other programs are added to `kill-ring's
of all cursors when yanking."
  (when-let* ((interprogram-paste (and (= n 0)
                                       interprogram-paste-function
                                       (funcall interprogram-paste-function))))
    (when (listp interprogram-paste)
      ;; Use `reverse' to avoid modifying external data.
      (setq interprogram-paste (reverse interprogram-paste)))
    ;; Add `interprogram-paste' to `kill-ring's of all cursors real and
    ;; fake. This is what `current-kill' do internally, but we have to do
    ;; it ourselves, because `interprogram-paste-function' is not a pure
    ;; function — it returns something only once.
    (let ((interprogram-cut-function nil)
          (interprogram-paste-function nil))
      ;; real cursor
      (if (listp interprogram-paste)
          (mapc 'kill-new interprogram-paste)
        (kill-new interprogram-paste))
      ;; fake cursors
      (dolist (cursor (helix-all-fake-cursors))
        (let ((kill-ring (overlay-get cursor 'kill-ring))
              (kill-ring-yank-pointer (overlay-get cursor 'kill-ring-yank-pointer)))
          (if (listp interprogram-paste)
              (mapc 'kill-new interprogram-paste)
            (kill-new interprogram-paste))
          (overlay-put cursor 'kill-ring kill-ring)
          (overlay-put cursor 'kill-ring-yank-pointer kill-ring-yank-pointer))))))

(helix-define-advice execute-kbd-macro (:around (orig-fun &rest args))
  "`execute-kbd-macro' should never be run for fake cursors.
The real cursor will execute the keyboard macro, resulting in new commands
in the command loop, and the fake cursors can pick up on those instead."
  (unless helix-executing-command-for-fake-cursor
    (apply orig-fun args)))

(helix-cache-input read-char)
(helix-cache-input read-quoted-char)
(helix-cache-input read-from-kill-ring)
(helix-cache-input read-char-from-minibuffer)
(helix-cache-input register-read-with-preview)  ; used by read-string

;; Commands that don't work with multiple-cursors
(helix-unsupported-command isearch-forward)
(helix-unsupported-command isearch-backward)

;;; Selection (mark & region)

(helix-advice-add 'exchange-point-and-mark :after #'helix-reveal-point-when-on-top)

(helix-define-advice pop-to-mark-command (:around (orig-fun))
  "When region is active, skip active mark and jump to one before it."
  (if (use-region-p)
      (helix-motion-loop (_ 2)
        (funcall orig-fun))
    (funcall orig-fun))
  (helix-reveal-point-when-on-top))

(dolist (command '(fill-region          ;; gq
                   indent-region        ;; =
                   indent-rigidly-left  ;; >
                   indent-rigidly-right ;; <
                   comment-dwim))       ;; gc
  (helix-advice-add command :around #'helix-keep-selection-a))

(helix-advice-add 'clone-indirect-buffer :before #'helix-deactivate-mark)

;;; Eldoc

(with-eval-after-load 'eldoc
  ;; Add motion commands to the `eldoc-message-commands' obarray.
  (eldoc-add-command 'helix-backward-char        ;; h
                     'helix-forward-char         ;; l
                     'helix-next-line            ;; j
                     'helix-previous-line        ;; k
                     'helix-forward-word-start   ;; w
                     'helix-forward-WORD-start   ;; W
                     'helix-backward-word-start  ;; b
                     'helix-backward-WORD-start  ;; B
                     'helix-forward-word-end     ;; e
                     'helix-forward-WORD-end     ;; E
                     'helix-first-non-blank      ;; gh
                     'helix-end-of-line-command  ;; gl
                     'helix-search-forward       ;; /
                     'helix-search-backward      ;; ?
                     'helix-search-next          ;; n
                     'helix-search-previous      ;; N
                     'helix-find-char-forward    ;; f
                     'helix-find-char-backward   ;; F
                     'helix-till-char-forward    ;; t
                     'helix-till-char-backward)) ;; T

;;; winner-mode & tab-bar-history-mode

(add-hook 'winner-mode-hook #'helix-setup-winner-mode-keys)
(defun helix-setup-winner-mode-keys ()
  (if winner-mode
      (helix-keymap-set helix-window-map nil
        "u" #'winner-undo
        "U" #'winner-redo)
    (helix-keymap-set helix-window-map nil
      "u" nil
      "U" nil)))

(add-hook 'tab-bar-history-mode-hook #'helix-setup-tab-bar-history-mode-keys)
(defun helix-setup-tab-bar-history-mode-keys ()
  (if tab-bar-history-mode
      (helix-keymap-set helix-window-map nil
        "u" #'tab-bar-history-back
        "U" #'tab-bar-history-forward)
    (helix-keymap-set helix-window-map nil
      "u" nil
      "U" nil)))

;;; Edebug

(with-eval-after-load 'edebug
  (add-hook 'edebug-mode-hook #'helix-update-active-keymaps)
  (helix-keymap-set edebug-mode-map nil
    "SPC" nil ; unding `edebug-step-mode'
    "h"   nil ; unding `edebug-goto-here'
    "s" #'edebug-step-mode
    "H" #'edebug-goto-here
    "C-c h" #'edebug-goto-here))

;;; Help

(with-eval-after-load 'help-mode
  (helix-set-initial-state 'help-mode 'normal)
  (helix-inhibit-insert-state help-mode-map)
  ;; (helix-keymap-set help-mode-map 'normal
  ;;   ;; "RET" (keymap-lookup help-mode-map "RET")
  ;;   "q" (keymap-lookup help-mode-map "q"))
  )

(with-eval-after-load 'helpful
  (helix-set-initial-state 'helpful-mode 'normal)
  (helix-inhibit-insert-state helpful-mode-map)
  ;; (helix-keymap-set helpful-mode-map 'normal
  ;;   "q" #'quit-window)
  (put 'helpful-variable 'multiple-cursors 'false))

;;; Button

(helix-advice-add 'forward-button :before #'helix-deactivate-mark)

;;; Special mode

;; hjkl keys are free in `special-mode-map' by default, so we can use them.
(helix-keymap-set special-mode-map nil
  "h" #'left-char
  "j" #'next-line
  "k" #'previous-line
  "l" #'right-char)

(helix-keymap-set special-mode-map nil
  "g"   nil ;; revert-buffer
  "g r" 'revert-buffer
  "g g" 'beginning-of-buffer ; also "<"
  "G"   'end-of-buffer)      ; also ">"

;;; Messages buffer

(helix-set-initial-state 'messages-buffer-mode 'normal)

;;; Minibuffer

;; (helix-keymap-set minibuffer-mode-map 'normal
;;   ;; "ESC" #'abort-minibuffers
;;   "<escape>" #'abort-recursive-edit
;;   ;; "<down>"   #'next-line-or-history-element
;;   ;; "<up>"     #'previous-line-or-history-element
;;   "C-j" #'next-line-or-history-element
;;   "C-k" #'previous-line-or-history-element)
;;
;; ;; (helix-keymap-set minibuffer-local-map 'insert
;; ;;   "C-j" #'next-line-or-history-element
;; ;;   "C-k" #'previous-line-or-history-element)

(helix-keymap-set minibuffer-mode-map 'normal
  ;; "ESC" #'abort-minibuffers
  "<escape>" #'abort-recursive-edit)

(helix-keymap-set minibuffer-mode-map nil
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element)

(helix-keymap-set read-expression-map 'normal
  "<down>" #'next-line-or-history-element
  "<up>"   #'previous-line-or-history-element)

(helix-keymap-set read-expression-map nil
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element)

;; C-j is binded in `read--expression-map' to `read--expression-try-read'
;; which is also binded to RET. Delete it, to make the binding from the parent
;; `read-expression-map' keymap available.
(keymap-unset read--expression-map "C-j" :remove)

;;;; Vertico

(with-eval-after-load 'vertico
  (helix-keymap-set vertico-map 'normal
    "y"   #'vertico-save ;; Copy current candidate to kill ring
    "j"   #'vertico-next
    "k"   #'vertico-previous
    "g g" #'vertico-first
    "G"   #'vertico-last
    "C-f" #'vertico-scroll-up
    "C-b" #'vertico-scroll-down
    "C-d" #'vertico-scroll-up
    "C-u" #'vertico-scroll-down
    ;; Rebind forward/backward paragraphs keys
    "] p" #'vertico-next-group
    "[ p" #'vertico-previous-group
    "}"   #'vertico-next-group
    "{"   #'vertico-previous-group))

;;; Xref

(with-eval-after-load 'xref
  (dolist (cmd '(xref-find-definitions
                 xref-find-references
                 xref-go-back
                 xref-go-forward
                 xref-goto-xref))
    (helix-advice-add cmd :around #'helix-jump-command))

  (helix-keymap-set xref--xref-buffer-mode-map nil
    "o"   #'xref-show-location-at-point
    "Q"   #'xref-quit-and-pop-marker-stack

    "C-j" #'xref-next-line
    "C-k" #'xref-prev-line

    "] p" #'xref-next-group
    "[ p" #'xref-prev-group
    "}"   #'xref-next-group
    "{"   #'xref-prev-group
    "z j" #'xref-next-group
    "z k" #'xref-prev-group))

;;; Occur mode

(with-eval-after-load 'replace
  (helix-keymap-set occur-mode-map nil
    "i"   #'occur-edit-mode
    "o"   #'occur-mode-display-occurrence           ; default `C-o'
    "g o" #'occur-mode-goto-occurrence-other-window ; default `o'
    "C-j" #'next-error-no-select
    "C-k" #'previous-error-no-select
    "n"   #'next-error-no-select
    "N"   #'previous-error-no-select)
  (helix-keymap-set occur-edit-mode-map nil
    "g o" 'occur-mode-goto-occurrence-other-window)
  (helix-advice-add 'occur-mode-goto-occurrence :around #'helix-jump-command))

;;; Wdired

(with-eval-after-load 'wdired
  (helix-set-initial-state 'wdired-mode 'normal)
  (helix-advice-add 'wdired-change-to-wdired-mode :after #'helix-switch-to-initial-state)
  (helix-advice-add 'wdired-change-to-dired-mode :after #'helix-switch-to-initial-state)

  (helix-keymap-set wdired-mode-map 'normal
    "j"        #'wdired-next-line
    "k"        #'wdired-previous-line
    "<up>"     #'wdired-next-line
    "<down>"   #'wdired-previous-line

    "Z Z"      #'wdired-finish-edit
    "Z Q"      #'wdired-abort-changes
    "<escape>" #'wdired-exit

    "o"        #'undefined
    "O"        #'undefined
    "J"        #'undefined
    "<remap> <save-buffer>" #'wdired-finish-edit)

  (dolist (cmd '(wdired-next-line
                 wdired-previous-line))
    (helix-advice-add cmd :before #'helix-deactivate-mark)
    (put cmd 'multiple-cursors t))

  (helix-advice-add 'wdired-change-to-dired-mode :before #'helix-deactivate-mark)
  (helix-advice-add 'wdired-change-to-dired-mode :before #'helix-delete-all-fake-cursors)

  (dolist (cmd '(wdired-finish-edit
                 wdired-abort-changes
                 wdired-exit))
    (put cmd 'multiple-cursors 'false)))

;;; Corfu

(with-eval-after-load 'corfu
  ;; Close corfu popup on Insert state exit.
  (add-hook 'helix-insert-state-exit-hook #'corfu-quit)

  (helix-keymap-set corfu-map nil
    "C-SPC" #'corfu-insert-separator

    "C-k" #'corfu-previous
    "C-j" #'corfu-next

    "C-h" #'corfu-info-documentation
    "C-l" #'corfu-info-location

    "C-f" #'corfu-scroll-up
    "C-b" #'corfu-scroll-down
    "C-u" #'corfu-scroll-down
    "C-d" #'corfu-scroll-up))

;;; Consult

(with-eval-after-load 'consult
  (helix-cache-input consult--read)

  ;; Execute for all cursors.
  (put 'consult-yank-pop 'multiple-cursors t)

  (dolist (cmd '(consult-line
                 consult-mark
                 consult-global-mark
                 consult-imenu
                 consult-outline
                 consult-grep
                 consult-git-grep
                 consult-ripgrep))
    (helix-advice-add cmd :before #'helix-deactivate-mark)))

;;; Outline

(dolist (cmd '(outline-up-heading
                   outline-next-visible-heading
                   outline-previous-visible-heading
                   outline-forward-same-level
                   outline-backward-same-level))
  (put cmd 'merge-selections 'extend-selection)
  (helix-advice-add cmd :before #'helix-deactivate-mark)
  (helix-advice-add cmd :after #'helix-reveal-point-when-on-top))

;;; Custom

(with-eval-after-load 'cus-edit
  (helix-set-initial-state 'Custom-mode 'normal)
  (helix-keymap-set custom-mode-map 'normal
    "] ]" #'widget-forward
    "[ [" #'widget-backward
    "z j" #'widget-forward
    "z k" #'widget-backward
    "z u" #'Custom-goto-parent
    "q"   #'Custom-buffer-done))

;;; Shortdoc

(with-eval-after-load 'shortdoc
  (keymap-set shortdoc-mode-map "y" #'shortdoc-copy-function-as-kill))

;;; Keypad

(with-eval-after-load 'keypad
  (helix-define-advice keypad (:after ())
    "Execute selected command for all cursors."
    (setq helix-this-command this-command))

  (put 'keypad 'multiple-cursors 'false)
  (put 'keypad-describe-key 'multiple-cursors 'false)

  (dolist (state '(normal motion))
    (helix-keymap-global-set state
      "SPC"      #'keypad
      "C-h k"    #'keypad-describe-key
      "<f1> k"   #'keypad-describe-key
      "<help> k" #'keypad-describe-key)))

;;; Major modes
;;;; emacs-lisp-mode (elisp)

;; Fontification for Helix macros.
(font-lock-add-keywords
 'emacs-lisp-mode
 (eval-when-compile
   `((,(concat "^\\s-*("
               (regexp-opt '("helix-define-command") t)
               "\\s-+\\(" (rx lisp-mode-symbol) "\\)")
      (1 'font-lock-keyword-face)
      (2 'font-lock-function-name-face nil t))
     (,(concat "^\\s-*("
               (regexp-opt '("helix-defvar-local") t)
               "\\s-+\\(" (rx lisp-mode-symbol) "\\)")
      (1 'font-lock-keyword-face)
      (2 'font-lock-variable-name-face nil t)))))

;; `emacs-lisp-mode' is inherited from `lisp-data-mode'.
(add-hook 'lisp-data-mode-hook  #'helix-configure-for-emacs-lisp)

(defun helix-configure-for-emacs-lisp ()
  ;; Add legacy quotes marks to Helix surround functionality.
  (helix-surround-add-pair ?` (cons "`" "'"))
  (helix-surround-add-pair ?' (cons "`" "'"))

  ;; Teach `imenu' about Helix macros.
  (dolist (i (eval-when-compile
              `(("Variables"
                 ,(concat "^\\s-*("
                          (regexp-opt '("helix-defvar-local") t)
                          "\\s-+\\(" (rx lisp-mode-symbol) "\\)")
                 2)
                (nil ;; top level
                 ,(concat "^\\s-*("
                          (regexp-opt '("helix-define-command") t)
                          "\\s-+'?\\(" (rx lisp-mode-symbol) "\\)")
                 2))))
    (cl-pushnew i imenu-generic-expression :test #'equal)))

(dolist (keymap (list emacs-lisp-mode-map
                      lisp-data-mode-map))
  (helix-keymap-set keymap 'normal
    "m `"   #'helix-mark-inner-legacy-quoted
    "m '"   #'helix-mark-inner-legacy-quoted
    "m i `" #'helix-mark-inner-legacy-quoted
    "m i '" #'helix-mark-inner-legacy-quoted
    "m a `" #'helix-mark-a-legacy-quoted
    "m a '" #'helix-mark-a-legacy-quoted))

(helix-define-command helix-mark-inner-legacy-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((_ beg end _) (helix-surround-4-bounds-at-point "`" "'"))
    (helix-set-region beg end)))

(helix-define-command helix-mark-a-legacy-quoted ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (-when-let ((beg _ _ end) (helix-surround-4-bounds-at-point "`" "'"))
    (helix-set-region beg end)))

;;;; org-mode

(declare-function org-in-regexp "org")
(defvar org-emph-re)
(defvar org-verbatim-re)
(defvar org-mode-map)

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'helix-surround-org-mode)
  (helix-keymap-set org-mode-map 'normal
    "g h"   #'helix-org-first-non-blank

    "d"     #'helix-org-cut
    "="     #'org-indent-region

    "[ p"   #'org-backward-paragraph
    "] p"   #'org-forward-paragraph
    "{"     #'org-backward-paragraph
    "}"     #'org-forward-paragraph

    "[ s"   #'org-backward-sentence
    "] s"   #'org-forward-sentence
    "[ ."   #'org-backward-sentence
    "] ."   #'org-forward-sentence

    "m ."   #'helix-org-mark-inner-sentence
    "m i s" #'helix-org-mark-inner-sentence
    "m a s" #'helix-org-mark-a-sentence

    "m /"   #'helix-mark-inner-org-emphasis
    "m i /" #'helix-mark-inner-org-emphasis
    "m a /" #'helix-mark-an-org-emphasis

    "m *"   #'helix-mark-inner-org-emphasis
    "m i *" #'helix-mark-inner-org-emphasis
    "m a *" #'helix-mark-an-org-emphasis

    "m _"   #'helix-mark-inner-org-emphasis
    "m i _" #'helix-mark-inner-org-emphasis
    "m a _" #'helix-mark-an-org-emphasis

    "m +"   #'helix-mark-inner-org-emphasis
    "m i +" #'helix-mark-inner-org-emphasis
    "m a +" #'helix-mark-an-org-emphasis

    "m ="   #'helix-mark-inner-org-verbatim
    "m i =" #'helix-mark-inner-org-verbatim
    "m a =" #'helix-mark-an-org-verbatim

    "m ~"   #'helix-mark-inner-org-verbatim
    "m i ~" #'helix-mark-inner-org-verbatim
    "m a ~" #'helix-mark-an-org-verbatim))

;;;;; Commands

(dolist (cmd '(org-cycle      ;; TAB
               org-shifttab)) ;; S-TAB
  (helix-advice-add cmd :before #'helix-deactivate-mark))

;; gh
(helix-define-command helix-org-first-non-blank ()
  "Move point to beginning of current visible line skipping indentation.

If this is a headline, and `org-special-ctrl-a/e' is not nil or
symbol `reversed', on the first attempt move to where the
headline text starts, and only move to beginning of line when the
cursor is already before the start of the text of the headline.

If `org-special-ctrl-a/e' is symbol `reversed' then go to the
start of the text on the second attempt."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (setq this-command 'org-beginning-of-line)
  (helix-set-region (if (or (eq last-command this-command)
                            helix--extend-selection)
                        (mark)
                      (point))
                    (progn (org-beginning-of-line)
                           (skip-syntax-forward " " (line-end-position))
                           (backward-prefix-chars)
                           (point))))

;; org-end-of-line

;; ]p or }
(helix-define-command helix-org-mark-paragraph-forward (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  ;; org-forward-paragraph
  (helix-mark-thing-forward 'helix-paragraph count))

;; d
(helix-define-command helix-org-cut (count)
  "Kill (cut) text in region. I.e. delete text and put it in the `kill-ring'.
If no selection — delete COUNT chars before point."
  :multiple-cursors t
  (interactive "p")
  (when (helix-logical-lines-p)
    (helix-restore-newline-at-eol))
  (cond ((use-region-p)
         (kill-region nil nil t))
        (t
         (org-delete-char (- count))))
  (helix-extend-selection -1))

;; mis
(helix-define-command helix-org-mark-inner-sentence (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-mark-inner-thing 'helix-org-sentence count t))

;; mas
(helix-define-command helix-org-mark-a-sentence ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (helix-mark-a-sentence 'helix-org-sentence))

;;;;; Things

;; `helix-org-sentence' thing
(put 'helix-org-sentence 'forward-op (lambda (count)
                                       (helix-motion-loop (dir count)
                                         (ignore-errors
                                           (if (natnump dir)
                                               (org-forward-sentence)
                                             (org-backward-sentence))))))

;;;;; Surround

(helix-define-command helix-mark-inner-org-emphasis ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when (org-in-regexp org-emph-re 2)
    (helix-set-region (match-beginning 4) (match-end 4)))
  ;; (when-let* ((bounds (bounds-of-thing-at-point 'defun))
  ;;             (nlines (count-lines (car bounds) (point)))
  ;;             ((org-in-regexp org-emph-re nlines)))
  ;;   (helix-set-region (match-beginning 4) (match-end 4)))
  )

(helix-define-command helix-mark-an-org-emphasis ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when (org-in-regexp org-emph-re 2)
    (helix-set-region (match-beginning 2) (match-end 2))))

(helix-define-command helix-mark-inner-org-verbatim ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when (org-in-regexp org-verbatim-re 2)
    (helix-set-region (match-beginning 4) (match-end 4))))

(helix-define-command helix-mark-an-org-verbatim ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when (org-in-regexp org-verbatim-re 2)
    (helix-set-region (match-beginning 2) (match-end 2))))

(defun helix-surround-org-mode ()
  "Configure Helix surround functionality for Org-mode."
  (dolist (char '(?/ ?* ?_ ?+ ?= ?~))
    (helix-surround-add-pair char (cons (char-to-string char)
                                        (char-to-string char))
      :search #'helix-surround--4-bounds-of-org-verbatim)))

(defun helix-surround--4-bounds-of-org-verbatim ()
  (when (org-in-regexp org-verbatim-re 2)
    (list (match-beginning 2)
          (match-beginning 4)
          (match-end 2)
          (match-end 4))))

(defun helix-surround--4-bounds-of-org-emphasis ()
  (when (org-in-regexp org-emph-re 2)
    (list (match-beginning 2)
          (match-beginning 4)
          (match-end 2)
          (match-end 4))))

(provide 'helix-integration)
;;; helix-integration.el ends here
