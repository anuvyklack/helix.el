;;; helix-integration.el --- Integration with other packages -*- lexical-binding: t; -*-
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

(helix-define-advice pop-to-mark-command (:around (orig-fun))
  "When region is active, skip active mark and jump to one before it."
  (if (use-region-p)
      (helix-motion-loop (_ 2)
        (funcall orig-fun))
    (funcall orig-fun)))

(dolist (command '(fill-region          ;; gq
                   indent-region        ;; =
                   indent-rigidly-left  ;; >
                   indent-rigidly-right ;; <
                   comment-dwim))       ;; gc
  (eval `(helix-define-advice ,command (:around (orig-fun &rest args))
           "Don't deactivate region."
           (let (deactivate-mark)
             (apply orig-fun args))
           (helix-extend-selection -1))))

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

;;; Keypad
(with-eval-after-load 'keypad
  (helix-define-advice keypad (:after ())
    "Execute selected command for all cursors."
    (setq helix-this-command this-command))

  (put 'keypad 'multiple-cursors 'false)
  (put 'keypad-describe-key 'multiple-cursors 'false)

  (dolist (state '(normal motion))
    (helix-keymap-set nil state
      "SPC"      'keypad
      "C-h k"    'keypad-describe-key
      "<f1> k"   'keypad-describe-key
      "<help> k" 'keypad-describe-key)))

;;; Edebug
(with-eval-after-load 'edebug
  (add-hook 'edebug-mode-hook #'helix-update-active-keymaps)
  (keymap-unset edebug-mode-map "SPC") ; edebug-step-mode
  (keymap-unset edebug-mode-map "h") ; edebug-goto-here
  (helix-keymap-set edebug-mode-map nil
    "s" #'edebug-step-mode
    "H" #'edebug-goto-here
    "C-c h" #'edebug-goto-here))

;;; Help

(helix-set-initial-state 'help-mode 'normal)
(with-eval-after-load 'help-mode
  (helix-keymap-set help-mode-map 'normal
    ;; "RET" (keymap-lookup help-mode-map "RET")
    "q" (keymap-lookup help-mode-map "q")))

(helix-set-initial-state 'helpful-mode 'normal)
(with-eval-after-load 'helpful
  (helix-keymap-set helpful-mode-map 'normal
    "q" #'quit-window)
  (put 'helpful-variable 'multiple-cursors 'false))

;;; Special mode
(helix-keymap-set special-mode-map nil ;; 'motion
  "h" #'left-char
  "j" #'next-line
  "k" #'previous-line
  "l" #'right-char)

;;; Messages buffer
(helix-set-initial-state 'messages-buffer-mode 'normal)

;;; Minibuffer
(helix-keymap-set minibuffer-mode-map 'normal
  ;; "ESC" 'abort-minibuffers
  "<escape>" 'abort-recursive-edit)

;;;; Vertico
(with-eval-after-load 'vertico
  (helix-keymap-set vertico-map 'normal
    "j"   'vertico-next
    "k"   'vertico-previous
    "g g" 'vertico-first
    "G"   'vertico-last
    "C-f" 'vertico-scroll-up
    "C-b" 'vertico-scroll-down
    "C-d" 'vertico-scroll-up
    "C-u" 'vertico-scroll-down
    "g j" 'vertico-next-group
    "g k" 'vertico-previous-group
    "] p" 'vertico-next-group
    "[ p" 'vertico-previous-group
    "}"   'vertico-next-group
    "{"   'vertico-previous-group
    ;; "n"   'vertico-next-group
    ;; "N"   'vertico-previous-group
    "y"   'vertico-save))

;;; Corfu
(with-eval-after-load 'corfu
  ;; Close corfu popup on Insert state exit.
  (add-hook 'helix-insert-state-exit-hook #'corfu-quit))

;;; Consult
(with-eval-after-load 'consult
  (helix-cache-input consult--read)
  (put 'consult-yank-pop 'multiple-cursors t))

;;; Major modes
;;;; Lisp mode

(add-hook 'emacs-lisp-mode-hook #'helix-surround-emacs-lisp)
(defun helix-surround-emacs-lisp ()
  (helix-surround-add-pair ?` (cons "`" "'"))
  (helix-surround-add-pair ?' (cons "`" "'")))

;;;; Org mode

(declare-function org-in-regexp "org")
(defvar org-emph-re)
(defvar org-verbatim-re)
(defvar org-mode-map)

(defun helix-mark-inner-org-emphasis ()
  (interactive)
  (when (org-in-regexp org-emph-re 2)
    (set-mark (match-beginning 4))
    (goto-char (match-end 4)))
  ;; (when-let* ((bounds (bounds-of-thing-at-point 'defun))
  ;;             (nlines (count-lines (car bounds) (point)))
  ;;             ((org-in-regexp org-emph-re nlines)))
  ;;   (set-mark (match-beginning 4))
  ;;   (goto-char (match-end 4)))
  )

(defun helix-mark-an-org-emphasis ()
  (interactive)
  (when (org-in-regexp org-emph-re 2)
    (set-mark (match-beginning 2))
    (goto-char (match-end 2))))

(defun helix-mark-inner-org-verbatim ()
  (interactive)
  (when (org-in-regexp org-verbatim-re 2)
    (set-mark (match-beginning 4))
    (goto-char (match-end 4))))

(defun helix-mark-an-org-verbatim ()
  (interactive)
  (when (org-in-regexp org-verbatim-re 2)
    (set-mark (match-beginning 2))
    (goto-char (match-end 2))))

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

(add-hook 'org-mode-hook #'helix-surround-org-mode)
(defun helix-surround-org-mode ()
  "Configure Helix surround for Org-mode."
  (dolist (char '(?/ ?* ?_ ?+ ?= ?~))
    (helix-surround-add-pair char (cons (char-to-string char)
                                        (char-to-string char))
      :search #'helix-surround--4-bounds-of-org-verbatim)))

(with-eval-after-load 'org
  (helix-keymap-set org-mode-map 'normal
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

(provide 'helix-integration)
;;; helix-integration.el ends here
