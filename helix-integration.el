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
  (unless helix--executing-command-for-fake-cursor
    (apply orig-fun args)))

(helix-cache-input read-char)
(helix-cache-input read-quoted-char)
(helix-cache-input read-from-kill-ring)
(helix-cache-input read-char-from-minibuffer)
(helix-cache-input register-read-with-preview)  ; used by read-string

;; Commands that don't work with multiple-cursors
(helix-unsupported-command isearch-forward)
(helix-unsupported-command isearch-backward)

;;; Keypad

(helix-define-advice keypad (:after ())
  "Execute selected command for all cursors."
  (setq helix-this-command this-command))

(with-eval-after-load 'keypad
  (helix-keymap-set nil 'normal
    "SPC"      #'keypad
    "C-h k"    #'keypad-describe-key
    "<f1> k"   #'keypad-describe-key
    "<help> k" #'keypad-describe-key)
  (helix-keymap-set nil 'motion
    "SPC"      #'keypad
    "C-h k"    #'keypad-describe-key
    "<f1> k"   #'keypad-describe-key
    "<help> k" #'keypad-describe-key))

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

;;; Edebug

(with-eval-after-load 'edebug
  (keymap-unset edebug-mode-map "SPC")
  (keymap-set edebug-mode-map "s" #'edebug-step-mode)
  (keymap-unset edebug-mode-map "h")
  (keymap-set edebug-mode-map "H" #'edebug-goto-here))

;;; Consult

(with-eval-after-load 'consult
  (helix-cache-input consult--read))

;;; Lisp

(add-hook 'lisp-mode-hook
          #'(lambda ()
              (helix-surround-add-pair ?` '("`" . "'"))
              (helix-surround-add-pair ?' '("`" . "'"))))

;;; Help

(helix-set-initial-state 'help-mode 'normal)
(with-eval-after-load 'help-mode
  (helix-keymap-set help-mode-map 'normal
    ;; "RET" (keymap-lookup help-mode-map "RET")
    "q" (keymap-lookup help-mode-map "q")))

(helix-set-initial-state 'helpful-mode 'normal)
(with-eval-after-load 'helpful
  (helix-keymap-set helpful-mode-map 'normal
    "q" #'quit-window))

;;; Org mode

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

(add-hook 'org-mode-hook
          #'(lambda ()
              (helix-surround-add-pair ?/ '("/" . "/") #'helix-surround--4-bounds-of-org-emphasis)
              (helix-surround-add-pair ?* '("*" . "*") #'helix-surround--4-bounds-of-org-emphasis)
              (helix-surround-add-pair ?_ '("_" . "_") #'helix-surround--4-bounds-of-org-emphasis)
              (helix-surround-add-pair ?+ '("+" . "+") #'helix-surround--4-bounds-of-org-emphasis)
              (helix-surround-add-pair ?= '("=" . "=") #'helix-surround--4-bounds-of-org-verbatim)
              (helix-surround-add-pair ?~ '("~" . "~") #'helix-surround--4-bounds-of-org-verbatim)))

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

;;; Corfu

(with-eval-after-load 'corfu
  (add-hook 'helix-insert-state-exit-hook #'corfu-quit))

(provide 'helix-integration)
;;; helix-integration.el ends here
