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
(require 'helix-common)
(require 'helix-commands)

;;; Distinguish `TAB' from `C-i' and `RET' from `C-m'

(defun helix-make-C-i-and-C-m-available ()
  "Make Emacs distinguish `TAB' from `C-i' and `RET' from `C-m'."
  (when (display-graphic-p) ;; do translation only in gui
    (keymap-set input-decode-map "C-i" [C-i])
    (keymap-set input-decode-map "C-m" [C-m])))

(helix-make-C-i-and-C-m-available)

;; For daemon mode
(add-hook 'after-make-frame-functions
          (defun helix--after-make-frame-hook (frame)
            (with-selected-frame frame
              (helix-make-C-i-and-C-m-available))))

;; (single-key-description 'C-i)
;; (key-valid-p "<C-i>")
;; (key-valid-p "C-<i>")

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

(dolist (command '(fill-region    ; gq
                   indent-region  ; =
                   comment-dwim)) ; gc
  (helix-advice-add command :around #'helix-keep-selection-a))

(helix-advice-add 'clone-indirect-buffer :before #'helix-deactivate-mark-a)

;;; Eldoc

(with-eval-after-load 'eldoc
  ;; Add motion commands to the `eldoc-message-commands' obarray.
  (eldoc-add-command 'helix-backward-char        ; h
                     'helix-forward-char         ; l
                     'helix-next-line            ; j
                     'helix-previous-line        ; k
                     'helix-forward-word-start   ; w
                     'helix-forward-WORD-start   ; W
                     'helix-backward-word-start  ; b
                     'helix-backward-WORD-start  ; B
                     'helix-forward-word-end     ; e
                     'helix-forward-WORD-end     ; E
                     'helix-first-non-blank      ; gh
                     'helix-end-of-line-command  ; gl
                     'helix-search-forward       ; /
                     'helix-search-backward      ; ?
                     'helix-search-next          ; n
                     'helix-search-previous      ; N
                     'helix-find-char-forward    ; f
                     'helix-find-char-backward   ; F
                     'helix-till-char-forward    ; t
                     'helix-till-char-backward)) ; T

;;; winner-mode & tab-bar-history-mode

(add-hook 'winner-mode-hook
          (defun helix-setup-winner-mode-keys ()
            (if winner-mode
                (helix-keymap-set helix-window-map
                  "u" #'winner-undo
                  "U" #'winner-redo)
              (helix-keymap-set helix-window-map
                "u" nil
                "U" nil))))

(add-hook 'tab-bar-history-mode-hook
          (defun helix-setup-tab-bar-history-mode-keys ()
            (if tab-bar-history-mode
                (helix-keymap-set helix-window-map
                  "u" #'tab-bar-history-back
                  "U" #'tab-bar-history-forward)
              (helix-keymap-set helix-window-map
                "u" nil
                "U" nil))))

;;; Edebug

(with-eval-after-load 'edebug
  (add-hook 'edebug-mode-hook #'helix-update-active-keymaps)
  (helix-keymap-set edebug-mode-map
    "SPC" nil ; unding `edebug-step-mode'
    "h"   nil ; unding `edebug-goto-here'
    "s" #'edebug-step-mode
    "H" #'edebug-goto-here
    "C-c h" #'edebug-goto-here))

;;; Help

(with-eval-after-load 'help-mode
  (helix-set-initial-state 'help-mode 'normal)
  (helix-inhibit-insert-state help-mode-map))

;;; Button

(helix-advice-add 'forward-button :before #'helix-deactivate-mark-a)

;;; repeat-mode

(with-eval-after-load 'repeat
  (setopt repeat-exit-key "<escape>")
  (put 'undo 'repeat-map nil))

;;; Special mode

;; hjkl keys are free in `special-mode-map' by default, so we can use them.
(helix-keymap-set special-mode-map
  "h" #'left-char
  "j" #'next-line
  "k" #'previous-line
  "l" #'right-char)

(helix-keymap-set special-mode-map
  "g"   nil ;; revert-buffer
  "g r" 'revert-buffer
  "g g" 'beginning-of-buffer ; also "<"
  "G"   'end-of-buffer)      ; also ">"

;;; Messages buffer

(helix-set-initial-state 'messages-buffer-mode 'normal)

;;; Minibuffer

;; (helix-keymap-set minibuffer-mode-map :state 'normal
;;   ;; "ESC" #'abort-minibuffers
;;   "<escape>" #'abort-recursive-edit
;;   ;; "<down>"   #'next-line-or-history-element
;;   ;; "<up>"     #'previous-line-or-history-element
;;   "C-j" #'next-line-or-history-element
;;   "C-k" #'previous-line-or-history-element)
;;
;; ;; (helix-keymap-set minibuffer-local-map :state 'insert
;; ;;   "C-j" #'next-line-or-history-element
;; ;;   "C-k" #'previous-line-or-history-element)

(helix-keymap-set minibuffer-mode-map :state 'normal
  ;; "ESC" #'abort-minibuffers
  "<escape>" #'abort-recursive-edit)

(helix-keymap-set minibuffer-mode-map
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element)

(helix-keymap-set read-expression-map :state 'normal
  "<down>" #'next-line-or-history-element
  "<up>"   #'previous-line-or-history-element)

(helix-keymap-set read-expression-map
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element)

;; C-j is binded in `read--expression-map' to `read--expression-try-read'
;; which is also binded to RET. Delete it, to make the binding from the parent
;; `read-expression-map' keymap available.
(keymap-unset read--expression-map "C-j" :remove)

;;; Xref

(with-eval-after-load 'xref
  (dolist (cmd '(xref-find-definitions
                 xref-find-references
                 xref-go-back
                 xref-go-forward
                 xref-goto-xref))
    (helix-advice-add cmd :around #'helix-jump-command-a))

  (helix-keymap-set xref--xref-buffer-mode-map
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

;;; Compilation

(helix-advice-add 'next-error     :around #'helix-jump-command-a)
(helix-advice-add 'previous-error :around #'helix-jump-command-a)

(with-eval-after-load 'compile
  (helix-keymap-set compilation-minor-mode-map
    "g"   nil ; unbind `recompile'
    "g r" #'recompile

    "C-j" #'compilation-next-error
    "C-k" #'compilation-previous-error
    "] ]" #'compilation-next-error
    "[ [" #'compilation-previous-error

    "[ p" #'compilation-previous-file
    "] p" #'compilation-next-file
    "{"   #'compilation-previous-file
    "}"   #'compilation-next-file))

;;; grep-mode

(with-eval-after-load 'grep
  ;; `grep-mode-map' is inherited from `compilation-minor-mode-map'
  (helix-keymap-set grep-mode-map :state 'motion
    "i"   #'wgrep-change-to-wgrep-mode
    "o"   #'compilation-display-error
    "g f" #'next-error-follow-minor-mode
    "g g" #'beginning-of-buffer
    "G"   #'end-of-buffer
    "C-j" #'next-error-no-select
    "C-k" #'previous-error-no-select
    ))

;;; Wgrep

(with-eval-after-load 'wgrep
  (helix-advice-add 'wgrep-change-to-wgrep-mode :after #'helix-switch-to-initial-state)

  (helix-advice-add 'wgrep-to-original-mode :before #'helix-deactivate-mark-a)
  (helix-advice-add 'wgrep-to-original-mode :before #'helix-delete-all-fake-cursors)
  (helix-advice-add 'wgrep-to-original-mode :after  #'helix-switch-to-initial-state)

  (helix-keymap-set wgrep-mode-map :state 'normal
    "<remap> <save-buffer>" #'wgrep-finish-edit
    "<escape>" #'wgrep-exit
    "Z Z"      #'wgrep-finish-edit
    "Z Q"      #'wgrep-abort-changes))

;;; Occur mode

(with-eval-after-load 'replace
  (helix-keymap-set occur-mode-map
    "i"   #'occur-edit-mode
    "o"   #'occur-mode-display-occurrence           ; default `C-o'
    "g o" #'occur-mode-goto-occurrence-other-window ; default `o'
    "C-j" #'next-error-no-select
    "C-k" #'previous-error-no-select
    "n"   #'next-error-no-select
    "N"   #'previous-error-no-select)
  (helix-keymap-set occur-edit-mode-map
    "g o" 'occur-mode-goto-occurrence-other-window)
  (helix-advice-add 'occur-mode-goto-occurrence :around #'helix-jump-command-a))

;;; Wdired

(with-eval-after-load 'wdired
  (helix-set-initial-state 'wdired-mode 'normal)
  (helix-advice-add 'wdired-change-to-wdired-mode :after #'helix-switch-to-initial-state)
  (helix-advice-add 'wdired-change-to-dired-mode  :after #'helix-switch-to-initial-state)

  (helix-keymap-set wdired-mode-map :state 'normal
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
    (helix-advice-add cmd :before #'helix-deactivate-mark-a)
    (put cmd 'multiple-cursors t))

  (helix-advice-add 'wdired-change-to-dired-mode :before #'helix-deactivate-mark-a)
  (helix-advice-add 'wdired-change-to-dired-mode :before #'helix-delete-all-fake-cursors)

  (dolist (cmd '(wdired-finish-edit
                 wdired-abort-changes
                 wdired-exit))
    (put cmd 'multiple-cursors 'false)))

;;; Shortdoc

(with-eval-after-load 'shortdoc
  (keymap-set shortdoc-mode-map "y" #'shortdoc-copy-function-as-kill))

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
  (helix-keymap-set keymap :state 'normal
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

(provide 'helix-integration)
;;; helix-integration.el ends here
