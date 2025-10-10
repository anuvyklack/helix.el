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
(require 'helix-leader)

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

(helix-advice-add 'exchange-point-and-mark :after #'helix-reveal-point-when-on-top)

(dolist (command '(fill-region    ;; gq
                   indent-region  ;; =
                   comment-dwim)) ;; gc
  (helix-advice-add command :around #'helix-keep-selection-a))

(helix-advice-add 'clone-indirect-buffer :before #'helix-deactivate-mark-a)

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

(with-eval-after-load 'helpful
  (helix-set-initial-state 'helpful-mode 'normal)
  (helix-inhibit-insert-state helpful-mode-map)
  (put 'helpful-at-point 'multiple-cursors 'false)

  ;; Open links to functions, variables and symbols in helpful buffer
  ;; in the same window.
  (add-to-list 'display-buffer-alist
               '((derived-mode . helpful-mode)    ; condition
                 display-buffer-reuse-mode-window ; action
                 (mode . helpful-mode))))         ; args

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

;;; Vertico

(with-eval-after-load 'vertico
  (helix-keymap-set vertico-map :state 'normal
    "y"   #'vertico-save ;; Copy current candidate to kill ring
    "j"   #'vertico-next
    "k"   #'vertico-previous
    "g g" #'vertico-first
    "G"   #'vertico-last)
  (helix-keymap-set vertico-map
    "M-j" #'next-history-element
    "M-k" #'previous-history-element
    ;; Rebind forward/backward paragraphs keys
    "<remap> <helix-mark-paragraph-forward>"  #'vertico-next-group
    "<remap> <helix-mark-paragraph-backward>" #'vertico-previous-group
    ;; Rebind scrolling keys
    "<remap> <helix-smooth-scroll-down>"      #'vertico-scroll-up
    "<remap> <helix-smooth-scroll-up>"        #'vertico-scroll-down
    "<remap> <helix-smooth-scroll-page-down>" #'vertico-scroll-up
    "<remap> <helix-smooth-scroll-page-up>"   #'vertico-scroll-down))

;;; Embark

(with-eval-after-load 'embark
  (helix-keymap-set embark-collect-mode-map
    "m" #'helix-embark-select
    "u" #'helix-embark-select
    "y" #'embark-copy-as-kill))

(with-eval-after-load 'embark-consult
  (helix-keymap-set embark-consult-rerun-map :state 'motion
    "g r" #'embark-rerun-collect-or-export))

(defun helix-embark-select ()
  "Add or remove the target from the current buffer's selection.
You can act on all selected targets at once with `embark-act-all'.
When called from outside `embark-act' this command will select
the first target at point."
  (interactive)
  (embark-select)
  (next-line))

;;; Compilation

(dolist (cmd '(next-error
               previous-error))
  (helix-advice-add cmd :around #'helix-jump-command-a))

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

;;; Deadgrep

(with-eval-after-load 'deadgrep
  (add-hook 'deadgrep-mode-hook
            (defun helix--deadgrep-mode-hook ()
              ;; TODO: upstream this
              (setq-local revert-buffer-function
                          (lambda (_ignore-auto _noconfirm)
                            (deadgrep-restart)))))
  (helix-keymap-set deadgrep-mode-map :state 'motion
    "RET" #'deadgrep-visit-result-other-window
    "o"   #'helix-deadgrep-show-result-other-window
    "C-o" #'helix-deadgrep-show-result-other-window
    "M-n" #'helix-deadgrep-forward-match-show-other-window
    "M-p" #'helix-deadgrep-backward-match-show-other-window
    "n"   #'deadgrep-forward-match
    "N"   #'deadgrep-backward-match
    "C-j" #'helix-deadgrep-forward-match-show-other-window
    "C-k" #'helix-deadgrep-backward-match-show-other-window
    "z j" #'deadgrep-forward-filename
    "z k" #'deadgrep-backward-filename
    "z u" #'deadgrep-parent-directory
    "a"   #'deadgrep-incremental ;; `a' for amend
    "i"   #'deadgrep-edit-mode
    "g r" #'deadgrep-restart)
  (helix-keymap-set deadgrep-edit-mode-map :state 'normal
    "<escape>" #'deadgrep-mode
    "Z Z" #'deadgrep-mode
    "o"   #'undefined
    "O"   #'undefined
    "J"   #'undefined)

  (helix-advice-add 'deadgrep-mode :before #'helix-deactivate-mark-a)
  (helix-advice-add 'deadgrep-mode :before #'helix-delete-all-fake-cursors)

  (dolist (cmd '(deadgrep-visit-result
                 deadgrep-visit-result-other-window))
    (helix-advice-add cmd :around #'helix-jump-command-a)))

(defun helix-deadgrep-show-result-other-window ()
  "Show search result at point in another window."
  (interactive)
  (unless next-error-follow-minor-mode
    (helix-recenter-point-on-jump
      (save-selected-window
        (deadgrep-visit-result-other-window)
        (deactivate-mark)))))

(defun helix-deadgrep-forward-match-show-other-window ()
  "Move point to next search result and show it in another window."
  (interactive)
  (deadgrep-forward-match)
  (helix-deadgrep-show-result-other-window))

(defun helix-deadgrep-backward-match-show-other-window ()
  "Move point to previous search result and show it in another window."
  (interactive)
  (deadgrep-backward-match)
  (helix-deadgrep-show-result-other-window))

;;; grep-mode

(with-eval-after-load 'grep
  (helix-keymap-set grep-mode-map :state 'motion
    "i"   #'wgrep-change-to-wgrep-mode
    "o"   #'compilation-display-error
    "g r" #'recompile
    "g f" #'next-error-follow-minor-mode
    "g g" #'beginning-of-buffer
    "G"   #'end-of-buffer
    ;; "C-j" #'next-error-no-select
    ;; "C-k" #'previous-error-no-select
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

;;; Corfu

(with-eval-after-load 'corfu
  ;; Close corfu popup on Insert state exit.
  (add-hook 'helix-insert-state-exit-hook #'corfu-quit)

  (helix-keymap-set corfu-map
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
    (helix-advice-add cmd :before #'helix-deactivate-mark-a)))

;;; Outline

(with-eval-after-load 'outline
  (helix-advice-add 'outline-minor-mode :after #'helix-update-active-keymaps-a)
  (dolist (cmd '(outline-up-heading
                 outline-next-visible-heading
                 outline-previous-visible-heading
                 outline-forward-same-level
                 outline-backward-same-level))
    (helix-advice-add cmd :before #'helix-deactivate-mark-a))

  (dolist (keymap (list outline-mode-map outline-minor-mode-map))
    (dolist (state '(normal motion))
      (helix-keymap-set keymap :state state
        "z <tab>"     #'outline-cycle
        "z <backtab>" #'outline-cycle-buffer
        "z <return>"  #'outline-insert-heading
        "m h"   #'outline-mark-subtree  ; `h' is for heading
        "m i h" #'outline-mark-subtree
        "z j"   #'outline-next-visible-heading
        "z k"   #'outline-previous-visible-heading
        "z C-j" #'outline-forward-same-level
        "z C-k" #'outline-backward-same-level
        "z u"   #'outline-up-heading
        "z o"   #'helix-outline-open
        "z c"   #'outline-hide-subtree
        "z r"   #'outline-show-all
        "z m"   #'outline-hide-sublevels
        "z 2"   #'helix-show-2-sublevels
        "z p"   #'helix-outline-hide-other
        "z O"   #'outline-show-branches
        "z <"   #'outline-promote
        "z >"   #'outline-demote
        "z M-h" #'outline-promote
        "z M-l" #'outline-demote
        "z M-j" #'outline-move-subtree-down
        "z M-k" #'outline-move-subtree-up)))

  (setq outline-navigation-repeat-map
        (define-keymap
          "u"   #'outline-up-heading
          "j"   #'outline-next-visible-heading
          "k"   #'outline-previous-visible-heading
          "C-j" #'outline-forward-same-level
          "C-k" #'outline-backward-same-level))

  (setq outline-editing-repeat-map
        (define-keymap
          "<"   #'outline-promote
          ">"   #'outline-demote
          "M-h" #'outline-promote
          "M-l" #'outline-demote
          "M-j" #'outline-move-subtree-down
          "M-k" #'outline-move-subtree-up)))

(defun helix-outline-open ()
  (interactive)
  (outline-show-entry)
  (outline-show-children))

(defun helix-outline-hide-other ()
  (interactive)
  (outline-hide-other)
  (outline-show-branches))

(defun helix-show-2-sublevels ()
  "Remain 2 top levels of headings visible."
  (interactive)
  (outline-hide-sublevels 2))

;;; Custom

(with-eval-after-load 'cus-edit
  (helix-set-initial-state 'Custom-mode 'normal)
  (helix-keymap-set custom-mode-map :state 'normal
    "] ]" #'widget-forward
    "[ [" #'widget-backward
    "z j" #'widget-forward
    "z k" #'widget-backward
    "z u" #'Custom-goto-parent
    "q"   #'Custom-buffer-done))

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

;;;; org-mode

(declare-function org-in-regexp "org")
(defvar org-emph-re)
(defvar org-verbatim-re)
(defvar org-mode-map)

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'helix-surround-settings-for-org-mode)

  (helix-advice-add 'org-mark-subtree :after #'helix-reveal-point-when-on-top)

  (dolist (cmd '(helix-org-up-heading
                 org-next-visible-heading
                 org-previous-visible-heading))
    (helix-advice-add cmd :after #'helix-maybe-deactivate-mark-a)))

(with-eval-after-load 'org
  (helix-keymap-set org-mode-map :state 'normal
    "z u"   #'helix-org-up-heading

    "g h"   #'helix-org-first-non-blank

    "d"     #'helix-org-cut
    "p"     #'helix-org-paste-after
    "P"     #'helix-org-paste-before
    "="     #'org-indent-region
    "<"     #'helix-org-<
    ">"     #'helix-org->

    ;; "C-o"   #'org-mark-ring-goto
    ;;         #'org-mark-ring-push

    "[ p"   #'helix-org-mark-paragraph-backward
    "] p"   #'helix-org-mark-paragraph-forward
    "{"     #'helix-org-mark-paragraph-backward
    "}"     #'helix-org-mark-paragraph-forward

    "[ s"   #'org-backward-sentence
    "] s"   #'org-forward-sentence
    "[ ."   #'org-backward-sentence
    "] ."   #'org-forward-sentence

    "M-o"   #'helix-org-up-element
    "M-i"   #'helix-org-down-element
    "M-n"   #'helix-org-next-element
    "M-p"   #'helix-org-previous-element

    "m ."   #'helix-org-mark-inner-sentence
    "m i s" #'helix-org-mark-inner-sentence
    "m a s" #'helix-org-mark-a-sentence

    "m p"   #'helix-org-mark-inner-paragraph
    "m i p" #'helix-org-mark-inner-paragraph
    "m a p" #'helix-org-mark-a-paragraph

    "m h"   #'org-mark-subtree
    "m i h" #'org-mark-subtree

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

(with-eval-after-load 'org-capture
  (helix-keymap-set org-capture-mode-map :state 'normal
    "Z R" 'org-capture-refile
    "Z Z" 'org-capture-finalize
    "Z Q" 'org-capture-kill))

;;;;; Commands

;; (dolist (cmd '(;; org-cycle      ;; TAB
;;                org-shifttab)) ;; S-TAB
;;   (helix-advice-add cmd :before #'helix-deactivate-mark-a))

;; (helix-advice-add 'org-cycle :around #'helix-keep-selection-a)

;; (helix-define-advice org-cycle (:aroung (command arg))
;;   (let ((deactivate-mark nil))
;;     (funcall command arg)))

;; zu
(helix-define-command helix-org-up-heading ()
  "Move up in the outline hierarchy to the parent heading."
  :multiple-cursors nil
  (interactive)
  (helix-delete-all-fake-cursors)
  (deactivate-mark)
  (helix-push-point)
  ;; (if (org-at-heading-p)
  ;;     (outline-up-heading 1)
  ;;   (org-previous-visible-heading 1))
  (if (org-at-heading-p)
      (outline-up-heading 1)
    (org-with-limited-levels (org-back-to-heading))))

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
  (let ((paragraph-start    (default-value 'paragraph-start))
        (paragraph-separate (default-value 'paragraph-separate)))
    (helix-mark-thing-forward 'helix-paragraph count)))

;; [p or {
(helix-define-command helix-org-mark-paragraph-backward (count)
  "Select from point to the start of the paragraph (or COUNT-th next paragraphs).
If no paragraph at point select COUNT previous paragraphs."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (let ((paragraph-start    (default-value 'paragraph-start))
        (paragraph-separate (default-value 'paragraph-separate)))
    (helix-mark-thing-forward 'helix-paragraph (- count))))

;; p
(helix-define-command helix-org-paste-after ()
  "Paste after selection."
  :multiple-cursors t
  (interactive)
  (helix-paste #'org-yank 1))

;; P
(helix-define-command helix-org-paste-before ()
  "Paste before selection."
  :multiple-cursors t
  (interactive)
  (helix-paste #'org-yank -1))

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

;; <
(helix-define-command helix-org-< (count)
  "Promote, dedent, move column left.
In items or headings, promote heading/item.
In code blocks, indent lines
In tables, move column to the left."
  (interactive "p")
  (cl-assert (/= count 0))
  (helix-indent #'helix-org-indent-left count))

(defun helix-org-indent-left (beg end)
  (cond
   ;; heading
   ((org-with-limited-levels
     (save-excursion (goto-char beg) (org-at-heading-p)))
    (org-map-region #'org-do-promote beg end))
   ;; table
   ((and (org-at-table-p) (helix-save-region
                            (helix-restore-newline-at-eol)
                            (org-at-table-p)))
    (org-table-move-column -1))
   ;; list
   ((and (save-excursion (goto-char beg) (org-at-item-p))
         (<= end (save-excursion (org-end-of-item-list))))
    (let* ((struct (org-list-struct))
           ;; If nil --- we are at the first item of the list with no
           ;; active selection --- indent full list.
           (no-subtree (or (not struct)
                           (not org-list-automatic-rules)
                           (region-active-p)
                           (/= (point-at-bol)
                               (org-list-get-top-point struct)))))
      (org-list-indent-item-generic -1 no-subtree struct)))
   (t
    (indent-rigidly-left beg end))))

;; >
(helix-define-command helix-org-> (count)
  "Demote, indent, move column right.
In items or headings, demote heading/item.
In code blocks, indent lines.
In tables, move column to the right."
  :multiple-cursors t
  (interactive "p")
  (cl-assert (/= count 0))
  (helix-indent #'helix-org-indent-right count))

(defun helix-org-indent-right (beg end)
  (cond
   ;; heading
   ((org-with-limited-levels
     (save-excursion (goto-char beg) (org-at-heading-p)))
    (org-map-region #'org-do-demote beg end))
   ;; table
   ((and (org-at-table-p) (helix-save-region
                            (helix-restore-newline-at-eol)
                            (org-at-table-p)))
    (org-table-move-column))
   ;; list
   ((and (save-excursion (goto-char beg) (org-at-item-p))
         (<= end (save-excursion (org-end-of-item-list))))
    (let* (;; (struct (save-excursion
           ;;           (goto-char beg)
           ;;           (org-list-struct)))
           (struct (org-list-struct))
           ;; If nil --- we are at the first item of the list with no
           ;; active selection --- indent full list.
           (no-subtree (or (not struct)
                           (not org-list-automatic-rules)
                           (region-active-p)
                           (/= (point-at-bol)
                               (org-list-get-top-point struct)))))
      (org-list-indent-item-generic 1 no-subtree struct)))
   (t
    (indent-rigidly-right beg end))))


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

;; mip
(helix-define-command helix-org-mark-inner-paragraph (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-push-point)
  (let ((paragraph-start    (default-value 'paragraph-start))
        (paragraph-separate (default-value 'paragraph-separate)))
    (helix-mark-inner-thing 'helix-paragraph count t))
  (helix-reveal-point-when-on-top))

;; map
(helix-define-command helix-org-mark-a-paragraph (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-push-point)
  (let ((paragraph-start    (default-value 'paragraph-start))
        (paragraph-separate (default-value 'paragraph-separate)))
    (helix-mark-a-thing 'helix-paragraph count t))
  (helix-reveal-point-when-on-top))

;;;;; AST climbing

(defun helix-org-element-in-section (&optional granularity)
  "Return the smallest element that completely encloses the active region.
With no active region point position is used.

The search is constrained within the encloses `section' element.
If the active region extends beyond the `section' boundaries, return nil.
Result element has fully parsed structure with AST virtual root at the
parent `section' element.

GRANULARITY specifies the parsing level (see `org-element-parse-buffer')."
  (-let [(beg . end) (if (use-region-p)
                         (car (region-bounds))
                       (cons (point) (point)))]
    (let* ((element (save-excursion
                      (goto-char beg)
                      (org-element-at-point)))
           ;; Climb up the AST until `section' node.
           (section (org-element-lineage element 'section)))
      ;; If region exceed section — truncate it to `section' boundaries.
      (setq beg (max beg (org-element-begin section))
            end (min end (org-element-end section)))
      ;; Find smallest enclosing element within `section' element AST.
      (cl-loop with element = (helix-org--parse-element section granularity)
               with nested-element
               do (setq nested-element
                        (-find (lambda (el)
                                 (<= (org-element-begin el)
                                     beg end
                                     (org-element-end el)))
                               (org-element-contents element)))
               while nested-element
               do (setq element nested-element)
               finally return element))))

(cl-defun helix-org-parse-element (element
                                   &optional
                                   (root (unless (org-element-type-p element 'section)
                                           (org-element-lineage element 'section)))
                                   (granularity 'element))
  "Return the fully parsed structure of the ELEMENT.

ROOT will be the virtual-root of the result AST and should be one of
parents of the element. By default it will be the parent `section' element.

GRANULARITY specifies the parsing level (see `org-element-parse-buffer')."
  (cond ((and (not root)
              (org-element-type-p element 'section))
         (helix-org--parse-element element granularity))
        (root
         (let ((beg (org-element-begin element))
               (end (org-element-end element))
               (type (org-element-type element))
               (element (helix-org--parse-element root granularity)))
           (while (and (setq element (-find (lambda (el)
                                             (<= (org-element-begin el)
                                                 beg end
                                                 (org-element-end el)))
                                           (org-element-contents element)))
                       (not (and (eq type (org-element-type element))
                                 (= beg (org-element-begin element))
                                 (= end (org-element-end element))))))
           element))))

(defun helix-org--parse-element (element &optional granularity)
  "Return the fully parsed structure of the ELEMENT.
GRANULARITY specifies the parsing level (see `org-element-parse-buffer')."
  (or granularity (setq granularity 'element))
  (save-excursion
    (with-restriction (org-element-begin element) (org-element-end element)
      (-> (org-element-parse-buffer granularity) ; -> org-data
          org-element-contents                   ; -> AST
          car))))                                ; -> ELEMENT node

(defun helix-org-at-heading-p ()
  (if (use-region-p)
      (save-excursion
        (goto-char (region-beginning))
        (org-at-heading-p))
    (org-at-heading-p)))

(helix-defvar-local helix-org--current-element nil
  "The cache for current element value.")

(cl-defun helix-org--current-element (&optional (new-element nil new-element?))
  "Return cached value when appropriate, or calculate new one."
  (cond (new-element?
         (setq helix-org--current-element
               (unless (org-element-type-p new-element 'headline)
                 new-element)))
        ((and (memq last-command '(helix-org-down-element
                                   helix-org-next-element
                                   helix-org-previous-element
                                   org-cycle                                ; TAB
                                   helix-smooth-scroll-line-not-to-very-top ; zz
                                   helix-smooth-scroll-line-to-center       ; zz
                                   helix-smooth-scroll-line-to-top          ; zt
                                   helix-smooth-scroll-line-to-bottom))     ; zb
              helix-org--current-element)
         helix-org--current-element)
        (t
         (setq helix-org--current-element (helix-org-element-in-section)))))

;; M-o
(helix-define-command helix-org-up-element (&optional arg)
  "Expand region to the parent element.
ARG is used to determine whether invocation was interactive and should not
be set manually."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-restore-newline-at-eol)
  (-let* (((beg . end) (if (use-region-p)
                           (car (region-bounds))
                         (cons (point) (point))))
          (element (save-excursion
                     (goto-char beg)
                     (org-element-at-point))))
    ;; Climb up the tree until element fully contains region.
    (while (and element
                (or (org-element-type-p element 'section) ; skip section
                    (let ((element-beg (org-element-begin element))
                          (element-end (- (org-element-end element)
                                          (org-element-post-blank element))))
                      (< beg element-beg)
                      (< element-end end)
                      (and (= beg element-beg)
                           (= element-end end)))))
      (setq element (org-element-parent element)))
    (if (or (not element)
            (org-element-type-p element 'org-data))
        (user-error "No enclosing element")
      ;; else
      (helix-org--current-element nil)
      (helix-set-region (org-element-begin element)
                        (- (org-element-end element)
                           (org-element-post-blank element))
                        -1 :adjust)
      (if arg (helix-reveal-point-when-on-top)))))

;; M-i
(helix-define-command helix-org-down-element ()
  "Contract region to the first child element."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (if (use-region-p)
      (-if-let (child (if (helix-org-at-heading-p)
                          (save-excursion
                            (goto-char (region-beginning))
                            (when-let* ((pos (-> (org-element-at-point)
                                                 (org-element-contents-begin))))
                              (goto-char pos)
                              (let ((child (org-element-at-point)))
                                (if (org-element-type-p child 'headline)
                                    child
                                  (helix-org-parse-element child)))))
                        ;; else
                        (-> (helix-org--current-element)
                            (org-element-contents)
                            (car-safe))))
          (progn
            (save-excursion
              (goto-char (region-beginning))
              (when (org-invisible-p (line-end-position))
                (org-cycle)))
            (helix-org--current-element child)
            (helix-set-region (org-element-begin child)
                              (- (org-element-end child)
                                 (org-element-post-blank child))
                              -1 :adjust)
            (helix-reveal-point-when-on-top))
        ;; (user-error "No content for this element")
        (user-error "No nested element"))
    ;; else
    (helix-org-up-element)
    (when (helix-org-at-heading-p)
      (helix-org-down-element))))

;; M-n
(helix-define-command helix-org-next-element ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (unless (use-region-p)
    (helix-org-up-element))
  (when-let ((next (helix-org--next-element)))
    (helix-org--current-element next)
    (helix-set-region (org-element-begin next)
                      (- (org-element-end next)
                         (org-element-post-blank next))
                      (helix-region-direction) :adjust)
    (helix-reveal-point-when-on-top)))

(defun helix-org--next-element ()
  (if (helix-org-at-heading-p)
      (save-excursion
        (goto-char (region-beginning))
        (org-forward-heading-same-level 1)
        (org-element-at-point))
    ;; else
    (let* ((current (helix-org--current-element))
           (parent (org-element-parent current)))
      (or
       ;; Try to find the node in PARENT next to ELEMENT.
       (let ((siblings (org-element-contents parent)))
         (nth (1+ (-find-index (lambda (elem) (eq elem current))
                               siblings))
              siblings))
       ;; No following element in current `section'.
       ;; Check `headline' directly after `section'.
       (if-let* (((org-element-type-p parent 'section))
                 (parent (org-element-lineage (org-element-at-point)
                                              '(headline org-data)))
                 (element (save-excursion
                            (goto-char (org-element-end current))
                            (org-element-at-point)))
                 ((eq parent (org-element-parent element))))
           element)))))

;; M-p
(helix-define-command helix-org-previous-element ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (unless (use-region-p)
    (helix-org-up-element))
  (when-let ((previous (helix-org--previous-element)))
    (helix-org--current-element previous)
    (helix-set-region (org-element-begin previous)
                      (- (org-element-end previous)
                         (org-element-post-blank previous))
                      (helix-region-direction) :adjust)
    (helix-reveal-point-when-on-top)))

(defun helix-org--previous-element ()
  (if (helix-org-at-heading-p)
      (save-excursion
        (goto-char (region-beginning))
        (let ((current (org-element-at-point)))
          (goto-char (org-element-begin current))
          (let ((pnt (point)))
            (org-backward-heading-same-level 1)
            (if (/= (point) pnt)
                (org-element-at-point)
              ;; else
              (skip-chars-backward " \r\t\n")
              (unless (bobp)
                (-> (org-element-lineage (org-element-at-point) 'section)
                    (helix-org-parse-element)
                    (org-element-contents)
                    (org-last)))))))
    ;; else
    (let* ((current (helix-org--current-element))
           (siblings (org-element-contents (org-element-parent current))))
      ;; Try to find the node in PARENT previous to ELEMENT.
      (nth (1- (-find-index (lambda (elem) (eq elem current))
                            siblings))
           siblings))))

;;;;; Thing

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

(defun helix-surround-settings-for-org-mode ()
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
