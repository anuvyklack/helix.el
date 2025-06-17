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
(require 'helix-common)
(require 'helix-multiple-cursors-core)
(require 'helix-commands)

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
  (eldoc-add-command 'helix-backward-char       ;; h
                     'helix-forward-char        ;; l
                     'helix-next-line           ;; j
                     'helix-previous-line       ;; k
                     'helix-forward-word-start  ;; w
                     'helix-forward-WORD-start  ;; W
                     'helix-backward-word-start ;; b
                     'helix-backward-WORD-start ;; B
                     'helix-forward-word-end    ;; e
                     'helix-forward-WORD-end    ;; E
                     'helix-search-forward      ;; /
                     'helix-search-backward     ;; ?
                     'helix-search-next         ;; n
                     'helix-search-previous))   ;; N

;;; Consult

(helix-cache-input consult--read)

;;; Lisp

(add-hook 'lisp-mode-hook
          #'(lambda ()
              (helix-surround-add-pair ?` '("`" . "'"))
              (helix-surround-add-pair ?' '("`" . "'"))))

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

(provide 'helix-integration)
;;; helix-integration.el ends here
