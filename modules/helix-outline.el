;;; helix-outline.el -*- lexical-binding: t; -*-
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
;; Integration Helix with Outline.
;;
;;; Code:
(require 'helix-common)
(require 'helix-core)
(require 'outline)

;;; Keybindings

(dolist (keymap (list outline-mode-map outline-minor-mode-map))
  (helix-keymap-set keymap :state 'normal
    "m h"   #'helix-outline-mark-subtree ; `h' is for heading
    "m i h" #'helix-outline-mark-subtree)
  (dolist (state '(normal motion))
    (helix-keymap-set keymap :state state
      "z <tab>"     #'outline-cycle
      "z <backtab>" #'outline-cycle-buffer
      "z <return>"  #'outline-insert-heading
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
      "z p"   #'helix-outline-hide-other ; `p' for path
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
        "M-k" #'outline-move-subtree-up))

;;; Advices

(helix-advice-add 'outline-minor-mode :after #'helix-update-active-keymaps-a)
(helix-advice-add 'outline-insert-heading :after #'helix-switch-to-insert-state-a)

(dolist (cmd '(outline-up-heading
               outline-next-visible-heading
               outline-previous-visible-heading
               outline-forward-same-level
               outline-backward-same-level))
  (helix-advice-add cmd :before #'helix-maybe-deactivate-mark-a))

(dolist (cmd '(outline-promote
               outline-demote))
 (helix-advice-add cmd :around #'helix-keep-selection-a))

;;; Commands

(defun helix-outline-mark-subtree ()
  "Mark the current subtree in an outlined document."
  (interactive)
  (helix-push-point)
  (if (outline-on-heading-p)
      ;; we are already looking at a heading
      (forward-line 0)
    ;; else go back to previous heading
    (outline-previous-visible-heading 1))
  (helix-set-region (point)
                    (progn (outline-end-of-subtree)
                           (unless (eobp) (forward-char))
                           (point))
                    -1 :adjust)
  (helix-reveal-point-when-on-top))

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

(provide 'helix-outline)
;;; helix-outline.el ends here
