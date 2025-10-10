;;; helix-deadgrep.el -*- lexical-binding: t; -*-
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
;; Integration Helix with Deadgrep package.
;;
;;; Code:
(require 'helix-core)
(require 'deadgrep)

;;; Keybindings

(helix-keymap-set deadgrep-mode-map :state 'motion
  "i"   #'deadgrep-edit-mode

  "a"   #'deadgrep-incremental ;; `a' for amend
  "g r" #'deadgrep-restart

  "RET" #'deadgrep-visit-result-other-window

  "o"   #'helix-deadgrep-show-result-other-window
  "C-o" #'helix-deadgrep-show-result-other-window

  "n"   #'deadgrep-forward-match
  "N"   #'deadgrep-backward-match

  "C-j" #'helix-deadgrep-forward-match-show-other-window
  "C-k" #'helix-deadgrep-backward-match-show-other-window

  "}"   #'deadgrep-forward-filename
  "{"   #'deadgrep-backward-filename
  "] p" #'deadgrep-forward-filename
  "[ p" #'deadgrep-backward-filename
  "z j" #'deadgrep-forward-filename
  "z k" #'deadgrep-backward-filename

  "z u" #'deadgrep-parent-directory)

(helix-keymap-set deadgrep-edit-mode-map :state 'normal
  "<escape>" #'deadgrep-mode
  "Z Z" #'deadgrep-mode

  ;; Commands bound to these keys have no sense for Deadgrep.
  "o"   #'undefined
  "O"   #'undefined
  "J"   #'undefined)

;;; Advices

(helix-advice-add 'deadgrep-mode :before #'helix-deactivate-mark-a)
(helix-advice-add 'deadgrep-mode :before #'helix-delete-all-fake-cursors)

(dolist (cmd '(deadgrep-visit-result
               deadgrep-visit-result-other-window))
  (helix-advice-add cmd :around #'helix-jump-command-a))

(add-hook 'deadgrep-mode-hook
          (defun helix--deadgrep-mode-hook ()
            ;; TODO: upstream this
            (setq-local revert-buffer-function
                        (lambda (_ignore-auto _noconfirm)
                          (deadgrep-restart)))))

;;; Commands

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

(provide 'helix-deadgrep)
;;; helix-deadgrep.el ends here
