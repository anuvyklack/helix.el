;;; helix-edit-indirect.el -*- lexical-binding: t; -*-
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
;; WARNING: To use this module, you must install the `edit-indirect' package
;;          as a dependency.
;;
;; This module rebinds `zn' key chord to `helix-edit-indirect' command.
;;
;; It differs from `helix-narrow-to-region-indirectly' (original `zn' binding)
;; which clones buffer inidrectly with narrowing in that the text properties
;; are not shared, so the parent buffer major mode and the edit-indirect buffer
;; major mode will not be able to tread on each other's toes by setting up
;; potentially conflicting text properties, which happens surprisingly often
;; when the font-lock mode is used.
;;
;; When done, exit with `edit-indirect-commit', which will remove the original
;; region and replace it with the edited version; or with `edit-indirect-abort',
;; which will drop the modifications.
;;
;; Edit-indirect buffers use the `edit-indirect-mode-map' keymap. Regions with
;; active edit-indirect buffers use the edit-indirect-overlay-map keymap.
;;
;; If there's already an edit-indirect buffer for region, use that. If there's
;; already an edit-indirect buffer active overlapping any portion of region, an
;; `edit-indirect-overlapping' error is signaled.
;;
;;; Code:
(require 'helix-core)

(when (memq 'helix-edit-indirect helix-modules)
  (helix-keymap-global-set :state 'normal
    "z n" #'helix-edit-indirect ; replace `helix-narrow-to-region-indirectly'
    "z w" nil))                ; unbind `helix-widen-indirectly-narrowed'

(with-eval-after-load 'edit-indirect
  (helix-keymap-set edit-indirect-mode-map :state 'normal
    "Z Z" #'edit-indirect-commit
    "Z Q" #'edit-indirect-abort))

;;;###autoload
(defun helix-edit-indirect (arg)
  "Copy region without text properties, to a separate buffer.
With `universal-argument' ask which major mode to use in edit-indirect buffer.

This differs from `helix-narrow-to-region-indirectly' which clones buffer
inidrectly with narrowing in that the text properties are not shared, so the
parent buffer major mode and the edit-indirect buffer major mode will not be
able to tread on each other's toes by setting up potentially conflicting text
properties, which happens surprisingly often when the font-lock mode is used.

When done, exit with `edit-indirect-commit', which will remove the original
region and replace it with the edited version; or with `edit-indirect-abort',
which will drop the modifications.

Edit-indirect buffers use the `edit-indirect-mode-map' keymap. Regions with
active edit-indirect buffers use the edit-indirect-overlay-map keymap.

If there's already an edit-indirect buffer for region, use that. If there's
already an edit-indirect buffer active overlapping any portion of region, an
`edit-indirect-overlapping' error is signaled."
  (interactive "P")
  (unless (require 'edit-indirect nil t)
    (error "`helix-edit-indirect' module requires `edit-indirect' package as a dependency."))
  (unless (use-region-p) (user-error "No region selected"))
  (helix-restore-newline-at-eol)
  (let ((beg (region-beginning))
        (end (region-end)))
    (deactivate-mark)
    (let ((mode (if current-prefix-arg
                    (completing-read "major mode: "
                                     (apropos-internal "-mode$" #'commandp)
                                     nil nil nil nil
                                     major-mode)
                  major-mode))
          (name (or buffer-file-name
                    list-buffers-directory))
          (vars (cl-loop for symbol in '(default-directory lexical-binding)
                         collect symbol
                         collect (symbol-value symbol))))
      (let ((buffer (edit-indirect-region beg end)))
        (set-buffer buffer)
        (funcall mode)
        (setq list-buffers-directory name)
        (eval `(setq-local ,@vars) t)
        (beginning-of-buffer)
        (switch-to-buffer buffer)))))

(add-hook 'edit-indirect-after-creation-hook #'helix-edit-indirect--dedent)
(add-hook 'edit-indirect-before-commit-hook  #'helix-edit-indirect--indent)

(defun helix-edit-indirect--dedent ()
  (setq-local helix-edit-indirect--intentation (helix-common-indentation))
  (save-excursion
    (indent-rigidly (point-min) (point-max)
                    (- helix-edit-indirect--intentation))))

(defun helix-edit-indirect--indent ()
  (when (boundp 'helix-edit-indirect--intentation)
    (save-excursion
      (indent-rigidly (point-min) (point-max)
                      helix-edit-indirect--intentation))))

(defun helix-common-indentation ()
  "Return the common indentation off all lines in the buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((indentation 0))
      (while (not (eobp))
        (unless (s-blank-str? (thing-at-point 'line))
          (setq indentation (min indentation (current-indentation))))
        (forward-line))
      indentation)))

(provide 'helix-edit-indirect)
;;; helix-edit-indirect.el ends here
