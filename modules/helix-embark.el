;;; helix-embark.el -*- lexical-binding: t; -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/anuvyklack/helix.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Keybindings for Embark package.
;;
;;; Code:

(helix-keymap-global-set
  "C-<m>" #'embark-act)

(helix-keymap-set minibuffer-local-map
  "C-c C-c" #'embark-export
  "C-c C-v" #'embark-collect)

(with-eval-after-load 'embark
  (helix-keymap-set embark-general-map
    "C-<m>" #'embark-select
    "y"     #'embark-copy-as-kill
    "w"     nil) ; unbind `embark-copy-as-kill'
  (helix-keymap-set embark-region-map
    "u"     nil  ; `upcase-region'
    "l"     nil  ; `downcase-region'
    ";"     nil) ; `comment-or-uncomment-region'
  (helix-keymap-set embark-collect-mode-map
    ;; `m' and `u' are common keys for selecting and unselecting in
    ;; Dired like buffers.
    "m"     #'helix-embark-select
    "u"     #'helix-embark-select
    "y"     #'embark-copy-as-kill)

  (with-eval-after-load 'helpful
    (helix-keymap-set embark-symbol-map
      "h"     #'helpful-symbol)))

(with-eval-after-load 'embark-consult
  (helix-keymap-set embark-consult-rerun-map
    "g"     nil
    "g r"   #'embark-rerun-collect-or-export))

;;; Commands

(defun helix-embark-select ()
  "Add or remove the target from the current buffer's selection.
You can act on all selected targets at once with `embark-act-all'.
When called from outside `embark-act' this command will select
the first target at point."
  (interactive)
  (embark-select)
  (next-line))

(provide 'helix-embark)
;;; helix-embark.el ends here
