;;; helix-states.el --- Helix states -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Default Helix states.
;;
;;; Code:

(require 'helix-core)
(require 'helix-multiple-cursors-core)

(helix-define-state normal
  "Normal state."
  :cursor helix-normal-state-cursor
  :keymap (define-keymap :full t :suppress t))

(helix-define-state insert
  "Insert state."
  :cursor helix-insert-state-cursor
  (if helix-insert-state
      (when (region-active-p)
        (setq helix--region-was-active-on-insert t)
        (helix-with-each-cursor
          (deactivate-mark)))
    ;; else
    (when helix--region-was-active-on-insert
      (helix-with-each-cursor
        (activate-mark)))
    (setq helix--region-was-active-on-insert nil)))

(helix-define-state motion
  "Motion state."
  :cursor helix-motion-state-cursor)

(provide 'helix-states)
;;; helix-states.el ends here
