;;; helix-states.el --- Helix states -*- lexical-binding: t; -*-
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
;;  Default Helix states.
;;
;;; Code:

(require 'helix-core)
(require 'helix-multiple-cursors-core)
;; (require 'helix-commands)

(helix-define-state normal
  "Normal state."
  :cursor helix-normal-state-main-cursor
  (when helix-normal-state
    ;; We need to run through all the cursors in any case to switch their color.
    (helix-with-each-cursor
      (when helix--region-was-active-on-insert
        (activate-mark)))
    (setq helix--region-was-active-on-insert nil)))

(helix-define-state insert
  "Insert state."
  :cursor helix-insert-state-main-cursor
  (when helix-insert-state
    (setq helix--region-was-active-on-insert (region-active-p))
    (helix-with-each-cursor
      (deactivate-mark)
      (setq helix--extend-selection nil))))

(helix-define-state motion
  "Motion state."
  :cursor 'hbar)

(provide 'helix-states)
;;; helix-states.el ends here
