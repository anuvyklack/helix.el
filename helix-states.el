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
(require 'helix-commands)

(helix-define-state normal
  "Default state for editable buffers."
  :cursor 'bar)

;;;

(helix-define-state insert
  "Insert state"
  :cursor 'box
  (cond ((helix-state 'insert)
         (setq helix--region-was-active-on-insert (region-active-p))
         (deactivate-mark))
        (t
         (when helix--region-was-active-on-insert
           (activate-mark 'no-tmm)))))

;;;

(helix-define-state motion
  "Motion state")

;;;

(provide 'helix-states)
;;; helix-states.el ends here
