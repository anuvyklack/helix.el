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
(require 'helix-vars)

(helix-define-state normal
  "Normal state"
  :cursor 'bar)

;;;

(helix-define-state insert
  "Insert state"
  :cursor 'box
  (unless helix-insert-state
    (when (and helix-select-on-insert
               (/= (point) helix-insert-pos))
      ;; activate region
      )))

;;;

(helix-define-state motion
  "Motion state")

;;;

(provide 'helix-states)
;;; helix-states.el ends here
