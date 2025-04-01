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
  "Normal state"
  :cursor 'bar)

;;;

(define-key helix-normal-state-map "h" #'helix-backward-char)
(define-key helix-normal-state-map "j" #'helix-next-line)
(define-key helix-normal-state-map "k" #'helix-previous-line)
(define-key helix-normal-state-map "l" #'helix-forward-char)

(define-key helix-normal-state-map "i" #'helix-insert)
(define-key helix-normal-state-map "a" #'helix-append)

(define-key helix-normal-state-map "w" #'helix-forward-word-start)
(define-key helix-normal-state-map "b" #'helix-backward-word-start)
(define-key helix-normal-state-map "e" #'helix-forward-word-end)
(define-key helix-normal-state-map "W" #'helix-forward-WORD-start)
(define-key helix-normal-state-map "B" #'helix-backward-WORD-start)
(define-key helix-normal-state-map "e" #'helix-forward-WORD-end)
(define-key helix-normal-state-map "x" #'helix-select-or-extend-line)

;;;

(helix-define-state insert
  "Insert state"
  :cursor 'box
  (unless helix-insert-state
    ;; (when (and helix-select-on-insert
    ;;            (/= (point) helix-insert-pos))
    ;;   ;; activate region
    ;;   )
    ))

(define-key helix-insert-state-map [escape] #'helix-normal-state)

;;;

(helix-define-state motion
  "Motion state")

;;;

(provide 'helix-states)
;;; helix-states.el ends here
