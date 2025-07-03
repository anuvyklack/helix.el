;;; helix.el --- Helix emulation -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/anuvyklack/helix
;; Package-Requires: ((emacs "28.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Emulation of the Helix text editing model.
;;
;;; Code:

(require 'helix-vars)
(require 'helix-states)
(require 'helix-commands)
(require 'helix-keybindings)
(require 'helix-integration)

;; Commands, after which `helix-merge-overlapping-regions' will be executed
;; if `helix--extend-selection' is non nil.
(mapc #'(lambda (cmd)
          (put cmd 'helix-merge-regions 'extend-selection))
      '(helix-backward-char        ;; h
        helix-forward-char         ;; l
        helix-next-line            ;; j
        helix-previous-line        ;; k
        helix-forward-word-start   ;; w
        helix-forward-WORD-start   ;; W
        helix-backward-word-start  ;; b
        helix-backward-WORD-start  ;; B
        helix-forward-word-end     ;; e
        helix-forward-WORD-end     ;; E
        helix-find-char-forward    ;; f
        helix-find-char-backward   ;; F
        helix-till-char-forward    ;; t
        helix-till-char-backward)) ;; T


;; Commands, after which `helix-merge-overlapping-regions' will be executed
(mapc #'(lambda (cmd)
          (put cmd 'helix-merge-regions t))
      `(helix-first-non-blank   ;; gh
        helix-beginning-of-line-command ;; gs
        helix-end-of-line-command ;; gl
        helix-copy-selection    ;; C
        helix-copy-selection-up ;; M-c
        helix-search-forward    ;; /
        helix-search-backward   ;; ?
        helix-search-next       ;; n
        helix-search-previous   ;; N
        ;; All `helix-mark-*' commands
        ,@(apropos-internal "^helix-mark" 'commandp)))

(provide 'helix)
;;; helix.el ends here
