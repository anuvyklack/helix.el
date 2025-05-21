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
(require 'helix-common)
(require 'helix-core)
(require 'helix-multiple-cursors)
(require 'helix-states)
(require 'helix-search)
(require 'helix-commands)
(require 'helix-keybindings)
(require 'helix-integration)

;; Merge overlapping regions after all `helix-mark-*' commands.
(setq helix--merge-regions-commands
      (append (apropos-internal "^helix-mark" 'commandp)
              helix--merge-regions-commands))

(provide 'helix)
;;; helix.el ends here
