;;; helix.el --- Helix emulation -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Created: March 27, 2025
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
(require 'helix-core)
(require 'helix-commands)
(require 'helix-keybindings)
(require 'helix-integration)

(provide 'helix)
;;; helix.el ends here
