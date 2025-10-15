;;; helix.el --- Helix emulation -*- lexical-binding: t; -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Created: March 27, 2025
;; Version: 0.0.1
;; Homepage: https://github.com/anuvyklack/helix.el
;; Package-Requires: ((emacs "29.1"))
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
