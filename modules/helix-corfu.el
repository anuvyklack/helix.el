;;; helix-corfu.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Yuriy Artemyev
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
;; Keybindings for Corfu package.
;;
;;; Code:
(require 'helix-core)
(require 'corfu)

(helix-keymap-set corfu-map
  "C-SPC" #'corfu-insert-separator

  "C-k"   #'corfu-previous
  "C-j"   #'corfu-next

  "C-h"   #'corfu-info-documentation
  "C-l"   #'corfu-info-location

  "C-f"   #'corfu-scroll-up
  "C-b"   #'corfu-scroll-down
  "C-u"   #'corfu-scroll-down
  "C-d"   #'corfu-scroll-up)

(provide 'helix-corfu)
;;; helix-corfu.el ends here
