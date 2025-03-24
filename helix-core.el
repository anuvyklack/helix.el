;;; helix-core.el --- Core functionality -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Core functionality
;;
;;; Code:

(require 'cl-lib)

(define-minor-mode helix-local-mode
  "Minor mode for setting up Helix in a current buffer."
  :global nil
  (if helix-local-mode
      (progn
        (cl-pushnew 'evil-mode-map-alist emulation-mode-map-alists)
        )
    (progn)))

(provide 'helix-core)
;;; helix-core.el ends here
