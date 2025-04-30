;;; helix-integration.el --- Integration with other packages -*- lexical-binding: t; -*-
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
;;  Helix integration with other packages.
;;
;;; Code:

(require 'helix-core)

;;; Org mode

(declare-function org-in-regexp "org")

(defun helix-mark-inner-org-emphasis ()
  (interactive)
  (save-match-data
    (when (org-in-regexp org-emph-re 2)
      (set-mark (match-beginning 4))
      (goto-char (match-end 4)))
    ;; (when-let* ((bounds (bounds-of-thing-at-point 'defun))
    ;;             (nlines (count-lines (car bounds) (point)))
    ;;             ((org-in-regexp org-emph-re nlines)))
    ;;   (set-mark (match-beginning 4))
    ;;   (goto-char (match-end 4)))
    ))

(defun helix-mark-an-org-emphasis ()
  (interactive)
  (save-match-data
    (when (org-in-regexp org-emph-re 2)
      (set-mark (match-beginning 2))
      (goto-char (match-end 2)))))

(defun helix-mark-inner-org-verbatim ()
  (interactive)
  (save-match-data
    (when (org-in-regexp org-verbatim-re 2)
      (set-mark (match-beginning 4))
      (goto-char (match-end 4)))))

(defun helix-mark-an-org-verbatim ()
  (interactive)
  (save-match-data
    (when (org-in-regexp org-verbatim-re 2)
      (set-mark (match-beginning 2))
      (goto-char (match-end 2)))))

(helix-keymap-set org-mode-map 'normal
  "m /"   #'helix-mark-inner-org-emphasis
  "m i /" #'helix-mark-inner-org-emphasis
  "m a /" #'helix-mark-an-org-emphasis

  "m *"   #'helix-mark-inner-org-emphasis
  "m i *" #'helix-mark-inner-org-emphasis
  "m a *" #'helix-mark-an-org-emphasis

  "m _"   #'helix-mark-inner-org-emphasis
  "m i _" #'helix-mark-inner-org-emphasis
  "m a _" #'helix-mark-an-org-emphasis

  "m +"   #'helix-mark-inner-org-emphasis
  "m i +" #'helix-mark-inner-org-emphasis
  "m a +" #'helix-mark-an-org-emphasis

  "m ="   #'helix-mark-inner-org-verbatim
  "m i =" #'helix-mark-inner-org-verbatim
  "m a =" #'helix-mark-an-org-verbatim

  "m ~"   #'helix-mark-inner-org-verbatim
  "m i ~" #'helix-mark-inner-org-verbatim
  "m a ~" #'helix-mark-an-org-verbatim)

(provide 'helix-integration)
;;; helix-integration.el ends here
