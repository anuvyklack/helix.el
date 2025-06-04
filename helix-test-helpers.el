;;; helix-test-helpers.el --- Unit test infrastructure for Helix -*- lexical-binding: t; -*-
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
;; This file contains helper functions for writing tests for Helix.
;;
;;; Code:

(defmacro helix-test-buffer (&rest body)
  "Execute BODY in a temporary buffer.

:state STATE   The initial state, defaults to `normal'.
:point STRING  String that denotes point, defaults to \"|\"
:mark STRING   String that denotes mark, defaults to \"$\"

\(fn [[KEY VALUE]...] FORMS...)"
  (declare (indent defun))
  (let (buffer-str
        (state 'normal)
        (point-str "|")
        (mark-str  "$"))
    ;; collect keywords
    (while (keywordp (car-safe body))
      (let ((key (pop body))
            (arg (pop body)))
        (pcase key
          (:state (setq state arg))
          (:point (setq point-str arg))
          (:mark  (setq mark-str arg)))))
    (when (stringp (car-safe body))
      (setq buffer-str (pop body)))
    `(let ((buffer (helix-test-init-buffer-from-string
                    ,buffer-str ,state ,point-str ,mark-str))
           (kill-ring kill-ring)
           (kill-ring-yank-pointer kill-ring-yank-pointer)
           message-log-max)
       (unwind-protect
           (save-window-excursion
             (with-current-buffer buffer
               (buffer-enable-undo)
               ;; parse remaining forms
               ,@(mapcar (lambda (form)

                           )
                         body)
               )
             )
         (when (buffer-name buffer) (kill-buffer buffer)))
       )
    )
  )

(defun helix-test-init-buffer-from-string (string state point-str mark-str)
  ;; (setq point-str (regexp-quote point-str)
  ;;       mark-str  (regexp-quote mark-str))
  (with-current-buffer (generate-new-buffer " *test*")
    (prog1 (current-buffer)
      (save-excursion (insert string))
      (helix-normal-state 1)
      (when (search-forward mark-str nil t)
        (set-mark (match-beginning 0))
        (delete-region (match-beginning 0) (match-end 0)))
      (when (search-forward point-str nil t)
        (goto-char (match-beginning 0))
        (delete-region (match-beginning 0) (match-end 0))))))

(defun helix-test-buffer-state (string point-str mark-str)
  "Validate the current buffer state according to STRING."
  )

(provide 'helix-test-helpers)
;;; helix-test-helpers.el ends here
