;;; helix-org-mode.el -*- lexical-binding: t; -*-
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
;; Integration Helix with Org-mode.
;;
;;; Code:
(require 'helix-outline)
(require 'org)

;;; Keybindings

(add-hook 'org-mode-hook #'helix-surround-settings-for-org-mode)

(helix-keymap-set org-mode-map :state 'normal
  "C-c SPC" #'org-ctrl-c-ctrl-c

  "z u"   #'helix-org-up-heading
  "g h"   #'helix-org-first-non-blank

  "d"     #'helix-org-cut
  "p"     #'helix-org-paste-after
  "P"     #'helix-org-paste-before
  "="     #'org-indent-region
  "<"     #'helix-org-<
  ">"     #'helix-org->

  ;; "C-o"   #'org-mark-ring-goto
  ;;         #'org-mark-ring-push

  "[ s"   #'org-backward-sentence
  "] s"   #'org-forward-sentence
  "[ ."   #'org-backward-sentence
  "] ."   #'org-forward-sentence

  "M-o"   #'helix-org-up-element
  "M-i"   #'helix-org-down-element
  "M-n"   #'helix-org-next-element
  "M-p"   #'helix-org-previous-element

  "m ."   #'helix-org-mark-inner-sentence
  "m i s" #'helix-org-mark-inner-sentence
  "m a s" #'helix-org-mark-a-sentence

  "m h"   #'org-mark-subtree
  "m i h" #'org-mark-subtree

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

(with-eval-after-load 'org-capture
  (helix-keymap-set org-capture-mode-map :state 'normal
    "Z R" #'org-capture-refile
    "Z Z" #'org-capture-finalize
    "Z Q" #'org-capture-kill))

(let ((map org-read-date-minibuffer-local-map))
  (org-defkey map (kbd "M-l") #'org-calendar-forward-day)
  (org-defkey map (kbd "M-h") #'org-calendar-backward-day)
  (org-defkey map (kbd "M-j") #'org-calendar-forward-week)
  (org-defkey map (kbd "M-k") #'org-calendar-backward-week)
  (org-defkey map (kbd "M-L") #'org-calendar-forward-month)
  (org-defkey map (kbd "M-H") #'org-calendar-backward-month)
  (org-defkey map (kbd "M-J") #'org-calendar-forward-year)
  (org-defkey map (kbd "M-K") #'org-calendar-backward-year))

(when helix-want-C-hjkl-keys
  (helix-keymap-set org-mode-map :state 'normal
    "C-h" #'helix-org-up-element
    "C-j" #'helix-org-next-element
    "C-k" #'helix-org-previous-element
    "C-l" #'helix-org-down-element))

(when helix-want-M-hjkl-keys
  (helix-keymap-set org-mode-map :state 'normal
    "M-h" #'helix-org-up-element
    "M-j" #'helix-org-next-element
    "M-k" #'helix-org-previous-element
    "M-l" #'helix-org-down-element))

;;; Advices

(helix-advice-add 'org-mark-subtree :after #'helix-reveal-point-when-on-top)

(helix-advice-add 'org-next-visible-heading     :before #'helix-deactivate-mark-a)
(helix-advice-add 'org-previous-visible-heading :before #'helix-deactivate-mark-a)

;;; Commands

;; (dolist (cmd '(org-cycle      ; TAB
;;                org-shifttab)) ; S-TAB
;;   (helix-advice-add cmd :before #'helix-deactivate-mark-a))

;; (helix-advice-add 'org-cycle :around #'helix-keep-selection-a)

;; (helix-define-advice org-cycle (:aroung (command arg))
;;   (let ((deactivate-mark nil))
;;     (funcall command arg)))

;; zu
(helix-define-command helix-org-up-heading ()
  "Move up in the outline hierarchy to the parent heading."
  :multiple-cursors nil
  (interactive)
  (helix-delete-all-fake-cursors)
  (deactivate-mark)
  (helix-push-point)
  (if (org-at-heading-p)
      (outline-up-heading 1)
    (org-with-limited-levels (org-back-to-heading))))

;; gh
(helix-define-command helix-org-first-non-blank ()
  "Move point to beginning of current visible line skipping indentation.

If this is a headline, and `org-special-ctrl-a/e' is not nil or symbol
`reversed', on the first attempt move to where the headline text starts,
and only move to beginning of line when the cursor is already before
the start of the text of the headline.

If `org-special-ctrl-a/e' is symbol `reversed' then go to the start of the
text on the second attempt."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (setq this-command 'org-beginning-of-line)
  (helix-set-region (if (or (eq last-command this-command)
                            helix--extend-selection)
                        (mark)
                      (point))
                    (progn (org-beginning-of-line) (point))))

;; org-end-of-line

;; p
(helix-define-command helix-org-paste-after ()
  "Paste after selection."
  :multiple-cursors t
  (interactive)
  (helix-paste #'org-yank 1))

;; P
(helix-define-command helix-org-paste-before ()
  "Paste before selection."
  :multiple-cursors t
  (interactive)
  (helix-paste #'org-yank -1))

;; d
(helix-define-command helix-org-cut (count)
  "Kill (cut) text in region. I.e. delete text and put it in the `kill-ring'.
If no selection — delete COUNT chars before point."
  :multiple-cursors t
  (interactive "p")
  (when (helix-logical-lines-p) (helix-restore-newline-at-eol))
  (if (use-region-p)
      (kill-region nil nil t)
    (org-delete-char (- count)))
  (helix-extend-selection -1))

;; <
(helix-define-command helix-org-< (count)
  "Promote, dedent, move column left.
In items or headings, promote heading/item.
In code blocks, indent lines
In tables, move column to the left."
  (interactive "p")
  (cl-assert (/= count 0))
  (helix-indent #'helix-org-indent-left count))

(defun helix-org-indent-left (beg end)
  (cond
   ;; heading
   ((org-with-limited-levels
     (save-excursion (goto-char beg) (org-at-heading-p)))
    (org-map-region #'org-do-promote beg end))
   ;; table
   ((and (org-at-table-p) (helix-save-region
                            (helix-restore-newline-at-eol)
                            (org-at-table-p)))
    (org-table-move-column -1))
   ;; list
   ((and (save-excursion (goto-char beg) (org-at-item-p))
         (<= end (save-excursion (org-end-of-item-list))))
    (let* ((struct (org-list-struct))
           ;; If nil --- we are at the first item of the list with no
           ;; active selection --- indent full list.
           (no-subtree (or (not struct)
                           (not org-list-automatic-rules)
                           (region-active-p)
                           (/= (point-at-bol)
                               (org-list-get-top-point struct)))))
      (org-list-indent-item-generic -1 no-subtree struct)))
   (t
    (indent-rigidly-left beg end))))

;; >
(helix-define-command helix-org-> (count)
  "Demote, indent, move column right.
In items or headings, demote heading/item.
In code blocks, indent lines.
In tables, move column to the right."
  :multiple-cursors t
  (interactive "p")
  (cl-assert (/= count 0))
  (helix-indent #'helix-org-indent-right count))

(defun helix-org-indent-right (beg end)
  (cond
   ;; heading
   ((org-with-limited-levels
     (save-excursion (goto-char beg) (org-at-heading-p)))
    (org-map-region #'org-do-demote beg end))
   ;; table
   ((and (org-at-table-p) (helix-save-region
                            (helix-restore-newline-at-eol)
                            (org-at-table-p)))
    (org-table-move-column))
   ;; list
   ((and (save-excursion (goto-char beg) (org-at-item-p))
         (<= end (save-excursion (org-end-of-item-list))))
    (let* (;; (struct (save-excursion
           ;;           (goto-char beg)
           ;;           (org-list-struct)))
           (struct (org-list-struct))
           ;; If nil --- we are at the first item of the list with no
           ;; active selection --- indent full list.
           (no-subtree (or (not struct)
                           (not org-list-automatic-rules)
                           (region-active-p)
                           (/= (point-at-bol)
                               (org-list-get-top-point struct)))))
      (org-list-indent-item-generic 1 no-subtree struct)))
   (t
    (indent-rigidly-right beg end))))


;; mis
(helix-define-command helix-org-mark-inner-sentence (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-mark-inner-thing 'helix-org-sentence count t))

;; mas
(helix-define-command helix-org-mark-a-sentence ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (helix-mark-a-sentence 'helix-org-sentence))

;;; AST climbing

(defun helix-org-element-in-section (&optional granularity)
  "Return the smallest element that completely encloses the active region.
With no active region point position is used.

The search is constrained within the encloses `section' element.
If the active region extends beyond the `section' boundaries, return nil.
Result element has fully parsed structure with AST virtual root at the
parent `section' element.

GRANULARITY specifies the parsing level (see `org-element-parse-buffer')."
  (-let [(beg . end) (if (use-region-p)
                         (car (region-bounds))
                       (cons (point) (point)))]
    (let* ((element (save-excursion
                      (goto-char beg)
                      (org-element-at-point)))
           ;; Climb up the AST until `section' node.
           (section (org-element-lineage element 'section)))
      ;; If region exceed section — truncate it to `section' boundaries.
      (setq beg (max beg (org-element-begin section))
            end (min end (org-element-end section)))
      ;; Find smallest enclosing element within `section' element AST.
      (cl-loop with element = (helix-org--parse-element section granularity)
               with nested-element
               do (setq nested-element
                        (-find (lambda (el)
                                 (<= (org-element-begin el)
                                     beg end
                                     (org-element-end el)))
                               (org-element-contents element)))
               while nested-element
               do (setq element nested-element)
               finally return element))))

(cl-defun helix-org-parse-element (element
                                   &optional
                                   (root (org-element-lineage element 'section t))
                                   (granularity 'element))
  "Return the fully parsed structure of the ELEMENT.

ROOT will be the virtual-root of the result AST and should be one of
parents of the element. By default it will be the parent `section' element.

GRANULARITY specifies the parsing level (see `org-element-parse-buffer')."
  (if (eq element root)
      (helix-org--parse-element element granularity)
    ;; else
    (let ((beg (org-element-begin element))
          (end (org-element-end element))
          (type (org-element-type element))
          (element (helix-org--parse-element root granularity)))
      (while (and (setq element (-find (lambda (el)
                                         (<= (org-element-begin el)
                                             beg end
                                             (org-element-end el)))
                                       (org-element-contents element)))
                  (not (and (eq type (org-element-type element))
                            (= beg (org-element-begin element))
                            (= end (org-element-end element))))))
      element)))

(defun helix-org--parse-element (element &optional granularity)
  "Return the fully parsed structure of the ELEMENT.
GRANULARITY specifies the parsing level (see `org-element-parse-buffer')."
  (or granularity (setq granularity 'element))
  (save-excursion
    (with-restriction (org-element-begin element) (org-element-end element)
      (-> (org-element-parse-buffer granularity) ; -> org-data
          org-element-contents                   ; -> AST
          car))))                                ; -> ELEMENT node

(defun helix-org-at-heading-p ()
  (if (use-region-p)
      (save-excursion
        (goto-char (region-beginning))
        (org-at-heading-p))
    (org-at-heading-p)))

(helix-defvar-local helix-org--current-element nil
  "The cache for current element value.")

(cl-defun helix-org--current-element (&optional (new-element nil new-element?))
  "Return cached value when appropriate, or calculate new one."
  (cond (new-element?
         (setq helix-org--current-element
               (unless (org-element-type-p new-element 'headline)
                 new-element)))
        ((and (memq last-command '(helix-org-down-element
                                   helix-org-next-element
                                   helix-org-previous-element
                                   org-cycle                                ; TAB
                                   helix-smooth-scroll-line-not-to-very-top ; zz
                                   helix-smooth-scroll-line-to-center       ; zz
                                   helix-smooth-scroll-line-to-top          ; zt
                                   helix-smooth-scroll-line-to-bottom))     ; zb
              helix-org--current-element)
         helix-org--current-element)
        (t
         (setq helix-org--current-element (helix-org-element-in-section)))))

;; M-o
(helix-define-command helix-org-up-element (&optional arg)
  "Expand region to the parent element.
ARG is used to determine whether invocation was interactive and should not
be set manually."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (helix-restore-newline-at-eol)
  (-let* (((beg . end) (if (use-region-p)
                           (car (region-bounds))
                         (cons (point) (point))))
          (element (save-excursion
                     (goto-char beg)
                     (org-element-at-point))))
    ;; Climb up the tree until element fully contains region.
    (while (and element
                (or (org-element-type-p element 'section) ; skip section
                    (let ((element-beg (org-element-begin element))
                          (element-end (- (org-element-end element)
                                          (org-element-post-blank element))))
                      (< beg element-beg)
                      (< element-end end)
                      (and (= beg element-beg)
                           (= element-end end)))))
      (setq element (org-element-parent element)))
    (if (or (not element)
            (org-element-type-p element 'org-data))
        (user-error "No enclosing element")
      ;; else
      (helix-org--current-element nil)
      (helix-set-region (org-element-begin element)
                        (- (org-element-end element)
                           (org-element-post-blank element))
                        -1 :adjust)
      (if arg (helix-reveal-point-when-on-top)))))

;; M-i
(helix-define-command helix-org-down-element ()
  "Contract region to the first child element."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (if (use-region-p)
      (-if-let (child (if (helix-org-at-heading-p)
                          (save-excursion
                            (goto-char (region-beginning))
                            (when-let* ((pos (-> (org-element-at-point)
                                                 (org-element-contents-begin))))
                              (goto-char pos)
                              (let ((child (org-element-at-point)))
                                (if (org-element-type-p child 'headline)
                                    child
                                  (helix-org-parse-element child)))))
                        ;; else
                        (-> (helix-org--current-element)
                            (org-element-contents)
                            (car-safe))))
          (progn
            (save-excursion
              (goto-char (region-beginning))
              (when (org-invisible-p (line-end-position))
                (org-cycle)))
            (helix-org--current-element child)
            (helix-set-region (org-element-begin child)
                              (- (org-element-end child)
                                 (org-element-post-blank child))
                              -1 :adjust)
            (helix-reveal-point-when-on-top))
        ;; (user-error "No content for this element")
        (user-error "No nested element"))
    ;; else
    (helix-org-up-element)
    (when (helix-org-at-heading-p)
      (helix-org-down-element))))

;; M-n
(helix-define-command helix-org-next-element ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (unless (use-region-p)
    (helix-org-up-element))
  (when-let ((next (helix-org--next-element)))
    (helix-org--current-element next)
    (helix-set-region (org-element-begin next)
                      (- (org-element-end next)
                         (org-element-post-blank next))
                      (helix-region-direction) :adjust)
    (helix-reveal-point-when-on-top)))

(defun helix-org--next-element ()
  (if (helix-org-at-heading-p)
      (save-excursion
        (goto-char (region-beginning))
        (org-forward-heading-same-level 1)
        (org-element-at-point))
    ;; else
    (let ((current (helix-org--current-element)))
      (or
       ;; Try to find the node in parent directly after ELEMENT.
       (let ((siblings (-> (org-element-parent current)
                           (org-element-contents))))
         (nth (1+ (-find-index (lambda (elem) (eq elem current))
                               siblings))
              siblings))
       ;; No following element in current `section', than check `headline'
       ;; directly after `section'.
       (if-let* (((org-element-type-p (helix-org-element-parent current)
                                      'section))
                 (parent (org-element-lineage (org-element-at-point)
                                              '(headline org-data)))
                 (element (save-excursion
                            (goto-char (org-element-end current))
                            (org-element-at-point)))
                 ((eq parent (org-element-parent element))))
           element)))))

(defun helix-org-element-parent (element)
  "Return the first parent of ELEMENT that spans a larger region.
This function is similar to `org-element-parent', except that if the parent has
exactly the same boundaries as ELEMENT (for example, a `plain-list' containing
a single `item'), it continues searching up the hierarchy until it finds
a parent with different boundaries or reaches a `section' element."
  (let ((beg (org-element-begin element))
        (end (org-element-end element))
        (parent (org-element-parent element)))
    (while (and parent
                ;; (not (org-element-type-p parent 'org-data))
                (not (org-element-type-p parent 'section))
                (= beg (org-element-begin parent))
                (= end (org-element-end parent)))
      (setq parent (org-element-parent parent)))
    parent))

;; M-p
(helix-define-command helix-org-previous-element ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (unless (use-region-p)
    (helix-org-up-element))
  (when-let ((previous (helix-org--previous-element)))
    (helix-org--current-element previous)
    (helix-set-region (org-element-begin previous)
                      (- (org-element-end previous)
                         (org-element-post-blank previous))
                      (helix-region-direction) :adjust)
    (helix-reveal-point-when-on-top)))

(defun helix-org--previous-element ()
  (if (helix-org-at-heading-p)
      (save-excursion
        (goto-char (region-beginning))
        (let ((current (org-element-at-point)))
          (goto-char (org-element-begin current))
          (let ((pnt (point)))
            (org-backward-heading-same-level 1)
            (if (/= (point) pnt)
                (org-element-at-point)
              ;; else
              (skip-chars-backward " \r\t\n")
              (unless (bobp)
                (-> (org-element-lineage (org-element-at-point) 'section)
                    (helix-org-parse-element)
                    (org-element-contents)
                    (org-last)))))))
    ;; else
    (let* ((current (helix-org--current-element))
           (siblings (org-element-contents (org-element-parent current))))
      ;; Try to find the node in PARENT previous to ELEMENT.
      (nth (1- (-find-index (lambda (elem) (eq elem current))
                            siblings))
           siblings))))

;;; Things

;; `helix-org-sentence' thing
(put 'helix-org-sentence 'forward-op (lambda (count)
                                       (helix-motion-loop (dir count)
                                         (ignore-errors
                                           (if (natnump dir)
                                               (org-forward-sentence)
                                             (org-backward-sentence))))))

;;; Surround

(helix-define-command helix-mark-inner-org-emphasis ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when (org-in-regexp org-emph-re 2)
    (helix-set-region (match-beginning 4) (match-end 4)))
  ;; (when-let* ((bounds (bounds-of-thing-at-point 'defun))
  ;;             (nlines (count-lines (car bounds) (point)))
  ;;             ((org-in-regexp org-emph-re nlines)))
  ;;   (helix-set-region (match-beginning 4) (match-end 4)))
  )

(helix-define-command helix-mark-an-org-emphasis ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when (org-in-regexp org-emph-re 2)
    (helix-set-region (match-beginning 2) (match-end 2))))

(helix-define-command helix-mark-inner-org-verbatim ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when (org-in-regexp org-verbatim-re 2)
    (helix-set-region (match-beginning 4) (match-end 4))))

(helix-define-command helix-mark-an-org-verbatim ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when (org-in-regexp org-verbatim-re 2)
    (helix-set-region (match-beginning 2) (match-end 2))))

(defun helix-surround-settings-for-org-mode ()
  "Configure Helix surround functionality for Org-mode."
  (dolist (char '(?/ ?* ?_ ?+ ?= ?~))
    (helix-surround-add-pair char (cons (char-to-string char)
                                        (char-to-string char))
      :search #'helix-surround--4-bounds-of-org-verbatim)))

(defun helix-surround--4-bounds-of-org-verbatim ()
  (when (org-in-regexp org-verbatim-re 2)
    (list (match-beginning 2)
          (match-beginning 4)
          (match-end 2)
          (match-end 4))))

(defun helix-surround--4-bounds-of-org-emphasis ()
  (when (org-in-regexp org-emph-re 2)
    (list (match-beginning 2)
          (match-beginning 4)
          (match-end 2)
          (match-end 4))))

(provide 'helix-org-mode)
;;; helix-org-mode.el ends here
