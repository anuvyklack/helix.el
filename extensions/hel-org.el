;;; hel-org.el -*- lexical-binding: t; -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Author: Yuriy Artemyev <anuvyklack@gmail.com>
;; Maintainer: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; Homepage: https://github.com/anuvyklack/hel.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Integration Hel with Org-mode.
;;
;;; Code:
;; (require 'hel-outline)
(require 'org)

;;; Keybindings

(add-hook 'org-mode-hook #'hel-surround-settings-for-org-mode)

(hel-keymap-set org-mode-map :state 'normal
  ;; `J' and `K' keys are already occupied in Hel.
  "H"     'org-shiftleft
  "L"     'org-shiftright

  "M-h"   'org-metaleft
  "M-j"   'org-metadown
  "M-k"   'org-metaup
  "M-l"   'org-metaright

  "M-H"   'org-shiftmetaleft
  "M-J"   'org-shiftmetadown
  "M-K"   'org-shiftmetaup
  "M-L"   'org-shiftmetaright

  "C-S-h" 'org-shiftcontrolleft
  "C-S-j" 'org-shiftcontroldown
  "C-S-k" 'org-shiftcontrolup
  "C-S-l" 'org-shiftcontrolright

  "C-c SPC" 'org-ctrl-c-ctrl-c

  "z u"   'hel-org-up-heading
  "g h"   'hel-org-first-non-blank

  "d"     'hel-org-cut
  "p"     'hel-org-paste-after
  "P"     'hel-org-paste-before
  "="     'org-indent-region
  "<"     'hel-org-<
  ">"     'hel-org->

  "[ RET" 'hel-org-insert-item-above
  "] RET" 'hel-org-insert-item-below

  ;; "C-o"   'org-mark-ring-goto
  ;;         'org-mark-ring-push

  "[ s"   'org-backward-sentence
  "] s"   'org-forward-sentence
  "[ ."   'org-backward-sentence
  "] ."   'org-forward-sentence

  "M-o"   'hel-org-up-element
  "M-i"   'hel-org-down-element
  "M-n"   'hel-org-next-element
  "M-p"   'hel-org-previous-element

  "m ."   'hel-org-mark-inner-sentence
  "m i s" 'hel-org-mark-inner-sentence
  "m a s" 'hel-org-mark-a-sentence

  "m h"   'org-mark-subtree
  "m i h" 'org-mark-subtree

  "m /"   'hel-mark-inner-org-emphasis
  "m i /" 'hel-mark-inner-org-emphasis
  "m a /" 'hel-mark-an-org-emphasis

  "m *"   'hel-mark-inner-org-emphasis
  "m i *" 'hel-mark-inner-org-emphasis
  "m a *" 'hel-mark-an-org-emphasis

  "m _"   'hel-mark-inner-org-emphasis
  "m i _" 'hel-mark-inner-org-emphasis
  "m a _" 'hel-mark-an-org-emphasis

  "m +"   'hel-mark-inner-org-emphasis
  "m i +" 'hel-mark-inner-org-emphasis
  "m a +" 'hel-mark-an-org-emphasis

  "m ="   'hel-mark-inner-org-verbatim
  "m i =" 'hel-mark-inner-org-verbatim
  "m a =" 'hel-mark-an-org-verbatim

  "m ~"   'hel-mark-inner-org-verbatim
  "m i ~" 'hel-mark-inner-org-verbatim
  "m a ~" 'hel-mark-an-org-verbatim)

(when hel-want-C-hjkl-keys
  (hel-keymap-set org-mode-map :state 'normal
    "C-h" 'hel-org-up-element
    "C-j" 'hel-org-next-element
    "C-k" 'hel-org-previous-element
    "C-l" 'hel-org-down-element))

(let ((map org-read-date-minibuffer-local-map))
  (org-defkey map (kbd "M-l") 'org-calendar-forward-day)
  (org-defkey map (kbd "M-h") 'org-calendar-backward-day)
  (org-defkey map (kbd "M-j") 'org-calendar-forward-week)
  (org-defkey map (kbd "M-k") 'org-calendar-backward-week)
  (org-defkey map (kbd "M-L") 'org-calendar-forward-month)
  (org-defkey map (kbd "M-H") 'org-calendar-backward-month)
  (org-defkey map (kbd "M-J") 'org-calendar-forward-year)
  (org-defkey map (kbd "M-K") 'org-calendar-backward-year))

(with-eval-after-load 'org-capture
  (hel-keymap-set org-capture-mode-map :state 'normal
    "Z R" 'org-capture-refile
    "Z Z" 'org-capture-finalize
    "Z Q" 'org-capture-kill))

(with-eval-after-load 'org-table
  (hel-keymap-set org-table-fedit-map :state 'normal
    "z x" 'org-table-fedit-finish
    "Z Z" 'org-table-fedit-finish
    "Z Q" 'org-table-fedit-abort))

;;;; org-src

(with-eval-after-load 'org-src
  (hel-keymap-set org-src-mode-map :state 'normal
    "z x" 'org-edit-src-save
    "Z Z" 'org-edit-src-exit
    "Z Q" 'org-edit-src-abort
    "C-c C-c" 'org-edit-src-exit
    "C-c SPC" 'org-edit-src-exit))

(add-hook 'org-src-mode-hook 'hel--org-src-h)

(defun hel--org-src-h ()
  (hel-update-active-keymaps)
  (when org-edit-src-persistent-message
    (setq header-line-format
          (let ((ZZ (propertize "ZZ" 'face 'help-key-binding))
                (ZQ (propertize "ZQ" 'face 'help-key-binding)))
            (if org-src--allow-write-back
                (format "Edit, then exit with %s or abort with %s" ZZ ZQ)
              (format "Exit with %s or abort with %s" ZZ ZQ))))))

;;;; Repeat mode

(put 'hel-org-up-heading 'repeat-map 'org-navigation-repeat-map)

(setq org-navigation-repeat-map
      (define-keymap
        "u" 'hel-org-up-heading))

;;; Advices

(hel-advice-add 'org-mark-subtree :after #'hel-reveal-point-when-on-top)

(hel-advice-add 'org-next-visible-heading     :before #'hel-deactivate-mark-a)
(hel-advice-add 'org-previous-visible-heading :before #'hel-deactivate-mark-a)

(dolist (cmd '(org-shiftleft
               org-shiftright
               org-shiftmetaleft
               org-shiftmetaright))
  (hel-advice-add cmd :around #'hel-keep-selection-a))

;;; Commands

;; (dolist (cmd '(org-cycle      ; TAB
;;                org-shifttab)) ; S-TAB
;;   (hel-advice-add cmd :before #'hel-deactivate-mark-a))

;; (hel-advice-add 'org-cycle :around #'hel-keep-selection-a)

;; (hel-define-advice org-cycle (:aroung (command arg))
;;   (let ((deactivate-mark nil))
;;     (funcall command arg)))

;; zu
(hel-define-command hel-org-up-heading ()
  "Move up in the outline hierarchy to the parent heading."
  :multiple-cursors nil
  (interactive)
  (hel-delete-all-fake-cursors)
  (deactivate-mark)
  (hel-push-point)
  (if (org-at-heading-p)
      (outline-up-heading 1)
    (org-with-limited-levels (org-back-to-heading))))

;; gh
(hel-define-command hel-org-first-non-blank ()
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
  (hel-set-region (if (or (eq last-command this-command)
                          hel--extend-selection)
                      (mark)
                    (point))
                  (progn (org-beginning-of-line) (point))))

;; org-end-of-line

;; p
(hel-define-command hel-org-paste-after (arg)
  "Paste after selection.
With \\[universal-argument] invokes `yank-rectangle' instead. See `hel-copy'."
  :multiple-cursors t
  (interactive "*P")
  (pcase arg
    ('(4) (insert-rectangle killed-rectangle))
    (_ (hel-paste #'org-yank 1))))

;; P
(hel-define-command hel-org-paste-before ()
  "Paste before selection."
  :multiple-cursors t
  (interactive "*")
  (hel-paste #'org-yank -1))

;; d
(hel-define-command hel-org-cut (count)
  "Kill (cut) text in region. I.e. delete text and put it in the `kill-ring'.
If no selection — delete COUNT chars before point."
  :multiple-cursors t
  (interactive "*p")
  (when (hel-logical-lines-p) (hel-restore-newline-at-eol))
  (if (use-region-p)
      (kill-region nil nil t)
    (org-delete-char (- count)))
  (hel-extend-selection -1))

;; <
(hel-define-command hel-org-< (count)
  "Promote, dedent, move column left.
In items or headings, promote heading/item.
In code blocks, indent lines
In tables, move column to the left."
  (interactive "p")
  (cl-assert (/= count 0))
  (hel-indent #'hel-org-indent-left count))

(defun hel-org-indent-left (beg end)
  (cond
   ;; heading
   ((org-with-limited-levels
     (save-excursion (goto-char beg) (org-at-heading-p)))
    (org-map-region #'org-do-promote beg end))
   ;; table
   ((and (org-at-table-p) (hel-save-region
                            (hel-restore-newline-at-eol)
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
(hel-define-command hel-org-> (count)
  "Demote, indent, move column right.
In items or headings, demote heading/item.
In code blocks, indent lines.
In tables, move column to the right."
  :multiple-cursors t
  (interactive "p")
  (cl-assert (/= count 0))
  (hel-indent #'hel-org-indent-right count))

(defun hel-org-indent-right (beg end)
  (cond
   ;; heading
   ((org-with-limited-levels
     (save-excursion (goto-char beg) (org-at-heading-p)))
    (org-map-region #'org-do-demote beg end))
   ;; table
   ((and (org-at-table-p) (hel-save-region
                            (hel-restore-newline-at-eol)
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
(hel-define-command hel-org-mark-inner-sentence (count)
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-mark-inner-thing 'hel-org-sentence count t))

;; mas
(hel-define-command hel-org-mark-a-sentence ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (hel-mark-a-sentence 'hel-org-sentence))

;; [ RET
(hel-define-command hel-org-insert-item-above ()
  "Inserts a new heading, list item or table cell above the current one."
  :multiple-cursors t
  (interactive)
  (hel-org--insert-item -1))

;; ] RET
(hel-define-command hel-org-insert-item-below ()
  "Inserts a new heading, lilst item or table cell below the current one."
  :multiple-cursors t
  (interactive)
  (hel-org--insert-item 1))

(defun hel-org--insert-item (direction)
  (pcase (-> (org-element-at-point)
             (org-element-lineage '( headline inlinetask
                                     plain-list item
                                     table table-row)
                                  :with-self)
             (org-element-type))
    ;; Add a new list item (carrying over checkboxes if necessary)
    ((or 'plain-list 'item)
     (let ((orig-point (point))
           (check-box? (progn (org-beginning-of-item)
                              (org-at-item-checkbox-p))))
       (if (< direction 0) (org-beginning-of-item) (org-end-of-item))
       (org-insert-item check-box?)))
    ;; Add a new table row
    ((or `table `table-row)
     (org-table-insert-row (natnump direction)))
    ;; Otherwise, insert a new heading.
    (_
     ;; We intentionally avoid `org-insert-heading' and the like because they
     ;; impose unpredictable whitespace rules depending on the cursor position.
     ;; It's simpler to express this command's responsibility at a lower level
     ;; than work around all the quirks in org's API.
     (let ((level (or (org-current-level) 1)))
       (if (natnump direction)
           (let (org-insert-heading-respect-content)
             (goto-char (line-end-position))
             (org-end-of-subtree)
             (insert "\n\n" (make-string level ?*) " "))
         ;; else
         (org-back-to-heading)
         (insert (make-string level ?*) " ")
         (save-excursion (insert "\n")))
       (run-hooks 'org-insert-heading-hook))))
  (when (org-invisible-p) (org-show-hidden-entry))
  ;; (hel-insert-state 1)
  )

;;; AST climbing

(defun hel-org-element-in-section (&optional granularity)
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
      (cl-callf max beg (org-element-begin section))
      (cl-callf min end (org-element-end section))
      ;; Find smallest enclosing element within `section' element AST.
      (cl-loop with element = (hel-org--parse-element section granularity)
               for nested-element = (-find (lambda (el)
                                             (<= (org-element-begin el)
                                                 beg end
                                                 (org-element-end el)))
                                           (org-element-contents element))
               while nested-element
               do (setq element nested-element)
               finally return element))))

(cl-defun hel-org-parse-element (element
                                 &optional
                                 (root (if-let* ((section (org-element-lineage element 'section t)))
                                           (or (org-element-lineage section 'headline)
                                               section)))
                                 (granularity 'element))
  "Return the fully parsed structure of the ELEMENT.

ROOT will be the virtual-root of the result AST and should be one of
parents of the element. By default it will be the parent `section' element.

GRANULARITY specifies the parsing level (see `org-element-parse-buffer')."
  (if (eq element root)
      (hel-org--parse-element element granularity)
    ;; else
    (let ((beg (org-element-begin element))
          (end (org-element-end element))
          (type (org-element-type element))
          (element (hel-org--parse-element root granularity)))
      (while (and (setq element (-find (lambda (el)
                                         (<= (org-element-begin el)
                                             beg end
                                             (org-element-end el)))
                                       (org-element-contents element)))
                  (not (and (eq type (org-element-type element))
                            (= beg (org-element-begin element))
                            (= end (org-element-end element))))))
      element)))

(defun hel-org--parse-element (element &optional granularity)
  "Return the fully parsed structure of the ELEMENT.
GRANULARITY specifies the parsing level (see `org-element-parse-buffer')."
  (or granularity (setq granularity 'element))
  (save-excursion
    (with-restriction (org-element-begin element) (org-element-end element)
      (-> (org-element-parse-buffer granularity) ; -> org-data
          (org-element-contents)                 ; -> AST
          (car)))))                              ; -> ELEMENT node

(defun hel-org-at-heading-p ()
  (if (use-region-p)
      (save-excursion
        (goto-char (region-beginning))
        (org-at-heading-p))
    (org-at-heading-p)))

(let (cache)
  (defun hel-org--current-element ()
    "Return org element (AST node) at point fully parsed."
    ;; Return cached value when appropriate.
    (if (and cache
             (memq last-command '(hel-org-down-element
                                  hel-org-next-element
                                  hel-org-previous-element
                                  org-cycle                           ; TAB
                                  hel-smooth-scroll-line-to-eye-level ; zz
                                  hel-smooth-scroll-line-to-center    ; zz
                                  hel-smooth-scroll-line-to-top       ; zt
                                  hel-smooth-scroll-line-to-bottom))) ; zb
        cache
      (setq cache (hel-org-element-in-section))))
  ;;
  (defun hel-org--set-current-element (element)
    "Update org element at point cache."
    (setq cache (if (org-element-type-p element 'headline)
                    nil
                  element))))

;; M-o
(hel-define-command hel-org-up-element (&optional arg)
  "Expand region to the parent element.
ARG is used to determine whether invocation was interactive and should not
be set manually."
  :multiple-cursors t
  :merge-selections t
  (interactive "p")
  (hel-restore-newline-at-eol)
  ;; BUG: When paragraph starts with link, currently we lands at invisible
  ;;   position, skip it and select the entire heading.
  (when (and (not (bobp))
             (invisible-p (1- (point))))
    (goto-char (previous-single-char-property-change (point) 'invisible)))
  (-let* (((beg . end) (if (use-region-p)
                           (car (region-bounds))
                         (cons (point) (point))))
          (element (save-excursion
                     (goto-char beg)
                     (org-element-at-point))))
    ;; Climb up the tree until element fully contains region.
    (while (and element
                (or (invisible-p (org-element-begin element))
                    (org-element-type-p element 'section) ; skip section
                    (let ((element-beg (org-element-begin element))
                          (element-end (org-element-end element))
                          ;; (element-end (- (org-element-end element)
                          ;;                 (org-element-post-blank element)))
                          )
                      (< beg element-beg)
                      (< element-end end)
                      (and (= beg element-beg)
                           (= element-end end)))))
      (setq element (org-element-parent element)))
    (if (or (not element)
            (org-element-type-p element 'org-data))
        (user-error "No enclosing element")
      ;; else
      (hel-org--set-current-element nil)
      (hel-set-region (org-element-begin element)
                      (org-element-end element)
                      ;; (- (org-element-end element)
                      ;;    (org-element-post-blank element))
                      -1 :adjust)
      (if arg (hel-reveal-point-when-on-top)))))

;; M-i
(hel-define-command hel-org-down-element ()
  "Contract region to the first child element."
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (if (use-region-p)
      (-if-let (child (if (hel-org-at-heading-p)
                          (save-excursion
                            (goto-char (region-beginning))
                            (when-let* ((pos (-> (org-element-at-point)
                                                 (org-element-contents-begin))))
                              (goto-char pos)
                              (let ((child (org-element-at-point)))
                                (if (org-element-type-p child 'headline)
                                    child
                                  (hel-org-parse-element child)))))
                        ;; else
                        (-> (hel-org--current-element)
                            (org-element-contents)
                            (car-safe))))
          (progn
            (save-excursion
              (goto-char (region-beginning))
              (while (org-invisible-p (line-end-position))
                (org-cycle)))
            (hel-org--set-current-element child)
            (hel-set-region (org-element-begin child)
                            (org-element-end child)
                            ;; ;; Skip empty lines
                            ;; (- (org-element-end child)
                            ;;    (org-element-post-blank child))
                            -1 :adjust)
            (hel-reveal-point-when-on-top))
        ;; (user-error "No content for this element")
        (user-error "No nested element"))
    ;; else
    (hel-org-up-element)
    (when (hel-org-at-heading-p)
      (hel-org-down-element))))

;; M-n
(hel-define-command hel-org-next-element ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (unless (use-region-p)
    (hel-org-up-element))
  (when-let ((next (hel-org--next-element)))
    (hel-org--set-current-element next)
    (hel-set-region (org-element-begin next)
                    (org-element-end next)
                    ;; ;; Skip empty lines
                    ;; (- (org-element-end next)
                    ;;    (org-element-post-blank next))
                    (hel-region-direction) :adjust)
    (hel-reveal-point-when-on-top)))

(defun hel-org--next-element ()
  (if (hel-org-at-heading-p)
      (save-excursion
        (goto-char (region-beginning))
        (org-forward-heading-same-level 1)
        (org-element-at-point))
    ;; else
    (let ((current (hel-org--current-element)))
      (or
       ;; Try to find the node in parent directly after ELEMENT.
       (let ((siblings (-> (org-element-parent current)
                           (org-element-contents))))
         (nth (1+ (-find-index (lambda (elem) (eq elem current))
                               siblings))
              siblings))
       ;; No following element in current `section', than check `headline'
       ;; directly after `section'.
       (if-let* (((org-element-type-p (hel-org-element-parent current)
                                      'section))
                 (parent (org-element-lineage (org-element-at-point)
                                              '(headline org-data)))
                 (element (save-excursion
                            (goto-char (org-element-end current))
                            (org-element-at-point)))
                 ((eq parent (org-element-parent element))))
           element)))))

(defun hel-org-element-parent (element)
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
(hel-define-command hel-org-previous-element ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (unless (use-region-p)
    (hel-org-up-element))
  (when-let ((previous (hel-org--previous-element)))
    (hel-org--set-current-element previous)
    (hel-set-region (org-element-begin previous)
                    (org-element-end previous)
                    ;; (- (org-element-end previous)
                    ;;    (org-element-post-blank previous))
                    (hel-region-direction) :adjust)
    (hel-reveal-point-when-on-top)))

(defun hel-org--previous-element ()
  (if (hel-org-at-heading-p)
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
                    (hel-org-parse-element)
                    (org-element-contents)
                    (org-last)))))))
    ;; else
    (let* ((current (hel-org--current-element))
           (siblings (org-element-contents (org-element-parent current))))
      ;; Try to find the node in PARENT previous to ELEMENT.
      (nth (1- (-find-index (lambda (elem) (eq elem current))
                            siblings))
           siblings))))

;;; Things

;; `hel-org-sentence' thing
(put 'hel-org-sentence 'forward-op (lambda (count)
                                     (hel-motion-loop (dir count)
                                       (ignore-errors
                                         (if (natnump dir)
                                             (org-forward-sentence)
                                           (org-backward-sentence))))))

;;; Surround

(hel-define-command hel-mark-inner-org-emphasis ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when (org-in-regexp org-emph-re 2)
    (hel-set-region (match-beginning 4) (match-end 4)))
  ;; (when-let* ((bounds (bounds-of-thing-at-point 'defun))
  ;;             (nlines (count-lines (car bounds) (point)))
  ;;             ((org-in-regexp org-emph-re nlines)))
  ;;   (hel-set-region (match-beginning 4) (match-end 4)))
  )

(hel-define-command hel-mark-an-org-emphasis ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when (org-in-regexp org-emph-re 2)
    (hel-set-region (match-beginning 2) (match-end 2))))

(hel-define-command hel-mark-inner-org-verbatim ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when (org-in-regexp org-verbatim-re 2)
    (hel-set-region (match-beginning 4) (match-end 4))))

(hel-define-command hel-mark-an-org-verbatim ()
  :multiple-cursors t
  :merge-selections t
  (interactive)
  (when (org-in-regexp org-verbatim-re 2)
    (hel-set-region (match-beginning 2) (match-end 2))))

(defun hel-surround-settings-for-org-mode ()
  "Configure Hel surround functionality for Org-mode."
  (dolist (char '(?/ ?* ?_ ?+ ?= ?~))
    (push `(,char :pair ,(cons (char-to-string char)
                               (char-to-string char))
                  :lookup hel-surround--4-bounds-of-org-emphasis)
          hel-surround-alist)))

(defun hel-surround--4-bounds-of-org-emphasis ()
  (when (org-in-regexp org-emph-re 2)
    (list (match-beginning 2)
          (match-beginning 4)
          (match-end 2)
          (match-end 4))))

(provide 'hel-org)
;;; hel-org.el ends here
