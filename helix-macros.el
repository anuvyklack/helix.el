;;; helix-macros.el --- Macros -*- lexical-binding: t; -*-
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
;;; Code:

(require 'cl-lib)
(require 'dash)

(defmacro helix-defvar-local (symbol &optional initvalue docstring)
  "The same as `defvar-local' but additionaly marks SYMBOL as permanent
buffer local variable."
  (declare (indent defun)
           (doc-string 3)
           (debug (symbolp &optional form stringp)))
  `(prog1 (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

(defmacro helix-add-to-alist (alist &rest elements)
  "Add the association of KEY and VAL to the value of ALIST.
If the list already contains an entry for KEY, update that entry;
otherwise prepend it to the list.

\(fn ALIST [KEY VAL]...)"
  (declare (indent defun))
  `(progn
     ,@(cl-loop for (key val) on elements by #'cddr
                collect `(setf (alist-get ,key ,alist nil nil #'equal)
                               ,val))
     ,alist))

(defvar helix--advices nil
  "Inner variable for `helix-define-advice'.")

(defmacro helix-define-advice (symbol args &rest body)
  "Wrapper around `define-advice' that automatically add/remove advice
when `helix-mode' is toggled on or off.

\(fn SYMBOL (HOW LAMBDA-LIST &optional NAME) &rest BODY)"
  (declare (indent 2) (doc-string 3) (debug (sexp sexp def-body)))
  (unless (listp args)
    (signal 'wrong-type-argument (list 'listp args)))
  (unless (<= 2 (length args) 4)
    (signal 'wrong-number-of-arguments (list 2 4 (length args))))
  (let* ((how (nth 0 args))
         (lambda-list (nth 1 args))
         (name (or (nth 2 args) 'helix))
         (advice (intern (format "%s@%s" symbol name))))
    `(prog1 (defun ,advice ,lambda-list ,@body)
       (cl-pushnew '(,symbol ,how ,advice) helix--advices
                   :test #'equal)
       (when helix-mode
         (advice-add ',symbol ,how #',advice)))))

(defmacro helix-advice-add (symbol how function)
  "Wrapper around `advice-add' that automatically add/remove advice
when `helix-mode' is toggled on or off"
  `(progn
     (cl-pushnew (list ,symbol ,how ,function) helix--advices
                 :test #'equal)
     (when helix-mode
       (advice-add ,symbol ,how ,function))))

(defmacro helix-with-restriction (start end &rest body)
  "Evaluate BODY with the buffer narrowed to START and END."
  (declare (indent 2) (debug t))
  (let ((beg1 (gensym "beg"))
        (end1 (gensym "end")))
    `(if-let* ((,beg1 ,start)
               (,end1 ,end))
         (save-restriction
           (narrow-to-region ,beg1 ,end1)
           ,@body)
       ;; else
       ,@body)))

(defmacro helix-with-recenter-point-on-jump (&rest body)
  "Recenter point on jumps if it lands out of the screen."
  (declare (indent 0) (debug t))
  `(let ((scroll-conservatively 0))
     (prog1 (progn ,@body)
       ;; Update the screen so that the temporary value for
       ;; `scroll-conservatively' is taken into account.
       (redisplay))))

(defmacro helix-save-region (&rest body)
  "Evaluate BODY with preserving original region.
The difference from `save-mark-and-excursion' is that both point and mark are
saved as markers and correctly handle case when text was inserted before region."
  (declare (indent 0) (debug t))
  (let ((pnt (gensym "point"))
        (beg (gensym "region-beg"))
        (end (gensym "region-end"))
        (dir (gensym "region-dir"))
        (line-selection? (gensym "linewise")))
    `(if (use-region-p)
         (let ((deactivate-mark nil)
               (,beg (copy-marker (region-beginning) t))
               (,end (copy-marker (region-end)))
               (,dir (helix-region-direction))
               (,line-selection? helix-linewise-selection))
           (unwind-protect
               (save-excursion ,@body)
             (helix-set-region ,beg ,end ,dir)
             (setq helix-linewise-selection
                   (and ,line-selection?
                        (helix-logical-lines-p ,beg (1+ ,end))))
             (set-marker ,beg nil)
             (set-marker ,end nil)))
       ;; else
       (let ((,pnt (copy-marker (point-marker) t)))
         (unwind-protect
             (save-excursion ,@body)
           (goto-char ,pnt)
           (set-marker ,pnt nil))))))

(defmacro helix-define-keymap-with-digit-argument (symbol &rest definitions)
  "Make sparse keymap, define it as prefix command and bind it to SYMBOL.

Binds digits 0-9 to a special command that sets the numeric argument
while keeping the keymap active.

SYMBOL should be an unquoted symbol.

Keywords:
`:parent'   Keymap that will be used as parent (see `set-keymap-parent').

KEY/DEFINITION pairs are as KEY and DEF in `keymap-set'.

\(fn SYMBOL &key PARENT &rest [KEY DEFINITION]...)"
  (declare (indent 1) (debug t))
  (let* ((digit-argument-fun-name (format "%s-digit-argument" symbol))
         (digit-argument-fun-symbol (make-symbol digit-argument-fun-name))
         (parent-map (pcase (car-safe definitions)
                       (:parent (pop definitions)
                                (pop definitions)))))
    `(progn
       (defvar ,symbol nil)
       (define-prefix-command ',symbol)
       ,@(when parent-map
           `((set-keymap-parent ,symbol ,parent-map)))

       ;; Digit argument command
       (defun ,digit-argument-fun-symbol (arg)
         ,(format "Like `digit-argument' but keep `%s' active." symbol)
         (interactive "P")
         (digit-argument arg)
         (set-transient-map ,symbol))
       (put ',digit-argument-fun-symbol 'multiple-cursors 'false)

       ;; Do not show keys binded to our digit argument command
       ;; in which-key popup.
       (with-eval-after-load 'which-key
         (defvar which-key-replacement-alist)
         (cl-pushnew '((nil . ,digit-argument-fun-name) . ignore)
                     which-key-replacement-alist
                     :test #'equal))

       ;; Set keybindings for 0-9 keys
       ,@(cl-loop for i from 0 to 9 collect
                  `(keymap-set ,symbol ,(format "%s" i) #',digit-argument-fun-symbol))
       ;; Set KEY DEFINITION pairs
       ,@(cl-loop for (key def) in (-partition 2 definitions)
                  collect
                  `(keymap-set ,symbol ,key ,def))
       ',symbol)))

(provide 'helix-macros)
;;; helix-macros.el ends here
