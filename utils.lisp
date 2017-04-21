;;;; utils.lisp
;;;; Some useful tool code. 

(in-package :matrix)


;;; Convinent tool for binding multi symbols to (gensym)
;;; Example:
;;; (defmacro with-redraw ((var objs) &body body)
;;;   (let ((gob (gensym))
;;;         (x0 (gensym)) (y0 (gensym))
;;;         (x1 (gensym)) (y1 (gensym)))
;;;     ...))
;;; Can be replaced by following:
;;; (defmacro with-redraw ((var objs) &body body)
;;;   (with-gensyms (gob x0 y0 x1 y1)
;;;     ...))
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))


;;; new-and-old is any number of pairs, a pair is
;;; a two element list, with first as new value and
;;; second element as old value
;;; e.g. (multi-subst tree '(a b) '(c d))
(defun multi-subst (tree &rest new-and-old)
  (labels ((rec-subst (tree new-and-old)
             (if (null new-and-old)
                 tree
                 (let ((lst (first new-and-old)))
                   (rec-subst (subst (first lst)
                                     (second lst)
                                     tree)
                              (cdr new-and-old))))))
    (rec-subst tree new-and-old)))


;;; Set nickname for functions
;;; e.g. (alias eye identity-matrix)
;;; After the setting, you can use function eye the same
;;; as identity-matrix
(defmacro alias (dst-name src-name)
  `(setf (symbol-function ',dst-name)
         (symbol-function ',src-name)))
