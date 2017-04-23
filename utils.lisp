;;;; utils.lisp
;;;; Some useful tool code for matrix library.

(defpackage #:matrix/utils
  (:use #:cl)
  (:export #:with-gensyms
           #:alias))

(in-package #:matrix/utils)


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


;;; Set nickname for functions
;;; e.g. (alias eye identity-matrix)
;;; After the setting, you can use function eye the same
;;; as identity-matrix
(defmacro alias (dst-name src-name)
  `(setf (symbol-function ',dst-name)
         (symbol-function ',src-name)))
