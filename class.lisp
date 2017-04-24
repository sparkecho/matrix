;;;; Class based matrix version

(defpackage #:matrix/class
  (:use #:cl)
  (:export #:matrix))

(in-package #:matrix/class)


(defclass matrix ()
  ((rows :reader matrix-rows :initarg :rows :type unsigned-byte)
   (cols :reader matrix-cols :initarg :cols :type unsigned-byte)
   (type :reader matrix-type :initarg :type :initform t)
   (data :accessor matrix-data :initarg :data))
  (:documentation "Matrix class."))


(defgeneric make-matrix (rows cols data type)
  (:documentation "Matrix constructor, use parameter data
initialize the matrix elements."))


(defmethod make-matrix (rows cols (data number) type)
  "Use a number to initialize the matrix elements"
  (let ((element-type (or type t)))
    (make-instance 'matrix :rows rows :cols cols :type element-type
                   :data (make-array (list rows cols) :element-type element-type
                                     :initial-element data))))


(defmethod make-matrix (rows cols (data symbol) type)
  "Use a symbol to initialize the matrix elements"
  (let ((element-type (or type 'symbol)))
    (make-instance 'matrix :rows rows :cols cols :type element-type
                   :data (make-array (list rows cols) :element-type element-type
                                     :initial-element data))))


(defmethod make-matrix (rows cols (data character) type)
  "Use a character to initialize the matrix elements"
  (let ((element-type (or type 'character)))
    (make-instance 'matrix :rows rows :cols cols :type element-type
                   :data (make-array (list rows cols) :element-type element-type
                                     :initial-element data))))


(defmethod make-matrix (rows cols (data string) type)
  "Use a string to initialize the matrix elements"
  (let ((element-type (or type 'string)))
    (make-instance 'matrix :rows rows :cols cols :type element-type
                   :data (make-array (list rows cols) :element-type element-type
                                     :initial-element data))))


(defmethod make-matrix (rows cols (data vector) type)
  "Use vector (one demension array) to initialize the matrix elements."
  (let ((element-type (or type (array-element-type data))))
    (make-instance 'matrix :rows rows :cols cols
                   :type element-type :data (vector-to-array2d data rows cols type))))


(defmethod make-matrix (rows cols (data array) type)
  "Use two demensions array to initialize the matrix elements."
  (let ((element-type (or type (array-element-type data))))
    (make-instance 'matrix :rows rows :cols cols
                   :type element-type :data (copy-array2d data rows cols type))))


(defmethod make-matrix (rows cols (data list) type)
  "Use list (one or two demensions) to initialize the matrix elements."
  (let ((element-type (or type (array-element-type data))))
    (make-instance 'matrix :rows rows :cols cols
                   :type element-type :data (list-to-array2d data rows cols type))))


(defmethod make-matrix (rows cols (data t) type)
  "Use data to initialize the matrix elements, when data is neither of types above"
  (let ((element-type (or type t)))
    (make-instance 'matrix :rows rows :cols cols :type element-type
                   :data (make-array (list rows cols) :element-type element-type
                                     :initial-element data))))



(defun matrix (rows cols &key initial-element initial-contents
                           (element-type nil type-supplied-p))
  "Matrix constructor"
  (when (and type-supplied-p (null element-type))
    (error "An attempt to access an array of element-type NIL was made."))
  (cond ((and (null initial-element)
              (null initial-contents))
         (make-matrix rows cols 0 element-type))
        ((null initial-element)  (make-matrix rows cols initial-contents element-type))
        ((null initial-contents) (make-matrix rows cols initial-element element-type))
        (t (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))))



;;; Transform a vector to a 2 dimentions array
(defun vector-to-array2d (vector rows cols type)
  "Transform a vector to a 2 dimentions array."
  (assert (= (length vector) (* rows cols)))
  (let ((array2d (make-array (list rows cols) :element-type type)))
    (dotimes (i rows)
      (dotimes (j cols)
        (setf (aref array2d i j) (aref vector (+ (* cols i) j)))))
    array2d))


;;; Return a copy of the given 2 dimentions array
(defun copy-array2d (array2d rows cols type)
  (let ((src-rows (array-dimension array2d 0))
        (src-cols (array-dimension array2d 1)))
    (assert (and (= src-rows rows) (= src-cols cols)))
    (let ((dst-array2d (make-array (list rows cols) :element-type type)))
      (dotimes (i rows)
        (dotimes (j cols)
          (setf (aref dst-array2d i j) (aref array2d i j))))
      dst-array2d)))


;;; Transform a list to a 2 dimentions array
(defun list-to-array2d (list rows cols type)
  (let ((list-length (length list))
        (total (* rows cols)))
    (cond ((= list-length rows)
           (make-array (list rows cols) :element-type type
                       :initial-contents list))
          ((= list-length total) (let ((array2d (make-array (list rows cols) :element-type type)))
                                   (loop for e in list
                                      for k = 0 then (+ k 1)
                                      do (multiple-value-bind (i j)
                                             (floor k cols)
                                           (setf (aref array2d i j) e)))
                                   array2d))
          (t (error "A ~A elements list or a nested list with ~A elements and each element is a ~A length list."
                    total rows cols)))))



;;; use macro, methods, functions to define matrix+, matrix-, matrix*
;;; and use type declaration to optimize
#|
*arr1* = 1000*1000, *arr2* = 1000*1000

(defun arr+ (arr1 arr2)
  (declare (type (simple-array (unsigned-byte 8) (* *)) arr1 arr2))
  (dotimes (i (array-dimension arr1 0))
    (dotimes (j (array-dimension arr1 1))
      (the (unsigned-byte 8) (+ (the (unsigned-byte 8) (aref arr1 i j))
                                (the (unsigned-byte 8) (aref arr2 i j)))))))
(time (dotimes (k 10) (arr+ *arr1* *arr2*))) ==> 0.063 seconds of real time

(defun arr+2 (arr1 arr2)
  (dotimes (i (array-dimension arr1 0))
    (dotimes (j (array-dimension arr1 1))
      (+ (aref arr1 i j) (aref arr2 i j)))))
(time (dotimes (k 10) (arr+2 *arr1* *arr2*))) ==> 0.259 seconds of real time

(defun arr+3 (arr1 arr2)
  (declare (type (simple-array (unsigned-byte 8) (1000 1000)) arr1 arr2))
  (dotimes (i (array-dimension arr1 0))
    (dotimes (j (array-dimension arr1 1))
      (the (unsigned-byte 8) (+ (the (unsigned-byte 8) (aref arr1 i j))
                                (the (unsigned-byte 8) (aref arr2 i j)))))))
(time (dotimes (k 10) (arr+3 *arr1* *arr2*))) ==> 0.039 seconds of real time
|#

#|
(declare (type (simple-array ,type (,rows ,cols)) array1))
(let ((result (make-array `(,rows ,cols) :element-type type))))
|#

#|
**
** type inferer
|#

;; (defmacro array+ (array1 array2 type1 type2 value-type &body body)
;;   (let ((array3 


;; (defmacro define-array- (name type1 type2 &body body))

;; (defmacro define-matrix* (name type1 type2 &body body))
