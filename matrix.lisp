;;;; matrix.lisp

(in-package #:matrix)


;;; Default matrix element type
(defparameter *default-type* t)


;;; Matrix structure
(defstruct (matrix (:print-function print-matrix))
  (rows 0)
  (cols 0)
  (type t)
  (data nil))


;;; Reture the dimentions of matrix in a list form
(defun matrix-dimentions (matrix)
  (list (matrix-rows matrix)
        (matrix-cols matrix)))


;;; A iterator for 2 level loop with anaphor macro (i & j)
;;; (i,j) has value vary from (`start-row',`start-col') to
;;; (`end-row'-1,`end-col'-1)
(defmacro with-i-j ((end-row end-col &key (start-row 0) (start-col 0))
                    &body body)
  `(loop for i from ,start-row below ,end-row
      do (loop for j from ,start-col below ,end-col
            do (progn ,@body))))


;;; A iterator for matrix
;;; The given specifier `elt' can be used to access the element of
;;; a rectangle area of matrix [ from (`start-row',`start-col') to
;;; (`end-row'-1,`end-col'-1) ]. `i' and `j' can be used
;;; to get current accessing index.
(defmacro with-matrix ((elt matrix &key (start-row 0) (start-col 0)
                            (end-row nil) (end-col nil))
                       &body body)
  (with-gensyms (max-row max-col data)
    `(let ((,max-row (or ,end-row (matrix-rows ,matrix)))
           (,max-col (or ,end-col (matrix-cols ,matrix)))
           (,data    (matrix-data ,matrix)))
       (loop for i from ,start-row below ,max-row
          do (loop for j from ,start-col below ,max-col
                do (progn
                     ,@(subst `(aref ,data i j) `,elt `,body)))))))


;;; Matrix pretty print function
(defun print-matrix (matrix stream depth)
  (declare (ignore depth))
  (with-slots (rows cols data) matrix
    (let ((elt-len-arr (make-array `(,rows ,cols) :element-type '(unsigned-byte 8)))
          (max-length 0))
      (with-i-j (rows cols)
        (let ((len (length (write-to-string (aref data i j)))))
          (setf (aref elt-len-arr i j) len)
          (when (> len max-length)
            (setf max-length len))))
      (dotimes (i rows)
        (format stream "~&")
        (dotimes (j cols)
          (format stream "~A~A"
                  ;; make sure at least 2 spaces to separate datas
                  (add-spaces "  " (- max-length (aref elt-len-arr i j)))
                  (aref data i j)))))))


;;; Transform a vector to a 2 dimentions array
(defun vector-to-array2d (vector rows cols &key (type 'fixnum))
  (assert (= (length vector) (* rows cols)))
  (let ((array2d (make-array (list rows cols) :element-type type)))
    (with-i-j (rows cols)
      (setf (aref array2d i j) (aref vector (+ (* cols i) j))))
    array2d))


;;; Return a copy of the given 2 dimentions array
(defun copy-array2d (array2d rows cols &key (type 'fixnum))
  (let ((src-rows (array-dimension array2d 0))
        (src-cols (array-dimension array2d 1)))
    (assert (and (= src-rows rows) (= src-cols cols)))
    (let ((dst-array2d (make-array (list rows cols) :element-type type)))
      (with-i-j (rows cols)
        (setf (aref dst-array2d i j) (aref array2d i j)))
      dst-array2d)))


;;; Transform a list to a 2 dimentions array
(defun list-to-array2d (list rows cols &key (type 'fixnum))
  (let ((list-length (length list)))
    (assert (or (= list-length (* rows cols))
                (= list-length rows)))
    (let ((array2d (make-array (list rows cols) :element-type type)))
      (if (= list-length rows)
          (loop for lst in list
             for i = 0 then (+ i 1)
             do (loop for e in lst
                   for j = 0 then (+ j 1)
                   do (setf (aref array2d i j) e)))
          (loop for e in list
             for k = 0 then (+ k 1)
             do (multiple-value-bind (i j) (floor k cols)
                  (setf (aref array2d i j) e))))
      array2d)))


;;; Constructor function of matrix, build a matrix
(defun matrix (rows cols &key (initial-element 0) initial-contents (type *default-type*))
  (let ((data (if (null initial-contents)
                  (make-array (list rows cols)
                              :element-type type
                              :initial-element initial-element)
                  (funcall (typecase initial-contents
                             (list        #'list-to-array2d)
                             ((array * 1) #'vector-to-array2d)
                             ((array * 2) #'copy-array2d)
                             (otherwise (error "Unsupported initial method.")))
                           initial-contents rows cols :type type))))
    (make-matrix :rows rows :cols cols :type type :data data)))


;;; A easier way to build matrix
;;; Usage:
;;;   #m()              =>   rows = 0, cols = 0, data = #2A()
;;;   #2m()             =>   rows = 2, cols = 2, data = #2A((0 0) (0 0))
;;;   #3m(1)            =>   rows = 3, cols = 3, data = #2A((1 1 1) (1 1 1) (1 1 1))
;;;   #2m(1 2)          =>   rows = 2, cols = 2, data = #2A((1 0) (0 2))
;;;   #m(1 2 3 4)       =>   rows = 1, cols = 4, data = #2A((1 2 3 4))
;;;   #m((1 2) ( 2 1))  =>   rows = 2, cols = 2, data = #2A((1 2) (2 1))
(set-dispatch-macro-character #\# #\m
                              #'(lambda (stream c n)
                                  (declare (ignore c))
                                  (let ((lst (read stream nil (values) t)))
                                    (if n
                                        (cond ((null lst) (matrix n n))
                                              ((null (cdr lst)) (matrix n n :initial-element (car lst)))
                                              (t (diag n n :diagonal-contents lst)))
                                        (if (atom (car lst))
                                            (matrix 1 (length lst) :initial-contents (list lst))
                                            (matrix (length lst) (length (car lst)) :initial-contents lst))))))


;;; Set diagonal elements with elements in diagonal-contents
(defun diagonal-from-contents (matrix diagonal-contents)
  (let ((data (matrix-data matrix)))
    (typecase diagonal-contents
      (list (loop for e in diagonal-contents
               for i = 0 then (+ i 1)
               do (setf (aref data i i) e)))
      ((array * 1) (dotimes (i (min (matrix-rows matrix)
                                    (matrix-cols matrix)))
                     (setf (aref data i i) (aref diagonal-contents i))))
      (otherwise (error "Unsupported contents type.")))))


;;; Make the given matrix to be a diagonal matrix with given element as
;;; diagonal elements or with given vector as diagonal elements
(defun diagonalf (matrix &key (diagonal-element 1) diagonal-contents)
  (if (null diagonal-contents)
      (with-slots (rows cols data) matrix
        (with-i-j (rows cols)
          (if (= i j)
              (setf (aref data i j) diagonal-element)
              (setf (aref data i j) 0))))
      (diagonal-from-contents matrix diagonal-contents))
  matrix)

(alias diagf diagonalf)


;;; Build a diagonal matrix which has all elements in the given vector/list
(defun diagonal-matrix (rows cols &key (diagonal-element 1) diagonal-contents (type *default-type*))
  (let ((matrix (matrix rows cols :type type)))
    (if (null diagonal-contents)
        (let ((data (matrix-data matrix)))
          (dotimes (i (min rows cols))
            (setf (aref data i i) diagonal-element)))
        (diagonal-from-contents matrix diagonal-contents))
    matrix))

(alias diag diagonal-matrix)


;; 判断是否是对角矩阵的谓词
;; Predicate of if the given matrix is a diag matrix
(defun diagonal-matrix-p (matrix)
  (with-slots (rows cols data) matrix
    (loop for i from 0 below rows
       do (unless (loop for j from 0 below cols
                     ;; when there is a nonzero element not on diagonal
                     do (when (and (/= (aref data i j) 0)
                                   (/= i j))
                          (return nil))
                     finally (return t))
            (return nil))
       finally (return t))))

(alias diagp diagonal-matrix-p)


;; 单位矩阵构造函数
;; Build a `n order' identity matrix (eye matrix)
(defun identity-matrix (n &key (type *default-type*))
  (diag n n :type type))

(alias eye identity-matrix)


;; 判断是否是单位矩阵的谓词
;; Predicate of if the given matrix is an identity matrix
(defun identity-matrix-p (matrix)
  (with-slots (rows cols data) matrix
    (and (= rows cols)
         (> rows 0)
         (> cols 0)
         (loop for i from 0 below rows
            do (unless (loop for j from 0 below cols
                          do (when (if (= i j)
                                       (/= (aref data i j) 1)
                                       (/= (aref data i j) 0))
                               (return nil))
                          finally (return t))
                 (return nil))
            finally (return t)))))

(alias eyep identity-matrix-p)


;; Build a new matrix with the same content of mat
(defun copy-matrix (matrix)
  (with-slots (rows cols type data) matrix
    (let* ((copy (matrix rows cols :type type))
           (data-of-copy (matrix-data copy)))
      (with-i-j (rows cols)
        (setf (aref data-of-copy i j) (aref data i j)))
      copy)))


;;; Matrix binary addition
(defun madd (matrix1 matrix2 &key type)
  (with-slots ((rows1 rows) (cols1 cols) (type1 type) (data1 data)) matrix1
    (with-slots ((rows2 rows) (cols2 cols) (type2 type) (data2 data)) matrix2
      (assert (and (= rows1 rows2) (= cols1 cols2)))
      (let* ((type3 (or type (type-strategy type1 type2)))
             (matrix3 (matrix rows1 cols1 :type type3))
             (data3 (matrix-data matrix3)))
          (with-i-j (rows1 cols1)
            (setf (aref data3 i j)
                  (+ (aref data1 i j) (aref data2 i j))))
          matrix3))))


;; Extended matrix addition, &rest argment supported
;; m+ can add n matrices together (n >= 1)
(defun m+ (matrix &rest matrices)
  (reduce #'madd (cons matrix matrices)))


;;; Matrix binary subtraction
(defun msub (matrix1 matrix2 &key type)
  (with-slots ((rows1 rows) (cols1 cols) (type1 type) (data1 data)) matrix1
    (with-slots ((rows2 rows) (cols2 cols) (type2 type) (data2 data)) matrix2
      (assert (and (= rows1 rows2) (= cols1 cols2)))
      (let* ((type3 (or type (type-strategy type1 type2)))
             (matrix3 (matrix rows1 cols1 :type type3))
             (data3 (matrix-data matrix3)))
        (with-i-j (rows1 cols1)
          (setf (aref data3 i j)
                (- (aref data1 i j) (aref data2 i j))))
        matrix3))))


;;; 矩阵取反运算
;;; Build a new matrix with each element negative of the corresponding one in mat
(defun mminus (matrix)
  (with-slots (rows cols type) matrix
    (let* ((result (matrix rows cols :type type))
           (data (matrix-data result)))
      (with-matrix (e matrix :end-row rows :end-col cols)
        (setf (aref data i j) (- e)))
      result)))


;;; 拓展的矩阵减法
;;; Extended subtraction of matrix
;;; When there is only one parameter passed, call function #'mminus instead of #'msub
(defun m- (matrix &rest matrices)
  (if (null matrices)
      (mminus matrix)
      (reduce #'msub (cons matrix matrices))))


;;; 二元矩阵乘法运算
;;; Binary multiplication of matrix
(defun mmul (matrix1 matrix2 &key type)
  (with-slots ((rows1 rows) (cols1 cols) (type1 type) (data1 data)) matrix1
    (with-slots ((rows2 rows) (cols2 cols) (type2 type) (data2 data)) matrix2
      (assert (= cols1 rows2))
      (let* ((type3 (or type (type-strategy type1 type2)))
             (matrix3 (matrix rows1 cols2 :type type3)))
      (with-matrix (e matrix3)
        (setf e (loop for k from 0 below cols1
                     sum (* (aref data1 i k) (aref data2 k j)))))
      (if (and (= rows1 1) (= cols2 1))
          (aref (matrix-data matrix3) 0 0)
          matrix3)))))


;;; 矩阵数乘运算
;;; Compute a matrix multiplied by a number
(defun nmul (num matrix)
  (with-slots (rows cols type data) matrix
    (let ((result (matrix rows cols :type type)))
      (with-matrix (e result :end-row rows :end-col cols)
        (setf e (* num (aref data i j))))
      result)))


;;; 混合二元乘法, 两个参数相乘, 每个参数要么是数字要么是矩阵
;;; Mixed binary multiplication of matrix
;;; The two parameters are both required either a number or a matrix
(defun mix* (num/mat1 num/mat2)
  (cond ((numberp num/mat1)
         (cond ((numberp num/mat2) (* num/mat1 num/mat2))
               (t (nmul num/mat1 num/mat2))))
        ((numberp num/mat2) (nmul num/mat2 num/mat1))
        (t (mmul num/mat1 num/mat2))))


;;; 拓展的矩阵乘法
;;; Extended multiplication of matrix
;;; Function #'m* can take more than one parameter, and each of them should be
;;; either a number or a matrix
(defun m* (num/mat &rest nums/mats)
  (reduce #'mix* (cons num/mat nums/mats)))


;;; 矩阵乘方(矩阵幂运算)
;;; Exponentiation of matrix
(defun mexpt (matrix power)
  (with-slots (rows cols type) matrix
    (assert (= rows cols))
    (let ((result (eye rows :type type)))
      (dotimes (i power)
        (setf result (m* result matrix)))
      result)))


;;; 矩阵转置
;;; Matrix Transposion
(defun trans (matrix)
  (with-slots (rows cols type data) matrix
    (let ((result (matrix cols rows :type type)))
      (with-matrix (e result)
        (setf e (aref data j i)))
      result)))


;;; 对矩阵的每个元素进行操作
;;; Do the given function on each element of mat
(defun mapeach (function matrix)
  (with-slots (rows cols type data) matrix
    (let ((result (matrix rows cols :type type)))
      (with-matrix (e result :end-row rows :end-col cols)
        (setf e (funcall function (aref data i j))))
      result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 初等行变换
;;; Elementary row operation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 倍法变换
;;; Row multiplication
(defun matrix-row-multiplyf (matrix i k)
  (with-slots (cols data) matrix
    (dotimes (col cols)
      (setf (aref data i col) (* k (aref data i col))))
    matrix))


;;; 消法变换
;;; Row addition
(defun matrix-row-addf (matrix i j k)
  (with-slots (cols data) matrix
    (dotimes (col cols)
      (incf (aref data i col)
            (* k (aref data j col))))
    matrix))


;;; 换法变换
;;; Row switching
(defun matrix-row-switchf (matrix i j)
  (with-slots (cols data) matrix
    (dotimes (col cols)
      (rotatef (aref data i col) (aref data j col)))
    matrix))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 初等列变换
;;; Elementary col operation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 倍法变换
;;; Col multiplication
(defun matrix-col-multiplyf (matrix i k)
  (with-slots (rows data) matrix
    (dotimes (row rows)
      (setf (aref data row i) (* k (aref data row i))))
    matrix))

;;; 消法变换
;;; Col addition
(defun matrix-col-addf (matrix i j k)
  (with-slots (rows data) matrix
    (dotimes (row rows)
      (incf (aref data row j) (* k (aref data row i))))
    matrix))

;;; 换法变换
;;; Col switching
(defun matrix-col-switchf (matrix i j)
  (with-slots (rows data) matrix
    (dotimes (row rows)
      (rotatef (aref data row i) (aref data row j)))
    matrix))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 二维数组初等行变换
;;; Elementary row operation on array2d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 倍法变换
;; Row multiplication on array2d
(defun row-multiplyf (array2d i k)
  (let ((cols (array-dimension array2d 1)))
    (loop for col from 0 below cols
       do (setf (aref array2d i col) (* k (aref array2d i col))))
    array2d))


;; 消法变换
;; Row addition on array2d
(defun row-addf (array2d i j k)
  (let ((cols (array-dimension array2d 1)))
    (loop for col from 0 below cols
       do (incf (aref array2d i col) (* k (aref array2d j col))))
    array2d))


;; 换法变换
;; Row switching on array2d
(defun row-switchf (array2d i j)
  (let ((cols (array-dimension array2d 1)))
    (loop for col from 0 below cols
       do (rotatef (aref array2d i col) (aref array2d j col)))
    array2d))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 初等行变换
;;; Elementary row operation on array2d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 倍法变换
;; Col multiplication on array2d
(defun col-multiplyf (array2d i k)
  (let ((rows (array-dimension array2d 0)))
    (loop for row from 0 below rows
       do (setf (aref array2d row i) (* k (aref array2d row i))))
    array2d))


;; 消法变换
;; Col addition on array2d
(defun col-addf (array2d i j k)
  (let ((rows (array-dimension array2d 0)))
    (loop for row from 0 below rows
       do (incf (aref array2d row j) (* k (aref array2d row i))))
    array2d))


;; 换法变换
;; Col switching on array2d
(defun col-switchf (array2d i j)
  (let ((rows (array-dimension array2d 0)))
    (loop for row from 0 below rows
       do (rotatef (aref array2d row i) (aref array2d row j)))
    array2d))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ends


;;; 统计矩阵第 row 行中前导的 0 的个数
;;; Aux function
;;; Count how many zeros are there in row-th row before a non-zero element
(defun count-prefix-zeros (array2d row)
  (let ((cnt 0))
    (dotimes (col (array-dimension array2d 1))
      (if (zerop (aref array2d row col))
          (incf cnt)
          (return cnt)))
    cnt))


;;; 将矩阵按照每行前导的 0 的个数的升序重排行
;;; Aux function
;;; Rearrange the matrix by quantity of prefixed zeros
(defun rearrangef (matrix)
  (with-slots (rows data) matrix
    (let ((nums (make-array rows :element-type '(unsigned-byte 8))))
      (dotimes (i rows)
        (setf (aref nums i) (count-prefix-zeros data i)))
      (loop for k from 0 below rows
         do (loop for i from 1 below rows
               do (let ((j (- i 1)))
                    (when (< (aref nums i) (aref nums j))
                      (rotatef (aref nums i) (aref nums j))
                      (row-switchf data i j)))))
      matrix)))


;;; 化为行阶梯矩阵
;;; Gaussian elimination (row reduction)
;;; Row echelon form
(defun row-echelon (matrix)
  (let ((tmat (copy-matrix matrix)))
    (rearrangef tmat)
    (let ((tdata (matrix-data tmat)))
      (with-slots (rows cols data) matrix
        (loop for i from 0 below (1- rows)
           do (let ((pos (count-prefix-zeros tdata i)))
                (loop for j from (1+ i) below rows
                   do (when (/= pos cols)
                        (when (/= (aref tdata j pos) 0)
                          (row-addf tdata j i
                                    (- (/ (aref tdata j pos) (aref tdata i pos)))))))))))
    tmat))


;;; 化为行最简矩阵 (行规范型矩阵)
;;; Reduced row echelon form / row canonical form
(defun row-canonical (matrix)
  (let* ((tmat (row-echelon matrix))
         (tdata (matrix-data tmat)))
    (with-slots (rows cols) matrix
      (loop for j from (1- rows) downto 1
         do (let ((pos (count-prefix-zeros tdata j)))
              (when (/= pos cols)
                (row-multiplyf tdata j (/ 1 (aref tdata j pos)))
                (loop for i from (1- j) downto 0
                   do (when (/= (aref tdata i pos) 0)
                        (row-addf tdata i j (- (/ (aref tdata i pos) (aref tdata j pos))))))))))
    tmat))


;;; 化为列最简矩阵
;;; Reduced col echelon form / col canonical form
(defun col-canonical (matrix)
  (trans (rearrangef (row-canonical (trans matrix)))))


;;; 化为标准型矩阵
;;; Transform the mat to canonical form
(defun canonical (matrix)
  (col-canonical (row-canonical matrix)))


;;; 矩阵的行秩
;;; Row rank of matrix
(defun row-rank (matrix)
  (let ((tmat (row-echelon matrix)))
    (with-slots (rows cols) matrix
      (let ((rank rows)
            (tdata (matrix-data tmat)))
        (loop for i from (1- rows) downto 0
           do (let ((flag t))
                (loop for j from (1- cols) downto 0
                   do (when (/= (aref tdata i j) 0)
                        (setf flag nil)
                        (return)))
                (when flag (decf rank))))
        rank))))


;;; 矩阵的列秩
;;; Col rank of matrix
(defun col-rank (matrix)
  (row-rank (trans matrix)))


;;; 矩阵的秩
;;; Rank fo matrix
(defun rank (matrix)
  (min (row-rank (row-canonical matrix))
       (row-rank (row-canonical (trans matrix)))))


;;; 逆序数
;;; Compute the inversion of the vector vec's permutation
(defun inversion (vec)
  (let ((len (length vec))
        (cnt 0))
    (loop for i from 0 below (1- len)
       do (loop for j from (1+ i) below len
             do (when (< (svref vec j) (svref vec i))
                  (incf cnt))))
    cnt))


;;; 生成全排列
;;; 根据给定的数组生成包含该数组的所有全排列的列表
;;; Generate permutation
;;; Generate a list that includes all the permutations of the given vector
(defun permutation (vec)
  (let ((result nil))
    (labels ((perm (vec k len)
               (if (= k len)
                   (push (copy-seq vec) result)
                   (loop for i from k below len
                      do (progn (rotatef (svref vec i) (svref vec k))
                                (perm vec (1+ k) len)
                                (rotatef (svref vec k) (svref vec i)))))))
      (perm vec 0 (length vec)))
    result))


;;; 生成一个从 0 到 n-1 的顺序数组
;;; Generate an ordered vector includes numbers that from 0 to n-1
(defun gen-seq-vec (n)
  (make-array n :initial-contents (loop for i from 0 below n collect i)))


;;; 行列式
;;; Compute the determinant of a square matrix
(defun det (matrix)
  (with-slots (rows cols data) matrix
    (assert (= rows cols))
    (let ((permutations (permutation (gen-seq-vec rows)))
          (acc 0))
      (dolist (perm permutations)
        (let ((mul 1))                    ;multiplicative
          (dotimes (i rows)
            (setf mul (* mul (aref data i (svref perm i)))))
          (incf acc (* mul (expt -1 (inversion perm))))))
      acc)))


;;; 生成除第 i 行和第 j 行元素的子矩阵
;;; Generate the submatrix of the matrix mat that exclude i-th row and j-th colum elements
(defun submatrix (matrix i j)
  (with-slots (rows cols type data) matrix
    (let* ((rmat (matrix (1- rows) (1- cols) :type type))
           (rdata (matrix-data rmat)))
      (dotimes (r (1- rows))
        (if (< r i)
            (dotimes (c (1- cols))
              (if (< c j)
                  (setf (aref rdata r c) (aref data r c))
                  (setf (aref rdata r c) (aref data r (1+ c)))))
            (dotimes (c (1- cols))
              (if (< c j)
                  (setf (aref rdata r c) (aref data (1+ r) c))
                  (setf (aref rdata r c) (aref data (1+ r) (1+ c)))))))
      rmat)))


;;; 余子式
;;; Minor
(defun minor (matrix i j)
  (det (submatrix matrix i j)))


;;; 代数余子式
;;; Signed minor of a matrix
(defun cofactor (matrix i j)
  (* (expt -1 (+ i j))
     (minor matrix i j)))

(alias signed-minor cofactor)


;;; 伴随矩阵
;;; Adjugate
(defun adj (matrix)
  (with-slots (rows cols type) matrix
    (assert (= rows cols))
    (let ((result (matrix rows rows :type type)))
      (with-matrix (e result)
        (setf e (cofactor matrix j i)))
      result)))


;;; 矩阵的逆
;;; Inverse of matrix
(defun inv (matrix)
  (let ((detvalue (det matrix)))
    (assert (/= detvalue 0))
    (m* (/ 1 detvalue) (adj matrix))))


;;; 矩阵的迹
;;; Trace of matrix
(defun tr (matrix)
  (with-slots (rows cols data) matrix
    (assert (= rows cols))
    (loop for i from 0 below rows sum (aref data i i))))


;;; 计算向量的内积(数量积)
;;; Compute inner product(scalar product) of two vector
(defun dot (vec1 vec2)
  (with-slots ((rows1 rows) (cols1 cols)) vec1
    (with-slots ((rows2 rows) (cols2 cols)) vec2
      (assert (and (= (min rows1 cols1) 1)
                   (= (min rows2 cols2) 1)
                   (= (max rows1 cols1) (max rows2 cols2))))
      (let ((v1 (if (= rows1 1)
                    vec1
                    (trans vec1)))
            (v2 (if (= cols2 1)
                    vec2
                    (trans vec2))))
        (m* v1 v2)))))


;;; A &rest argument version of dot (inner product) operation
(defun dot-* (vector &rest vectors)
  (if (null vectors)
      vector
      (reduce #'dot (cons vector vectors))))


;;; 计算矩阵所有元素之和
;;; Compute the sum of all elements of the given mat
(defun msum (matrix)
  (let ((sum 0))
    (with-matrix (e matrix)
      (incf sum e))
    sum))


;;; p-范数
;;; Compute p-norm of vector vec
(defun norm (vec &optional (p 2))
  (with-slots (rows cols) vec
    (assert (or (= rows 1) (= cols 1)))
    (expt (msum (mapeach #'(lambda (x) (expt (abs x) p)) vec))
          (/ 1 p))))


;;; 欧几里得距离(欧式距离)
;;; Euclidean distance of two vectors
(defun euclidean-distance (vec1 vec2)
  (let ((vec (m- vec1 vec2)))
    (norm vec)))
