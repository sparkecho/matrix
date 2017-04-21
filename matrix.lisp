;;;; matrix.lisp

(in-package #:matrix)


;;; Matrix structure
(defstruct (matrix (:print-function print-matrix))
  (rows 0)
  (cols 0)
  (type 'fixnum)
  (data nil))


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
;;; (`end-row'-1,`end-col'-1) ]. `row-index' and `col-index' can be used
;;; to get current accessing index.
(defmacro with-matrix ((elt matrix &key (start-row 0) (start-col 0)
                            (end-row nil) (end-col nil))
                       &body body)
  (with-gensyms (max-row max-col data)
    `(let ((,max-row (or ,end-row (matrix-rows ,matrix)))
           (,max-col (or ,end-col (matrix-cols ,matrix)))
           (,data    (matrix-data ,matrix)))
       (loop for row-index from ,start-row below ,max-row
          do (loop for col-index from ,start-col below ,max-col
                do (progn
                     ,@(subst `(aref ,data row-index col-index) `,elt `,body)))))))

;; (defmacro with-matrices ((((elt1 matrix1) (elt2 matrix2) &rest args)
;;                           &key (start-row 0) (start-col 0) (end-row nil) (end-col nil))
;;                          &body body)
;;   `(list ',elt1 ',matrix1 ',elt2 ',matrix2 ',args ',start-row ',start-col ',end-row ',end-col ',body))


;;; Matrix pretty print function
(defun print-matrix (matrix stream depth)
  (declare (ignore depth))
  (let* ((data (matrix-data matrix))
         (rows (array-dimension data 0))
         (cols (array-dimension data 1))
         (max-length 0))
    (with-i-j (rows cols)
      (let ((len (length (write-to-string (aref data i j)))))
        (when (> len max-length)
          (setf max-length len))))
    (incf max-length 2)          ;make sure 2 spaces to separate datas
    (dotimes (i rows)
      (format stream "~&")
      (dotimes (j cols)
        (format-mincol stream max-length (aref data i j))))))


;;; Transform a vector to a 2 dimentions array
(defun vector-to-array2d (vector rows cols &key (element-type 'fixnum))
  (assert (= (length vector) (* rows cols)))
  (let ((array2d (make-array (list rows cols) :element-type element-type)))
    (with-i-j (rows cols)
      (setf (aref array2d i j) (aref vector (+ (* cols i) j))))
    array2d))


;;; Return a copy of the given 2 dimentions array
(defun copy-array2d (array2d rows cols &key (element-type 'fixnum))
  (let ((src-rows (array-dimension array2d 0))
        (src-cols (array-dimension array2d 1)))
    (assert (and (= src-rows rows) (= src-cols cols)))
    (let ((dst-array2d (make-array (list rows cols) :element-type element-type)))
      (with-i-j (rows cols)
        (setf (aref dst-array2d i j) (aref array2d i j)))
      dst-array2d)))

;;; Transform a list to a 2 dimentions array
(defun list-to-array2d (list rows cols &key (element-type 'fixnum))
  (let ((list-length (length list)))
    (assert (or (= list-length (* rows cols))
                (= list-length rows)))
    (let ((array2d (make-array (list rows cols) :element-type element-type)))
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
(defun matrix (rows cols &key (initial-element 0) initial-contents (element-type 'fixnum))
  (let ((data (if (null initial-contents)
                  (make-array (list rows cols)
                              :element-type element-type
                              :initial-element initial-element)
                  (funcall (typecase initial-contents
                             (list        #'list-to-array2d)
                             ((array * 1) #'vector-to-array2d)
                             ((array * 2) #'copy-array2d)
                             (otherwise (error "Unsupported initial method.")))
                           initial-contents rows cols :element-type element-type))))
    (make-matrix :rows rows :cols cols :type element-type :data data)))


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
      (let ((data (matrix-data matrix)))
        (with-i-j ((matrix-rows matrix)  (matrix-cols matrix))
          (if (= i j)
              (setf (aref data i j) diagonal-element)
              (setf (aref data i j) 0))))
      (diagonal-from-contents matrix diagonal-contents))
  matrix)

(alias diagf diagonalf)


;;; Build a diagonal matrix which has all elements in the given vector/list
(defun diagonal-matrix (rows cols &key (diagonal-element 1) diagonal-contents (type 'fixnum))
  (let ((matrix (matrix rows cols :element-type type)))
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
  (let ((data (matrix-data matrix)))
    (loop for i from 0 below (matrix-rows matrix)
       do (unless (loop for j from 0 below (matrix-cols matrix)
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
(defun identity-matrix (n &key (type 'fixnum))
  (diag n n :type type))

(alias eye identity-matrix)


;; 判断是否是单位矩阵的谓词
;; Predicate of if the given matrix is an identity matrix
(defun identity-matrix-p (matrix)
  (let ((rows (matrix-rows matrix))
        (cols (matrix-cols matrix))
        (data (matrix-data matrix)))
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
  (let* ((rows (matrix-rows matrix))
         (cols (matrix-cols matrix))
         (data (matrix-data matrix))
         (copy (matrix rows cols :element-type (matrix-type matrix)))
         (data-of-copy (matrix-data copy)))
    (with-i-j (rows cols)
      (setf (aref data-of-copy i j) (aref data i j)))
    copy))



;;; Matrix binary addition
(defun madd (matrix1 matrix2 &key type)
  (let ((rows1 (matrix-rows matrix1))
        (cols1 (matrix-cols matrix1))
        (rows2 (matrix-rows matrix2))
        (cols2 (matrix-cols matrix2))
        (data1 (matrix-data matrix1))
        (data2 (matrix-data matrix2))
        (type3 (or type
                   (type-strategy (matrix-type matrix1) (matrix-type matrix2)))))
    (assert (and (= rows1 rows2) (= cols1 cols2)))
    (let* ((matrix3 (matrix rows1 cols1 :element-type type3))
           (data3 (matrix-data matrix3)))
      (with-i-j (rows1 cols1)
        (setf (aref data3 i j)
              (+ (aref data1 i j) (aref data2 i j))))
      matrix3)))

;; ;; 二元矩阵加法运算
;; ;; Binary matrix addition
;; (defun madd (mat1 mat2)
;;   (let ((rows1 (array-dimension mat1 0))
;;         (cols1 (array-dimension mat1 1))
;;         (rows2 (array-dimension mat2 0))
;;         (cols2 (array-dimension mat2 1)))
;;     (assert (and (= rows1 rows2) (= cols1 cols2)))
;;     (let ((mat3 (matrix rows1 cols1)))
;;       (loop for i from 0 below rows1
;;          do (loop for j from 0 below cols1
;;                do (setf (aref mat3 i j)
;;                         (+ (aref mat1 i j) (aref mat2 i j)))))
;;       mat3)))

;; ;; 拓展的矩阵加法运算
;; ;; Extended matrix addition
;; ;; m+ can add n matrices together (n >= 1)
;; (defun m+ (mat &rest mats)
;;   (reduce #'madd (cons mat mats)))


;; ;; 矩阵二元减法运算
;; ;; Binary subtraction of matrix
;; (defun msub (mat1 mat2)
;;   (let ((rows1 (array-dimension mat1 0))
;;         (cols1 (array-dimension mat1 1))
;;         (rows2 (array-dimension mat2 0))
;;         (cols2 (array-dimension mat2 1)))
;;     (assert (and (= rows1 rows2) (= cols1 cols2)))
;;     (let ((mat3 (matrix rows1 cols1)))
;;       (loop for i from 0 below rows1
;;          do (loop for j from 0 below cols1
;;                do (setf (aref mat3 i j)
;;                         (- (aref mat1 i j) (aref mat2 i j)))))
;;       mat3)))

;; ;; 矩阵取负(反)运算
;; ;; Build a new matrix with each element negative of the corresponding one in mat
;; (defun mminus (mat)
;;   (let* ((rows (array-dimension mat 0))
;;          (cols (array-dimension mat 1))
;;          (result (matrix rows cols)))
;;     (loop for i from 0 below rows
;;        do (loop for j from 0 below cols
;;              do (setf (aref result i j) (- (aref mat i j)))))
;;     result))

;; ;; 拓展的矩阵减法
;; ;; Extended subtraction of matrix
;; ;; When there is only one parameter passed, call function #'mminus instead of #'msub
;; (defun m- (mat &rest mats)
;;   (if (null mats)
;;       (mminus mat)
;;       (reduce #'msub (cons mat mats))))


;; ;; 二元矩阵乘法运算
;; ;; Binary multiplication of matrix
;; (defun mmul (mat1 mat2)
;;   (let ((rows1 (array-dimension mat1 0))
;;         (cols1 (array-dimension mat1 1))
;;         (rows2 (array-dimension mat2 0))
;;         (cols2 (array-dimension mat2 1)))
;;     (assert (= cols1 rows2))
;;     (let ((result (matrix rows1 cols2)))
;;       (loop for i from 0 below rows1
;;          do (loop for j from 0 below cols2
;;                do (setf (aref result i j)
;;                         (loop for k from 0 below cols1
;;                            sum (* (aref mat1 i k) (aref mat2 k j))))))
;;       (if (and (= rows1 1) (= cols2 1))
;;           (aref result 0 0)
;;           result))))

;; ;; 矩阵数乘运算
;; ;; Compute a matrix multiplied by a number
;; (defun nmul (num mat)
;;   (let* ((rows (array-dimension mat 0))
;;          (cols (array-dimension mat 1))
;;          (result (matrix rows cols)))
;;     (loop for i from 0 below rows
;;        do (loop for j from 0 below cols
;;              do (setf (aref result i j) (* num (aref mat i j)))))
;;     result))

;; ;; 混合二元乘法, 两个参数相乘, 每个参数要么是数字要么是矩阵
;; ;; Mixed binary multiplication of matrix
;; ;; The two parameters are both required either a number or a matrix
;; (defun mix* (num/mat1 num/mat2)
;;   (cond ((numberp num/mat1) (cond ((numberp num/mat2) (* num/mat1 num/mat2))
;;                                   (t (nmul num/mat1 num/mat2))))
;;         ((numberp num/mat2) (nmul num/mat2 num/mat1))
;;         (t (mmul num/mat1 num/mat2))))

;; ;; 拓展的矩阵乘法
;; ;; Extended multiplication of matrix
;; ;; Function #'m* can take more than one parameter, and each of them should be
;; ;; either a number or a matrix
;; (defun m* (num/mat &rest nums/mats)
;;   (reduce #'mix* (cons num/mat nums/mats)))


;; ;; 矩阵乘方(矩阵幂运算)
;; ;; Exponentiation of matrix
;; (defun mexpt (mat power)
;;   (let ((rows (array-dimension mat 0))
;;         (cols (array-dimension mat 1)))
;;     (assert (= rows cols))
;;     (let ((result (eye rows)))
;;       (dotimes (i power)
;;         (setf result (m* result mat)))
;;       result)))

;; ;; 开平方
;; ;; 结果中的每个元素为原矩阵中对应元素的平方根
;; ;; Square root of the matrix
;; ;; Each element of the result is square root of the corresponding element
;; ;; of the origin matrix
;; (defun msqrt (mat)
;;   (let* ((rows (array-dimension mat 0))
;;          (cols (array-dimension mat 1))
;;          (result (matrix rows cols)))
;;     (loop for i from 0 below rows
;;        do (loop for j from 0 below cols
;;              do (setf (aref result i j)
;;                       (sqrt (aref mat i j)))))
;;     result))

;; ;; 矩阵转置
;; ;; Matrix Transposion
;; (defun trans (mat)
;;   (let* ((rows (array-dimension mat 0))
;;          (cols (array-dimension mat 1))
;;          (result (matrix cols rows)))
;;     (loop for i from 0 below rows
;;        do (loop for j from 0 below cols
;;              do (setf (aref result j i) (aref mat i j))))
;;     result))



;; ;;; 初等行变换
;; ;;; Elementary row operation

;; ;; 倍法变换
;; ;; Row multiplication
;; (defun row-multiplyf (mat i k)
;;   (let ((cols (array-dimension mat 1)))
;;     (loop for col from 0 below cols
;;        do (setf (aref mat i col) (* k (aref mat i col))))
;;     mat))

;; ;; 消法变换
;; ;; Row addition
;; (defun row-addf (mat i j k)
;;   (let ((cols (array-dimension mat 1)))
;;     (loop for col from 0 below cols
;;        do (incf (aref mat i col) (* k (aref mat j col))))
;;     mat))

;; ;; 换法变换
;; ;; Row switching
;; (defun row-switchf (mat i j)
;;   (let ((cols (array-dimension mat 1)))
;;     (loop for col from 0 below cols
;;        do (rotatef (aref mat i col) (aref mat j col)))
;;     mat))


;; ;;; 初等行变换
;; ;;; Elementary row operation

;; ;; 倍法变换
;; ;; Col multiplication
;; (defun col-multiplyf (mat i k)
;;   (let ((rows (array-dimension mat 0)))
;;     (loop for row from 0 below rows
;;        do (setf (aref mat row i) (* k (aref mat row i))))
;;     mat))

;; ;; 消法变换
;; ;; Col addition
;; (defun col-addf (mat i j k)
;;   (let ((rows (array-dimension mat 0)))
;;     (loop for row from 0 below rows
;;        do (incf (aref mat row j) (* k (aref mat row i))))
;;     mat))

;; ;; 换法变换
;; ;; Col switching
;; (defun col-switchf (mat i j)
;;   (let ((rows (array-dimension mat 0)))
;;     (loop for row from 0 below rows
;;        do (rotatef (aref mat row i) (aref mat row j)))
;;     mat))


;; ;; 统计矩阵第 row 行中前导的 0 的个数
;; ;; Aux function
;; ;; Count how many zeros are there in row-th row before a non-zero element
;; (defun count-prefix-zeros (mat row)
;;   (let ((cnt 0))
;;     (dotimes (col (array-dimension mat 1))
;;       (if (zerop (aref mat row col))
;;           (incf cnt)
;;           (return cnt)))
;;     cnt))

;; ;; 将矩阵按照每行前导的 0 的个数的升序重排行
;; ;; Aux function
;; ;; Rearrange the matrix by quantity of prefixed zeros
;; (defun rearrangef (mat)
;;   (let* ((rows (array-dimension mat 0))
;;          (nums (make-array rows)))
;;     (dotimes (i rows)
;;       (setf (aref nums i) (count-prefix-zeros mat i)))
;;       (loop for k from 0 below rows
;;          do (loop for i from 1 below rows
;;                do (let ((j (- i 1)))
;;                     (when (< (aref nums i) (aref nums j))
;;                       (rotatef (aref nums i) (aref nums j))
;;                       (row-switchf mat i j)))))
;;       mat))


;; ;; 化为行阶梯矩阵
;; ;; Gaussian elimination (row reduction)
;; ;; Row echelon form
;; (defun row-echelon (mat)
;;   (let ((tmat (copy-matrix mat)))
;;     (rearrangef tmat)
;;     (let ((rows (array-dimension mat 0))
;;           (cols (array-dimension mat 1)))
;;       (loop for i from 0 below (1- rows)
;;          do (let ((pos (count-prefix-zeros tmat i)))
;;               (loop for j from (1+ i) below rows
;;                  do (when (/= pos cols)
;;                       (when (/= (aref tmat j pos) 0)
;;                         (row-addf tmat j i (- (/ (aref tmat j pos) (aref tmat i pos)))))))))
;;       tmat)))


;; ;; 化为行最简矩阵 (行规范型矩阵)
;; ;; Reduced row echelon form / row canonical form
;; (defun row-canonical (mat)
;;   (let ((tmat (row-echelon mat))
;;         (rows (array-dimension mat 0))
;;         (cols (array-dimension mat 1)))
;;     (loop for j from (1- rows) downto 1
;;        do (let ((pos (count-prefix-zeros tmat j)))
;;             (when (/= pos cols)
;;               (row-multiplyf tmat j (/ 1 (aref tmat j pos)))
;;               (loop for i from (1- j) downto 0
;;                  do (when (/= (aref tmat i pos) 0)
;;                       (row-addf tmat i j (- (/ (aref tmat i pos) (aref tmat j pos)))))))))
;;     tmat))


;; ;; 化为列最简矩阵
;; ;; Reduced col echelon form / col canonical form
;; (defun col-canonical (mat)
;;   (trans (rearrange (row-canonical (trans mat)))))


;; ;; 化为标准型矩阵
;; ;; Transform the mat to canonical form
;; (defun canonical (mat)
;;   (col-canonical (row-canonical mat)))


;; ;; 矩阵的行秩
;; ;; Row rank of matrix
;; (defun row-rank (mat)
;;   (let* ((tmat (row-echelon mat))
;;          (rows (array-dimension mat 0))
;;          (cols (array-dimension mat 1))
;;          (rank rows))
;;     (loop for i from (1- rows) downto 0
;;        do (let ((flag t))
;;             (loop for j from (1- cols) downto 0
;;                do (when (/= (aref tmat i j) 0)
;;                     (setf flag nil)
;;                     (return)))
;;             (when flag (decf rank))))
;;     rank))


;; ;; 矩阵的列秩
;; ;; Col rank of matrix
;; (defun col-rank (mat)
;;   (row-rank (trans mat)))

;; ;; 矩阵的秩
;; ;; Rank fo matrix
;; (defun rank (mat)
;;   (min (row-rank (row-canonical mat))
;;        (row-rank (row-canonical (trans mat)))))



;; ;; 逆序数
;; ;; Compute the inversion of the vector vec's permutation
;; (defun inversion (vec)
;;   (let ((len (length vec))
;;         (cnt 0))
;;     (loop for i from 0 below (1- len)
;;        do (loop for j from (1+ i) below len
;;              do (when (< (svref vec j) (svref vec i))
;;                   (incf cnt))))
;;     cnt))


;; ;; 生成全排列
;; ;; 根据给定的数组生成包含该数组的所有全排列的列表
;; ;; Generate permutation
;; ;; Generate a list that includes all the permutations of the given vector
;; (defun permutation (vec)
;;   (let ((result nil))
;;     (labels ((perm (vec k len)
;;                (if (= k len)
;;                    (push (copy-seq vec) result)
;;                    (loop for i from k below len
;;                       do (progn (rotatef (aref vec i) (aref vec k))
;;                                 (perm vec (1+ k) len)
;;                                 (rotatef (aref vec k) (aref vec i)))))))
;;       (perm vec 0 (length vec)))
;;     result))

;; ;; 生成一个从 1 到 n 的顺序数组
;; ;; Generate an ordered vector includes numbers that from 1 to n
;; (defun gen-seq-vec (n)
;;   (make-array n :initial-contents (loop for i from 0 below n collect i)))

;; ;; 行列式
;; ;; Compute the determinant of a square matrix
;; (defun det (mat)
;;   (let ((rows (array-dimension mat 0))
;;         (cols (array-dimension mat 1)))
;;   (assert (= rows cols))
;;   (let ((permutations (permutation (gen-seq-vec rows)))
;;         (acc 0))
;;     (dolist (perm permutations)
;;       (let ((mul 1))                    ;multiplicative
;;         (dotimes (i rows)
;;           (setf mul (* mul (aref mat i (svref perm i)))))
;;         (incf acc (* mul (expt -1 (inversion perm))))))
;;     acc)))


;; ;; 生成除第 i 行和第 j 行元素的子矩阵
;; ;; Generate the submatrix of the matrix mat that exclude i-th row and j-th colum elements
;; (defun submatrix (mat i j)
;;   (let* ((rows (array-dimension mat 0))
;;          (cols (array-dimension mat 1))
;;          (result (matrix (1- rows) (1- cols))))
;;     (loop for r from 0 below (1- rows)
;;        do (if (< r i)
;;               (loop for c from 0 below (1- cols)
;;                  do (if (< c j)
;;                         (setf (aref result r c) (aref mat r c))
;;                         (setf (aref result r c) (aref mat r (1+ c)))))
;;               (loop for c from 0 below (1- cols)
;;                  do (if (< c j)
;;                         (setf (aref result r c) (aref mat (1+ r) c))
;;                         (setf (aref result r c) (aref mat (1+ r) (1+ c)))))))
;;     result))


;; ;; 余子式
;; ;; Minor
;; (defun minor (mat i j)
;;   (det (submatrix mat i j)))

;; ;; 代数余子式
;; ;; Signed minor of a matrix
;; (defun cofactor (mat i j)
;;   (* (expt -1 (+ i j))
;;      (minor mat i j)))

;; (defun signed-minor (mat i j)
;;   (cofactor mat i j))


;; ;; 伴随矩阵
;; ;; Adjugate
;; (defun adj (mat)
;;   (let* ((order (array-dimension mat 0))
;;          (result (matrix order order)))
;;     (loop for i from 0 below order
;;        do (loop for j from 0 below order
;;              do (setf (aref result i j)
;;                       (cofactor mat j i))))
;;     result))


;; ;; 矩阵的逆
;; ;; Inverse of matrix
;; (defun inv (mat)
;;   (assert (/= (det mat) 0))
;;   (m* (/ 1 (det mat)) (adj mat)))


;; ;; 矩阵的迹
;; ;; Trace of matrix
;; (defun tr (mat)
;;   (assert (reduce #'= (array-dimensions mat)))
;;   (let ((order (array-dimension mat 0)))
;;     (loop for i from 0 below order sum (aref mat i i))))

;; ;; 计算向量的内积(数量积)
;; ;; Compute inner product(scalar product) of two vector
;; (defun dot (vec1 vec2)
;;   (let ((rows1 (array-dimension vec1 0))
;;         (cols1 (array-dimension vec1 1))
;;         (rows2 (array-dimension vec2 0))
;;         (cols2 (array-dimension vec2 1)))
;;     (assert (and (= (min rows1 cols1) 1)
;;                  (= (min rows2 cols2) 1)
;;                  (= (max rows1 cols1) (max rows2 cols2))))
;;     (let ((v1 (if (= rows1 1)
;;                   vec1
;;                   (trans vec1)))
;;           (v2 (if (= cols2 1)
;;                   vec2
;;                   (trans vec2))))
;;       (m* v1 v2))))

;; ;; 对矩阵的每个元素进行操作
;; ;; Do the given function on each element of mat
;; (defun mapeach (function mat)
;;   (let* ((rows (array-dimension mat 0))
;;          (cols (array-dimension mat 1))
;;          (result (matrix rows cols)))
;;     (loop for i from 0 below rows
;;        do (loop for j from 0 below cols
;;              do (setf (aref result i j)
;;                       (funcall function (aref mat i j)))))
;;     result))


;; ;; 计算矩阵所有元素之和
;; ;; Compute the sum of all elements of the given mat
;; (defun msum (mat)
;;   (let ((rows (array-dimension mat 0))
;;         (cols (array-dimension mat 1)))
;;     (loop for i from 0 below rows
;;        sum (loop for j from 0 below cols
;;                 sum (aref mat i j)))))


;; ;; p-范数
;; ;; Compute p-norm of vector vec
;; (defun norm (vec &optional (p 2))
;;   (assert (= (apply #'min (array-dimensions vec)) 1))
;;   (expt (msum (mapeach #'(lambda (x) (expt (abs x) p)) vec))
;;         (/ 1 p)))


;; ;; 欧几里得距离(欧式距离)
;; ;; Euclidean distance of two vectors
;; (defun euclidean-distance (vec1 vec2)
;;   (assert (equal (array-dimensions vec1) (array-dimensions vec2)))
;;   (let ((vec (m- vec1 vec2)))
;;     (norm vec)))
