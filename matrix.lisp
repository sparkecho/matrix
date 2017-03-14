;;;; matrix.lisp

(in-package #:matrix)

;;; "matrix" goes here. Hacks and glory await!

;; 矩阵构造函数
(defun matrix (rows cols &optional initvec)
  (let ((mat (make-array (list rows cols) :initial-element 0)))
    (if initvec
        (loop for i from 0 below rows
           do (loop for j from 0 below cols
                 do (let ((k (cl-user::+ (cl-user::* i cols) j)))
                      (setf (aref mat i j) (svref initvec k))))))
    mat))

;; 单位矩阵构造函数
(defun eye (n)
  (let ((mat (matrix n n)))
    (loop for i from 0 below n
       do (loop for j from 0 below n
             do (if (= i j)
                    (setf (aref mat i j) 1)
                    (setf (aref mat i j) 0))))
    mat))

(defun copy-matrix (mat)
  (let* ((dims (array-dimensions mat))
         (rows (first dims))
         (cols (second dims))
         (copy (matrix rows cols)))
    (loop for i from 0 below rows
       do (loop for j from 0 below cols
             do (setf (aref copy i j) (aref mat i j))))
    copy))


;; 矩阵美观打印函数
(defun print-matrix (mat)
  (let* ((dims (array-dimensions mat))
         (rows (first dims))
         (cols (second dims)))
    (loop for i from 0 below rows
       do (progn (loop for j from 0 below cols
                    do (format t "~A~A" #\Tab (aref mat i j)))
                 (format t "~%")))))

(defun pr (mat) (print-matrix mat))


;; 二元矩阵加法运算
(defun madd (mat1 mat2)
  (let ((dims1 (array-dimensions mat1))
        (dims2 (array-dimensions mat2)))
    (assert (equal dims1 dims2))
    (let* ((rows (first dims1))
           (cols (second dims1))
           (mat3 (matrix rows cols)))
      (loop for i from 0 below rows
         do (loop for j from 0 below cols
               do (setf (aref mat3 i j)
                        (cl-user::+ (aref mat1 i j) (aref mat2 i j)))))
      mat3)))

;; 拓展的矩阵加法运算
(defun m+ (mat &rest mats)
  (reduce #'madd (cons mat mats)))


;; 矩阵二元减法运算
(defun msub (mat1 mat2)
  (let ((dims1 (array-dimensions mat1))
        (dims2 (array-dimensions mat2)))
    (assert (equal dims1 dims2))
    (let* ((rows (first dims1))
           (cols (second dims1))
           (mat3 (matrix rows cols)))
      (loop for i from 0 below rows
         do (loop for j from 0 below cols
               do (setf (aref mat3 i j)
                        (cl-user::- (aref mat1 i j) (aref mat2 i j)))))
      mat3)))

;; 矩阵取负(反)运算
(defun mminus (mat)
  (let* ((dims (array-dimensions mat))
         (rows (first dims))
         (cols (second dims))
         (result (matrix rows cols)))
    (loop for i from 0 below rows
       do (loop for j from 0 below cols
             do (setf (aref result i j) (- (aref mat i j)))))
    result))

;; 拓展的矩阵减法
(defun m- (mat &rest mats)
  (if (null mats)
      (mminus mat)
      (reduce #'msub (cons mat mats))))


;; 矩阵乘法运算
(defun mmul (mat1 mat2)
  (let* ((dims1 (array-dimensions mat1))
         (dims2 (array-dimensions mat2))
         (m (first dims1))
         (n (second dims1))
         (q (first dims2))
         (p (second dims2)))
    (assert (= n q))
    (let ((result (matrix m p)))
      (loop for i from 0 below m
         do (loop for j from 0 below p
               do (let ((sum 0))
                    (loop for k from 0 below n
                       do (incf sum (cl-user::* (aref mat1 i k)
                                                (aref mat2 k j))))
                    (setf (aref result i j) sum))))
      result)))

;; 矩阵数乘运算
(defun nmul (num mat)
  (let* ((dims (array-dimensions mat))
         (rows (first dims))
         (cols (second dims))
         (result (matrix rows cols)))
    (loop for i from 0 below rows
       do (loop for j from 0 below cols
             do (setf (aref result i j) (* num (aref mat i j)))))
    result))

;; 混合二元乘法, 两个参数相乘, 每个参数要么是数字要么是矩阵
(defun mix* (num/mat1 num/mat2)
  (cond ((numberp num/mat1) (cond ((numberp num/mat2) (* num/mat1 num/mat2))
                                  (t (nmul num/mat1 num/mat2))))
        ((numberp num/mat2) (nmul num/mat2 num/mat1))
        (t (mmul num/mat1 num/mat2))))

;; 拓展的矩阵乘法
(defun m* (mat &rest mats)
  (reduce #'mix* (cons mat mats)))


;; 矩阵乘方
(defun mexpt (mat power)
  (let* ((dims (array-dimensions mat))
         (rows (first dims))
         (cols (second dims)))
    (assert (= rows cols))
    (let ((result (eye rows)))
      (dotimes (i power)
        (setf result (m* result mat)))
      result)))

;; 矩阵转置
;; Matrix Transpose
(defun trans (mat)
  (let* ((dims (array-dimensions mat))
         (rows (first dims))
         (cols (second dims))
         (result (matrix cols rows)))
    (loop for i from 0 below rows
       do (loop for j from 0 below cols
             do (setf (aref result j i) (aref mat i j))))
    result))
  


;;; 初等行变换
;;; Elementary row operation

;; 倍法变换
;; Row multiplication
(defun row-multiplyf (mat i k)
  (let ((cols (array-dimension mat 1)))
    (loop for col from 0 below cols
       do (setf (aref mat i col) (* k (aref mat i col))))
    mat))

;; 消法变换
;; Row addition
(defun row-addf (mat i j k)
  (let ((cols (array-dimension mat 1)))
    (loop for col from 0 below cols
       do (incf (aref mat i col) (* k (aref mat j col))))
    mat))

;; 换法变换
;; Row switching
(defun row-switchf (mat i j)
  (let ((cols (array-dimension mat 1)))
    (loop for col from 0 below cols
       do (rotatef (aref mat i col) (aref mat j col)))
    mat))
  

;;; 初等行变换
;;; Elementary row operation

;; 倍法变换
;; Col multiplication
(defun col-multiplyf (mat i k)
  (let ((rows (array-dimension mat 0)))
    (loop for row from 0 below rows
       do (setf (aref mat row i) (* k (aref mat row i))))
    mat))

;; 消法变换
;; Col addition
(defun col-addf (mat i j k)
  (let ((rows (array-dimension mat 0)))
    (loop for row from 0 below rows
       do (incf (aref mat row j) (* k (aref mat row i))))
    mat))

;; 换法变换
;; Col switching
(defun col-switchf (mat i j)
  (let ((rows (array-dimension mat 0)))
    (loop for row from 0 below rows
       do (rotatef (aref mat row i) (aref mat row j)))
    mat))
  

;; 统计矩阵第 row 行中前导的 0 的个数
;; aux function
(defun count-prefix-zeros (mat row)
  (let ((cnt 0))
    (dotimes (col (array-dimension mat 1))
      (if (zerop (aref mat row col))
          (incf cnt)
          (return cnt)))
    cnt))

;; 将矩阵按照每行前导的 0 的个数的升序重排行
;; aux function
(defun rearrangef (mat)
  (let* ((rows (array-dimension mat 0))
         (nums (make-array rows)))
    (dotimes (i rows)
      (setf (aref nums i) (count-prefix-zeros mat i)))
      (loop for k from 0 below rows
         do (loop for i from 1 below rows
               do (let ((j (- i 1)))
                    (when (< (aref nums i) (aref nums j))
                      (rotatef (aref nums i) (aref nums j))
                      (row-switchf mat i j)))))
      mat))


;; 化为行阶梯矩阵
;; Gaussian elimination (row reduction)
;; row echelon form
(defun row-echelon (mat)
  (let ((tmat (copy-matrix mat)))
    (rearrangef tmat)
    (let* ((dims (array-dimensions tmat))
           (rows (first dims))
           (cols (second dims)))
      (loop for i from 0 below (1- rows)
         do (let ((pos (count-prefix-zeros tmat i)))
              (loop for j from (1+ i) below rows
                 do (when (/= pos cols)
                      (when (/= (aref tmat j pos) 0)
                        (row-addf tmat j i (- (/ (aref tmat j pos) (aref tmat i pos)))))))))
      tmat)))


;; 化为行最简矩阵 (行规范型矩阵)
;; reduced row echelon form / row canonical form
(defun row-canonical (mat)
  (let* ((tmat (row-echelon mat))
         (dims (array-dimensions tmat))
         (rows (first dims))
         (cols (second dims)))
    (loop for j from (1- rows) downto 1
       do (let ((pos (count-prefix-zeros tmat j)))
            (when (/= pos cols)
              (row-multiplyf tmat j (/ 1 (aref tmat j pos)))
              (loop for i from (1- j) downto 0
                 do (when (/= (aref tmat i pos) 0)
                      (row-addf tmat i j (- (/ (aref tmat i pos) (aref tmat j pos)))))))))
    tmat))


;; 化为列最简矩阵
(defun col-canonical (mat)
  (trans (rearrange (row-canonical (trans mat)))))


;; 化为标准型矩阵
(defun canonical (mat)
  (col-canonical (row-canonical mat)))


;; 矩阵的行秩
;; row rank of matrix
(defun row-rank (mat)
  (let* ((tmat (row-echelon mat))
         (dims (array-dimensions tmat))
         (rows (first dims))
         (cols (second dims))
         (rank rows))
    (loop for i from (1- rows) downto 0
       do (let ((flag t))
            (loop for j from (1- cols) downto 0
               do (when (/= (aref tmat i j) 0)
                    (setf flag nil)
                    (return)))
            (when flag (decf rank))))
    rank))


;; 矩阵的列秩
(defun col-rank (mat)
  (row-rank (trans mat)))

;; 矩阵的秩
;; Rank fo matrix
(defun rank (mat)
  (min (row-rank (row-canonical mat))
       (row-rank (row-canonical (trans mat)))))
               


;; 逆序数
;; Inversion
(defun inversion (vec)
  (let ((len (length vec))
        (cnt 0))
    (loop for i from 0 below (1- len)
       do (loop for j from (1+ i) below len
             do (when (< (svref vec j) (svref vec i))
                  (incf cnt))))
    cnt))


;; 生成全排列
;; Permutation
(defun permutation (vec)
  (let ((result nil))
    (labels ((perm (vec k len)
               (if (= k len)
                   (push (copy-seq vec) result)
                   (loop for i from k below len
                      do (progn (rotatef (aref vec i) (aref vec k))
                                (perm vec (1+ k) len)
                                (rotatef (aref vec k) (aref vec i)))))))
      (perm vec 0 (length vec)))
    result))


(defun gen-seq-vec (n)
  (make-array n :initial-contents (loop for i from 0 below n collect i)))

;; 行列式
;; Determinant
(defun det (mat)
  (let* ((dims (array-dimensions mat))
         (rows (first dims))
         (cols (second dims)))
  (assert (= rows cols))
  (let ((permutations (permutation (gen-seq-vec rows)))
        (acc 0))
    (dolist (perm permutations)
      (let ((mul 1))                    ;multiplicative
        (dotimes (i rows)
          (setf mul (* mul (aref mat i (svref perm i)))))
        (incf acc (* mul (expt -1 (inversion perm))))))
    acc)))


(defun submatrix (mat i j)
  (let* ((dims (array-dimensions mat))
         (rows (first dims))
         (cols (second dims))
         (result (matrix (1- rows) (1- cols))))
    (loop for r from 0 below (1- rows)
       do (if (< r i)              
              (loop for c from 0 below (1- cols)
                 do (if (< c j)
                        (setf (aref result r c) (aref mat r c))
                        (setf (aref result r c) (aref mat r (1+ c)))))
              (loop for c from 0 below (1- cols)
                 do (if (< c j)
                        (setf (aref result r c) (aref mat (1+ r) c))
                        (setf (aref result r c) (aref mat (1+ r) (1+ c)))))))
    result))


;; 余子式
;; Minor
(defun minor (mat i j)
  (det (submatrix mat i j)))

;; 代数余子式
;; signed minor of a matrix
(defun cofactor (mat i j)
  (* (expt -1 (+ i j))
     (minor mat i j)))

(defun signed-minor (mat i j)
  (cofactor mat i j))


;; 伴随矩阵
;; Adjugate
(defun adj (mat)
  (let* ((order (array-dimension mat 0))
         (result (matrix order order)))
    (loop for i from 0 below order
       do (loop for j from 0 below order
             do (setf (aref result i j)
                      (cofactor mat j i))))
    result))


;; 矩阵的逆
;; Inverse of matrix
(defun inv (mat)
  (assert (/= (det mat) 0))
  (m* (/ 1 (det mat)) (adj mat)))


;; 矩阵的迹
;; trace of matrix
(defun tr (mat)
  (assert (reduce #'= (array-dimensions mat)))
  (let ((order (array-dimension mat 0)))
    (loop for i from 0 below order sum (aref mat i i))))
