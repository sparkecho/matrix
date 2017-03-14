;;;; package.lisp

(defpackage #:matrix
  (:use #:cl)
  (:export #:matrix
           #:m+
           #:m-
           #:m*
           #:eye
           #:copy-matrix
           #:pr
           #:print-matrix
           #:mexpt
           #:trans
           #:row-multiplyf
           #:row-addf
           #:row-switchf
           #:col-multiplyf
           #:col-addf
           #:col-switchf
           #:rearrangef
           #:row-echelon
           #:row-canonical
           #:col-canonical
           #:canonical
           #:row-rank
           #:col-rank
           #:rank
           #:inversion
           #:det
           #:submatrix
           #:minor
           #:cofactor
           #:signed-minor
           #:adj
           #:inv
           #:tr))

