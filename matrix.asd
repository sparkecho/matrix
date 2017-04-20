;;;; matrix.asd

(asdf:defsystem #:matrix
  :description "This is a simple matrix manipulation library."
  :author "zhz"
  :license "GPL"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "matrix")))

