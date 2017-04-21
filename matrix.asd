;;;; matrix.asd

(asdf:defsystem #:matrix
  :description "This is a simple matrix manipulation library."
  :author "sparkecho <echozhz@126.com>"
  :license "GPL"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "matrix")))

