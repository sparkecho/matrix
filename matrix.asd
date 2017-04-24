;;;; matrix.asd

(asdf:defsystem #:matrix
  :description "This is a simple matrix manipulation library."
  :author "sparkecho <echozhz@126.com>"
  :license "GPL3.0"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (#:matrix/class
               #:matrix/array2d
               #:matrix/structure))
