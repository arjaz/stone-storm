;;;; stone-storm.asd

(asdf:defsystem #:stone-storm
  :description "A game"
  :author "Eugene Rossokha <hsugeneyos@gmail.com>"
  :license "MIT public license"
  :version "0.0.1"
  :serial t
  :depends-on (#:chakra
               #:rutils
               #:iterate
               #:log4cl
               #:group-by
               #:cl-blt)
  :components ((:file "package")
               (:file "linalg")
               (:file "stone-storm")))

(asdf:defsystem #:chakra
  :description "Entity Component system"
  :license "MIT public license"
  :serial t
  :components ((:file "chakra"))
  :depends-on (#:iterate #:alexandria))
