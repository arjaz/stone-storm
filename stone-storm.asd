;;;; stone-storm.asd

(asdf:defsystem #:stone-storm
  :description "A game"
  :author "Eugene Rossokha <hsugeneyos@gmail.com>"
  :license  "TODO"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-gamekit
               #:chakra
               #:iterate
               #:log4cl)
  :components ((:file "package")
               (:file "stone-storm")))
