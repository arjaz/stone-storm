;;;; package.lisp

(defpackage #:stone-storm
  (:use #:cl #:iterate #:group-by)
  (:local-nicknames
   (:r #:rutils)
   (:gk #:gamekit)
   (:bm #:bodge-math)
   (:c #:chakra)))
