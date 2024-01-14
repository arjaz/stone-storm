(in-package #:stone-storm)

(named-readtables:in-readtable r:rutils-readtable)

(defun copy-vec3 (v)
  #v((aref v 0)
     (aref v 1)
     (aref v 2)))

(defun vec2+ (v1 v2)
  #v((+ (aref v1 0) (aref v2 0))
     (+ (aref v1 1) (aref v2 1))))

(defun vec3+ (v1 v2)
  #v((+ (aref v1 0) (aref v2 0))
     (+ (aref v1 1) (aref v2 1))
     (+ (aref v1 2) (aref v2 2))))

(defun vec3->vec2 (v)
  #v((aref v 0) (aref v 1)))
