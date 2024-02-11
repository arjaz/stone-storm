(in-package #:stone-storm)

(in-readtable stone-storm-readtable)

(defun ivec2+ (v1 v2)
  #i((+ (aref v1 0) (aref v2 0))
     (+ (aref v1 1) (aref v2 1))))

(defun ivec3+ (v1 v2)
  #i((+ (aref v1 0) (aref v2 0))
     (+ (aref v1 1) (aref v2 1))
     (+ (aref v1 2) (aref v2 2))))

(defun ivec3->ivec2 (v)
  #i((aref v 0) (aref v 1)))
