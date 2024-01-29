(in-package #:stone-storm)

(in-readtable stone-storm-readtable)

(defun vec2+ (v1 v2)
  #a((+ (aref v1 0) (aref v2 0))
     (+ (aref v1 1) (aref v2 1))))

(defun vec3+ (v1 v2)
  #a((+ (aref v1 0) (aref v2 0))
     (+ (aref v1 1) (aref v2 1))
     (+ (aref v1 2) (aref v2 2))))

(defun vec3->vec2 (v)
  #a((aref v 0) (aref v 1)))
