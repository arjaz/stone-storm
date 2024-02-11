(in-package #:stone-storm)

(defun |#a-reader| (stream char arg)
  "Literal syntax for non-adjustable vectors.
   Unlike #() evaluates its contents before vector creation

   Examples:

      CL-USER> #a(1 2 3)
      #(1 2 3)

      CL-USER> #a((+ 1 2))
      #(3)
  "
  (declare (ignore char arg))
  (read-char stream)
  (let* ((vals (read-delimited-list #\) stream t)))
    `(make-array ,(length vals) :initial-contents (list ,@vals))))

(defun |#i-reader| (stream char arg)
  "Literal syntax for non-adjustable vectors of fixnum elements.
   Unlike #() evaluates its contents before vector creation

   Examples:

      CL-USER> #i(1 2)
      #(1 2)

      CL-USER> #i((+ 1 2) 3)
      #(3 3)
  "
  (declare (ignore char arg))
  (read-char stream)
  (let* ((vals (read-delimited-list #\) stream t)))
    `(make-array ,(length vals) :initial-contents (list ,@vals) :element-type 'fixnum)))

(defreadtable stone-storm-readtable
  (:merge :standard)
  (:merge r:rutils-readtable)
  (:dispatch-macro-char #\# #\a #'|#a-reader|)
  (:dispatch-macro-char #\# #\i #'|#i-reader|))
