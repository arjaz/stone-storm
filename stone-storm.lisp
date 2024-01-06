;;;; stone-storm.lisp

(in-package #:stone-storm)
(named-readtables:in-readtable r:rutils-readtable)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)

(defclass stone-storm-world (c:world) ())
(defparameter *debug* t)
(defvar *world*)

(defun vec2+ (v1 v2)
  #v((+ (aref v1 0) (aref v2 0))
     (+ (aref v1 1) (aref v2 1))))

(defun vec3+ (v1 v2)
  #v((+ (aref v1 0) (aref v2 0))
     (+ (aref v1 1) (aref v2 1))
     (+ (aref v1 2) (aref v2 2))))

(defun vec3->vec2 (v)
  #v((aref v 0) (aref v 1)))

(defun real-time-seconds ()
  "Return current seconds"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defclass pos (c:component) ((v :accessor v :initarg :v)))
(defclass tile (c:component)
  ((tile :accessor tile :initarg :tile)
   (priority :accessor priority :initarg :priority :initform 0)))
(defclass player (c:component) ())
(defclass collider (c:component) ())
(defclass door (c:component) ())
(defclass health (c:component) ((health :accessor health :initarg :health)))

(defun place-player (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'player)
   (make-instance 'collider)
   (make-instance 'pos :v #v(x y 99))
   (make-instance 'tile :tile #\@)
   (make-instance 'health :health 3)))

(defun place-enemy (world x y tile)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'collider)
   (make-instance 'pos :v #v(x y 50))
   (make-instance 'tile :tile tile)
   (make-instance 'health :health 3)))

(defun place-wall (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'collider)
   (make-instance 'tile :tile #\#)
   (make-instance 'pos :v #v(x y 1))))

(defun place-closed-door (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'collider)
   (make-instance 'door)
   (make-instance 'tile :tile #\+)
   (make-instance 'pos :v #v(x y 1))))

(defun load-level (world filename)
  (iter
    (for line in-sequence (r:split #\newline (r:read-file filename)) with-index i)
    (when (> i *screen-height*)
      (error "Level [~a] is bigger than allowed, expected max ~a rows, found at least ~a" filename *screen-height* i))
    (iter (for char in-string line with-index j)
      (when (> j *screen-width*)
        (error "Level [~a] is bigger than allowed, expected max ~a colums, found at least ~a" filename *screen-width* j))
      (let ((x j)
            (y i))
        (case char
          ((#\#) (place-wall world x y))
          ((#\+) (place-closed-door world x y))
          ((#\@) (place-player world x y))
          ((#\e) (place-enemy world x y #\e)))))))

(defun in-world-map-p (pos)
  (and (<= 0 (aref pos 0) *screen-width*)
       (<= 0 (aref pos 1) *screen-height*)))

(defun equal-coordinates-p (pos1 pos2)
  (equalp (vec3->vec2 pos1) (vec3->vec2 pos2)))

(defun collides-with-any (pos colliders)
  "Return entity collided with if the given position collides with any of the colliders from the (entity pos collider) query."
  (iter (for entity in colliders)
    (when (equal-coordinates-p pos (v (second entity)))
      (return (first entity)))))

(defun query-component (world entity component)
  (r:? world 'c::entity-components entity component))

(defun is-a (component world entity)
  (r:true (query-component world entity component)))

(deftype direction () '(member :up :down :left :right))
(defun direction->add-vec3 (direction)
  (declare (type direction direction))
  (case direction
    ((:up) #v(0 -1 0))
    ((:down) #v(0 1 0))
    ((:left) #v(-1 0 0))
    ((:right) #v(1 0 0))))

(defun open-door (world door)
  (c:remove-component world door 'collider)
  (c:remove-component world door 'tile)
  (c:add-component world door (make-instance 'tile :tile #\…)))

(defun close-door (world door)
  (c:add-component world door (make-instance 'collider))
  (c:remove-component world door 'tile)
  (c:add-component world door (make-instance 'tile :tile #\+)))

(defun damage-health (world entity damage)
  (decf (r:? world 'c::entity-components entity 'health 'health) damage)
  (log:info "damaged to ~a" (r:? world 'c::entity-components entity 'health 'health))
  (when (>= 0 (r:? world 'c::entity-components entity 'health 'health))
    (c:remove-entity world entity)))

(defun handle-move (world colliders from-pos-component new-pos)
  (when (in-world-map-p new-pos)
    (r:if-let (collided (collides-with-any new-pos colliders))
      (progn
        (when (is-a 'door world collided)
          (open-door world collided))
        (when (is-a 'health world collided)
          (damage-health world collided 1)))
      (setf (v from-pos-component) new-pos))))

(c:defsystem move-player (world event payload (entity pos player))
  (declare (ignore event))
  (handle-move world
               (c:query world '(_ pos collider))
               (second entity)
               (vec2+ (v (second entity)) (direction->add-vec3 payload))))

(defun sorted-by-z (query)
  "Each position is a vec3, we reverse sort by z"
  (r:safe-sort
   query
   (lambda (e1 e2)
     (> (aref (v (first e1)) 2) (aref (v (first e2)) 2)))))

(defun render-tile (position tile)
  (setf (blt:color) (blt:white)
        (blt:cell-char (aref position 0) (aref position 1)) tile))

(defun draw (world)
  (blt:clear)
  (iter
    (for (position . entities)
         in (group-by
             (c:query world '(_ pos tile))
             :test #'equal-coordinates-p
             :key (lambda (e) (vec3->vec2 (v (second e))))))
    (render-tile position (tile (second (first entities)))))
  (blt:refresh))

(defun configure-window ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = Store Storm"))

(defun init-world ()
  (setf *world* (make-instance 'stone-storm-world))
  (load-level *world* #p"assets/levels/01")
  (c:add-system *world*
                :move-player
                (make-instance 'move-player)))

(defun run ()
  (draw *world*)
  (blt:key-case (blt:read)
                (:escape (return-from run))
                (:close (return-from run))
                (:r (init-world))
                (:left (c:tick-event *world* :move-player :left))
                (:right (c:tick-event *world* :move-player :right))
                (:up (c:tick-event *world* :move-player :up))
                (:down (c:tick-event *world* :move-player :down)))
  (run))

(defun start ()
  (init-world)
  (blt:with-terminal
    (configure-window)
    (run)))

;; (start)
