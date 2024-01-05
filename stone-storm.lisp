;;;; stone-storm.lisp

(in-package #:stone-storm)
(named-readtables:in-readtable r:rutils-readtable)
;; (r:toggle-print-hash-table)

;; Rendering features:
;;   The z coordinate is used to determine the rendering order.
;; The first feature:
;;   The player can move around the map and not fall over the edges.
;; The second feature:
;;   There are walls that the player can't pass through.
;; The third feature:
;;   There are doors which are closed by default and opened once you collide with them.

(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter +tile-size+ 25.0)
;; So bad, I need to fix this.
(defparameter *physical-width* (/ *window-width* +tile-size+))
;; Leave some space at the top for messages
(defparameter *physical-height* (1- (/ *window-height* +tile-size+)))

(defclass stone-storm-world (c:world) ())
(defparameter *debug* t)
(defvar *world*)
(defvar *cursor-position* (gamekit:vec2 0 0))

(gk:defgame stone-storm () ()
  (:viewport-width *window-width*)
  (:viewport-height *window-height*)
  (:viewport-title "Stone Storm"))

(defun real-time-seconds ()
  "Return current seconds"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun cursor-position->world-coordinates (cursor-position)
  (let ((x (bm:x cursor-position))
        (y (bm:y cursor-position)))
    (bm:vec2 (floor x +tile-size+) (floor y +tile-size+))))

(defun format-world-coordinates (coordinates)
  (format nil "[~a : ~a]" (bm:x coordinates) (bm:y coordinates)))

(defclass pos (c:component) ((pos :accessor pos :initarg :pos)))
(defclass tile (c:component)
  ((tile :accessor tile :initarg :tile)
   (priority :accessor priority :initarg :priority :initform 0)))
(defclass player (c:component) ())
(defclass collider (c:component) ())
(defclass door (c:component) ())


(defun place-player (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'player)
   (make-instance 'collider)
   (make-instance 'pos :pos (gk:vec3 x y 99))
   (make-instance 'tile :tile #\@)))

(defun place-wall (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'collider)
   (make-instance 'tile :tile #\#)
   (make-instance 'pos :pos (gk:vec3 x y 1))))

(defun place-closed-door (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'collider)
   (make-instance 'door)
   (make-instance 'tile :tile #\+)
   (make-instance 'pos :pos (gk:vec3 x y 1))))

(defun load-level (world filename)
  (iter
    (for line in-sequence (r:split #\newline (r:read-file filename)) with-index i)
    (when (> i *physical-height*)
      (error "Level [~a] is bigger than allowed, expected max ~a rows, found at least ~a" filename *physical-height* i))
    (iter (for char in-string line with-index j)
      (when (> j *physical-width*)
        (error "Level [~a] is bigger than allowed, expected max ~a colums, found at least ~a" filename *physical-width* j))
      (let ((x j)
            (y (- *physical-height* i 1)))
        (case char
          ((#\#) (place-wall world x y))
          ((#\+) (place-closed-door world x y))
          ((#\@) (place-player world x y)))))))

(defun in-world-map-p (pos)
  (and (<= 0 (bm:x pos) *physical-width*)
       (<= 0 (bm:y pos) *physical-height*)))

(defun equal-coordinates-p (pos1 pos2)
  (bm:vec= (bm:value->vec2 pos1) (bm:value->vec2 pos2)))

(defun collides-with-any (pos colliders)
  "Return entity collided with if the given position collides with any of the colliders from the (entity pos collider) query."
  (iter (for entity in colliders)
    (when (equal-coordinates-p pos (pos (second entity)))
      (return (first entity)))))

(defun query-component (world entity component)
  (r:? world 'c::entity-components entity component))

(defun is-a-door-p (world entity)
  (r:true (query-component world entity 'door)))

(deftype direction () '(member :up :down :left :right))
(defun direction->add-vec3 (direction)
  (declare (type direction direction))
  (case direction
    ((:up) (bm:vec3 0 1 0))
    ((:down) (bm:vec3 0 -1 0))
    ((:left) (bm:vec3 -1 0 0))
    ((:right) (bm:vec3 1 0 0))))

(defun open-door (world door)
  (c:remove-component world door 'tile)
  (c:add-component world door (make-instance 'tile :tile #\…)))

(defun close-door (world door)
  (c:remove-component world door 'tile)
  (c:add-component world door (make-instance 'tile :tile #\+)))

(defun handle-move (world colliders from-pos-component new-pos)
  (when (in-world-map-p new-pos)
    (r:if-let (collided (collides-with-any new-pos colliders))
      (when (is-a-door-p world collided)
        (setf (pos from-pos-component) new-pos)
        (open-door world collided))
      (setf (pos from-pos-component) new-pos))))

(c:defsystem move-player (world event payload (entity pos player))
  (declare (ignore event))
  (handle-move world
               (c:query world '(_ pos collider))
               (second entity)
               (bm:add (pos (second entity)) (direction->add-vec3 payload))))

;; (gk:start 'stone-storm)
;; (gk:stop)
(defvar *cursor-position* (gamekit:vec2 0 0))

(gk:define-font stone-storm::monospace "assets/Press_Start_2P/PressStart2P-Regular.ttf")
(defun init-world ()
  (setf *world* (make-instance 'stone-storm-world))
  (load-level *world* #p"assets/levels/01")
  (c:add-system *world*
                :move-player
                (make-instance 'move-player))
  (gamekit:bind-cursor (lambda (x y)
                         "Save cursor position"
                         (setf (gamekit:x *cursor-position*) x
                               (gamekit:y *cursor-position*) y)))
  (gk:bind-button :left :pressed
                  (lambda ()
                    (c:tick-event *world* :move-player :left)))
  (gk:bind-button :right :pressed
                  (lambda ()
                    (c:tick-event *world* :move-player :right)))
  (gk:bind-button :up :pressed
                  (lambda ()
                    (c:tick-event *world* :move-player :up)))
  (gk:bind-button :down :pressed
                  (lambda ()
                    (c:tick-event *world* :move-player :down))))
(defmethod gk:post-initialize ((app stone-storm))
  (init-world))

(defun sorted-by-z (query)
  "Each position is a vec3, we reverse sort by z"
  (r:safe-sort
   query
   (lambda (e1 e2)
     (> (bm:z (pos (first e1))) (bm:z (pos (first e2)))))))

(defun render-tile (position tile)
  (gk:draw-text (string tile)
                (bm:mult position +tile-size+)
                :font (gk:make-font 'stone-storm::monospace +tile-size+)))

(defmethod gk:draw ((app stone-storm))
  (iter
    (for (position . entities)
         in (group-by
             (c:query *world* '(_ pos tile))
             :test #'equal-coordinates-p
             :key (lambda (e) (bm:value->vec2 (pos (second e))))))
    (render-tile position (tile (second (first entities)))))
  (when *debug*
    (gk:draw-text (format-world-coordinates (cursor-position->world-coordinates *cursor-position*))
               (bm:vec2 0 (- *window-height* +tile-size+ -5)))))

(defmethod gk:act ((app stone-storm)))
