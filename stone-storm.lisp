;;;; stone-storm.lisp

(in-package #:stone-storm)
(named-readtables:in-readtable r:rutils-readtable)
;; (r:toggle-print-hash-table)

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
(defparameter *physical-height* (/ *window-height* +tile-size+))

(defclass stone-storm-world (c:world) ())
(defvar *world*)

(gk:defgame stone-storm () ()
  (:viewport-width *window-width*)
  (:viewport-height *window-height*)
  (:viewport-title "Stone Storm"))

(defun real-time-seconds ()
  "Return current seconds"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defclass pos (c:component) ((pos :accessor pos :initarg :pos)))
(defclass tile (c:component) ((tile :accessor tile :initarg :tile)))
(defclass player (c:component) ())
(defclass collider (c:component) ())
(defclass door (c:component) ())

(c:defsystem render-all (world event payload (entity pos tile))
  (declare (ignore world event payload))
  (let* ((x (gk:x (pos (second entity))))
         (y (gk:y (pos (second entity))))
         (pos (gk:vec2 (* x +tile-size+) (* y +tile-size+))))
    (gk:draw-text (string (tile (third entity)))
                       pos
                       :font (gk:make-font 'stone-storm::monospace +tile-size+))))

(defun make-player (world)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'player)
   (make-instance 'collider)
   (make-instance 'pos :pos (gk:vec2 1 1))
   (make-instance 'tile :tile #\@)))

(defun place-wall (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'collider)
   (make-instance 'tile :tile #\#)
   (make-instance 'pos :pos (gk:vec2 x y))))

(defun place-closed-door (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'collider)
   (make-instance 'door)
   (make-instance 'tile :tile #\+)
   (make-instance 'pos :pos (gk:vec2 x y))))

(defun in-world-map-p (pos)
  (and (<= 0 (bm:x pos) *physical-width*)
       (<= 0 (bm:y pos) *physical-height*)))

(defun collides-with-any (pos colliders)
  "Return entity collided with if the given position collides with any of the colliders from the (entity pos collider) query."
  (r:iter (:for entity :in colliders)
    ;; Doesn't check for self-collision
    ;; because you shouldn't move into yourself
    (when (bm:vec= pos (pos (second entity)))
      (return (first entity)))))

(defun is-a-door-p (world entity)
  (r:? world 'c::entity-components entity 'door))

(defun direction->vec2 (direction)
  (ecase direction
    ((:up) (bm:vec2 0 1))
    ((:down) (bm:vec2 0 -1))
    ((:left) (bm:vec2 -1 0))
    ((:right) (bm:vec2 1 0))))

(defun handle-move (world colliders from-pos-component new-pos)
  (when (in-world-map-p new-pos)
    (r:if-let (collided (collides-with-any new-pos colliders))
      (when (is-a-door-p world collided)
        (setf (pos from-pos-component) new-pos)
        (c:remove-component world collided 'tile)
        (c:add-component world collided (make-instance 'tile :tile #\…)))
      (setf (pos from-pos-component) new-pos))))

(c:defsystem move-player (world event payload (entity pos player))
  (declare (ignore event))
  (handle-move world
               (c:query world '(_ pos collider))
               (second entity)
               (bm:add (pos (second entity)) (direction->vec2 payload))))

;; (gk:start 'stone-storm)
;; (gk:stop)

(gk:define-font stone-storm::monospace "assets/Press_Start_2P/PressStart2P-Regular.ttf")
(defmethod gk:post-initialize ((app stone-storm))
  (setf *world* (make-instance 'stone-storm-world))
  (make-player *world*)
  (r:iter (:for i :from 0 :to 10)
    (when (not (= i 5))
      (place-wall *world* i 5)))
  (place-closed-door *world* 5 5)
  (r:iter (:for i :from 0 :to 4)
    (place-wall *world* 10 i))
  (c:add-system *world*
                     :render-all
                     (make-instance 'render-all))
  (c:add-system *world*
                     :move-player
                     (make-instance 'move-player))
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

(defmethod gk:draw ((app stone-storm))
  (c:tick-event *world* :render-all))

(defmethod gk:act ((app stone-storm)))
