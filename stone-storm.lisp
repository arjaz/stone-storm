;;;; stone-storm.lisp

(in-package #:stone-storm)

;; The first feature:
;;   The player can move around the map and not fall over the edges.
;; The second feature:
;;   There are walls that the player can't pass through.

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

(defun in-world-map-p (x y)
  (and (<= 0 x *physical-width*)
       (<= 0 y *physical-height*)))

(defun collides-with-any-p (x y colliders)
  "Return true if the given position collides with any of the colliders from the (entity pos collider) query."
  (iter (for entity in colliders)
    ;; Doesn't check for self-collision
    ;; because you shouldn't move into yourself
    (when (and (= x (gk:x (pos (second entity))))
               (= y (gk:y (pos (second entity)))))
      (return t))))

(c:defsystem move-player (world event payload (entity pos player))
  (declare (ignore event))
  (let ((x (gk:x (pos (second entity))))
        (y (gk:y (pos (second entity))))
        (colliders (c:query world '(another pos collider))))
    (ecase payload
      ((:up)
       (when (and (in-world-map-p x (1+ y)) (not (collides-with-any-p x (1+ y) colliders)))
         (incf (gk:y (pos (second entity))))))
      ((:down)
       (when (and (in-world-map-p x (1- y)) (not (collides-with-any-p x (1- y) colliders)))
         (decf (gk:y (pos (second entity))))))
      ((:left)
       (when (and (in-world-map-p (1- x) y) (not (collides-with-any-p (1- x) y colliders)))
         (decf (gk:x (pos (second entity))))))
      ((:right)
       (when (and (in-world-map-p (1+ x) y) (not (collides-with-any-p (1+ x) y colliders)))
         (incf (gk:x (pos (second entity)))))))))

;; (gk:start 'stone-storm)
;; (gk:stop)

(gk:define-font stone-storm::monospace "assets/Press_Start_2P/PressStart2P-Regular.ttf")
(defmethod gk:post-initialize ((app stone-storm))
  (setf *world* (make-instance 'stone-storm-world))
  (make-player *world*)
  (iter (for i from 1 to 10)
    (place-wall *world* i 5))
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
