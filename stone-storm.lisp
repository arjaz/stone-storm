;;;; stone-storm.lisp

(in-package #:stone-storm)

;; The first feature:
;;   The player can move around the map and not fall over the edges.

(defparameter *window-width* 800)
(defparameter *window-height* 600)
(defparameter +tile-size+ 25.0)
;; So bad, I need to fix this.
(defparameter *physical-width* (/ *window-width* +tile-size+))
(defparameter *physical-height* (/ *window-height* +tile-size+))

(defclass stone-storm-world (chakra:world) ())
(defvar *world*)

(gamekit:defgame stone-storm () ()
  (:viewport-width *window-width*)
  (:viewport-height *window-height*)
  (:viewport-title "Stone Storm"))

(defun real-time-seconds ()
  "Return current seconds"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defclass pos (chakra:component) ((pos :accessor pos :initarg :pos)))
(defclass tile (chakra:component) ((tile :accessor tile :initarg :tile)))
(defclass player (chakra:component) ())

(chakra:defsystem render-all (world event payload (entity pos tile))
  (declare (ignore world event payload))
  (let* ((x (gamekit:x (pos (second entity))))
         (y (gamekit:y (pos (second entity))))
         (pos (gamekit:vec2 (* x +tile-size+) (* y +tile-size+))))
    (gamekit:draw-text (string (tile (third entity)))
                       pos
                       :font (gamekit:make-font 'stone-storm::monospace +tile-size+))))

(defun make-player (world)
  (chakra:add-components
   world (chakra:make-entity world)
   (make-instance 'player)
   (make-instance 'pos :pos (gamekit:vec2 1 1))
   (make-instance 'tile :tile #\@)))

(defun in-world-map-p (x y)
  (and (<= 0 x *physical-width*)
       (<= 0 y *physical-height*)))

(chakra:defsystem move-player (world event payload (entity pos player))
  (declare (ignore world event))
  (let ((x (gamekit:x (pos (second entity))))
        (y (gamekit:y (pos (second entity)))))
    (ecase payload
      ((:up)
       (when (in-world-map-p x (1+ y))
         (incf (gamekit:y (pos (second entity))))))
      ((:down)
       (when (in-world-map-p x (1- y))
         (decf (gamekit:y (pos (second entity))))))
      ((:left)
       (when (in-world-map-p (1- x) y)
         (decf (gamekit:x (pos (second entity))))))
      ((:right)
       (when (in-world-map-p (1+ x) y)
         (incf (gamekit:x (pos (second entity)))))))))

;; (gamekit:start 'stone-storm)
;; (gamekit:stop)

(gamekit:define-font stone-storm::monospace "assets/Press_Start_2P/PressStart2P-Regular.ttf")
(defmethod gamekit:post-initialize ((app stone-storm))
  (setf *world* (make-instance 'stone-storm-world))
  (make-player *world*)
  (chakra:add-system *world*
                     :render-all
                     (make-instance 'render-all))
  (chakra:add-system *world*
                     :move-player
                     (make-instance 'move-player))
  (gamekit:bind-button :left :pressed
                       (lambda ()
                         (chakra:tick-event *world* :move-player :left)))
  (gamekit:bind-button :right :pressed
                       (lambda ()
                         (chakra:tick-event *world* :move-player :right)))
  (gamekit:bind-button :up :pressed
                       (lambda ()
                         (chakra:tick-event *world* :move-player :up)))
  (gamekit:bind-button :down :pressed
                       (lambda ()
                         (chakra:tick-event *world* :move-player :down))))

(defmethod gamekit:draw ((app stone-storm))
  (chakra:tick-event *world* :render-all))

(defmethod gamekit:act ((app stone-storm)))
