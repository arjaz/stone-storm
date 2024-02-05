;;;; stone-storm.lisp

(in-package #:stone-storm)

(in-readtable stone-storm-readtable)

(defparameter +target-fps+ 60)
(defparameter *screen-width* 80)
(defparameter *screen-height* 50)
(defparameter *message-height* 10)
(defparameter *viewport-width* *screen-width*)
(defparameter *viewport-height* (- *screen-height* *message-height*))

(defparameter *debug* t)
(defvar *running*)
(defvar *world*)

(defclass stone-storm-world (c:world)
  ((modes :accessor modes :initarg :modes :initform nil)
   (logs :accessor logs :initarg :logs :initform nil)))

(defun push-log (world msg)
  (push msg (logs world)))

(defclass pos () ((v :accessor v :initarg :v)))
(defclass tile () ((tile :accessor tile :initarg :tile)))
(defclass player () ())
(defclass collider () ())
(defclass door () ())
(defclass health () ((health :accessor health :initarg :health)))
(defclass named () ((name :accessor name :initarg :name)))
(defclass inventory () ((items :accessor items :initarg :items)))
(defclass wall () () (:documentation "Used to render the walls nicely."))
(defclass glasses () ())
(defclass seen () ((v :accessor v :initarg :v :documentation "Coordinates where seen")))
(defclass visible () ())

(defun place-monocle (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'glasses)
   (make-instance 'named :name "Monocle")
   (make-instance 'tile :tile #\o)
   (make-instance 'pos :v #a(x y 10))))

(defun place-player (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'player)
   (make-instance 'named :name "Player")
   (make-instance 'collider)
   (make-instance 'pos :v #a(x y 99))
   (make-instance 'tile :tile #\@)
   (make-instance 'health :health 3)
   (make-instance 'inventory :items nil)))

(defun place-enemy (world x y tile &key (name nil))
  (let ((enemy (c:make-entity world)))
    (c:add-components
     world enemy
     (make-instance 'collider)
     (make-instance 'pos :v #a(x y 50))
     (make-instance 'tile :tile tile)
     (make-instance 'health :health 3))
    (when name
      (c:add-component world enemy (make-instance 'named :name name)))
    enemy))

(defun place-enemy-pillar (world x y)
  (let ((enemy (place-enemy world x y #\P :name "Moving pillar"))
        (pillar (c:make-entity world)))
    (c:add-components
     world pillar
     (make-instance 'named :name "Stone pillar")
     (make-instance 'collider)
     (make-instance 'tile :tile #\O))
    (c:add-component
     world enemy
     (make-instance 'inventory :items (list pillar)))))

(defun place-wall (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'named :name "Wall")
   (make-instance 'collider)
   (make-instance 'wall)
   (make-instance 'tile :tile #\#)
   (make-instance 'pos :v #a(x y 1))))

(defun place-closed-door (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'named :name "Closed door")
   (make-instance 'collider)
   (make-instance 'door)
   (make-instance 'tile :tile #\+)
   (make-instance 'pos :v #a(x y 1))))

(defun load-level (world filename)
  (iter
    (for line in-sequence (r:split #\newline (r:read-file filename)) with-index y)
    (when (> y *viewport-height*)
      (error "Level [~a] is bigger than allowed, expected max ~a rows, found at least ~a" filename *viewport-height* y))
    (iter (for char in-string line with-index x)
      (when (> x *viewport-width*)
        (error "Level [~a] is bigger than allowed, expected max ~a colums, found at least ~a" filename *viewport-width* x))
      (case char
        ((#\#) (place-wall world x y))
        ((#\+) (place-closed-door world x y))
        ((#\@) (place-player world x y))
        ((#\o) (place-monocle world x y))
        ((#\P) (place-enemy-pillar world x y))
        ((#\g) (place-enemy world x y #\g :name "Grave robber"))))))

(defun in-world-map-p (pos)
  (and (<= 0 (aref pos 0) (1- *viewport-width*))
       (<= 0 (aref pos 1) (1- *viewport-height*))))

(defun equal-coordinates-p (pos1 pos2)
  (and (equal (aref pos1 0) (aref pos2 0))
       (equal (aref pos1 1) (aref pos2 1))))

(defun entity-at (pos query &key (get-position #'second))
  "Returns any entity at the given coordinates in the query."
  (iter (for entity in query)
    (for entity-pos = (v (funcall get-position entity)))
    (finding (first entity) such-that (equal-coordinates-p pos entity-pos))))

(defun entities-at (pos query &key (get-position #'second))
  "Returns all entities at the given coordinates in the query."
  (iter (for entity in query)
    (when (equal-coordinates-p pos (v (funcall get-position entity)))
      (collect (first entity)))))

(defun direction->add-vec3 (direction)
  (ecase direction
    ((:up) #(0 -1 0))
    ((:down) #(0 1 0))
    ((:left) #(-1 0 0))
    ((:right) #(1 0 0))))

(defun open-door (world door)
  (c:remove-component world door 'collider)
  (c:add-components world door
                    (make-instance 'named :name "Opened door")
                    (make-instance 'tile :tile #\…)))

(defun close-door (world door)
  (c:add-components world door
                    (make-instance 'collider)
                    (make-instance 'named :name "Closed door")
                    (make-instance 'tile :tile #\+)))

(defun toggle-door (world door)
  (if (c:has-a 'collider world door)
    (open-door world door)
    (close-door world door)))

(defun kill-entity (world entity name)
  (push-log world (format nil "~a died" name))
  (r:when-let (position (c:component world entity 'pos))
    (r:when-let (inventory (c:component world entity 'inventory))
      (iter (for e in (items inventory))
        (for new-pos = (make-instance 'pos :v (copy-seq (v position))))
        (c:add-component world e new-pos))))
  (c:remove-entity world entity))

(defun describe-entity (world entity)
  (r:if-let (named (c:component world entity 'named))
    (name named)
    (format nil "#~a" entity)))

(defun damage-health (world entity damage)
  (let ((name (describe-entity world entity)))
    (decf (health (c:component world entity 'health)) damage)
    (push-log world (format nil "Dealt ~a damage to ~a" damage name))
    (when (>= 0 (health (c:component world entity 'health)))
      (kill-entity world entity name))))

(defun handle-move (world colliders from-pos-component new-pos)
  (when (in-world-map-p new-pos)
    (r:if-let (collided (entity-at new-pos colliders))
      (progn
        (when (c:has-a 'door world collided)
          (open-door world collided))
        (when (c:has-a 'health world collided)
          (damage-health world collided 1)))
      (setf (v from-pos-component) new-pos))))

(defun move-player (world direction)
  (iter
    (with colliders = (c:query world '(pos collider)))
    (for (entity pos player) in (c:query world '(pos player)))
    (declare (ignorable entity player))
    (handle-move world colliders pos
                 (vec3+ (v pos) (direction->add-vec3 direction)))))

(defgeneric render (mode world))
(defgeneric handle-input (mode world key))

(defun render-tile (position tile &key (color (blt:white)))
  (setf (blt:color)
        color
        (blt:cell-char (aref position 0) (aref position 1))
        tile))

(defun render-string (position string)
  (blt:print (aref position 0) (aref position 1) string))

(defun render-at-message-box (line string)
  (render-string #a(0 (+ line *viewport-height*)) string))

(defclass main-game-mode () ())

(defun enter-lookup-mode (world)
  (let* ((player-pos (second (first (c:query world '(pos player)))))
         (mode (make-instance
                'lookup-mode
                :crosshair-pos (make-instance 'pos :v (copy-seq (v player-pos)))
                :crosshair-start-time (get-internal-real-time))))
    (enter-mode world mode)
    (describe-at world (crosshair-pos mode))))

(defun enter-interaction-mode (world)
  (enter-mode world (make-instance 'interaction-mode)))

(defmethod handle-input ((mode main-game-mode) world key)
  (blt:key-case
   key
   (:q (setf *running* nil))
   (:escape (setf *running* nil))
   (:close (setf *running* nil))
   (:r (init-world))
   (:l (enter-lookup-mode world))
   (:space (enter-interaction-mode world))
   (:left (move-player world :left))
   (:right (move-player world :right))
   (:up (move-player world :up))
   (:down (move-player world :down))))

(defun wall-bitmap (world position)
  (let ((walls (c:query world '(pos wall)))
        (v-pos (v position))
        (bitmap 0))
    (when (entity-at (vec3+ v-pos #(0 -1 0)) walls)
      (incf bitmap #b0001))
    (when (entity-at (vec3+ v-pos #(0 1 0)) walls)
      (incf bitmap #b0010))
    (when (entity-at (vec3+ v-pos #(-1 0 0)) walls)
      (incf bitmap #b0100))
    (when (entity-at (vec3+ v-pos #(1 0 0)) walls)
      (incf bitmap #b1000))
    bitmap))

(defun wall-bitmap->tile (bitmap)
  (case bitmap
    ((#b0000) #\○)
    ((#b0001) #\○)
    ((#b0010) #\○)
    ((#b0011) #\║)
    ((#b0100) #\○)
    ((#b0101) #\╝)
    ((#b0110) #\╗)
    ((#b0111) #\╣)
    ((#b1000) #\○)
    ((#b1001) #\╚)
    ((#b1010) #\╔)
    ((#b1011) #\╠)
    ((#b1100) #\═)
    ((#b1101) #\╩)
    ((#b1110) #\╦)
    ((#b1111) #\╬)))

(defun render-wall (world position)
  (render-tile (v position) (wall-bitmap->tile (wall-bitmap world position))))

(defmethod render ((mode main-game-mode) world)
  (iter
    (with glasses-on =
      (iter
        (for p in (c:query world '(player inventory)))
        (for inventory = (items (third p)))
        (thereis
         (iter (for e in inventory)
           (thereis (c:has-a 'glasses world e))))))
    (with sorted =
      (group-by
       (c:query world '(pos tile))
       :test #'equal-coordinates-p
       :value #'identity
       :key (lambda (e) (vec3->vec2 (v (second e))))))
    (for entry in sorted)
    (for (entity pos tile) = (second entry))
    (if (and glasses-on (c:has-a 'wall world entity))
        (render-wall world pos)
        (render-tile (v pos) (tile tile))))
  (when (equal mode (first (modes world)))
    (render-logs (logs world))))

(defclass interaction-mode () ())

(defun interact-with (world direction)
  (iter
    (for (player-entity pos player) in (c:query world '(pos player)))
    (with entities-with-positions = (c:query world '(pos)))
    (for target-position = (vec3+ (v pos) (direction->add-vec3 direction)))
    (declare (ignorable player))
    (when (in-world-map-p target-position)
      (r:when-let (targetted (entity-at target-position entities-with-positions))
        (cond
          ((c:has-a 'door world targetted)
           (toggle-door world targetted))
          ((c:has-a 'glasses world targetted)
           (r:when-let (inventory (c:component world player-entity 'inventory))
             (push targetted (items inventory))
             (c:remove-component world targetted 'pos))))))))

(defmethod handle-input ((mode interaction-mode) world key)
  (blt:key-case
   key
   (:escape (leave-mode world))
   (:q (leave-mode world))
   (:left
    (interact-with world :left)
    (leave-mode world))
   (:right
    (interact-with world :right)
    (leave-mode world))
   (:up
    (interact-with world :up)
    (leave-mode world))
   (:down
    (interact-with world :down)
    (leave-mode world))))

(defmethod render ((mode interaction-mode) world)
  (render-at-message-box 0 "INTERACTION")
  (render-at-message-box 1 "Choose a direction"))

(defclass lookup-mode ()
  ((crosshair-pos :accessor crosshair-pos :initarg :crosshair-pos)
   (crosshair-start-time
    :accessor crosshair-start-time :initarg :crosshair-start-time
    :documentation "Used for blinking the crosshair")))

(defun move-crosshair (crosshair-pos direction)
  (let ((new-pos (vec3+ (v crosshair-pos)
                        (direction->add-vec3 direction))))
    (when (in-world-map-p new-pos)
      (setf (v crosshair-pos) new-pos))))

(defun describe-at (world pos)
  (iter
    (for entity in-sequence (entities-at (v pos) (c:query world '(pos)))
         with-index i)
    (for name = (describe-entity world entity))
    (render-at-message-box i (format nil "Looking at ~a" name))))

(defmethod handle-input ((mode lookup-mode) world key)
  (blt:key-case
   key
   (:q (leave-mode world))
   (:escape (leave-mode world))
   (:left
    (move-crosshair (crosshair-pos mode) :left)
    ;; We reset the time when we move the crosshair
    (setf (crosshair-start-time mode) (get-internal-real-time)))
   (:right
    (move-crosshair (crosshair-pos mode) :right)
    (setf (crosshair-start-time mode) (get-internal-real-time)))
   (:up
    (move-crosshair (crosshair-pos mode) :up)
    (setf (crosshair-start-time mode) (get-internal-real-time)))
   (:down
    (move-crosshair (crosshair-pos mode) :down)
    (setf (crosshair-start-time mode) (get-internal-real-time)))))

(defmethod render ((mode lookup-mode) world)
  (let ((half-a-second 500000)
        (crosshair (crosshair-pos mode))
        (mode-start-time (crosshair-start-time mode))
        (current-time (get-internal-real-time)))
    (when (evenp (floor (- current-time mode-start-time) half-a-second))
      (render-tile (v crosshair) #\·)))
  (describe-at world (crosshair-pos mode)))

(defun render-logs (logs)
  (iter (for msg in-sequence logs with-index i)
    (when (>= (1+ i) *message-height*) (return))
    (render-at-message-box i (format nil "~d: ~a" i msg))))

(defun draw (world)
  (blt:clear)
  (iter (for mode in (reverse (modes world)))
    (render mode world))
  (blt:refresh))

(defun configure-window ()
  (blt:set "window.resizeable = false")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = Store Storm"))

(defun init-world ()
  (setf *world* (make-instance 'stone-storm-world))
  (enter-mode *world* (make-instance 'main-game-mode))
  (load-level *world* #p"assets/levels/01"))

(defun enter-mode (world mode)
  (push mode (modes world)))

(defun leave-mode (world)
  (pop (modes world)))

(defun run (time)
  (when *running*
    (let* ((new-time   (get-internal-real-time))
           (elapsed-ms (/ (- new-time time) 100000000))
           (to-sleep   (/ (- (/ 1000 +target-fps+) elapsed-ms) 1000)))
      (sleep to-sleep)
      (draw *world*)
      (when (blt:has-input-p)
        (handle-input (first (modes *world*)) *world* (blt:read)))
      (run new-time))))

(defun start ()
  (init-world)
  (setf *running* t)
  (blt:with-terminal
    (configure-window)
    (run (get-internal-real-time))))

;; (start)

;; (ql:quickload :sb-sprof)
;; (sb-sprof:with-profiling (:report :flat
;;                           :max-samples 1000)
;;   (start))

;; (sb-profile:profile c:query)
;; (sb-profile:unprofile c:query)
;; (sb-profile:report)
