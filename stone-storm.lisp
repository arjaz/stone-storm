;;;; stone-storm.lisp

(in-package #:stone-storm)

(named-readtables:in-readtable r:rutils-readtable)

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

(defgeneric handle-input (mode world key))
(defgeneric render (mode world))

(defun push-log (world msg)
  (push msg (logs world)))

(defclass pos (c:component) ((v :accessor v :initarg :v)))
(defclass tile (c:component) ((tile :accessor tile :initarg :tile)))
(defclass player (c:component) ())
(defclass collider (c:component) ())
(defclass door (c:component) ())
(defclass health (c:component) ((health :accessor health :initarg :health)))
(defclass named (c:component) ((name :accessor name :initarg :name)))
(defclass inventory (c:component) ((items :accessor items :initarg :items)))
(defclass wall (c:component) () (:documentation "Used to render the walls nicely."))

(defun place-player (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'player)
   (make-instance 'named :name "Player")
   (make-instance 'collider)
   (make-instance 'pos :v #v(x y 99))
   (make-instance 'tile :tile #\@)
   (make-instance 'health :health 3)))

(defun place-enemy (world x y tile &key (name nil))
  (let ((enemy (c:make-entity world)))
    (c:add-components
     world enemy
     (make-instance 'collider)
     (make-instance 'pos :v #v(x y 50))
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
   (make-instance 'pos :v #v(x y 1))))

(defun place-closed-door (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'named :name "Closed door")
   (make-instance 'collider)
   (make-instance 'door)
   (make-instance 'tile :tile #\+)
   (make-instance 'pos :v #v(x y 1))))

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
        ((#\P) (place-enemy-pillar world x y))
        ((#\g) (place-enemy world x y #\g :name "Grave robber"))))))

(defun in-world-map-p (pos)
  (and (<= 0 (aref pos 0) (1- *viewport-width*))
       (<= 0 (aref pos 1) (1- *viewport-height*))))

(defun equal-coordinates-p (pos1 pos2)
  (equalp (vec3->vec2 pos1) (vec3->vec2 pos2)))

(defun entity-at (pos query &key (get-position #'second))
  "Returns any entity at the given coordinates in the query."
  (iter (for entity in query)
    (when (equal-coordinates-p pos (v (funcall get-position entity)))
      (return (first entity)))))

(defun entities-at (pos query &key (get-position #'second))
  "Returns all entities at the given coordinates in the query."
  (iter (for entity in query)
    (when (equal-coordinates-p pos (v (funcall get-position entity)))
      (collect (first entity)))))

(defun direction->add-vec3 (direction)
  (ecase direction
    ((:up) #v(0 -1 0))
    ((:down) #v(0 1 0))
    ((:left) #v(-1 0 0))
    ((:right) #v(1 0 0))))

(defun open-door (world door)
  (c:remove-component world door 'collider)
  (c:add-components world door
                    (make-instance 'named :name "Opened door")
                    (make-instance 'tile :tile #\…)))

(defun close-door (world door)
  (c:add-components world door
                    (make-instance 'collider)
                    (make-instance 'named :named "Closed door")
                    (make-instance 'tile :tile #\+)))

(defun kill-entity (world entity name)
  (push-log world (format nil "~a died" name))
  (r:when-let (position (c:component world entity 'pos))
    (r:when-let (inventory (c:component world entity 'inventory))
      (iter (for e in (items inventory))
        (for new-pos = (make-instance 'pos :v (copy-vec3 (v position))))
        (c:add-component world e new-pos))))
  (c:remove-entity world entity))

(defun damage-health (world entity damage)
  (let ((name (r:if-let (named (c:component world entity 'named))
                (name named)
                (format nil "#~a" entity))))
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
    (for (entity pos player) in (c:query world '(entity pos player)))
    (with colliders = (c:query world '(_ pos collider)))
    (declare (ignorable entity player))
    (handle-move world colliders pos
                 (vec3+ (v pos) (direction->add-vec3 direction)))))

(defun render-tile (position tile)
  (setf (blt:color)
        (blt:white)
        (blt:cell-char (aref position 0) (aref position 1))
        tile))

(defclass main-game-mode () ())

(defmethod handle-input ((mode main-game-mode) world key)
  (blt:key-case
   key
   (:escape (setf *running* nil))
   (:close (setf *running* nil))
   (:r (init-world))
   (:l
    (let* ((player-pos (second (first (c:query *world* '(_ pos player)))))
           (mode (make-instance 'lookup-mode
                                :crosshair-pos (make-instance 'pos :v (copy-vec3 (v player-pos))))))
      (enter-mode world mode)
      (describe-at-crosshair world (crosshair-pos mode))))
   (:left (move-player world :left))
   (:right (move-player world :right))
   (:up (move-player world :up))
   (:down (move-player world :down))))

(defun wall-bitmap (world position)
  (let ((walls (c:query world '(_ pos wall)))
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

(defun sorted-by-z (query)
  "Each position is a vec3, we reverse sort by z"
  (r:safe-sort
   query
   (lambda (e1 e2)
     (> (aref (v (first e1)) 2) (aref (v (first e2)) 2)))))

(defmethod render ((mode main-game-mode) world)
  (iter
    (with sorted = (group-by
                    (c:query world '(_ pos tile))
                    :test #'equal-coordinates-p
                    :value #'identity
                    :key (lambda (e) (vec3->vec2 (v (second e))))))
    (for entry in sorted)
    (for (entity pos tile) = (second entry))
    (if (c:has-a 'wall world entity)
        (render-wall world pos)
        (render-tile (v pos) (tile tile)))))

(defun move-crosshair (crosshair-pos direction)
  (let ((new-pos (vec3+ (v crosshair-pos)
                        (direction->add-vec3 direction))))
    (when (in-world-map-p new-pos)
      (setf (v crosshair-pos) new-pos))))

(defun describe-at-crosshair (world crosshair-pos)
  (iter
    (for entity in (entities-at (v crosshair-pos) (c:query world '(_ pos))))
    (for name = (r:if-let (named (c:component world entity 'named))
                  (name named)
                  (format nil "#~a" entity)))
    (push-log world (format nil "Looking at ~a" name))))

(defclass lookup-mode () ((crosshair-pos :accessor crosshair-pos :initarg :crosshair-pos)))

(defmethod handle-input ((mode lookup-mode) world key)
  (blt:key-case
   key
   (:q (leave-mode world))
   (:escape (leave-mode world))
   (:left
    (move-crosshair (crosshair-pos mode) :left)
    (describe-at-crosshair world (crosshair-pos mode)))
   (:right
    (move-crosshair (crosshair-pos mode) :right)
    (describe-at-crosshair world (crosshair-pos mode)))
   (:up
    (move-crosshair (crosshair-pos mode) :up)
    (describe-at-crosshair world (crosshair-pos mode)))
   (:down
    (move-crosshair (crosshair-pos mode) :down)
    (describe-at-crosshair world (crosshair-pos mode)))))

(defmethod render ((mode lookup-mode) world)
  (let ((half-a-second 500000)
        (crosshair (crosshair-pos mode)))
    (when (oddp (floor (get-internal-real-time) half-a-second))
      (render-tile (v crosshair) #\·))))

(defun render-logs (logs)
  (iter (for msg in-sequence logs with-index i)
    (when (>= (1+ i) *message-height*)
      (return))
    (blt:print 0 (+ *viewport-height* i)
               (format nil "~d: ~a" i msg))))

(defun draw (world)
  (blt:clear)
  (iter (for mode in (reverse (modes world)))
    (render mode world))
  (render-logs (logs world))
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

(defun run ()
  (when *running*
    (draw *world*)
    (when (blt:has-input-p)
      (handle-input (first (modes *world*)) *world* (blt:read)))
    (run)))

(defun start ()
  (init-world)
  (setf *running* t)
  (blt:with-terminal
    (configure-window)
    (run)))

;; (start)
