;;;; stone-storm.lisp

(in-package #:stone-storm)

(named-readtables:in-readtable r:rutils-readtable)

(defparameter *screen-width* 80)
(defparameter *screen-height* 50)
(defparameter *message-height* 10)
(defparameter *viewport-width* *screen-width*)
(defparameter *viewport-height* (- *screen-height* *message-height*))

(defparameter *debug* t)
(defvar *world*)

(defclass stone-storm-world (c:world)
  ((logs :accessor logs :initarg :logs :initform nil)))

(defun push-log (world msg)
  (push msg (logs world)))

(defun copy-vec3 (v)
  #v((aref v 0)
     (aref v 1)
     (aref v 2)))

(defun vec2+ (v1 v2)
  #v((+ (aref v1 0) (aref v2 0))
     (+ (aref v1 1) (aref v2 1))))

(defun vec3+ (v1 v2)
  #v((+ (aref v1 0) (aref v2 0))
     (+ (aref v1 1) (aref v2 1))
     (+ (aref v1 2) (aref v2 2))))

(defun vec3->vec2 (v)
  #v((aref v 0) (aref v 1)))

(defclass pos (c:component) ((v :accessor v :initarg :v)))
(defclass tile (c:component) ((tile :accessor tile :initarg :tile)))
(defclass player (c:component) ())
(defclass collider (c:component) ())
(defclass door (c:component) ())
(defclass health (c:component) ((health :accessor health :initarg :health)))
(defclass named (c:component) ((name :accessor name :initarg :name)))
(defclass inventory (c:component) ((items :accessor items :initarg :items)))

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
   (make-instance 'tile :tile #\#)
   (make-instance 'pos :v #v(x y 1))))

(defun place-closed-door (world x y)
  (c:add-components
   world (c:make-entity world)
   (make-instance 'named :name "Door")
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
        ((#\g) (place-enemy world x y #\g :name "Goblin"))))))

(defun in-world-map-p (pos)
  (and (<= 0 (aref pos 0) *viewport-width*)
       (<= 0 (aref pos 1) *viewport-height*)))

(defun equal-coordinates-p (pos1 pos2)
  (equalp (vec3->vec2 pos1) (vec3->vec2 pos2)))

(defun entity-at (pos query &key (get-position #'second))
  "Returns any entity at the given coordinates in the query."
  (iter (for entity in query)
    (when (equal-coordinates-p pos (v (funcall get-position entity)))
      (return (first entity)))))

(defun direction->add-vec3 (direction)
  (ecase direction
    ((:up) #v(0 -1 0))
    ((:down) #v(0 1 0))
    ((:left) #v(-1 0 0))
    ((:right) #v(1 0 0))))

(defun open-door (world door)
  (c:remove-component world door 'collider)
  (c:add-component world door
                   (make-instance 'tile :tile #\…)))

(defun close-door (world door)
  (c:add-components world door
                    (make-instance 'collider)
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
                 (vec2+ (v pos) (direction->add-vec3 direction)))))

(defun sorted-by-z (query)
  "Each position is a vec3, we reverse sort by z"
  (r:safe-sort
   query
   (lambda (e1 e2)
     (> (aref (v (first e1)) 2) (aref (v (first e2)) 2)))))

(defun render-tile (position tile)
  (setf (blt:color)
        (blt:white)
        (blt:cell-char (aref position 0) (aref position 1))
        tile))

(defun render-world (world)
  (iter
    (for (position . entities)
         in (group-by
             (c:query world '(_ pos tile))
             :test #'equal-coordinates-p
             :key (lambda (e) (vec3->vec2 (v (second e))))))
    (render-tile position (tile (second (first entities))))))

(defun render-logs (logs)
  (iter (for msg in-sequence logs with-index i)
    (when (>= (1+ i) *message-height*)
      (return))
    (blt:print 0 (+ *viewport-height* i)
               (format nil "~d: ~a" i msg))))

(defun draw (world)
  (blt:clear)
  (render-world world)
  (render-logs (logs world))
  (blt:refresh))

(defun configure-window ()
  (blt:set "window.resizeable = false")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = Store Storm"))

(defun init-world ()
  (setf *world* (make-instance 'stone-storm-world))
  (load-level *world* #p"assets/levels/01"))

;; The way we should do interaction is by binding a key to a fuction
;; that would itself call key-case and determine what to do
(defun interact-with (world)
  (declare (ignore world))
  (blt:key-case
   (blt:read)
   (:left nil)
   (:right nil)
   (:up nil)
   (:down nil)))

(defun run ()
  (draw *world*)
  (blt:key-case
   (blt:read)
   (:escape (return-from run))
   (:close (return-from run))
   (:r (init-world))
   (:space (interact-with *world*))
   (:left (move-player *world* :left))
   (:right (move-player *world* :right))
   (:up (move-player *world* :up))
   (:down (move-player *world* :down)))
  (run))

(defun start ()
  (init-world)
  (blt:with-terminal
    (configure-window)
    (run)))

;; (start)
