;;;; chakra.lisp

(defpackage #:chakra
  (:use #:cl #:iter)
  (:export
   #:world
   #:query
   #:resource
   #:system
   #:defsystem
   #:make-entity
   #:remove-entity
   #:remove-entities
   #:tick-event
   #:add-component
   #:add-components
   #:remove-component
   #:remove-components
   #:add-system
   #:add-systems
   #:remove-system
   #:remove-systems
   #:add-resource
   #:add-resources
   #:remove-resource
   #:remove-resources
   #:component
   #:components
   #:has-a))

(in-package #:chakra)

(defclass world ()
  ((entity-components
    :initform (make-hash-table)
    :type hash-table
    :accessor entity-components
    :documentation "A hash table from the entity id to its components - hash tables from the component types to their data.")
   (entity-ids
    :initform (make-array 256 :fill-pointer 0
                              :adjustable t
                              :element-type 'bit
                              :initial-element 0)
    :type array
    :accessor entity-ids
    :documentation "An array of all entity ids.")
   (systems
    :initform (make-hash-table)
    :type hash-table
    :accessor systems
    :documentation "A hash table from the event type to the systems - hash tables from the system type to the system object.")
   (resources
    :initform (make-hash-table)
    :type hash-table
    :accessor resources
    :documentation "A hash table from resource type to its value."))
  (:documentation "Handles the entities and the systems. Hash tables are used to enforce uniqueness."))

(defclass component () ()
  (:documentation "A base class for user-defined components"))
(defclass resource () ()
  (:documentation "A base class for user-defined resources"))

(defclass system ()
  ((queries
    :initarg :queries
    :type list
    :accessor system-queries
    :documentation "The list of queries of the given system.
A query is a list where the first element is the name,
and the rest are either component names or lists of two elements of form (not component-name).")))

(defun query-positive-dependencies (query)
  "Extract the dependencies of the QUERY which must be present."
  (remove-if-not #'symbolp (rest query)))

(defun query-negative-dependencies (query)
  "Extract the dependencies of the QUERY which must not be present."
  (mapcar #'second (remove-if #'symbolp (rest query))))

(defgeneric tick-system-fn (system))
(defmacro defsystem (name (&rest queries) &body body)
  "Create a system with a NAME.
Creates a class to store the QUERIES, and a function to run the BODY.
The first argument of the function must be the world, and the second must be the event."
  (let ((world-name   (first queries))
        (event-name   (second queries))
        (payload-name (third queries))
        (query-names  (iter (for query in (rest (rest (rest queries))))
                        (collect (first query)))))
    `(progn
       ;; TODO: query-names destructuring
       (defun ,name (,world-name ,event-name ,payload-name ,@query-names) ,@body)
       (defclass ,name (system) ()
         (:default-initargs
          :queries ',(rest (rest (rest queries)))))
       (defmethod tick-system-fn ((s ,name))
         (declare (ignore s))
         (symbol-function (quote ,name))))))

(defun in-hash-table-p (key hash-table)
  "T if the KEY is in the HASH-TABLE."
  (nth-value 1 (gethash key hash-table)))

(defun negative-dependencies-satisfied-p (world entity query)
  "Check if the negative component dependencies of the QUERY which must not be present in the ENTITY
are indeed not associated with that ENTITY in the given WORLD."
  (let ((components (gethash entity (entity-components world))))
    (iter (for component-type in (query-negative-dependencies query))
      (when (in-hash-table-p component-type components)
        (leave))
      (finally (return t)))))

(defun positive-dependencies (world entity query)
  "Check if the positive component dependencies of the QUERY which must be present in the ENTITY
are indeed associated with that ENTITY in the given WORLD."
  (let ((components (gethash entity (entity-components world))))
    (iter (for component-type in (query-positive-dependencies query))
      (alexandria:if-let (component (gethash component-type components))
        (collect component into collected-components)
        (leave))
      (finally (return (values collected-components t))))))

(defun entity-defined-p (world entity)
  "Check if the ENTITY is present in the WORLD."
  (not (zerop (aref (entity-ids world) entity))))

(defun query (world query)
  "Extract the list with the data of matching components based on the QUERY.
The second value indicates whether the query was succesful.

E.g. a query '(_ position health) would return a list of matched entities:
'((0 pos-0 health-0)
  (1 pos-1 health-1))"
  (iter (for entity from 0 below (length (entity-ids world)))
    (when (and (entity-defined-p world entity)
               (negative-dependencies-satisfied-p world entity query))
      (alexandria:when-let (components (positive-dependencies world entity query))
        (collect (append (list entity) components) into collected-components)))
    (finally (return (values collected-components t)))))

(defun query-entity-components (world entity query)
  "Extract the list with the data of matching components based on the QUERY.
The second value indicates whether the query was successful."
  (when (negative-dependencies-satisfied-p world entity (append (list 'ignore) query))
    (positive-dependencies world entity (append (list 'ignore) query))))

(defun make-entity (world)
  "Inserts a new empty entity into the WORLD and returns it."
  (flet ((initialize-components (entity)
           (setf (gethash entity (entity-components world)) (make-hash-table))))
    (iter (for entity from 0 below (length (entity-ids world)))
      ;; found an empty slot in the entities array - use it
      (unless (entity-defined-p world entity)
        (setf (aref (entity-ids world) entity) 1)
        (initialize-components entity)
        (leave entity))
      ;; if no empty slots found - extend the entities array
      (finally (vector-push-extend 1 (entity-ids world))
               (initialize-components entity)
               (return entity)))))

(defun remove-entity (world entity)
  "Removes the given ENTITY from the WORLD and clears out all associated components."
  (when (zerop (aref (entity-ids world) entity))
    (warn "Entity ~a not found, nothing removed.~%" entity)
    (return-from remove-entity nil))
  (remhash entity (entity-components world))
  (setf (aref (entity-ids world) entity) 0))

(defun remove-entities (world &rest entities)
  "Removes all the ENTITIES from the WORLD and clears out all associated components"
  (iter (for e in entities)
    (remove-entity world e)))

(defun cartesian-product (l)
  "Compute the n-cartesian product of a list of sets (each of them represented as list)."
  (if (null l)
      (list nil)
      ;; TODO: iter
      (loop for x in (car l)
            nconc (loop for y in (cartesian-product (cdr l))
                        collect (cons x y)))))

(defun tick-system (world system event &optional payload)
  "Tick the SYSTEM of the WORLD with the passed event and the payload.
Runs the system against all components matching the query of the SYSTEM."
  (let ((queried-data (iter (for query in (system-queries system))
                        (collect (query world query)))))
    ;; TODO: skip if any two of them are the same
    (iter (for args in (cartesian-product queried-data))
      (apply (tick-system-fn system) world event payload args))))

(defun tick-event (world event &optional payload)
  "Tick all systems subscribed to the EVENT in the WORLD with the PAYLOAD."
  (iter (for (system-type system) in-hashtable (gethash event (systems world)))
    (tick-system world system event payload)))

;; TODO: setf interface for all the things, setf with nil to delete?
(defun component (world entity component-type)
  "Query a COMPONENT-TYPE of the given ENTITY."
  (let ((components (gethash entity (entity-components world))))
    (if components
        (values (gethash component-type components) t)
        (values nil nil))))

(defun add-component (world entity component)
  "Adds a COMPONENT to the ENTITY in the WORLD."
  (let ((components (gethash entity (entity-components world))))
    (if components
        (setf (gethash (type-of component) components) component)
        (error "no entity found ~a" entity))))

(defun add-components (world entity &rest components)
  (iter (for c in components)
    (add-component world entity c)))

(defun components (world entity)
  "Get all components of the given entity."
  (gethash entity (entity-components world)))

(defun has-a (component world entity)
  "Queries the COMPONENT and returns T if it's there."
  (and (component world entity component) t))

(defun remove-component (world entity component-type)
  "Removes the COMPONENT-TYPE from the ENTITY in the WORLD."
  (let ((e-components (gethash entity (entity-components world))))
    (remhash component-type e-components)))

(defun remove-components (world entity &rest components)
  "Removes all COMPENENTS from the ENTITY in the WORLD."
  (iter (for c in components)
    (remove-component world entity c)))

(defun add-system (world event system)
  "Creates a SYSTEM in the WORLD fired after the EVENT."
  (unless (in-hash-table-p event (systems world))
    (setf (gethash event (systems world)) (make-hash-table)))
  (setf (gethash (type-of system) (gethash event (systems world))) system))

(defun add-systems (world event &rest systems)
  "Add all SYSTEMS to the WORLD fired after the EVENT.."
  (iter (for s in systems)
    (add-system world event s)))

(defun remove-system (world event system)
  "Removes the SYSTEM bound to the EVENT from the WORLD."
  (when (in-hash-table-p event (systems world))
    (remhash (type-of system) (gethash event (systems world)))))

(defun remove-systems (world event &rest systems)
  "Removes all SYSTEMS bound to the EVENT from the world"
  (iter (for s in systems)
    (remove-system world event s)))

(defun get-system (world event system-type)
  "Gets the system object by its type and event from the WORLD."
  (gethash system-type (gethash event (systems world))))

(defun get-resource (world resource-type)
  "Gets the resource object by its type from the WORLD."
  (gethash resource-type (resources world)))

(defun add-resource (world resource)
  "Adds the RESOURCE to the WORLD."
  (setf (gethash (type-of resource) (resources world)) resource))

(defun add-resources (world &rest resources)
  "Adds all RESOURCES to the WORLD."
  (iter (for r in resources)
    (add-resource world r)))

(defun remove-resource (world resource)
  "Removes the RESOURCE from the WORLD."
  (remhash (type-of resource) (resources world)))

(defun remove-resources (world &rest resources)
  "Removes all REOUSRCES from the WORLD."
  (iter (for r in resources)
    (remove-resource world r)))
