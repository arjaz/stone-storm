;;;; chakra.lisp

(defpackage #:chakra
  (:use #:cl #:iter)
  (:export
   #:world
   #:query
   #:resource
   #:make-entity
   #:remove-entity
   #:remove-entities
   #:add-component
   #:add-components
   #:remove-component
   #:remove-components
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
    :accessor entity-components
    :documentation "A hash table from the entity id to its components - hash tables from the component types to their data.")
   (entity-ids
    :initform (make-array 256 :fill-pointer 0
                              :adjustable t
                              :element-type 'bit
                              :initial-element 0)
    :accessor entity-ids
    :documentation "An array of all entity ids.")
   (resources
    :initform (make-hash-table)
    :accessor resources
    :documentation "A hash table from resource type to its value."))
  (:documentation "Handles the entities. Hash tables are used to enforce uniqueness."))

(defclass component () ()
  (:documentation "A base class for user-defined components"))
(defclass resource () ()
  (:documentation "A base class for user-defined resources"))

(defun query-positive-dependencies (query)
  "Extract the dependencies of the QUERY which must be present."
  (remove-if-not #'symbolp (rest query)))

(defun query-negative-dependencies (query)
  "Extract the dependencies of the QUERY which must not be present."
  (mapcar #'second (remove-if #'symbolp (rest query))))

(defun negative-dependencies-satisfied-p (world entity query)
  "Check if the negative component dependencies of the QUERY which must not be present in the ENTITY
are indeed not associated with that ENTITY in the given WORLD."
  (let ((components (gethash entity (entity-components world))))
    (iter (for component-type in (query-negative-dependencies query))
      (when (nth-value 1 (gethash component-type components))
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
  (handler-case
      (not (zerop (aref (entity-ids world) entity)))
    (sb-int:invalid-array-index-error () nil)))

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
