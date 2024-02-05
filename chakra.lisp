;;;; chakra.lisp

(defpackage #:chakra
  (:use #:cl #:iter)
  (:local-nicknames
   (:r #:rutils))
  (:export
   #:world
   #:query
   #:make-entity
   #:remove-entity
   #:remove-entities
   #:add-component
   #:add-components
   #:remove-component
   #:remove-components
   #:component
   #:has-a))

(in-package #:chakra)

(defclass world ()
  ((entity-components
    :initform (make-hash-table)
    :accessor entity-components
    :documentation "A hash table from component types to arrays of components.")
   (entity-ids
    :initform (make-array 255 :fill-pointer 255
                              :adjustable t
                              :element-type 'bit
                              :initial-element 0)
    :accessor entity-ids
    :documentation "An array of all entity ids."))
  (:documentation "Handles the entities and their components."))

(defun negatives-satisfied-p (world entity negatives)
  (iter (for component-type in negatives)
    (when (has-a component-type world entity)
      (leave))
    (finally (return t))))

(defun positive-dependencies (world entity query)
  "Check if the positive component dependencies of the QUERY which must be present in the ENTITY
are indeed associated with that ENTITY in the given WORLD."
  (iter (for component-type in query)
    (r:if-let (component (component world entity component-type))
      (collect component into collected-components)
      (leave))
    (finally (return (values collected-components t)))))

(declaim (inline entity-defined-p))
(defun entity-defined-p (world entity)
  "Check if the ENTITY is present in the WORLD."
  (not (zerop (aref (entity-ids world) entity))))

;; TODO: I sort of expect it to be slow because of the lists
(defun query (world query &key without)
  "Extract the list with the data of matching components based on the QUERY.
   The second value indicates whether the query was succesful.
   You can also pass in a list of components as WITHOUT to fetch only entities that don't have it.
   E.g. a query '(position health) would return a list of matched entities:
   '((0 pos-0 health-0)
     (1 pos-1 health-1) ...)"
  (iter (for entity from 0 below (length (entity-ids world)))
    (when (and (entity-defined-p world entity)
               (if without (negatives-satisfied-p world entity without) t))
      (r:when-let (components (positive-dependencies world entity query))
        (collect (append (list entity) components) into collected-components)))
    (finally (return (values collected-components t)))))

(defun make-entity (world)
  "Inserts a new empty entity into the WORLD and returns it."
  (let ((ids (entity-ids world)))
    (declare (type (array bit) ids))
    (iter (for entity from 0 below (length ids))
      ;; found an empty slot in the entities array - use it
      (when (zerop (aref ids entity))
        (setf (aref ids entity) 1)
        (leave entity))
      ;; if no empty slots found - extend the entities array
      (finally
       (vector-push-extend 1 ids entity)
       (return entity)))))

(defun remove-entity (world entity)
  "Removes the given ENTITY from the WORLD and clears out all associated components."
  (unless (entity-defined-p world entity)
    (warn "Entity ~a not found, nothing removed.~%" entity)
    (return-from remove-entity nil))
  (iter (for (component-type components) in-hashtable (entity-components world))
    (setf (aref components entity) nil))
  (setf (aref (entity-ids world) entity) 0))

(defun remove-entities (world &rest entities)
  "Removes all the ENTITIES from the WORLD and clears out all associated components"
  (iter (for e in entities)
    (remove-entity world e)))

;; TODO: We may also check that the entity is really defined
(defun component (world entity component-type)
  "Query a COMPONENT-TYPE of the given ENTITY.
   The second returned values indicates whether such a COMPONENT-TYPE exists."
  (let ((components (gethash component-type (entity-components world))))
    (if components
        (values (aref components entity) t)
        (values nil nil))))

(defun add-component (world entity component)
  "Adds a COMPONENT to the ENTITY in the WORLD."
  (let ((components
          (r:getsethash
           (type-of component) (entity-components world)
           (make-array (max entity 255)
                       :fill-pointer 0
                       :adjustable t
                       :initial-element nil))))
    (when (> entity (1- (array-total-size components)))
      (adjust-array components (floor (* entity 1.5))))
    (setf (aref components entity) component)))

(defun add-components (world entity &rest components)
  (iter (for c in components)
    (add-component world entity c)))

(defun has-a (component-type world entity)
  "Queries the COMPONENT-TYPE and returns T if it's there."
  (and (component world entity component-type) t))

(defun remove-component (world entity component-type)
  "Removes the COMPONENT-TYPE from the ENTITY in the WORLD."
  (let ((components (gethash component-type (entity-components world))))
    (setf (aref components entity) nil)))

(defun remove-components (world entity &rest components)
  "Removes all COMPONENTS from the ENTITY in the WORLD."
  (iter (for c in components)
    (remove-component world entity c)))

