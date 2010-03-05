;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)


(document "Define the abstract scene model: container, root, node, and leaf classes."

  (copyright "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
             "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  (history
   (delta 20050909 "james.anderson@setf.de" "conditional class finalization before using class slots")
   (delta 20030901 "james.anderson@setf.de")))


(defclass node (setf.graph:node)
  ())

(defclass model (node setf.graph:root)
  ()
  (:documentation
   "a model is the root of a 3d scene graph. it manages the global model properties such as the extent
and inventories, and binds as children the immediate scene components. these are likely to be compound
elements which introduce locational or appearance properties, but could also be an elementary geometry
component."))


(defclass monad (node)
  ((m-value 
    :initform nil
    :accessor m-value))
  (:documentation
   "a monad is the most abstract representation of an node in a scene graph. it extends the abstract
setf.graph:node with a location slot."))

(defclass dyad (monad)
  ((d-value
    :initform nil
    :accessor d-value))
  (:documentation
   "a dyad extends a monadic node with an end slot."))

(defclass polyad (dyad)
  ((p-values
    :initform nil
    :accessor p-values))
  (:documentation
   "a polyad extends a dyadic node with an end slot."))

(defclass triad (dyad)
  ((t-value
    :initform nil
    :accessor t-value))
  (:documentation
   "a triad extends a polyadic node with an medial slot to bind the third location and constrains the
position ordinality to three."))

(defclass located (monad)
  ((m-value
    :initform (make-location 'world :x 0.0d0 :y 0.0d0 :z 0.0d0)
    :initarg :location :initarg location
    :accessor location)))
(setf (find-class 'located-node) (find-class 'located))

(defclass apparent (node)
   ((properties
    :initform nil :initarg :properties :initarg properties
    :accessor properties))
   (:documentation
    "an appearance node adds a slot for composite manifest properties such as
     color, or finish."))

(setf (find-class 'apparent-node) (find-class 'apparent))

(defclass reference ()
  ((referent
    :initarg :referent :initarg referent
    :accessor referent))
  (:documentation
   "a reference is a delegate for some other instance. it defines a single slot, reference, for a
required base instance. it incorporates no intrinsic super-classes and is intended to be specialized as
required for individual classes with the requisite delegate accessors."))

(setf (find-class 'reference-node) (find-class 'reference))

(defclass child (node setf.graph:child)
  ((parent
    :initform nil :initarg :parent :initarg parent
    :accessor node-parent)))

(setf (find-class 'child-node) (find-class 'child))

(defclass constituent (child)
  ((annotations
    :initform nil :initarg :annotations :initarg annotations
    :accessor annotations
    :documentation
    "binds the general apparent and positional properties of a constituent."))
  (:documentation
   "a constituent is a part of a component assembly."))

(setf (find-class 'constituent-node) (find-class 'constituent))

(defclass component (constituent-node setf.graph:parent)
  ((children
    :initform nil :initarg :children :initarg children
    :accessor node-children)
   (connections
    :initform nil :initarg :connections :initarg connections
    :accessor node-connections))
  (:documentation
   "a component is an assembly, a structure that can have parts and connections.
 (see 'http://www-ksl.stanford.edu/knowledge-sharing/ontologies/html/component-modeling.text.html').
 a component may be a constituent of another assembly."))

(setf (find-class 'component-node) (find-class 'component))

(def-class-constructors
  monad
  dyad
  apparent
  reference
  constituent
  component
  )

;;;
;;;



;;;
;;;

#+ignore.in-library
(defGeneric print-object-slot-names (function instance-class stream-class)
  (:documentation "this is used by the denominated-progn method combination to generate constrain the
method qualifiers introspectively.")
  (:method ((function t) (instance-class class) (stream-class class))
           "the general method just computes the names of the combined class and instance slots"
           (finalize-if-needed instance-class)
           (class-slot-names instance-class)))

#+ignore.in-library
(defmethod print-object-slots (instance stream)
  (:method-combination denominated-progn
                       :verbose-p nil
                       :qualifiers print-object-slot-names
                       )
  (:method :between ((instance t) (stream t)) (write-string " " stream)))

(defmethod print-object-slots m-value ((instance monad) (stream t))
  "a default method for monads"
  (format stream ":m-value ~s"
          (when (slot-boundp instance 'm-value)
            (slot-value instance 'm-value))))

(defmethod print-object-slots d-value ((instance dyad) (stream t))
  "a default method for dyads"
  (format stream ":d-value ~s"
          (when (slot-boundp instance 'd-value)
            (slot-value instance 'd-value))))

(defmethod print-object-slots t-value ((instance triad) (stream t))
  "a default method for triads"
  (format stream ":t-value ~s"
          (when (slot-boundp instance 't-value)
            (slot-value instance 't-value))))

(defmethod print-object-slots p-value ((instance polyad) (stream t))
  "a default method for polyads"
  (write-string ":p-value " stream)
  (print-unreadable-object ((when (slot-boundp instance 'p-value)
                              (slot-value instance 'p-value))
                            stream)))

(defmethod print-object-slots m-value ((instance located) (stream t))
  (format stream ":location ~s" (location instance)))

(defmethod print-object-slots properties ((instance apparent) (stream t))
  (write-string ":reference (" stream)
  (let ((props (properties instance)))
    (loop (unless props (return))
          (print-unreadable-object ((pop props) stream))
          (when props (write-char #\space stream))))
  (write-char #\) stream))

(defmethod print-object-slots reference ((instance reference) (stream t))
  (format stream ":reference <~a>" (type-of (referent instance))))



(defmethod print-object ((instance monad) stream)
  (print-unreadable-object (instance stream :identity t :type t)
    (print-object-slots instance stream)))


#|
(make-instance 'monad)
(make-instance 'dyad)
(make-instance 'located)
|#

:de.setf.graphics

