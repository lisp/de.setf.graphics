;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.object-graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.object-graphics.implementation)

(document "Concrete model geometric constituents for the de.setf.graphics library"

  (copyright "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
             "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

  (history
   (delta 20051007 "janderson@ravenpac.com" "added start-location, end-location")
   (delta 20030901 "james.anderson@setf.de" "scene graph model")
   (delta 19891108 "jaa@dtmg" "ported to clx/pcl")
   (delta 19880616 "jaa@dtmg"))

  (long-description
   "Definitions for the geometric constituent classes classes:

  - line
  - polyline
  - quadrangle
  - rectangle
  - triangle
  - sphere
  - circle"))


(defclass line (located-node dyad)
  ((m-value
    :accessor start-location)
   (d-value
    :initform (make-location :x 0.0d0 :y 0.0d0 :z 0.0d0)
    :initarg :end 
    :accessor end :accessor end-location)))

(defclass polyline (located-node polyad)
  ((p-value :accessor locations)))

(defclass quadrilateral (geometric-element)
  ()
  (:documentation
   "a quadilateral is the abstract generalization of four-sided figures.
 is it specialized as either a rectangle, that is with right angles, or as a
 quadrangle."))

(defclass quadrangle (quadilateral)
  ())

(defclass rectangle (quadilateral)
  ()
  (:documentation
   "a rectangle is a quadrilateral with right angles. the extremes are bound as location and end."))

(defclass triangle (triad located-node)
  ())

(defclass circle (dyad located-node)
  ((d-value
    :initform 0.0d0 :initarg :radius :initarg radius
    :accessor radius
    :documentation
    "the circle radius")))

;;;
;;;

(defmethod shared-initialize :after ((instance rectangle) (slots t) &rest initargs)
  (with-slots (locations location end) instance
    (when (or (eq slots t) (find 'location slots))
      (setf location (first locations)))
    (when (or (eq slots t) (find 'end slots))
      (setf end (third locations)))
    (when (or (eq slots t) (find 'locations slots))
      (unless (and location end)
        (error "requires location and end: ~s." initargs))
      (cond ((= (location-x location) (location-x end))
             (setf locations (list location
                                   (make-location-world :x (location-x location)
                                                          :y (location-y location)
                                                          :z (location-z end))
                                   end
                                   (make-location-world :x (location-x location)
                                                          :y (location-y end)
                                                          :z (location-z location)))))
            ((= (location-y location) (location-y end))
             (setf locations (list location
                                   (make-location-world :x (location-x end)
                                                          :y (location-y location)
                                                          :z (location-z location))
                                   end
                                   (make-location-world :x (location-x location)
                                                          :y (location-y location)
                                                          :z (location-z end)))))
            ((= (location-z location) (location-z end))
             (setf locations (list location
                                   (make-location-world :x (location-x end)
                                                          :y (location-y location)
                                                          :z (location-z location))
                                   end
                                   (make-location-world :x (location-x location)
                                                          :y (location-y end)
                                                          :z (location-z location)))))
            (t
             (error "non-planar locations: ~s, ~s" location end))))))

(defmethod shared-initialize ((instance polyline) (slots t)
                              &key locations
                              &allow-other-keys)
  "when initializing a polyline maintain the constraint, that location is the first, and end is the last
point in the locations."
  (with-slots (m-value d-value) instance
    (when locations
      (unless m-value (setf m-value (first locations)))
      (unless d-value (setf d-value (first (last locations)))))
    (call-next-method)
    (unless locations
      (when (or (eq slots t) (find 'm-value slots))
        (setf m-value (first (locations instance))))
      (when (or (eq slots t) (find 'd-value slots))
        (setf d-value (first (last (locations instance))))))))


;;;
;;;

(def-class-constructors
  line
  quadrangle
  quadrilateral
  polyline
  rectangle
  triangle
  )

(defmethod line ((l1 location-2) &rest args)
  (apply 'line2 l1 args))
(defmethod line ((l1 fixnum) &rest args)
  (apply 'line2 l1 args))

(defmethod rectangle ((l1 location-2) &rest args)
  (apply 'rectangle2 l1 args))
(defmethod rectangle ((l1 fixnum) &rest args)
  (apply 'rectangle2 l1 args))

;;;
;;;


(defmethod print-object ((instance rectangle) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~s ~s" (location instance) (locations instance))))


(defGeneric sample-projection (raster projection-context)
  (:documentation
   "compute the representation of the raster sample data for the given context."))





:de.setf.graphics
