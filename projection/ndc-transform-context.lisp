;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)


(document
  "This file implements normalized device coordinate transform contexts for the de.sef.graphics library."

  (copyright "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
           "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

  (history
   (delta 20060426 "janderson" "raster operations accept two locations rahter than a location and a size")
   (delta 20031003 "jaa@setf.de"
          "reformed the parameters to use the double float locations vectors as produced by the location
transform function. this has the advantage the the respective concrete context function can specify
coercion. it also is the only way to handle both  apis for which the coordinate data is volatile, eg.
core-graphics, and contexts for which that data must have indefinte extent, eg clim. the former can
coerce to dynamic-extent data, while the latter most definitely cannot.")
   (delta 20030911 "james.anderson@setf.de"))

  (long-description
   "this file implements the abstract projection operations for for contexts based on a library which
uses locations in normalized device coordinates, such as clim and apple's core graphics, also known as
quartz."))



(defclass ndc-transform-context (transform-context)
  ;; not automaitcally a view context, eg. svg
  ((arc-ndc :reader context-arc-ndc :initarg :arc-ndc)
   (line-ndc :reader context-line-ndc :initarg :line-ndc)
   (raster-ndc :reader context-raster-ndc :initarg :raster-ndc)
   (rectangle-ndc :reader context-rectangle-ndc :initarg :rectangle-ndc)
   (text-ndc :reader context-text-ndc :initarg :text-ndc))
  (:documentation
   "the context for ndc graphics operations"))

(defgeneric context-ndc-context (context)
  (:documentation
   "returns the respective ndc context to provide to the ndc-oriented operators. in some cases, this may be a view or window. in others it is an ephemeral internal value.")
  (:method ((context t))
           (error "no ndc context defined for context: ~s." context)))

(defmethod call-with-projection-context
           ((function function) (context ndc-transform-context)
                     &key
                     &allow-other-keys)
  (let* ((*arc-ndc* (fdefinition (context-arc-ndc context)))
         (*line-ndc* (fdefinition (context-line-ndc context)))
         (*raster-ndc* (fdefinition (context-raster-ndc context)))
         (*rectangle-ndc* (fdefinition (context-rectangle-ndc context)))
         (*text-ndc* (fdefinition (context-text-ndc context)))
         (*ndc-context* (context-ndc-context context)))
    (call-next-method)))

(defParameter *stack-allocate-coordinates-p* t
  "the default is stack allocated")

;;; 
;;; geometric elements
;;;
;;; these functions draw the basic geometric figures.
;;; they require the explicit points - either as instances or as spread coordinates.
;;; all locations are absolute.



(defmethod context-line*3 ((context ndc-transform-context) xw1 yw1 zw1 xw2 yw2 zw2 &optional properties)
  "draw a line given the 3d location in object/world coordinates"
  (declare (optimize (speed 3) (safety 0)))
  (with-transformed-coordinates ((ndc-l1 (xw1 yw1 zw1)) (ndc-l2 (xw2 yw2 zw2)) :transform *context-projection-transform*)
    (funcall *line-ndc* *ndc-context* ndc-l1 ndc-l2 properties)))

(defmethod context-line*2 ((context ndc-transform-context) x1 y1 x2 y2 &optional properties)
  "draw a line given the x and y coordinates. iff the values are not ndc type, they are presumed object/world in the x-y plane and projected. otherwise they are taken to be ndc coordinates."
  (if (and (typep x1 *ndc-coordinate-type*) (typep y1 *ndc-coordinate-type*)
           (typep x2 *ndc-coordinate-type*) (typep y2 *ndc-coordinate-type*))
    (with-location-vectors ((ndc-l1 x1 y1) (ndc-l2 x2 y2))
      (funcall *line-ndc* *ndc-context* ndc-l1 ndc-l2 properties))
    (context-line*3 context x1 y1 *context-default-z* x2 y2 *context-default-z* properties)))

(defmethod context-line ((context ndc-transform-context) l1 l2 &optional properties)
  "draw a line given the end locations (2-d or 3-d) in object/world coordinates"
  (declare (optimize (speed 3) (safety 0)))  
  (with-transformed-coordinates ((ndc-l1 l1) (ndc-l2 l2) :transform *context-projection-transform*)
    (funcall *line-ndc* *ndc-context* ndc-l1 ndc-l2 properties)))



;;;
;;; rectangle

(defmethod context-rectangle*3 ((context ndc-transform-context) xw1 yw1 zw1 xw2 yw2 zw2 &optional properties)
  "draw a rectangle given the 3d coordinates"
  (declare (optimize (speed 3) (safety 0)))
  (with-coerced-variables ((double-float xw1 yw1 zw1 xw2 yw2 zw2))
    (with-location-vectors ((first xw1 yw1 zw1) (second xw2 yw2 zw2))
      (context-rectangle context first second properties))))

(defmethod context-rectangle*2 ((context ndc-transform-context) x1 y1 x2 y2 &optional properties)
  "draw a rectangle given the 2d coordinates"
  (if (and (typep x1 *ndc-coordinate-type*) (typep y1 *ndc-coordinate-type*)
           (typep x2 *ndc-coordinate-type*) (typep y2 *ndc-coordinate-type*))
    (let ((xmin (min x1 x2))
          (ymin (min y1 y2))
          (xmax (max x1 x2))
          (ymax (max y1 y2)))
      (with-location-vectors ((ndc-l1 xmin ymin) (ndc-l2 xmin ymax)
                                    (ndc-l3 xmax ymax) (ndc-l4 xmax ymin))
        (funcall *rectangle-ndc* *ndc-context* ndc-l1 ndc-l2 ndc-l3 ndc-l4 properties)))
    (context-rectangle*3 context x1 y1 *context-default-z* x2 y2 *context-default-z* properties)))      

(defmethod context-rectangle ((context ndc-transform-context) l1 l2 &optional properties)
  "draw a rectangle given the corner locations (2-d or 3-d)"
  (declare (optimize (speed 3) (safety 0)))
  (flet ((transform-and-render (l1 l2 l3 l4)
           (with-transformed-coordinates ((ndc-l1 l1) (ndc-l2 l2) (ndc-l3 l3) (ndc-l4 l4)
                                  :transform *context-projection-transform*)
             (funcall *rectangle-ndc* *ndc-context* ndc-l1 ndc-l2 ndc-l3 ndc-l4 properties))))
    (declare (dynamic-extent #'transform-and-render))
    (call-with-expanded-rectangle #'transform-and-render l1 l2)))


(defun call-with-expanded-rectangle (function first second)
  "call the argument function with the first and second locations normalizes the locations to four location arranged in a clockwise order viewed from the respective constant axis, given the cg lower-left origin."
  (with-location-vectors (l0 l1 l2 l3)
    (symbol-macrolet ((l0.x (aref l0 0)) (l0.y (aref l0 1)) (l0.z (aref l0 2))
                      (l1.x (aref l1 0)) (l1.y (aref l1 1)) (l1.z (aref l1 2))
                      (l2.x (aref l2 0)) (l2.y (aref l2 1)) (l2.z (aref l2 2))
                      (l3.x (aref l3 0)) (l3.y (aref l3 1)) (l3.z (aref l3 2)))
      (setf (aref l0 3) 1.0d0 (aref l1 3) 1.0d0 (aref l2 3) 1.0d0 (aref l3 3) 1.0d0)
      
    (cond ((= (location-x first) (location-x second))          ; in z-y plane
           (setf l0.x (location-x first) l2.x l0.x
                 l0.y (min (location-y first) (location-y second)) l2.y (max (location-y first) (location-y second))
                 l0.z (min (location-z first) (location-z second)) l2.z (max (location-z first) (location-z second)))
           (setf l1.x l0.x l1.y l0.y l1.z l2.z
                 l3.x l0.x l3.y l2.y l3.z l0.z))
          ((= (location-y first) (location-y second))          ; in z-x plane
           (setf l0.x (min (location-x first) (location-x second)) l2.x (max (location-x first) (location-x second))
                 l0.z (min (location-z first) (location-z second)) l2.z (max (location-z first) (location-z second))
                 l0.y (location-y first) l2.y l0.y)
           (setf l1.x l0.x l1.y l0.y l1.z l2.z
                 l3.x l2.x l3.y l0.y l3.z l0.z))
          (t                            ; in x-y plane, presuming (= (location-z first) (location-z second))
           (setf l0.x (min (location-x first) (location-x second)) l2.x (max (location-x first) (location-x second))
                 l0.y (min (location-y first) (location-y second)) l2.y (max (location-y first) (location-y second))
                 l0.z (location-z first) l2.z l0.z)
           (setf l1.x l0.x l1.y l2.y l1.z l0.z
                 l3.x l2.x l3.y l0.y l3.z l0.z)))
    (funcall function l0 l1 l2 l3))))



;;;
;;; polygon
;;;
;;; polygon implementation resides with the api-specific interface



;;;
;;; arc
  
(defmethod context-arc*3 ((context ndc-transform-context) wx wy wz radius start end direction &optional properties)
  (with-transformed-coordinates ((ndc-location (wx wy wz)) :transform *context-projection-transform*)
    (let ((ndc-radius 0.0s0))
      (declare (type short-float ndc-radius)
               ;(dynamic-extent ndc-radius)
               )
      (etypecase radius
        (number (with-location-vectors ((r-location 0.0d0 0.0d0 0.0d0))
                  (with-coerced-variables ((double-float radius))
                    (setf (aref r-location 0) radius))
                  (location-scale-magnitude *context-transform* r-location)
                  (setf ndc-radius (float (aref r-location 3) 1.0s0))))
        ((or location-2 location-vector cons)
         (setf ndc-radius (location-scale-magnitude *context-transform* radius))))
      (funcall *arc-ndc* *ndc-context* ndc-location ndc-radius start end direction properties))))

(defmethod context-arc*2 ((context ndc-transform-context) x y radius start end direction &optional properties)
  "draw a rectangle given the 2d coordinates"
  (if (and (typep x *ndc-coordinate-type*) (typep y *ndc-coordinate-type*) (typep radius *ndc-coordinate-type*))
    (with-location-vectors ((ndc-location x y))
      (funcall *arc-ndc* *ndc-context* ndc-location radius start end direction properties))
    (context-arc*3 context x y *context-default-z* radius start end direction properties)))

(defmethod context-arc ((context ndc-transform-context) location radius start end direction &optional properties)
  (with-transformed-coordinates ((ndc-location location) :transform *context-projection-transform*)
    (let ((ndc-radius 0.0s0))
      (declare (type short-float ndc-radius)
              ; (dynamic-extent ndc-radius)
               )
      (etypecase radius
        (number (with-location-vectors ((r-location 0.0d0 0.0d0 0.0d0))
                  (with-coerced-variables ((double-float radius))
                    (setf (aref r-location 0) radius))
                  (location-scale-magnitude *context-transform* r-location)
                  (setf ndc-radius (float (aref r-location 3) 1.0s0))))
        ((or location-2 location-vector cons)
         (setf ndc-radius (location-scale-magnitude *context-transform* radius))))
      (funcall *arc-ndc* *ndc-context* ndc-location ndc-radius start end direction properties))))



;;;
;;; text

(defmethod context-text*3 ((context ndc-transform-context) xw yw zw text font &optional properties)
  (with-transformed-coordinates ((ndc-location (xw yw zw)) :transform *context-projection-transform*)
    (funcall *text-ndc* *ndc-context* ndc-location text font properties)))

(defmethod context-text*2 ((context ndc-transform-context) x y text font &optional properties)
  (if (and (typep x *ndc-coordinate-type*) (typep y *ndc-coordinate-type*))
    (with-location-vectors ((ndc-location x y))
      (funcall *text-ndc* *ndc-context* ndc-location text font properties))
    (context-text*3 context x y *context-default-z* text font properties)))

(defmethod context-text ((context ndc-transform-context) location text font &optional properties)
  (with-transformed-coordinates ((ndc-location location) :transform *context-projection-transform*)
    (funcall *text-ndc* *ndc-context* ndc-location text font properties)))


;;;
;;; raster

(defmethod context-raster*3 ((context ndc-transform-context) xw1 yw1 zw1 xw2 yw2 zw2 raster &optional properties)
  "draw a sample array given the 3d coordinates"
  (declare (optimize (speed 3) (safety 0)))
  (with-transformed-coordinates ((ndc-location1 (xw1 yw1 zw1))
                                 (ndc-location2 (xw2 yw2 zw2))
                                 :transform *context-projection-transform*)
    (funcall *raster-ndc* *ndc-context* ndc-location1 ndc-location2 raster properties)))

(defmethod context-raster*2 ((context ndc-transform-context) x1 y1 x2 y2 raster &optional properties)
  "draw a sample array given the 2d coordinates"
  (if (and (typep x1 *ndc-coordinate-type*) (typep y1 *ndc-coordinate-type*)
           (typep x2 *ndc-coordinate-type*) (typep y2 *ndc-coordinate-type*))
    (funcall *raster-ndc* *ndc-context* x1 y1 x2 y2 raster properties)
    (context-raster*3 context x1 y1 *context-default-z* x2 y2 *context-default-z* raster properties)))

(defmethod context-raster ((context ndc-transform-context) l1 l2 raster &optional properties)
  "draw a sample array given the location and size (2-d or 3-d)"
  (declare (optimize (speed 3) (safety 0)))  
  (with-transformed-coordinates ((ndc-l1 l1) (ndc-l2 l2)
                         :transform *context-projection-transform*)
    (funcall *raster-ndc* *ndc-context* ndc-l1 ndc-l2 raster properties)))




;;;
;;; transforms


(defmethod context-set-projection-transform ((context ndc-transform-context) op &rest args)
  (declare (dynamic-extent args))
  (flet ((install-projection-matrix (new-pm)
           (unless (eq new-pm *context-projection-transform*)
             (return-matrix (shiftf *context-projection-transform* new-pm)))
           (matrix-catenate new-pm *context-view-transform* *context-transform*)
           *context-projection-transform*))
    (declare (dynamic-extent #'install-projection-matrix))
    (modify-transform *context-projection-transform* #'install-projection-matrix op args)))

(defmethod context-save-projection-transform ((context ndc-transform-context))
  (flet ((reset-projection-transform-and-release (context matrix)
           (context-set-projection-transform context :set matrix)
           (return-matrix matrix)))
    (push-projection-variable #'reset-projection-transform-and-release
                             context (matrix-copy *context-projection-transform* (get-matrix)))))



(defmethod context-set-view-transform ((context ndc-transform-context) (op t) &rest args)
  (declare (ignore args))
  (error "no transform method for the view transform."))

(defmethod context-save-view-transform ((context ndc-transform-context))
  (flet ((reset-view-transform-and-release (context matrix)
           (context-set-view-transform context :set matrix)
           (return-matrix matrix)))
    (push-projection-variable #'reset-view-transform-and-release
                              context (matrix-copy (context-view-transform context) (get-matrix)))))




:de.setf.graphics

