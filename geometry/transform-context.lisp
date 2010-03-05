;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.object-graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.object-graphics.implementation)


(document "This file defines coordinate transformation contexts for the 'de.setf.graphics' library."

  (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
   "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  (history
   (delta 20060420 "janderson@ravenpack.com" "added optional to spread-optional-coordinates")
   (delta 20030905 "james.anderson@setf.de"))

  (long-description
   "Each of the abstract classes for object, world, ndc, and port coordinate systems binds the transforms
 needed to project locations into that domain. This means, from world coordinates dow to 2-d, for X, or
 not at all for OpenGL."))



(defclass transform-context ()
  ((transform
    :initarg :transform :initform nil
    :documentation
    "the view transform is a specifies the coordinate transformation matrix from object coordinate to port coordinates. it combines the projection and ndc transform.")
   (inverse-transform
    :initform nil
    :documentation
    "the view inverse-transform is the inverse of the view transform. it is computed on demand.")
   (projection-transform
    :initarg :projection-transform :initform (transform-identity)
    :reader context-projection-transform)
   (view-transform
    :initarg :view-transform :initform (matrix-identity)
    :reader context-view-transform)
   (inverse-ndc-transform)
   (port-parameters
    :initarg :parameters :initarg :port-parameters :initform nil
    :accessor view-parameters
    :documentation
    "describes the port viewing parameters")))

(def-initialize-clone transform-context
  ;; reuse everything: any changes replace the existing
  (transform
   projection-transform
   view-transform
   port-parameters))

(defmethod context-transform ((context transform-context))
  (with-slots (transform view-transform projection-transform)
              context
    (if transform transform
        (setf transform 
              (matrix-catenate projection-transform view-transform)))))              

(defmethod context-inverse-transform ((context transform-context))
  (with-slots (transform inverse-transform) context
    (or inverse-transform
        (setf inverse-transform
              (matrix-inverse transform (transform-matrix))))))

(defmethod call-with-projection-context
           ((function t) (context transform-context)
            &key
            &allow-other-keys)
  (let* ((*context-view-transform*
          (matrix-copy (context-view-transform context)))
         (*context-projection-transform*
          (matrix-copy (context-projection-transform context)))
         (*context-transform*
          (matrix-copy (context-transform context))))
    (call-next-method)))


;;;
;;; manipulating transforms

(defmacro spread-optional-coordinates_ (x y &optional z)
  `(etypecase ,x
     (number (unless ,y (psetf ,x (point-h ,x)
                               ,y (point-v ,x)
                               ,@(when z `(,z 0)))))
     (location-2 (setf ,@(when z `(,z (location-z ,x)))
                       ,y (location-y ,x)
                       ,x (location-x ,x)))
     (cons (loop (etypecase (first ,x)
                   (number (return))
                   ((and symbol (not null)) (pop ,x))))
           (psetf ,x (pop ,x)
                  ,y (pop ,x)
                  ,@(when z `(,z (pop ,x)))))))


(defun modify-transform (target install op args)
  (ecase op
    (:scale (destructuring-bind (x &optional y z) args
              (spread-optional-coordinates_ x y z)
              (let ((new-m (get-matrix)))
                (with-coerced-variables ((double-float x y z))
                  (with-location-vectors ((delta x y z))
                    (matrix-scale delta target new-m)
                    (funcall install new-m))))))
    (:translate (destructuring-bind (x &optional y z) args
                  (spread-optional-coordinates_ x y z)
                  (let ((new-m (get-matrix)))
                    (with-coerced-variables ((double-float x y z))
                      (with-location-vectors ((delta x y z))
                        (matrix-translate delta target new-m)
                        (funcall install new-m))))))
    (:rotate (destructuring-bind (x &optional y z) args
              (spread-optional-coordinates_ x y z)
               (let ((new-m (get-matrix)))
                 (with-coerced-variables ((double-float x y z))
                   (with-location-vectors ((delta x y z))
                     (matrix-rotate delta target new-m)
                     (funcall install new-m))))))
    (:catenate (destructuring-bind (delta &rest entries) args
                 (declare (dynamic-extent entries))
                 (let ((new-m (get-matrix)))
                   (etypecase delta
                     (number (with-matrices ((delta-m))
                               (apply #'matrix-fill delta-m delta entries)
                               (matrix-catenate target delta-m new-m)))
                     (simple-array (matrix-catenate target delta new-m)))
                   (funcall install new-m))))
    (:set (destructuring-bind (new &rest entries) args
            (declare (dynamic-extent entries))
            (etypecase new
              (number (apply #'matrix-fill target new entries))
              (simple-array (matrix-copy new target)))
            (funcall install target)))
    ((:clear :initialize) (destructuring-bind () args
                            (matrix-initialize target)
                            (funcall install target)))))

:eof

