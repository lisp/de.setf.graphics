;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)


(document
  "This file implements port  coordinate transform contexts for the de.setf.graphics library."

  (copyright "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
             "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

  (history
   (delta 20060703 "janderson" "added check on *port-coordinate-type* for NIL, to force transforms of all
    coordinates.")
   (delta 20060620 "janderson" "eliminated context-port-context and
   *port-context*. they were redundant wrt context-view, had no initializing
   accessor, and was read prior to context-view. which meant that it was bound
   to nil on first use and the view slot was computed in the next method.")
   (delta 20060426 "janderson" "raster operations accept two
   locations rather than a location and a size")
   (delta 20040614 "janderson@ravenpack.com" "simplified function names.")
   (delta 20031006 "jaa@setf.de"
          "in keeping wth the changes to ndc-transform-context, reformed the parameters to use the
double float location vectors as produced by the location transform function. the operations are now
this has the advantage the the respective concrete context function can specify coercion. it also is the
only way to handle both  apis for which the coordinate data is volatile, eg. core-graphics, and contexts
for which that data must have indefinte extent, eg clim. the former can coerce to dynamic-extent data,
while the latter most definitely cannot.")
   (delta 20030921
          "abstracted as the class port-transform-context. the operations are now
context-_operation_{,*2,*3,*2port}.")
   (delta 20030905 "james.anderson@setf.de"
          "rewritten for dynamic projection contexts. the general interface is
quickdraw-_operation_{,*2,*3,*2port}. the first function accepts locations, which it projects to port
coordinates and then delegates to the *2port form. the second function extends the psread coordiante
arguments to include zero values for z and delegates to the *3 form, which projects and delegated to the
*2port form. the base function performs the actual rendering in observance of the properties.")
   (delta 19940523 "jaa@dtmg"
          "took out direct toolbox calls and replaced them with methods in order to allow
specializations (eg. for backing store).")
   (delta 19891113 "jaa@dtmg" "ported to clx/pcl; added polygon modes to arc function.")
   (delta 19880625 "jaa@dtmg"
          "added coercion to point data in order to allow 4-d coordinate results from
coordinate-transform also added functions for pictures, arcs.")
   (delta 19880614 "jaa@dtmg"
          "as OG-graphics-standards, a preliminary version which does not take account of fill, invert, ... in polygon
mode. this layer takes atomic parameters, rather than og-elements, since it is intended to be parallel to the lower
level graphics with the addition of only coordinate system transformations."))

  (long-description
   "Defines the concrete implementation for graphics projection operations in terms of quickdraw-like
 functions. The implementation evolved from a library which was single-targeted at quickdraw and supported
 specialization for functionality such as backing store and display-lists via methods for the individual
 operations. As dynamic projection contexts handle the variation a level up, this interface could be returned
 to the earlier simpler form."))



#+ignore.20060620  ;; duplicates *context-view*
(defParameter *port-context* nil
  "bound within a call-with-projection-context for a port-transform-context for use the port-based operators.")


(defclass port-transform-context (view-context transform-context)
  ((arc-port :reader context-arc-port :initarg :arc-port)
   (line-port :reader context-line-port :initarg :line-port)
   (raster-port :reader context-raster-port :initarg :raster-port)
   (rectangle-port :reader context-rectangle-port :initarg :rectangle-port)
   (text-port :reader context-text-port :initarg :text-port))
  (:documentation
   "a port-transform-context specializes view-context and transform-context.
    it implements rendering operations by transforming the world-coordinate arguments to
    port-coordinates and using a specialization's view-port-based geometry operators.
    these should be implemented in terms of a view port object and view port coordinates.
    the specialization must also provide a context-make-view method to instantiate the view port object
    appropriate for the port-coordinate operations."))

#+ignore.20060620  ;; duplicates context-view
(defgeneric context-port-context (context)
  (:documentation
   "returns the respective port context to provide to the port-oriented operators. in some cases, this may be a view or
    window. in others it is an ephemeral internal value.")
  (:method ((context t))
           (error "no port context defined for context: ~s." context)))

(defmethod call-with-projection-context
           ((function t) (context port-transform-context)
            &key
            &allow-other-keys)
  (let* ((*arc-port* (fdefinition (context-arc-port context)))
         (*line-port* (fdefinition (context-line-port context)))
         (*raster-port* (fdefinition (context-raster-port context)))
         (*rectangle-port* (fdefinition (context-rectangle-port context)))
         (*text-port* (fdefinition (context-text-port context))))
    (call-next-method)))

  
 

;;; 
;;; geometric elements
;;;
;;; these functions draw the basic geometric figures.
;;; they require the explicit points - either as instances or as spread coordinates.
;;; all locations are absolute.

(defmethod context-line*3 ((context port-transform-context) xw1 yw1 zw1 xw2 yw2 zw2 &optional properties)
  "draw a line given the 3d location in object/world coordinates"
  (declare (optimize (speed 3) (safety 0)))
  (with-transformed-coordinates ((p-l1 (xw1 yw1 zw1)) (p-l2 (xw2 yw2 zw2)) :transform *context-transform*)
    #+ignore
    (format *trace-output* "~&(~s ~s ~s)-(~s ~s ~s) : ~s-~s"
            xw1 yw1 zw1 xw2 yw2 zw2 (coerce p-l1 'list) (coerce p-l2 'list))
    (funcall *line-port* *context-view* p-l1 p-l2 properties)))

(defmethod context-line*2 ((context port-transform-context) x1 y1 x2 y2 &optional properties)
  "draw a line given the x and y coordinates.
   iff the values are port-type, they are presumed port coordinates and are _not_ transformed.
   object/world in the x-y plane and projected."
  (if (and *port-coordinate-type*
           (typep x1 *port-coordinate-type*) (typep y1 *port-coordinate-type*)
           (typep x2 *port-coordinate-type*) (typep y2 *port-coordinate-type*))
    (with-location-vectors ((p-l1 x1 y1) (p-l2 x2 y2))
      (funcall *line-port* *context-view* p-l1 p-l2 properties))
    (context-line*3 context x1 y1 *context-default-z* x2 y2 *context-default-z* properties)))

(defmethod context-line ((context port-transform-context) l1 l2 &optional properties)
  "draw a line given the end locations (2-d or 3-d) in object/world coordinates"
  (declare (optimize (speed 3) (safety 0)))  
  (with-transformed-coordinates ((p-l1 l1) (p-l2 l2) :transform  *context-transform*)
    (funcall *line-port* *context-view* p-l1 p-l2 properties)))

(defmethod context-line ((context port-transform-context) (l1 location-port) (l2 location-port) &optional properties)
  "draw a line given the end locations (2-d or 3-d) in object/world coordinates"
  (declare (optimize (speed 3) (safety 0)))
  (with-location-vectors (p-l1 p-l2)
    (location-vector-copy l1 p-l1)
    (location-vector-copy l2 p-l2)
    (funcall *line-port* *context-view* p-l1 p-l2 properties)))



;;;
;;; rectangle


(defmethod context-rectangle*3 ((context port-transform-context) xw1 yw1 zw1 xw2 yw2 zw2 &optional properties)
  "draw a rectangle given the 3d coordinates"
  (declare (optimize (speed 3) (safety 0))) ; (setq *tmp-matrix* *context-transform*)
  (with-transformed-coordinates ((p-l1 (xw1 yw1 zw1)) (p-l2 (xw2 yw2 zw2)) :transform  *context-transform*)
    (funcall *rectangle-port* *context-view* p-l1 p-l2 properties)))

(defmethod context-rectangle*2 ((context port-transform-context) x1 y1 x2 y2 &optional properties)
  "draw a rectangle given the x and y coordinates. iff the values are double-float, they are presumed object/world in the x-y plane and projected. otherwise they are taken to be port coordinates."
  (if (and *port-coordinate-type*
           (typep x1 *port-coordinate-type*) (typep y1 *port-coordinate-type*)
           (typep x2 *port-coordinate-type*) (typep y2 *port-coordinate-type*))
    (with-location-vectors ((p-l1 x1 y1) (p-l2 x2 y2))
      (funcall *rectangle-port* *context-view* p-l1 p-l2 properties))
    (context-rectangle*3 context x1 y1 *context-default-z* x2 y2 *context-default-z* properties)))

(defmethod context-rectangle ((context port-transform-context) l1 l2 &optional properties)
  "draw a rectangle given the corner locations (2-d or 3-d)"
  (declare (optimize (speed 3) (safety 0)))  
  (with-transformed-coordinates ((p-l1 l1) (p-l2 l2) :transform  *context-transform*)
    (funcall *rectangle-port* *context-view* p-l1 p-l2 properties)))



;;;
;;; polygon
;;;
;;; a window-system independant implementation make little sense for polygons: the transformation
;;; may be interleaved with the polygon rendering, which makes it unwieldy to convert all
;;; locations before delegating to the port-based function



;;;
;;; arc

(defmethod context-arc*3 ((context port-transform-context) wx wy wz radius start end direction &optional properties)
  (with-transformed-coordinates ((p-location (wx wy wz)) :transform *context-transform*)
    (let ((port-radius 0))
      (etypecase radius
        (number (with-location-vectors ((r-location 0.0d0 0.0d0 0.0d0))
                  (with-coerced-variables ((double-float radius))
                    (setf (aref r-location 0) radius))
                  (location-scale-magnitude *context-transform* r-location)
                  (setf port-radius (%round  (aref r-location 3)))))
        ((or location-2 location-vector cons)
         (setf port-radius (%round (location-scale-magnitude *context-transform* radius)))))
      (funcall *arc-port* *context-view* p-location port-radius start end direction properties))))

(defmethod context-arc*2 ((context port-transform-context) x y radius start end direction &optional properties)
  "draw an arc given the 2d coordinates"
  (if (and *port-coordinate-type*
           (typep x *port-coordinate-type*) (typep y *port-coordinate-type*))
    (with-location-vectors ((p-location x y))
      (funcall *arc-port* *context-view* p-location radius start end direction properties))
    (context-arc*3 context x y *context-default-z* radius start end direction properties)))

(defmethod context-arc ((context port-transform-context) location radius start end direction &optional properties)
  (with-transformed-coordinates ((p-location location) :transform *context-transform*)
    (let ((port-radius 0))
      (etypecase radius
        (number (with-location-vectors ((r-location 0.0d0 0.0d0 0.0d0))
                  (with-coerced-variables ((double-float radius))
                    (setf (aref r-location 0) radius))
                  (location-scale-magnitude *context-transform* r-location)
                  (setf port-radius (%round  (aref r-location 3)))))
        ((or location-2 location-vector cons)
         (setf port-radius (%round (location-scale-magnitude *context-transform* radius)))))
      (funcall *arc-port* *context-view* p-location port-radius start end direction properties))))

   

;;;
;;; text

(defmethod context-text*3 ((context port-transform-context) xw yw zw text font &optional properties)
  (with-transformed-coordinates ((p-location (xw yw zw)) :transform *context-transform*)
    (funcall *text-port* *context-view* p-location text font properties)))

(defmethod context-text*2 ((context port-transform-context) x y text font &optional properties)
  (if (and *port-coordinate-type*
           (typep x *port-coordinate-type*) (typep y *port-coordinate-type*))
    (with-location-vectors ((p-location x y))
      (funcall *text-port* *context-view* p-location text font properties))
    (context-text*3 context x y *context-default-z* text font properties)))

(defmethod context-text ((context port-transform-context) location text font &optional properties)
  (with-transformed-coordinates ((p-location location) :transform  *context-transform*)
    (funcall *text-port* *context-view* p-location text font properties)))


;;;
;;; raster

(defmethod context-raster*3 ((context port-transform-context) xw1 yw1 zw1 xw2 yw2 zw2 raster &optional properties)
  "draw a sample array given the 3d coordinates"
  (declare (optimize (speed 3) (safety 0)))
  (with-transformed-coordinates ((p-location1 (xw1 yw1 zw1))
                                 (p-location2 (xw2 yw2 zw2))
                                 :transform *context-transform*)
    (funcall *raster-port* *context-view* p-location1 p-location2 raster properties)))

(defmethod context-raster*2 ((context port-transform-context) x1 y1 x2 y2 raster &optional properties)
  "draw a sample array given the 2d coordinates"
  (if (and *port-coordinate-type*
           (typep x1 *port-coordinate-type*) (typep y1 *port-coordinate-type*)
           (typep x2 *port-coordinate-type*) (typep y2 *port-coordinate-type*))
    (with-location-vectors ((p-l1 x1 y1) (p-l2 x2 y2))
      (funcall *raster-port* *context-view* p-l1 p-l2 raster properties))
    (context-raster*3 context x1 y1 *context-default-z* x2 y2 *context-default-z* raster properties)))

(defmethod context-raster ((context port-transform-context) l1 l2 raster &optional properties)
  "draw a sample array given the location and size (2-d or 3-d)"
  (declare (optimize (speed 3) (safety 0)))  
  (with-transformed-coordinates ((p-location1 l1) (p-location2 l2) :transform *context-transform*)
    (funcall *raster-port* *context-view* p-location1 p-location2 raster properties)))




;;;
;;; transforms



(defmethod context-set-projection-transform ((context port-transform-context) op &rest args)
  (declare (dynamic-extent args))
  (flet ((install-projection-matrix (new-pm)
           (unless (eq new-pm *context-projection-transform*)
             (return-matrix (shiftf *context-projection-transform* new-pm)))
           (matrix-catenate new-pm *context-view-transform* *context-transform*)
           *context-projection-transform*))
    (declare (dynamic-extent #'install-projection-matrix))
    (modify-transform *context-projection-transform* #'install-projection-matrix op args)))

(defun port-restore-projection-transform-and-release (context matrix)
  (context-set-projection-transform context :set matrix)
  (return-matrix matrix))

(defmethod context-save-projection-transform ((context port-transform-context))
  (push-projection-variable #'port-restore-projection-transform-and-release
                            context (matrix-copy *context-projection-transform* (get-matrix))))


(defmethod context-set-view-transform ((context port-transform-context) op &rest args)
  (declare (dynamic-extent args))
  (flet ((install-view-matrix (new-vm)
           (unless (eq new-vm *context-view-transform*)
             (return-matrix (shiftf *context-view-transform* new-vm)))
           (matrix-catenate *context-projection-transform* new-vm *context-transform*)
           *context-view-transform*))
    (declare (dynamic-extent #'install-view-matrix))
    (modify-transform *context-view-transform* #'install-view-matrix op args)))

(defun port-restore-view-transform-and-release (context matrix)
  (context-set-view-transform context :set matrix)
  (return-matrix matrix))

(defmethod context-save-view-transform ((context port-transform-context))
  (push-projection-variable #'port-restore-view-transform-and-release
                            context (matrix-copy *context-view-transform* (get-matrix))))


:de.sef.graphics


