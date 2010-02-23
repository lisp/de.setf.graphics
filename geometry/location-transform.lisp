;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.object-graphics.implementation; -*-

(in-package :de.setf.object-graphics.implementation)

(document "location transformation operations based on matrix repersentations for the 'de.setf.graphics' library."
 
  (copyright "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
             "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  (history
   (delta 20030918 "added list coordinates to location-transform and location-scale")
   (delta 20030901 "james.anderson@setf.de" "reworked to use float arrays for intermediate values.
   simplified to always use 4-tuples (homogeneous 3-d) locations.")
   (copyright 2003 "[james anderson](mailto:james.anderson@setf.de)")
   (delta 19891115 "jaa@dtmg" "ported to clx/pcl")
   (delta 19880616 "jaa@dtmg"))

  (long-description
   " these are defined in two ways, as matrix-based and as context-based methods.
 the latter because they are used mostly in a rendering or interaction context. they distinguish the order of transform
 according to the data type of the position. fixnum's are handled as mac points and catenated with
 3x3 matrices, while location-4's are combined with 4x4 matrices."))


(defmacro coerce-coordinates (location-vector x y z &optional h)
  "coerce the coordinate variables to double-float store them in a location vector."
  (flet ((coerce-form (exp)
           (if (atom exp)
             `(if (typep ,exp 'double-float) ,exp (coerce ,exp 'double-float))
             (let ((binding (gensym)))
               `(let ((,binding ,exp))
                  (if (typep ,binding 'double-float) ,binding (coerce ,binding 'double-float)))))))
    `(progn (setf (aref ,location-vector 0) ,(coerce-form x))
            (setf (aref ,location-vector 1) ,(coerce-form y))
            (setf (aref ,location-vector 2) ,(coerce-form z))
            ,@(when h
                `((setf (aref ,location-vector 3) ,(coerce-form h)))))))



(defMacro with-transformed-coordinates (coordinate-bindings &rest body &environment env)
  "execute the body in the context of the specified port coordinate bindings.
   <br ><code>(with-transformed-coordinates <i>bindings</i> <i>transform-matrix</i> . <i>body<.I>)</code><br />
each binding take the form <code>((<i>x</i> <i>y</i> &optional <i>z</i>) <i>world-location</i>), where the location may be a single expression which evaluates to a location instance, or it may be a sequence of expressions which evaluate to coordinates. within the body, the <code><i>x</i></code>, <code><i>y</i></code>, and optionally <code><i>z</i></code> variables are bound to single-float ndc-coordiante values. the ndc values are declared to have dynamic-extent."
  (let ((bindings nil)
        (form nil)
        (keywords (member-if #'keywordp coordinate-bindings)))
    (setf coordinate-bindings (ldiff coordinate-bindings keywords))
    (destructuring-bind (&key transform) keywords
      (assert (not (null transform)))
      (flet ((generate-binding-if-needed (expr &aux (variable nil))
               (unless (and (symbolp expr) (eq expr (macroexpand-1 expr env)))
                 (setf variable (gensym "TEMP-"))
                 (push `(,variable ,expr) bindings)
                 (setf expr variable))
               expr))
        (setf transform (generate-binding-if-needed transform))
        (setf form
              `(with-location-vectors (,@(mapcar #'first coordinate-bindings))
                 ,@(mapcar #'(lambda (binding)
                               (destructuring-bind (ndc-location-vector world-location-spec &key (function 'location-transform))
                                                   binding
                                 (setf world-location-spec
                                       (if (consp world-location-spec)
                                         (mapcar #'generate-binding-if-needed world-location-spec)
                                         (generate-binding-if-needed world-location-spec)))
                                 ;; if the location is a fixnum value, treat it as a poinr in port coordinates
                                 ;; in which case, no transform occurrs.
                                 ;; otherwise always treat them as world coordinates
                                 (if (symbolp world-location-spec)
                                   `(if (typep ,world-location-spec 'fixnum)
                                      (location-vector-copy ,world-location-spec ,ndc-location-vector)
                                      (,function ,transform ,world-location-spec ,ndc-location-vector))
                                   (let ((temp-world-location-vector (gensym "WLV-")))
                                     `(with-location-vectors ((,temp-world-location-vector))
                                        ,(destructuring-bind (wx wy &optional (wz 0.0d0)) world-location-spec
                                           `(coerce-coordinates ,temp-world-location-vector ,wx ,wy ,wz 1.0d0))
                                        (,function ,transform ,temp-world-location-vector ,ndc-location-vector))))))
                           coordinate-bindings)
                   ,@body)))
        (if bindings
          `(let ,bindings ,form)
          form))))

(defMacro with-port-coordinates* (coordinate-bindings transform &rest body)
  "execute the body in the context of the specified port coordinate bindings.
   <br ><code>(with-port-coordinates* <i>bindings</i> <i>transform-matrix</i> . <i>body<.I>)</code><br />
each binding take the form <code>((<i>x y</i>) <i>world-location</i>), where the location may be a single expression which evaluates to a location instance, or it may be a sequence of expressions which evaluate to coordinates. within the body, the <code><i>x</i></code> and <code><i>y</i></code> variables are bound to fixnum port-coordiante values."
  (let ((bindings nil)
        (pv-name (gensym "PLV-"))
        (double (gensym "DOUBLE-"))
        (form nil)
        (all-port-coordinates nil))
    (flet ((generate-binding-if-needed (expr &aux (variable nil))
             (unless (symbolp expr)
               (setf variable (gensym "TEMP-"))
               (push `(,variable ,expr) bindings)
               (setf expr variable))
             expr))
      (setf transform (generate-binding-if-needed transform))
      (setf form
            `(with-location-vectors ((,pv-name))
               (let ((,double 0.0d0)
                     ,@(apply #'append (mapcar #'(lambda (coordinate-binding)
                                                   (let ((port-coordinates (subseq (first coordinate-binding) 0 2)))
                                                     (setf all-port-coordinates
                                                           (append all-port-coordinates port-coordinates))
                                                     (mapcar #'(lambda (c) (list c 0)) port-coordinates)))
                                               coordinate-bindings)))
                 (declare (type fixnum ,@all-port-coordinates)
                          (type double-float ,double) (dynamic-extent ,double))
                 ,@(mapcar #'(lambda (binding &aux form world-location)
                               (destructuring-bind (port-coordinates &rest world-location-spec) binding
                                 (destructuring-bind (px py &key (function 'location-transform)) port-coordinates
                                   (cond ((rest world-location-spec)
                                          (setf world-location-spec (mapcar #'generate-binding-if-needed world-location-spec)
                                                world-location (gensym "WLV-")))
                                         (t
                                          (setf world-location-spec (generate-binding-if-needed (first world-location-spec))
                                                world-location world-location-spec)))
                                   (setf form `(progn (,function ,transform ,world-location ,pv-name)
                                                      (setf ,double (aref ,pv-name 0) ,px (%round ,double)
                                                            ,double (aref ,pv-name 1) ,py (%round ,double))))
                                   (if (symbolp world-location-spec)
                                     `(if (typep ,world-location-spec 'fixnum)
                                        (setf ,px (point-h ,world-location-spec) ,py (point-v ,world-location-spec))
                                        ,form)
                                     `(with-location-vectors ((,world-location))
                                        ,(destructuring-bind (wx wy &optional (wz 0.0d0)) world-location-spec
                                           `(coerce-coordinates ,world-location ,wx ,wy ,wz 1.0d0))
                                        ,form)))))
                           coordinate-bindings)
                 ,@body)))
      (if bindings
        `(let ,bindings ,form)
        form))))


(defMacro with-ndc-coordinates* (coordinate-bindings &rest body)
  "execute the body in the context of the specified port coordinate bindings.
   <br ><code>(with-port-coordinates* <i>bindings</i> <i>transform-matrix</i> . <i>body<.I>)</code><br />
each binding take the form <code>((<i>x</i> <i>y</i> &optional <i>z</i>) <i>world-location</i>), where the location may be a single expression which evaluates to a location instance, or it may be a sequence of expressions which evaluate to coordinates. within the body, the <code><i>x</i></code>, <code><i>y</i></code>, and optionally <code><i>z</i></code> variables are bound to single-float ndc-coordiante values. the ndc values are declared to have dynamic-extent."
  (let ((bindings nil)
        (ndcv-name (gensym "NDCLV-"))
        (double (gensym "DOUBLE-"))
        (form nil)
        (all-ndc-coordinates nil)
        (keywords (member-if #'keywordp coordinate-bindings)))
    (setf coordinate-bindings (ldiff coordinate-bindings keywords))
    (destructuring-bind (&key transform) keywords
      (assert (not (null transform)))
      (flet ((generate-binding-if-needed (expr &aux (variable nil))
               (unless (symbolp expr)
                 (setf variable (gensym "TEMP-"))
                 (push `(,variable ,expr) bindings)
                 (setf expr variable))
               expr))
        (setf transform (generate-binding-if-needed transform))
        (setf form
              `(with-location-vectors ((,ndcv-name))
                 (let ((,double 0.0d0)
                       ,@(apply #'append (mapcar #'(lambda (coordinate-binding &aux ndc-coordinates)
                                                     (destructuring-bind (ndcx ndcy &optional ndcz &rest rest)
                                                                         (first coordinate-binding)
                                                       (declare (ignore rest))
                                                       (setf ndc-coordinates
                                                             (list* ndcx ndcy (typecase ndcz
                                                                                ((or keyword null) nil)
                                                                                (t (list ndcz)))))
                                                       (setf all-ndc-coordinates
                                                             (append all-ndc-coordinates ndc-coordinates))
                                                       (mapcar #'(lambda (c) (list c 0.0s0)) ndc-coordinates)))
                                                 coordinate-bindings)))
                   (declare (type short-float ,@all-ndc-coordinates)
                            (dynamic-extent ,@all-ndc-coordinates)
                            (type double-float ,double) (dynamic-extent ,double))
                   ,@(mapcar #'(lambda (binding &aux form world-location)
                                 (destructuring-bind (ndc-coordinates &rest world-location-spec) binding
                                   (destructuring-bind (ndcx ndcy &rest z-and-keys &aux ndcz) ndc-coordinates
                                     (typecase (first z-and-keys)
                                       ((or keyword null) nil)
                                       (t (setf ndcz (pop z-and-keys))))
                                     (destructuring-bind (&key (function 'location-transform)) z-and-keys
                                       (cond ((rest world-location-spec)
                                              (setf world-location-spec (mapcar #'generate-binding-if-needed world-location-spec)
                                                    world-location (gensym "WLV-")))
                                             (t
                                              (setf world-location-spec (generate-binding-if-needed (first world-location-spec))
                                                    world-location world-location-spec)))
                                       ;; transform the coordinate and assicn the respective values
                                       (setf form `(progn (,function ,transform ,world-location ,ndcv-name)
                                                          (setf ,double (aref ,ndcv-name 0) ,ndcx (float ,double 0.0s0)
                                                                ,double (aref ,ndcv-name 1) ,ndcy (float ,double 0.0s0)
                                                                ,@(when ndcz
                                                                    `(,double (aref ,ndcv-name 2) ,ndcz (float ,double 0.0s0))))))
                                       ;; if the location is a single value, then it may be a fixnum point
                                       ;; in which case, no transform occurrs.
                                       ;; otherwise always treat them as world coordinates
                                       (if (symbolp world-location-spec)
                                         `(if (typep ,world-location-spec 'fixnum)
                                            (setf ,ndcx (coerce (point-h ,world-location-spec) 'short-float)
                                                  ,ndcy (coerce (point-v ,world-location-spec) 'short-float)
                                                  ,@(when ndcz `(,ndcz 0.0s0)))
                                            ,form)
                                         `(with-location-vectors ((,world-location))
                                            ,(destructuring-bind (wx wy &optional (wz 0.0d0)) world-location-spec
                                               `(coerce-coordinates ,world-location ,wx ,wy ,wz 1.0d0))
                                            ,form))))))
                             coordinate-bindings)
                   ,@body)))
        (if bindings
          `(let ,bindings ,form)
          form)))))

;;;
;;; transform a location based on a matrix
;;;
;;; first, the type specific functions, then the generic function
;;;

(defun double-location-transform (m p result)
  (declare (optimize (speed 3) (safety 0))
           (type location-vector p result)
           (type transform-matrix m))
  (let ((x 0.0d0)
        (y 0.0d0)
        (z 0.0d0)
        (hr 0.0d0))
    (declare (type double-float hr x y z)
             (dynamic-extent hr x y z))
    (symbol-macrolet ((xr (aref result 0))
                      (yr (aref result 1))
                      (zr (aref result 2)))
      (setf x (aref p 0) y (aref p 1) z (aref p 2))
      (setf hr (+ (+ (* x (aref m 0 3)) (* y (aref m 1 3))) (+ (* z (aref m 2 3)) (aref m 3 3))))
      (if (= hr 0.0d0)
        (setf xr most-positive-double-float
              yr most-positive-double-float
              zr most-positive-double-float)
        (setf xr (/ (+ (+ (* x (aref m 0 0)) (* y (aref m 1 0))) (+ (* z (aref m 2 0)) (aref m 3 0))) hr)
              yr (/ (+ (+ (* x (aref m 0 1)) (* y (aref m 1 1))) (+ (* z (aref m 2 1)) (aref m 3 1))) hr)
              zr (/ (+ (+ (* x (aref m 0 2)) (* y (aref m 1 2))) (+ (* z (aref m 2 2)) (aref m 3 2))) hr)))
      (setf (aref result 3) 1.0d0))
    result))

(defun short-location-transform (m p result)
  (declare (optimize (speed 3) (safety 0))
           (type short-location-vector p result)
           (type transform-matrix m))
  (let ((x 0.0d0)
        (y 0.0d0)
        (z 0.0d0)
        (hr 0.0d0))
    (declare (type double-float hr x y z)
             (dynamic-extent hr x y z))
    (symbol-macrolet ((xr (aref result 0))
                      (yr (aref result 1))
                      (zr (aref result 2)))
      (setf x (float (aref p 0) 1.0d0) y (* 1.0d0 (aref p 1)) z (* 1.0d0 (aref p 2)))
      (setf hr (+ (+ (* x (aref m 0 3)) (* y (aref m 1 3))) (+ (* z (aref m 2 3)) (aref m 3 3))))
      (if (= hr 0.0d0)
        (setf xr most-positive-short-float
              yr most-positive-short-float
              zr most-positive-short-float)
        (setf xr (float (/ (+ (+ (* x (aref m 0 0)) (* y (aref m 1 0))) (+ (* z (aref m 2 0)) (aref m 3 0))) hr) 1.0s0)
              yr (float (/ (+ (+ (* x (aref m 0 1)) (* y (aref m 1 1))) (+ (* z (aref m 2 1)) (aref m 3 1))) hr) 1.0s0)
              zr (float (/ (+ (+ (* x (aref m 0 2)) (* y (aref m 1 2))) (+ (* z (aref m 2 2)) (aref m 3 2))) hr) 1.0s0)))
      (setf (aref result 3) 1.0s0))
    result))

(defun fixnum-location-transform (m p result)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum-location-vector p result)
           (type transform-matrix m))
  (let ((x 0.0d0)
        (y 0.0d0)
        (z 0.0d0)
        (hr 0.0d0))
    (declare (type double-float hr x y z)
             (dynamic-extent hr x y z))
    (symbol-macrolet ((xr (aref result 0))
                      (yr (aref result 1))
                      (zr (aref result 2)))
      (setf x (float (aref p 0) 0.0d0) y (float (aref p 1) 0.0d0) z (float (aref p 2) 0.0d0))
      (setf hr (+ (+ (* x (aref m 0 3)) (* y (aref m 1 3))) (+ (* z (aref m 2 3)) (aref m 3 3))))
      (if (= hr 0.0d0)
        (setf xr most-positive-fixnun
              yr most-positive-fixnun
              zr most-positive-fixnun)
        (setf xr (%round (/ (+ (+ (* x (aref m 0 0)) (* y (aref m 1 0))) (+ (* z (aref m 2 0)) (aref m 3 0))) hr))
              yr (%round (/ (+ (+ (* x (aref m 0 1)) (* y (aref m 1 1))) (+ (* z (aref m 2 1)) (aref m 3 1))) hr))
              zr (%round (/ (+ (+ (* x (aref m 0 2)) (* y (aref m 1 2))) (+ (* z (aref m 2 2)) (aref m 3 2))) hr))))
      (setf (aref result 3) 1))
    result))

(defun general-location-transform (m p result)
  (declare (optimize (speed 3) (safety 0))
           (type vector p result)
           (type transform-matrix m))
  (let ((x 0.0d0)
        (y 0.0d0)
        (z 0.0d0)
        (hr 0.0d0))
    (declare (type double-float hr x y z)
             (dynamic-extent hr x y z))
    (symbol-macrolet ((xr (aref result 0))
                      (yr (aref result 1))
                      (zr (aref result 2)))
      (setf x (float (aref p 0) 0.0d0) y (float (aref p 1) 0.0d0) z (float (aref p 2) 0.0d0))
      (setf hr (+ (+ (* x (aref m 0 3)) (* y (aref m 1 3))) (+ (* z (aref m 2 3)) (aref m 3 3))))
      (if (= hr 0.0d0)
        (setf xr most-positive-double-float
              yr most-positive-double-float
              zr most-positive-double-float)
        (setf xr (round (/ (+ (+ (* x (aref m 0 0)) (* y (aref m 1 0))) (+ (* z (aref m 2 0)) (aref m 3 0))) hr))
              yr (round (/ (+ (+ (* x (aref m 0 1)) (* y (aref m 1 1))) (+ (* z (aref m 2 1)) (aref m 3 1))) hr))
              zr (round (/ (+ (+ (* x (aref m 0 2)) (* y (aref m 1 2))) (+ (* z (aref m 2 2)) (aref m 3 2))) hr))))
      (setf (aref result 3) 1.0d0))
    result))

(defGeneric location-transform (transformer location &optional result)
  (:documentation
   "the location-transform generic function combines a number of situations:
    either a rendering context or a transformation matrix serves as the transformer. the former case uses the respective transform matrix and delegates to the latter method. as the location either a location instance or a location vector can be supplied. in the former case, a location-port is copied, while for other types a temporary vector is interposed. where aview is supplied with an location instance, an and location uses the port transform, while other types use the perspective transform. if the transformer argument is a function, it is applied to the location and result arguments.")

  (:method ((transformer function) location &optional (result (make-location-vector)))
           "allow a functional transformer"
           (funcall transformer location result))

  (:method ((transformer t) (location fixnum) &optional (result (make-location-vector)))
           "a fixnum point is not transformed further"
           (declare (type location-vector result))
           (setf (location-x result) (coerce (point-h location) 'double-float)
                 (location-y result) (coerce (point-v location) 'double-float)
                 (location-x result) 0.0d0)
           result)

  (:method ((transformer t) (location cons) &optional (result (make-location-vector)))
           (destructuring-bind (x y &optional (z 1.0d0)) (if (symbolp (first location)) (rest location) location) 
             (with-coerced-variables ((double-float x y z))
               (with-location-vectors ((in-xy-plane x y z))
                 (location-transform transformer in-xy-plane result)))))

  (:method ((m array) (p location-2) &optional (result (location-vector)))
           "tranform a location instance. the coordinates are always double-float.
            a 2-d point (non-fixnum) is taken to be at z==0"
           (declare (optimize (speed 3) (safety 0))
                    (type location-vector result)
                    (type transform-matrix m))
           (let ((x (location-x p))
                 (y (location-y p))
                 (z (location-z p))
                 (hr 0.0d0))
             (declare (type double-float x y z hr)
                      (dynamic-extent x y z hr))
             (symbol-macrolet ((xr (aref result 0))
                               (yr (aref result 1))
                               (zr (aref result 2)))
               (setf hr (+ (+ (* x (aref m 0 3))
                              (* y (aref m 1 3)))
                           (+ (* z (aref m 2 3))
                              (aref m 3 3))))
               (if (= hr 0.0d0)
                 (setf xr most-positive-double-float
                       yr most-positive-double-float
                       zr most-positive-double-float)
                 (setf xr (/ (+ (+ (* x (aref m 0 0))
                                   (* y (aref m 1 0)))
                                (+ (* z (aref m 2 0))
                                   (aref m 3 0)))
                             hr)
                       yr (/ (+ (+ (* x (aref m 0 1))
                                   (* y (aref m 1 1)))
                                (+ (* z (aref m 2 1))
                                   (aref m 3 1)))
                             hr)
                       zr (/ (+ (+ (* x (aref m 0 2))
                                   (* y (aref m 1 2)))
                                (+ (* z (aref m 2 2))
                                   (aref m 3 2)))
                             hr)))
               (setf (aref result 3) 1.0d0)))
           result)

  (:method ((m array) (p vector) &optional result)
           "tranform a location stored in a vector. distinguish single, fixnum, and double-float vectors"
           (if (typep p 'simple-array)
             (case (array-element-type p)
               ;; this does not fully constrain the type, but hten it doesn't chek the length either
               ((long-float double-float)
                (double-location-transform m p (or result (location-vector))))
               ((short-float #+allegro single-float)
                (short-location-transform m p (or result  (short-location-vector))))
               (t (if (fixnum-location-vector-p result)
                    (fixnum-location-transform m p (or result (fixnum-location-vector)))
                    (general-location-transform m p (or result (location-vector))))))
             (general-location-transform m p (or result (location-vector)))))

  (:method ((self transform-context) (p location-ndc) &optional (result (make-location-vector)))
           (location-transform  (context-view-transform self) p result))

  (:method ((self transform-context) (p location-world) &optional (result (make-location-vector)))
           (location-transform  (context-transform self) p result))

  (:method ((self transform-context) (p location-object) &optional (result (make-location-vector)))
           (location-transform  (context-transform self) p result))

  (:method ((self transform-context) (p vector) &optional (result (make-location-vector)))
           (location-transform  (context-transform self) p result))
  )

(defGeneric location-inverse-transform (context location &optional result)
  (:method ((context transform-context) (location t) &optional (result (make-location-vector)))
           (location-transform (context-inverse-transform context) location result))
  (:method ((context array) (location t) &optional (result (make-location-vector)))
           (with-matrices ((inverse))
             (matrix-inverse context inverse)
             (location-transform inverse location result))))

;;;
;;;

(defun double-location-scale (m l result)
  (declare (optimize (speed 3) (safety 0))
           (type location-vector l result)
           (type transform-matrix m))
  (let ((x 0.0d0)
        (y 0.0d0)
        (z 0.0d0))
    (declare (type double-float x y z)
             (dynamic-extent x y z))
    (symbol-macrolet ((xr (aref result 0))
                      (yr (aref result 1))
                      (zr (aref result 2)))
      (setf x (aref l 0)
            y (aref l 1)
            z (aref l 2))
      (setf xr (+ (+ (* x (aref m 0 0))
                     (* y (aref m 1 0)))
                  (* z (aref m 2 0)))
            yr (+ (+ (* x (aref m 0 1))
                     (* y (aref m 1 1)))
                  (* z (aref m 2 1)))
            zr (+ (+ (* x (aref m 0 2))
                     (* y (aref m 1 2)))
                  (* z (aref m 2 2)))
            (aref result 3) 1.0d0)))
  result)

(defun short-location-scale (m l result)
  (declare (optimize (speed 3) (safety 0))
           (type short-location-vector l result)
           (type transform-matrix m))
  (let ((x 0.0d0)
        (y 0.0d0)
        (z 0.0d0))
    (declare (type double-float x y z)
             (dynamic-extent x y z))
    (symbol-macrolet ((xr (aref result 0))
                      (yr (aref result 1))
                      (zr (aref result 2)))
      (setf x (float (aref l 0) 1.0d0) y (* 1.0d0 (aref l 1)) z (* 1.0d0 (aref l 2)))
      (setf xr (float (+ (+ (* x (aref m 0 0)) (* y (aref m 1 0))) (* z (aref m 2 0))) 1.0s0)
            yr (float (+ (+ (* x (aref m 0 1)) (* y (aref m 1 1))) (* z (aref m 2 1))) 1.0s0)
            zr (float (+ (+ (* x (aref m 0 2)) (* y (aref m 1 2))) (* z (aref m 2 2))) 1.0s0)
            (aref result 3) 1.0s0))
  result))

(defun fixnum-location-scale (m l result)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum-location-vector l result)
           (type transform-matrix m))
  (let ((x 0.0d0)
        (y 0.0d0)
        (z 0.0d0))
    (declare (type double-float x y z)
             (dynamic-extent x y z))
    (symbol-macrolet ((xr (aref result 0))
                      (yr (aref result 1))
                      (zr (aref result 2)))
      (setf x (float (aref l 0) 0.0d0) y (float (aref l 1) 0.0d0) z (float (aref l 2) 0.0d0))
      (setf xr (round (+ (+ (* x (aref m 0 0)) (* y (aref m 1 0))) (* z (aref m 2 0))))
            yr (round (+ (+ (* x (aref m 0 1)) (* y (aref m 1 1))) (* z (aref m 2 1))))
            zr (round (+ (+ (* x (aref m 0 2)) (* y (aref m 1 2))) (* z (aref m 2 2))))
            (aref result 3) 1))
    result))

(defun general-location-scale (m l result)
  (declare (optimize (speed 3) (safety 1))
           (type vector l result)
           (type transform-matrix m))
  (let ((x 0.0d0)
        (y 0.0d0)
        (z 0.0d0))
    (declare (type double-float x y z)
             (dynamic-extent x y z))
    (symbol-macrolet ((xr (aref result 0))
                      (yr (aref result 1))
                      (zr (aref result 2)))
      (setf x (float (aref l 0) 0.0d0) y (float (aref l 1) 0.0d0) z (float (aref l 2) 0.0d0))
      (setf xr (+ (+ (* x (aref m 0 0)) (* y (aref m 1 0))) (* z (aref m 2 0)))
            yr (+ (+ (* x (aref m 0 1)) (* y (aref m 1 1))) (* z (aref m 2 1)))
            zr (+ (+ (* x (aref m 0 2)) (* y (aref m 1 2))) (* z (aref m 2 2)))
            (aref result 3) 1.0d0))
    result))

(defGeneric location-scale (transformer location &optional result)
  (:documentation "")

  (:method ((transformer function) location &optional (result (make-location-vector)))
           "allow a functional transformer"
           (funcall transformer location result))

  (:method ((transformer t) (location fixnum) &optional (result (make-location-vector)))
           "a fixnum point is not transformed further"
           (declare (type location-vector result))
           (setf (location-x result) (coerce (point-h location) 'double-float)
                 (location-y result) (coerce (point-v location) 'double-float)
                 (location-x result) 0.0d0)
           result)

  (:method ((transformer t) (location cons) &optional (result (make-location-vector)))
           (destructuring-bind (x y &optional (z 1.0d0)) (if (symbolp (first location)) (rest location) location)
             (with-coerced-variables ((double-float x y z))
               (with-location-vectors ((in-xy-plane x y z 1.0d0))
                 (location-scale transformer in-xy-plane result)))))

  (:method ((m array) (p location-2) &optional (result (make-location-vector)))
           "transform a 3-d location. a 2-d location (non-fixnum) is taken to be at z==0"
           (declare (optimize (speed 3) (safety 0))
                    (type location-vector result)
                    (type transform-matrix m))
           (assert-type m transform-matrix)
           (let ((x (location-x p))
                 (y (location-y p))
                 (z (location-z p))
                 )
             (declare (type double-float x y z)
                      (dynamic-extent x y z))
             (symbol-macrolet ((xr (aref result 0))
                               (yr (aref result 1))
                               (zr (aref result 2)))
               (setf xr (+ (+ (* x (aref m 0 0))
			      (* y (aref m 1 0)))
                           (* z (aref m 2 0)))
                     yr (+ (+ (* x (aref m 0 1))
			      (* y (aref m 1 1)))
                           (* z (aref m 2 1)))
                     zr (+ (+ (* x (aref m 0 2))
                              (* y (aref m 1 2)))
                           (* z (aref m 2 2)))
                     (aref result 3) 1.0d0)))
           result)

  (:method ((m array) (l vector) &optional (result (make-location-vector)))
           "scale a location stored in a vector. distinguish single, fixnum, and double-float vectors"
           (assert-type m transform-matrix)
           (if (typep l 'simple-array)
             (case (array-element-type l)
               ;; this does not fully constrain the type, but hten it doesn't chek the length either
               ((long-float double-float)
                (double-location-scale m l (or result (location-vector))))
               ((short-float #+allegro single-float)
                (short-location-scale m l (or result  (short-location-vector))))
               (t (if (fixnum-location-vector-p result)
                    (fixnum-location-scale m l (or result (fixnum-location-vector)))
                    (general-location-scale m l (or result (location-vector))))))
             (general-location-scale m l (or result (location-vector)))))

  (:method ((self transform-context) (l location-ndc) &optional (result (make-location-vector)))
           (location-scale  (context-view-transform self) l result))

  (:method ((self transform-context) (l location-world) &optional (result (make-location-vector)))
           (location-scale  (context-transform self) l result))

  )


(defGeneric location-inverse-scale (context location &optional result)
  (:method ((context transform-context) (location t) &optional (result (make-location-vector)))
           (location-scale (context-inverse-transform context) location result))
  (:method ((context array) (location t) &optional (result (make-location-vector)))
           (with-matrices ((inverse))
             (matrix-inverse context inverse)
             (location-scale inverse location result))))


(defun location-vector-scale-magnitude (transformer vector)
  (declare (optimize (speed 3) (safety 0)))
  (with-location-vectors ((result))
    (location-scale transformer vector result)
    (location-magnitude result)
    #| nb. keep this around as a note: this ends up overwriting 0.0d0, if the consumer
       is not also declare dynamoc extent and assigns the result
      (if consumer
      (let ((scalar-result 0.0d0))
        (declare (type double-float scalar-result)
                 (dynamic-extent double-float scalar-result))
        (setf scalar-result (aref result 3))
        (funcall consumer scalar-result))
      (aref result 3))|#
    ;; result is left in (aref result 3)
    (setf (aref vector 3) (aref result 3))))

(defGeneric location-scale-magnitude (transformer location)
  (:documentation
   "compute the scaled magnitude. if the argument is a location-vector, leave the result as (aref 3). otherwise extract and returns it.")

  (:method ((transformer function) location)
           "allow a functional transformer"
           (funcall transformer location))

  (:method ((transformer t) (location fixnum))
           "a fixnum point is not transformed further"
           (location-magnitude location))

  (:method ((transformer t) (location location-2))
           "a 2-d point (non-fixnum) is taken to be at z==0"
           (with-location-vectors ((location-vector (location-x location) (location-y location)
                                                    (location-z location) 1.0d0))
             (location-vector-scale-magnitude transformer location-vector)
             (aref location-vector 3)))

  (:method ((transformer t) (location cons))
           "a 2-d point (non-fixnum) is taken to be at z==0"
           (destructuring-bind (x y &optional (z 1.0d0)) (if (symbolp (first location)) (rest location) location)
             (with-location-vectors ((location-vector x y z 1.0d0))
               (location-vector-scale-magnitude transformer location-vector))))

  (:method ((transformer array) (l vector))
           "scale a location stored in a vector"
           (location-vector-scale-magnitude transformer l))

  (:method ((self transform-context) (p location-world))
           (location-scale-magnitude  (context-transform self) p))

  (:method ((self transform-context) (p vector))
           (location-scale-magnitude  (context-transform self) p))

  )

#|
(defun test-scale (count xform p r)
  (dotimes (x count) (location-scale xform p r)))

(defun test-transform (count xform p r)
  (dotimes (x count) (location-transform xform p r)))

(defun test-scale-magnitude (count xform p &optional consumer &aux result)
  (dotimes (x count) (setf result (location-scale-magnitude xform p consumer)))
  result)

(let* ((scale (make-location-vector :x 0.0d0 :y 1.0d0 :z 2.0d0))
       (matrix (print-matrix (matrix-scale scale (transform-identity))))
       (location (make-location-world :x 1.0 :y 1.0 :z 1.0))
       (result (location-vector)))
   (time (test-scale 100000 matrix location result))
   result)

(let* ((scale (make-location-vector :x 2.0d0 :y 2.0d0 :z 2.0d0))
       (translation (make-location-vector :x 0.0d0 :y 1.0d0 :z 2.0d0))
       (result (location-vector))
       (matrix (print-matrix (matrix-scale scale (matrix-translate translation (transform-identity)))))
       (location (make-location-world :x 1.0 :y 1.0 :z 1.0)))
   (time (test-transform 1 matrix location result))
   result)

(let* ((scale (make-location-vector :x 2.0d0 :y 2.0d0 :z 2.0d0))
       (matrix (print-matrix (matrix-scale scale (transform-identity))))
       (location (make-location-world :x 1.0 :y 1.0 :z 1.0)))
   (time (test-scale-magnitude 100000 matrix location)))

(let* ((scale (make-location-vector :x 2.0d0 :y 2.0d0 :z 2.0d0))
       (matrix (print-matrix (matrix-scale scale (transform-identity))))
       (location (make-location-world :x 1.0 :y 1.0 :z 1.0))
       (consumer #'(lambda (f) (%round f))))
   (time (test-scale-magnitude 100000 matrix location consumer)))

(let ((vector (make-location-vector :x 1.0d0 :y 1.0d0 :z 1.0d0))
      ; (result (make-location-vector :x 1.0d0 :y 1.0d0 :z 1.0d0))
      (matrix (matrix-set 2.0d0 0.0d0 0.0d0 0.0d0
                          0.0d0 2.0d0 0.0d0 0.0d0
                          0.0d0 0.0d0 2.0d0 0.0d0
                          0.0d0 0.0d0 0.0d0 1.0d0
                          (transform-matrix))))
  (time (location-scale-magnitude  matrix vector)))

(let ((vector (make-short-location-vector :x 1.0s0 :y 1.0s0 :z 1.0s0))
      (result (short-location-vector))
      (matrix (matrix-set 2.0d0 0.0d0 0.0d0 0.0d0
                          0.0d0 2.0d0 0.0d0 0.0d0
                          0.0d0 0.0d0 2.0d0 0.0d0
                          0.0d0 1.0d0 2.0d0 1.0d0
                          (transform-matrix))))
  (time (location-transform matrix vector result)))

(let ((vector (make-fixnum-location-vector :x 1.0s0 :y 1.0d0 :z 1))
      (result (fixnum-location-vector))
      (matrix (matrix-set 2.0d0 0.0d0 0.0d0 0.0d0
                          0.0d0 2.0d0 0.0d0 0.0d0
                          0.0d0 0.0d0 2.0d0 0.0d0
                          0.0d0 1.0d0 2.0d0 1.0d0
                          (transform-matrix))))
  (time (location-transform matrix vector result)))


|#

:eof
