;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-


(in-package :de.setf.graphics.implementation)


(document "This file defines combination operators for locations for the 'de.setf.graphics' library."

  (copyright "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
             "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  (history
   (delta 20060630 "janderson" "added vector to location deistance.")
   (delta 20030830 "jaa@setf.de" "updated to simplified accessor names;
   replaced case forms with generic functions which perform coercion")
   (delta 19891115 "jaa@dtmg" "ported to clx/pcl")
   (delta 19880829 "jaa@dtmg" "exports")
   (delta 19880616 "jaa@dtmg"))

  (long-description
   "nb.  not all functions are exported as many would likely be used by an
  interval math package only, which can import them itself and export them as appropriate."))


;;;
;;; transcendatal operations
;;; the !<function> macros expect an operand and an existing double-float into which the result is
;;; to be stored.


;;;; --------------------------------------------------------------------------
;;;; first macros to define the functions

(eval-when (:compile-toplevel :execute)

  (defun make-location-operator-name (op)
    (intern (concatenate 'string (string :location-) (symbol-name op))
            :de.setf.object-graphics))
  (defun prefix-location-operator-name (prefix op)
    (intern (concatenate 'string (string prefix) (symbol-name op))
            :de.setf.object-graphics))

  (defmacro define-location-binary-op
            (op
             &key (point-op
                        `(lambda (p1 p2)
                           (make-point (,op (point-h p1) (point-h p2))
                                       (,op (point-v p1) (point-v p2)))))
             (identity (error "identity required"))
             &aux (interface-op (make-location-operator-name op))
             (generic-op (prefix-location-operator-name :_ interface-op)))
    `(progn
       (defun ,interface-op
	      (p1 p2 &optional (result (if (vectorp p1) (make-location-vector) (make-location (type-of p1)))))
         (,generic-op p1 p2 result))
       (defgeneric ,generic-op (location-1 location-2 result)
         (:argument-precedence-order result location-1 location-2)
         (:method ((p1 location-2) (p2 location-2) (result location-2))
                  (setf (location-x result)
                        (,op (location-x p1) (location-x p2))
                        (location-y result)
                        (,op (location-y p1) (location-y p2))
                        (location-h result) 1.0d0)
                  result)
         (:method ((p1 location-2) (p2 location-2) (result location-3))
                  (setf (location-z result)
                        (,op (if (location-3-p p1) (location-z p1) ,identity)
                             (if (location-3-p p2) (location-z p2) ,identity)))
                  (call-next-method))
         (:method ((p1 location-2) (p2 location-2) (result location-4))
                  (setf (location-time result)
                        (,op (if (location-4-p p1) (location-time p1) ,identity)
                             (if (location-4-p p2) (location-time p2) ,identity)))
                  (call-next-method))

         (:method ((lp1 location-port) (lp2 location-port) (result location-port))
                  (locally (declare (ftype (function (location-port) integer)
                                           %location-port-x %location-port-y %location-port-z))
                    (setf (%location-port-x result) (,op (%location-port-x lp1) (%location-port-x lp2))
                          (%location-port-y result) (,op (%location-port-y lp1) (%location-port-y lp2))
                          (%location-port-z result) (,op (%location-port-z lp1) (%location-port-z lp2))
                          (%location-port-h result) 1)
                    result))
         (:method ((ln1 location-ndc) (ln2 location-ndc) (result location-ndc))
                  (locally (declare (ftype (function (location-ndc) single-float)
                                           %location-ndc-x %location-ndc-y %location-ndc-z))
                    (setf (%location-ndc-x result) (,op (%location-ndc-x ln1) (%location-ndc-x ln2))
                          (%location-ndc-y result) (,op (%location-ndc-y ln1) (%location-ndc-y ln2))
                          (%location-ndc-z result) (,op (%location-ndc-z ln1) (%location-ndc-z ln2))
                          (%location-ndc-h result) 1.0s0)
                    result))
         (:method ((lo1 location-object) (lo2 location-object) (result location-object))
                  (locally (declare (ftype (function (location-object) double-float)
                                           %location-object-x %location-object-y %location-object-z))
                    (setf (%location-object-x result) (,op (%location-object-x lo1) (%location-object-x lo2))
                          (%location-object-y result) (,op (%location-object-y lo1) (%location-object-y lo2))
                          (%location-object-z result) (,op (%location-object-z lo1) (%location-object-z lo2))
                          (%location-object-h result) 1.0d0)
                    result))
         (:method ((lw1 location-world) (lw2 location-world) (result location-world))
                  (locally (declare (ftype (function (location-world) double-float)
                                           %location-world-x %location-world-y %location-world-z))
                    (setf (%location-world-x result) (,op (%location-world-x lw1) (%location-world-x lw2))
                          (%location-world-y result) (,op (%location-world-y lw1) (%location-world-y lw2))
                          (%location-world-z result) (,op (%location-world-z lw1) (%location-world-z lw2))
                          (%location-world-h result) 1.0d0)
                    result))
         (:method ((l1 vector) (l2 vector) (result vector))
                  (map-into result #',op l1 l2))

         (:method ((p1 fixnum) (p2 fixnum) (result fixnum))
                  (,point-op p1 p2))
         (:method ((p1 t) (p2 fixnum) (result t))
                  (,generic-op p1 (make-location-2 :x (point-h p2) :y (point-v p2)) result))
         (:method ((p1 fixnum) (p2 t) (result t))
                  (,generic-op (make-location-2 :x (point-h p1) :y (point-v p1)) p2 result))

         (:method ((p1 location-polar) (p2 location-polar) (result location-polar))
                  (setf (location-alpha result)
                        (,op (location-alpha p1) (location-alpha p2))
                        (location-radius result)
                        (,op (location-radius p1) (location-radius p2)))
                  result)
         (:method ((p1 location-polar) (p2 location-polar) (result location-spherical))
                  (setf (location-beta result)
                        (,op (if (location-polar-p p1) (location-beta p1) ,identity)
                             (if (location-polar-p p2) (location-beta p2) ,identity)))
                  (call-next-method))

         (:method ((p1 location-polar) (p2 location-2) (result location-polar))
                  (,generic-op p1
                               (cartesian->spherical p2 (make-location (type-of result)))
                               result))
         (:method ((p1 location-2) (p2 location-polar) (result location-polar))
                  (,generic-op (cartesian->spherical p1 (make-location (type-of result)))
                               p2
                               result))

         (:method ((p1 location-polar) (p2 location-2) (result location-2))
                  (,generic-op (spherical->cartesian p1 (make-location (type-of result)))
                               p2
                               result))
         (:method ((p1 location-2) (p2 location-polar) (result location-2))
                  (,generic-op p1
                               (spherical->cartesian p2 (make-location (type-of result)))
                               result))
         (:method ((p1 t) (p2 t) (result t))
                  (error "cannot combine [~s] and [~s] under ~s to [~s]." p1 p2 ',op result))
         )
       (setf (get 'location ',op) ',interface-op)))
  
  (defmacro define-location-unary-op
            (op
             &optional (point-op `(lambda (p1)
                                    (make-point (,op (point-h p1))
                                                (,op (point-v p1)))))
             &aux (location-op (make-location-operator-name op)))
    `(progn
       (defun ,location-op
	      (p1 &optional (result (make-location (type-of p1))))
         (typecase p1
           (fixnum
            (setf result (,point-op p1)))
           (location-port
            (locally (declare (ftype (function (location-port) integer)
                                     %location-port-x %location-port-y %location-port-z))
              (setf (%location-port-x result) (,op (%location-port-x p1))
                    (%location-port-y result) (,op (%location-port-y p1))
                    (%location-port-z result) (,op (%location-port-z p1))
                    (location-port-h result) 1)))
           (location-ndc
            (locally (declare (ftype (function (location-ndc) single-float)
                                     %location-ndc-x %location-ndc-y %location-ndc-z))
              (setf (%location-ndc-x result) (,op (%location-ndc-x p1))
                    (%location-ndc-y result) (,op (%location-ndc-y p1))
                    (%location-ndc-z result) (,op (%location-ndc-z p1))
                    (%location-ndc-h result) 1.0s0)))
           (location-world
            (locally (declare (ftype (function (location-world) double-float)
                                     %location-world-x %location-world-y %location-world-z))
              (setf (%location-world-x result) (,op (%location-world-x p1))
                    (%location-world-y result) (,op (%location-world-y p1))
                    (%location-world-z result) (,op (%location-world-z p1))
                    (%location-h result) 1.0d0)))
           (location-object
            (locally (declare (ftype (function (location-object) double-float)
                                     %location-object-x %location-object-y %location-object-z))
              (setf (%location-object-x result) (,op (%location-object-x p1))
                    (%location-object-y result) (,op (%location-object-y p1))
                    (%location-object-z result) (,op (%location-object-z p1))
                    (%location-object-h result) 1.0d0)))
           
           (location-2
            (setf (location-x result) (,op (location-x p1))
                  (location-y result) (,op (location-y p1))
                  (location-h result) 1)
            (if (location-3-p result)
              (setf (location-z result) (,op (location-z p1)))))

           (location-polar
            (setf (location-alpha result) (,op (location-alpha p1))
                  (location-radius result) (,op (location-radius p1)))
            (if (location-spherical-p result)
              (setf (location-beta result) (,op (location-beta p1)))))

           (t
            (error "operand of improper type [~s]." p1))))
       (setf (get 'location ',op) ',location-op)))
  
  (defmacro define-location-binary-predicate
            (op &aux (location-op (make-location-operator-name op)))
    `(progn
       (defgeneric ,location-op (location-1 location-2)
         (:method ((p1 fixnum) (p2 fixnum))
           (and (,op (point-h p1) (point-h p2))
                (,op (point-v p1) (point-v p2))))
         (:method ((p1 t) (p2 fixnum))
           (,location-op p1 (make-location-2 :x (point-h p2) :y (point-v p2))))
         (:method ((p1 fixnum) (p2 t))
           (,location-op (make-location-2 :x (point-h p1) :y (point-v p1)) p2))
         
         (:method ((p1 location-2) (p2 location-2))
           (and (,op (location-x p1) (location-x p2))
                (,op (location-y p1) (location-y p2))
                (if (or (location-3-p p1) (location-3-p p2))
                  (,op (location-z p1) (location-z p2))
                  t)
                (if (or (location-4-p p1) (location-4-p p2))
                  (,op (location-time p1) (location-time p2))
                  t))) #|
         (:method ((p1 location-3) (p2 location-3))
           (and (,op (location-x p1) (location-x p2))
                (,op (location-y p1) (location-y p2))
                (,op (location-z p1) (location-z p2))
                (if (or (location-4-p p1) (location-4-p p2))
                  (,op (location-time p1) (location-time p2))
                  t)))
         (:method ((p1 location-4) (p2 location-4))
           (and (,op (location-x p1) (location-x p2))
                (,op (location-y p1) (location-y p2))
                (,op (location-z p1) (location-z p2))
                (,op (location-time p1) (location-time p2))))
         
         (:method ((p1 location-port) (p2 location-port))
           (locally (declare (ftype (function (location-port) integer)
                                    %location-x %location-y %location-z))
             (and (,op (%location-x p1) (%location-x p2))
                  (,op (%location-y p1) (%location-y p2))
                  (,op (%location-z p1) (%location-z p2)))))
         (:method ((p1 location-ndc) (p2 location-ndc))
           (locally (declare (ftype (function (location-port) single-float)
                                    %location-x %location-y %location-z))
             (and (,op (%location-x p1) (%location-x p2))
                  (,op (%location-y p1) (%location-y p2))
                  (,op (%location-z p1) (%location-z p2)))))
         (:method ((p1 location-object) (p2 location-object))
           (locally (declare (ftype (function (location-port) double-float)
                                    %location-x %location-y %location-z))
             (and (,op (%location-x p1) (%location-x p2))
                  (,op (%location-y p1) (%location-y p2))
                  (,op (%location-z p1) (%location-z p2)))))
         (:method ((p1 location-world) (p2 location-world))
           (locally (declare (ftype (function (location-port) double-float)
                                    %location-x %location-y %location-z))
             (and (,op (%location-x p1) (%location-x p2))
                  (,op (%location-y p1) (%location-y p2))
                  (,op (%location-z p1) (%location-z p2)))))
         
         (:method ((p1 location-polar) (p2 location-polar))
           (and (,op (location-alpha p1) (location-alpha p2))
                (,op (location-radius p1) (location-radius p2))
                (if (or (location-spherical-p p1) (location-spherical-p p2))
                  (,op (location-beta p1) (location-beta p2))
                  t)))
         (:method ((p1 location-spherical) (p2 location-spherical))
           (and (,op (location-alpha p1) (location-alpha p2))
                (,op (location-radius p1) (location-radius p2))
                (,op (location-beta p1) (location-beta p2))))
         
         (:method ((p1 location-polar) (p2 location-2))
           (,location-op p1 (cartesian->spherical p2 (make-location (type-of p1)))))
         
         (:method ((p1 location-2) (p2 location-polar))
           (,location-op p1 (spherical->cartesian p2 (make-location (type-of p1)))))
         
         (:method ((p1 t) (p2 t))
           (error "cannot compare [~s] and [~s] under ~s." p1 p2 ',op))|#
         )

       (setf (get 'location ',op) ',location-op)))
  
  (defmacro define-location-unary-predicate
            (op &aux (location-op (make-location-operator-name op)))
    `(progn
       (defun ,location-op (p1)
         (etypecase p1
	   (fixnum
	    (and (,op (point-h p1)) (,op (point-v p1))))
	   (location-2
	    (and (,op (location-x p1))
	         (,op (location-y p1))
	         (if (location-3-p p1)
                   (,op (location-z p1))
                   t)))
	   (location-polar
	    (and (,op (location-alpha p1))
	         (,op (location-radius p1))
	         (if (location-spherical-p p1)
                   (,op (location-beta p1))
                   t)))))
       (setf (get 'location ',op) ',location-op)))
  
  ) ; eval-when


;;;; --------------------------------------------------------------------------
;;;; standard binary and unary operations

(define-location-binary-op + :point-op add-points :identity 0)
(define-location-binary-op - :point-op subtract-points :identity 0)
(define-location-binary-op / :identity 1)
(define-location-binary-op * :identity 1)

(define-location-unary-op abs)

; these are for compatibility
(setf (symbol-function 'location-sum) (symbol-function 'location-+))
(setf (symbol-function 'location-product) (symbol-function 'location-*))
(setf (symbol-function 'location-ratio) (symbol-function 'location-/))
(setf (symbol-function 'location-difference) (symbol-function 'location--))
(setf (symbol-function 'location-displacement)
      (symbol-function 'location--))

;;;; --------------------------------------------------------------------------
;;;; predicate operations

(define-location-binary-predicate =)
(define-location-binary-predicate <)
(define-location-binary-predicate >)
(define-location-binary-predicate <=)
(define-location-binary-predicate >=)
(define-location-binary-predicate /=)
(define-location-unary-predicate numberp)
(define-location-unary-predicate zerop)

(setf (get 'location '=) 'location-=)
(setf (get 'location '<) 'location-<)
(setf (get 'location '>) 'location->)
(setf (get 'location '<=) 'location-<=)
(setf (get 'location '>=) 'location->=)
(setf (get 'location '/=) 'location-/=)
(setf (get 'location 'numberp) 'location-numberp)
(setf (get 'location 'zerop) 'location-zerop)

(defun location-min (p1 p2)
  (if (location-< p1 p2)
    p1 p2))

(defun location-max (p1 p2)
  (if (location-> p1 p2)
    p1 p2))

(setf (get 'location 'min) 'location-min)
(setf (get 'location 'max) 'location-max)



;;;; --------------------------------------------------------------------------
;;;; location distance operations have special structure

(defun location-distance (p1 p2 &aux dx dy dz)
  (check-type-match p1 p2)
  (cond ((fixnump p1)
	 (setq dx (- (point-h p2) (point-h p1))
	       dy (- (point-v p2) (point-v p1)))
	 (sqrt (+ (* dx dx) (* dy dy))))
	((location-3-p p1)
	 (setq dx (- (location-x p1) (location-x p2))
	       dy (- (location-y p1) (location-y p2))
	       dz (- (location-z p1) (location-z p2)))
	 (sqrt (+ (* dx dx) (+ (* dy dy) (* dz dz)))))
	((location-2-p p1)
	 (setq dx (- (location-x p1) (location-x p2))
	       dy (- (location-y p1) (location-y p2)))
	 (sqrt (+ (* dx dx) (* dy dy))))
        ((location-vector-p p1)
         (setq dx (- (aref p1 0) (aref p2 0))
	       dy (- (aref p1 1) (aref p2 1))
	       dz (- (aref p1 2) (aref p2 2)))
         (sqrt (+ (* dx dx) (* dy dy))))
	(t
	 (error "operand of improper type [~s] or [~s]." p1 p2))))




(defun location-normalize
    (vec &optional (r vec) (D (location-magnitude vec)))
  (if (zerop D)
    (setf (location-x r) 0
          (location-y r) 0
          (location-z r) 0)
    (setf (location-x r) (/ (location-x vec) D)
          (location-y r) (/ (location-y vec) D)
          (location-z r) (/ (location-z vec) D)))
  r)

(defgeneric location-magnitude (location)
  (:documentation "compute the magnitude of the location, taken as a vector.")
  
  #+ignore ;; standardized version
  (:method ((location fixnum))
    "a fixnum point is not transformed further. just compute the magnitude"
    (let ((x (point-h location)) (y (point-v location)))
      (sqrt (+ (* x x) (* y y)))))
  
  (:method ((location fixnum))
    "a fixnum point is not transformed further. just compute the magnitude"
    (let ((x (point-h location)) (y (point-v location))
          (x2+y2 0)
          (mag2 0.0d0)
          (mag 0.0d0))
      (declare (type fixnum x y)
               (type double-float mag2 mag)
               (dynamic-extent mag mag2))
      (setf x2+y2 (+ (* x x) (* y y)))
      (double-float! x2+y2 mag2)         ;(setf mag2 (coerce x2+y2 'double-float))
      (sqrt! mag2 mag)           ; (setf mag (sqrt mag2))
      (%round mag)))
  
  (:method ((location location-2))
    (let ((x (location-x location))
          (y (location-y location))
          (z (location-z location)))
      (sqrt (+ (* x x) (* y y) (* z z)))))
  
  (:method ((l vector))
    "scale a location stored in a vector"
    (let ((x2 0.0d0)
          (y2 0.0d0)
          (z2 0.0d0)
          (t1 0.0d0)
          (t2 0.0d0))
      (declare (type double-float x2 y2 z2 t1 t2)
               (dynamic-extent x2 y2 z2 t1 t2))
      (symbol-macrolet ((lx (aref l 0))
                        (ly (aref l 1))
                        (lz (aref l 2)))
        (setf x2 (* lx lx) y2 (* ly ly) z2 (* lz lz))
        (setf t1 (+ x2 y2) t2 (+ t1 z2))
        (if (<= (length l) 3)
          (sqrt t2)
          (let ((result 0.0d0))
            (declare (type double-float result)
                     (dynamic-extent result)
                     (type location-vector l))
            (assert-type l location-vector)
            (sqrt! t2 result)
            (setf (aref l 3) result)
            l))))))

;(let ((v (make-location-vector :x 1.0d0 :y 1.0d0))) (time (dotimes (x 100000) (location-magnitude v))) (aref v 3))

;;(time (dotimes (x 10) (location-magnitude (make-point 10 10))))


;; the dot product of two vectors
(defun location-dot
  (vec1 vec2)
  (+ (* (location-x vec1) (location-x vec2))
     (* (location-y vec1) (location-y vec2))
     (* (location-z vec1) (location-z vec2))))

;; the cross product of two vectors
(defun location-cross
    (vec1 vec2 &optional (r (make-location (type-of vec1))))
  (setf (location-x r) (- (* (location-y vec1) (location-z vec2))
			      (* (location-z vec1) (location-y vec2)))
	(location-y r) (- (- (* (location-x vec1) (location-z vec2))
				 (* (location-z vec1) (location-x vec2))))
	(location-z r) (- (* (location-x vec1) (location-y vec2))
			      (* (location-y vec1) (location-x vec2))))
  r)


;;;; --------------------------------------------------------------------------
;;;; min and max

(defun location-minimum
    (c1 c2 &optional (r (make-location (type-of c1))))
  (setf (location-x r) (min (location-x c1) (location-x c2))
	(location-y r) (min (location-y c1) (location-y c2))
	(location-h r) (min (location-h c1) (location-h c2)))
  (when (location-3-p c1)
    (setf (location-z r) (min (location-z c1) (location-z c2)))
    (when (location-4-p c1)
      (setf (location-time r) (min (location-time c1) (location-time c2)))))
  r)

(defun location-maximum
    (c1 c2 &optional (r (make-location (type-of c1))))
  (setf (location-x r) (max (location-x c1) (location-x c2))
	(location-y r) (max (location-y c1) (location-y c2))
	(location-h r) (max (location-h c1) (location-h c2)))
  (when (location-3-p c1)
    (setf (location-z r) (max (location-z c1) (location-z c2)))
    (when (location-4-p c1)
      (setf (location-time r) (max (location-time c1) (location-time c2)))))
  r)


;;;; --------------------------------------------------------------------------
;;;; conversion functions

(defmethod cartesian->spherical
           ((c location-2) &optional (s (location-spherical))
            &aux (x2 (expt (location-x c) 2))
            (y2 (expt (location-y c) 2))
            (z2 (expt (location-z c) 2))
	    (xz (sqrt (+ x2 z2))))
  (if (and (zerop x2) (zerop z2))
    (if (zerop y2)
      (progn (setf (location-alpha s) 0.0
	           (location-radius s) 0.0)
             (when (location-spherical-p s)
               (setf (location-beta s) 0.0)))
      (progn (setf (location-alpha s) 0.0
                   (location-radius s) (location-y c))
             (when (location-spherical-p s)
	       (location-beta s) (atan (location-y c) xz))))
    (progn (setf (location-alpha s) (atan (location-x c) (location-z c))
	         (location-radius s) (sqrt (+ x2 y2 z2)))
           (when (location-spherical-p s)
             (location-beta s) (atan (location-y c) xz))))
  s)

(defmethod spherical->cartesian
    ((s location-polar) &optional (c (make-location-world))
       &aux (xz-rad (* (location-radius s) (cos (location-beta s)))))
  (setf (location-x c) (* xz-rad (sin (location-alpha s)))
	(location-y c) (* (location-radius s) (sin (location-beta s))))
  (when (location-3-p c)
    (setf (location-z c) (* xz-rad (cos (location-alpha s)))))
  c)
		       
#|
(list (location-+ #@(1 1 1) #@(2 2 2) (make-location-2))
      (location-- #@(1 1 1) #@(2 2 2))
      (location-* #@(1 1 1) #@(2 2 2))
      (location-- #@(1 1 1) #@(2 2 2))
      
      (location-+ #@(spherical #.pi #.pi 1) #@(polar #.pi 0) #@(polar 0 0))
      (location-* #@(1 1 1) #@(2 2 2)))

(location-+ #@(1 1 1) #@(2 2 2))
(location-+ #@(1 1 1) #@( 2 2))
(location-+ #@(spherical #.pi 0 1) (spherical->cartesian #@(spherical #.pi 0 1)))

(location-= #@(spherical #.pi 0 1) (spherical->cartesian #@(spherical #.pi 0 1)))
(location-= #@(spherical #.pi 0 1) #@(spherical #.pi 0 1))


(let ((l #@(|3| 4 3 2 1))
      (lr #@(|3| 0 0 0 0))
      (lp #@(port 4 3 2 1))
      (lpr #@(port 4 3 2 1)))

  (print (time (dotimes (x 1) (location-* l l lr))))
  (print (time (dotimes (x 1000) (location-* lp lp lpr))))
  (time (dotimes (x 1000) (setf (location-z l) (* (location-x l) 2))))
  )


|#
:eof
