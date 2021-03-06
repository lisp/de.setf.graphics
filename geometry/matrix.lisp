;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.object-graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.object-graphics.implementation)

(document "This file defines matrix-based location transforms for the 'de.setf.graphics' library."

  (copyright "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
             "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

  (history
   (delta 20060512 "janderson" "patch for matric-fill on amd64")
   (delta 20060421 "janderson@ravenpack.com"
          "extended matrix-fill for 4 and 6 arguments; matrix initialize returns the matrix")
   (delta 20050926 "janderson@ravenpack.com" "corrected short-matrix type definition")
   (delta 20030830 "jaa@setf.de"
          "reconstructed to incorporate declarations and to use a vector as the amount parameter.
   this together with explicit intermediates and float-double trancendentals allows con-free operations.
  
   - mcl optimizes aref and transcendentals
   - allegro optimizes aref. transcentendtals are optimized under windows only
   - clisp makes simple arrays of type t, which makes them hard to recognize.")
   (delta 19929294 "jaa@dtmg" "removed 2d option for matrices. their use is so limited that they're
   better handled as 3d matrices: the wasted space doesn't matter, and
   they're not used in time-critical places.")
   (delta 19891115 "jaa@dtmg" "ported to clx/pcl")
   (delta 19880616 "jaa@dtmg"))

  (long-description
   "the standard marix operations for location transformations
  (catenate, translate, scale, rotate, inverse - with the requisite determinant, reduce, cofactor)
  are implemeted for 4x4 double-float arrays. catenation and initialization examine the actual
  array dimension and thus can be used for arbitrary sized arrays. for the other methods, intermediate
  results are resourced, which emans the 4x4 size is required."))



;;; type definitions
;;;
;;; type declarations have two forms.
;;; the deftype versions yield enough information for the compiler to optimize aref
;;; while the predicate versions test without consing dimensions.
;;; nb. need to check whether the typecase descrimination conses, or one must resort to testing the element type...

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defVar *transform-matrix-type*
    ;; allegro returns the general array type for 2-d arrays
    #-(or allegro clisp)(type-of (make-array '(4 4) :initial-element 0.0d0 :element-type 'double-float))
    #+(or allegro clisp) '(simple-array double-float (4 4)))
  (defVar *view-matrix-type*
    ;; allegro returns the general array type for 2-d arrays
    #-(or allegro clisp)(type-of (make-array '(3 2) :initial-element 0.0s0 :element-type 'short-float))
    #+(or allegro clisp) '(simple-array short-float (4 4)))
  (defVar *double-matrix-type*
    `(,(first *transform-matrix-type*) ,(second *transform-matrix-type*) (* *)))
  (defVar *short-matrix-type*
    `(,(first *view-matrix-type*) ,(second *view-matrix-type*) (* *)))
  
  (deftype transform-matrix ()
    "a (4 4) double float simple array intended for coordinate-system transformations"
    *transform-matrix-type*)

  (deftype view-matrix ()
    "a (3 2) short float simple array intended for coordinate-system transformations"
    *view-matrix-type*)

  (deftype double-matrix ()
    "a (* *) double float simple array"
    *double-matrix-type*)

  (deftype short-matrix ()
    "a (* *) double float simple array"
    *short-matrix-type*)

  (defun double-matrix-p (x)
    "determine if an array is of type double-matrix w/o consing dimensions"
    (and (typep x 'simple-array)
         #-clisp (eq (array-element-type x) 'double-float)
         (= (array-rank x) 2)))

  (defun short-matrix-p (x)
    "determine if an array is of type short-matrix w/o consing dimensions"
    (and (typep x 'simple-array)
         #-clisp (eq (array-element-type x) 'short-float)
         (= (array-rank x) 2)))

  (defun transform-matrix-p (x)
    "determine if an array is of type transform-matrix w/o consing dimensions"
    (and (double-matrix-p x)
         (= (array-dimension x 0) 4)
         (= (array-dimension x 1) 4)))

  (defun view-matrix-p (x)
    "determine if an array is of type view-matrix w/o consing dimensions"
    (and (short-matrix-p x)
         (= (array-dimension x 0) 3)
         (= (array-dimension x 1) 2)))
  
  ) ; eval when
  
(declaim (ftype (function () transform-matrix) transform-matrix get-matrix)
         (ftype (function () view-matrix) view-matrix get-view-matrix)
         (ftype (function (&optional list) double-matrix) matrix)
         (ftype (function (&optional list) short-matrix) short-matrix)
         (ftype (function (transform-matrix &optional transform-matrix) transform-matrix)
                matrix-cofactors matrix-inverse matrix-reduce)
         (ftype (function (double-matrix &optional double-matrix) double-matrix)
                matrix-copy)
         (ftype (function (location-vector &optional transform-matrix transform-matrix) transform-matrix)
                matrix-rotate matrix-scale matrix-translate)
         (ftype (function (double-matrix double-matrix &optional double-matrix) double-matrix)
                matrix-catenate)
         (ftype (function (transform-matrix transform-matrix &optional transform-matrix) transform-matrix)
                transform-matrix-catenate)
         (ftype (function (&optional double-matrix) double-matrix) matrix-identity)
         (ftype (function (transform-matrix) double-float) matrix-determinant)
         (ftype (function (double-float double-float double-float double-float 
                                        double-float double-float double-float double-float 
                                        double-float double-float double-float double-float 
                                        double-float double-float double-float double-float 
                                        &optional transform-matrix) transform-matrix)
                matrix-set)
         (ftype (function () location-vector) location-vector get-location-vector)
         (ftype (function (double-float double-float double-float double-float
                                        &optional location-vector) location-vector)
                location-vector-set))

(defParameter *default.array-dimensions* '(4 4))

(defun matrix (&optional (dimensions *default.array-dimensions*))
  (make-array dimensions :element-type 'double-float :initial-element 0.0d0))

(defun short-matrix (&optional (dimensions *default.array-dimensions*))
  (make-array dimensions :element-type 'short-float :initial-element 0.0s0))

(defun transform-matrix ()
  (make-array '(4 4) :element-type 'double-float :initial-element 0.0d0))

(defun view-matrix ()
  (make-array '(3 2) :element-type 'short-float :initial-element 0.0s0))


(defun print-matrix (array &optional (stream *standard-output*) &key (element-format "~s"))
  (let ((i-max (array-dimension array 0))
        (j-max (array-dimension array 1)))
    (declare (type fixnum i-max j-max))
    (write-string "#2a(" stream)
    (dotimes (i i-max)
      (declare (type fixnum i))
      (unless (= i 0) (write-char #\space stream))
      (write-char #\( stream)
      (dotimes (j j-max)
        (declare (type fixnum j))
        (unless (= j 0) (write-char #\space stream))
        (format stream element-format (aref array i j)))
      (write-char #\) stream))
    (write-char #\) stream)
    array))

(defun matrix-list (array)
  "do the equivalent of (coerce array 'list) for specialized matrices."
  (let ((i-max (array-dimension array 0))
        (j-max (array-dimension array 1)))
    (declare (type fixnum i-max j-max))
    (loop for i from 0 below i-max
          collect (loop for j from 0 below j-max
                        collect (aref array i j)))))
        
; nb. testing the type may cons getting the dimensions.
; thus the declarations below are for {double-transform}-matrix, but the test are by predicate
;(let ((m (matrix))) (time (typep m 'matrix)))
;(let ((m (matrix))) (time (typep m 'simple-array-double-float)))

(defun matrix-identity (&optional (nm (matrix) result-p))
  "initialize an optional argument matrix to the diagonal = 1.0d0.
   if no result matrix is provided a new matrix is constructed."
  #|(declare  (optimize (speed 3) (safety 0)) (type double-matrix nm))|#
  (when result-p (assert-type nm double-matrix))
  (let ((i-max (array-dimension nm 0))
        (j-max (array-dimension nm 1)))
    (declare (type fixnum i-max j-max))
    (dotimes (i i-max)
      (dotimes (j j-max)
        (if (= i j)
          (setf (aref nm i j) 1.0d0)
          (when result-p                  ; a defaulted result is already initialized
            (setf (aref nm i j) 0.0d0))))))
  nm)

(defun transform-identity ()
  (matrix-identity (transform-matrix)))

(defun matrix-random (&optional (nm (matrix) result-p) (max 1.0d0))
  "initialize an optional argument matrix to the diagonal = 1.0d0.
   if no result matrix is provided a new matrix is constructed."
  (declare (optimize (speed 3) (safety 0))
           (type double-matrix nm))
  (when result-p (assert-type nm double-matrix))
  (let ((i-max (array-dimension nm 0))
        (j-max (array-dimension nm 1)))
    (declare (type fixnum i-max j-max))
    (dotimes (i i-max)
      (dotimes (j j-max)
        (setf (aref nm i j) (random max)))))
  nm)



(defmacro _matrix-set (v00 v01 v02 v03 v10 v11 v12 v13 v20 v21 v22 v23 v30 v31 v32 v33
                           m)
  "generate (setf (aref i j) v_ij) for the sixteen values and the matrix."
  `(progn (setf (aref ,m 0 0) ,v00 (aref ,m 0 1) ,v01 (aref ,m 0 2) ,v02 (aref ,m 0 3) ,v03)
          (setf (aref ,m 1 0) ,v10 (aref ,m 1 1) ,v11 (aref ,m 1 2) ,v12 (aref ,m 1 3) ,v13)
          (setf (aref ,m 2 0) ,v20 (aref ,m 2 1) ,v21 (aref ,m 2 2) ,v22 (aref ,m 2 3) ,v23)
          (setf (aref ,m 3 0) ,v30 (aref ,m 3 1) ,v31 (aref ,m 3 2) ,v32 (aref ,m 3 3) ,v33)
          ,m))

(defun matrix-set (v00 v01 v02 v03 v10 v11 v12 v13 v20 v21 v22 v23 v30 v31 v32 v33
                         &optional (m (transform-matrix) result-p))
  "set the contents of a matrix to the 16 argument values, arranged in row-major order,
   as if the initial-contents argument to make-array were flattened. the final, optional,
   argument is the matrix. if none is provided, a new matrix is constructed.
   the initialized matrix is returned."
  (declare (type double-float v00 v01 v02 v03 v10 v11 v12 v13 v20 v21 v22 v23 v30 v31 v32 v33)
           (type transform-matrix m)
           (optimize (speed 3) (safety 0)))
  (when result-p (assert-type m transform-matrix))
  (_matrix-set v00 v01 v02 v03 v10 v11 v12 v13 v20 v21 v22 v23 v30 v31 v32 v33
               m)
  m)

#-(and allegro-version>= (version>= 8 0))
(defun matrix-fill (m v00 v01 v02 v03
                      &optional v10 v11 v12 v13 v20 v21 v22 v23 v30 v31 v32 v33)
  "fill a matrix from the either 4, 6, 12, or 16 argument values, arranged in row-major order."
  (declare (type transform-matrix m)
           (optimize (speed 3) (safety 0)))
  (assert-type m transform-matrix)
  (with-coerced-variables ((double-float v00 v01 v02 v03))
    (if (numberp v10)
      (with-coerced-variables ((double-float v10 v11))
        (if (numberp v12)
          (with-coerced-variables ((double-float v12 v13 v20 v21 v22 v23))
            (if (numberp v30)
              (with-coerced-variables ((double-float v30 v31 v32 v33))
                (_matrix-set v00 v01 v02 v03 v10 v11 v12 v13 v20 v21 v22 v23 v30 v31 v32 v33 m))
              (_matrix-set v00 v01 v02 0.0d0 v03 v10 v11 0.0d0 v12 v13 v20 0.0d0 v21 v22 v23 1.0d0 m)))
          (_matrix-set v00 v01 0.0d0 0.0d0 v02 v03 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 0.0d0 v10 v11 0.0d0 1.0d0 m)))
      (_matrix-set v00 0.0d0 0.0d0 0.0d0 0.0d0 v01 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 0.0d0 v02 v03 0.0d0 1.0d0 m)))
  m)
#+(and allegro-version>= (version>= 8 0))
(defun matrix-fill (m v00 v01 v02 v03
                      &optional v10 v11 v12 v13 v20 v21 v22 v23 v30 v31 v32 v33)
  "fill a matrix from the either 4, 6, 12, or 16 argument values, arranged in row-major order."
  (declare (type transform-matrix m)
           (optimize (speed 3) (safety 0)))
  (assert-type m transform-matrix)
  (with-coerced-variables ((double-float v00 v01 v02 v03))
    (if (numberp v10)
      (with-coerced-variables ((double-float v10 v11))
        (if (numberp v12)
          (with-coerced-variables ((double-float v12 v13 v20 v21 v22 v23))
            (if (numberp v30)
              (with-coerced-variables ((double-float v30 v31 v32 v33))
                (matrix-set v00 v01 v02 v03 v10 v11 v12 v13 v20 v21 v22 v23 v30 v31 v32 v33 m))
              (matrix-set v00 v01 v02 0.0d0 v03 v10 v11 0.0d0 v12 v13 v20 0.0d0 v21 v22 v23 1.0d0 m)))
          (matrix-set v00 v01 0.0d0 0.0d0 v02 v03 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 0.0d0 v10 v11 0.0d0 1.0d0 m)))
      (matrix-set v00 0.0d0 0.0d0 0.0d0 0.0d0 v01 0.0d0 0.0d0 0.0d0 0.0d0 1.0d0 0.0d0 v02 v03 0.0d0 1.0d0 m)))
  m)

(defun matrix-initialize (m)
  (declare (type double-matrix m)
           (optimize (speed 3) (safety 0)))
  (assert-type m double-matrix)
  (dotimes (i (array-dimension m 0))
    (dotimes (j (array-dimension m 1))
      (if (= i j)
        (setf (aref m i j) 1.0d0)
        (setf (aref m i j) 0.0d0))))
  m)

(defun matrix-copy (m &optional (result (matrix (array-dimensions m)) result-p))
  (declare (type double-matrix m result)
           (optimize (speed 3) (safety 0)))
  (assert-type m double-matrix)
  (when result-p
    (assert-type result double-matrix))
  (dotimes (i (array-dimension m 0))
    (dotimes (j (array-dimension m 1))
      (setf (aref result i j) (aref m i j))))
  result)
          
;;
;; adjustable vectors to resource matrices and location vectors
;; they are allocated on-demand to support thread-specific bindings

(defParameter *matrices* nil)

(defParameter *view-matrices* nil)

(defun get-matrix ()
  (unless *matrices*
    (setq *matrices* (make-array 0 :fill-pointer 0 :adjustable t)))
  (if (> (fill-pointer *matrices*) 0)
    (vector-pop *matrices*)
    (transform-matrix)))

(defun get-view-matrix ()
  (unless *view-matrices*
    (setq *view-matrices* (make-array 0 :fill-pointer 0 :adjustable t)))
  (if (> (fill-pointer *view-matrices*) 0)
    (vector-pop *view-matrices*)
    (view-matrix)))

(defun return-matrix (matrix)
  (unless (eq (array-element-type matrix) 'double-float)
    (error "not a transform matrix"))
  (when *matrices*
    (if (find matrix *matrices*)
      (warn "duplicate-return.")
      (vector-push-extend matrix *matrices*))))

(defun return-view-matrix (matrix)
  (unless (eq (array-element-type matrix) 'short-float)
    (error "not a view matrix"))
  (when *view-matrices*
    (vector-push-extend matrix *view-matrices*)))



(defMacro with-matrices (bindings &rest body)
  "establish dynamic bindings to resourced matrices and initialize them according to value specifications
   of the form expected by marix-set."
  `(let ,(mapcar #'(lambda (binding &aux (name (if (listp binding) (first binding) binding)))
                     `(,name (get-matrix)))
                 bindings)
     (declare (type transform-matrix ,@(mapcar #'(lambda (binding) (if (listp binding) (first binding) binding))
                                     bindings)))
     ,@(remove nil (mapcar #'(lambda (binding &aux (initial-elements (when (listp binding) (rest binding))))
                               (when initial-elements
                                 `(_matrix-set ,@initial-elements ,(first binding))))
                           bindings))
     (unwind-protect (progn ,@body)
       ,@(mapcar #'(lambda (binding) `(return-matrix ,(if (listp binding) (first binding) binding)))
                 bindings))))



;;
;; martix operations
;; catenate, translate, scale, rotate, inverse, determinant, reduce, cofactor

(defun matrix-catenate (m1 m2
                           &optional
                           (na (matrix (list (array-dimension m1 0) (array-dimension m2 1))) result-p))
  "catenate two argument matrices and store the result in the optional third result argument.
   if no result matrix is provided, a new matrix is constructed.
   the result matrix is returned."
  (assert-type m1 double-matrix)
  (assert-type m2 double-matrix)
  (when result-p (assert-type na double-matrix))
  (let ((i-max (array-dimension m1 0))
        (j-max (array-dimension m1 1))
        (k-max (array-dimension m2 1)))
    (declare (type fixnum i-max j-max k-max))
    (unless (= j-max (array-dimension m2 0))
      (error "array-dimension mismatch: ~s ~s." (array-dimensions m1) (array-dimensions m2)))
    ;; the unrolled version offers about 25% advantage
    (if (and (= i-max 4) (= j-max 4) (= k-max 4))
      (matrix-catenate-4x4 m1 m2 na)
      (matrix-catenate-*x* m1 m2 na i-max j-max k-max))))

(defun matrix-catenate-*x* (m1 m2 na
                               &optional
                               (i-max (array-dimension m1 0))
                               (j-max (array-dimension m1 1))
                               (k-max (array-dimension m2 1)))
  (declare (optimize (speed 3) (safety 0))
           (type double-matrix m1 m2 na)
           (type fixnum i-max j-max k-max))
  (let ((cell 0.0d0))
    (declare (type double-float cell)
             (dynamic-extent cell))
    (dotimes (i i-max na)
      (declare (type fixnum i))
      (dotimes (k k-max)
        (declare (type fixnum k))
        (setf cell 0.0d0)
        (dotimes (j j-max)
          (declare (type fixnum j))
          (setf cell (+ cell (* (aref m1 i j) (aref m2 j k))))
          )
        (setf (aref na i k) cell)
        ))))

(defun matrix-catenate-4x4 (m1 m2 na)
   (declare (type transform-matrix m1 m2 na)
           (optimize (speed 3) (safety 0)))
   (dotimes (i 4 na)
     (declare (type fixnum i))
     (dotimes (j 4)
       (declare (type fixnum j))
       (setf (aref na i j)
             (the double-float
               (+ (the double-float (+ (the double-float
                                         (* (the double-float (aref m1 i 0))
                                            (the double-float (aref m2 0 j))))
                                       (the double-float
                                         (* (the double-float (aref m1 i 1))
                                            (the double-float (aref m2 1 j))))))
                  (the double-float (+ (the double-float
                                         (* (the double-float (aref m1 i 2))
                                            (the double-float (aref m2 2 j))))
                                       (the double-float
                                         (* (the double-float (aref m1 i 3))
                                            (the double-float (aref m2 3 j))))))))))))


(defun matrix-translate (amount &optional
                                (matrix (matrix-identity (transform-matrix)) matrix-p)
                                (result (transform-matrix) result-p))
  "translate an optional argument matrix according to an initial location-vector argument 
   and store the result in the optional third result argument.
   if no argument matrix is provided an initial matrix is constructed.
   if no result matrix is provided, a new matrix is constructed.
   the result matrix is returned."
  (declare (type transform-matrix matrix result)
           (type location-vector amount)
           (optimize (speed 3) (safety 0)))
  (assert-type amount location-vector)
  (when matrix-p (assert-type matrix transform-matrix))
  (when result-p (assert-type result transform-matrix))

  (with-matrices ((t-matrix 1.0d0 0.0d0 0.0d0 0.0d0
                            0.0d0 1.0d0 0.0d0 0.0d0
                            0.0d0 0.0d0 1.0d0 0.0d0
                            (aref amount 0) (aref amount 1) (aref amount 2) 1.0d0))
    (matrix-catenate matrix t-matrix result)))



(defun matrix-scale (amount &optional
                            (matrix (matrix-identity (transform-matrix)) matrix-p)
                            (result (transform-matrix) result-p))
  "scale an optional argument matrix according to an initial location-vector argument 
   and store the result in the optional third result argument.
   if no argument matrix is provided an initial matrix is constructed.
   if no result matrix is provided, a new matrix is constructed.
   the result matrix is returned."
  (declare (type transform-matrix matrix result)
           (type location-vector amount)
           (optimize (speed 3) (safety 0)))
  (assert-type amount location-vector)
  (when matrix-p (assert-type matrix transform-matrix))
  (when result-p (assert-type result transform-matrix))
  
  (with-matrices ((scale-matrix (aref amount 0) 0.0d0 0.0d0 0.0d0
                                0.0d0 (aref amount 1) 0.0d0 0.0d0
                                0.0d0 0.0d0 (aref amount 2) 0.0d0
                                0.0d0 0.0d0 0.0d0 1.0d0)
                  (translated-matrix)
                  (scaled-matrix))
    (with-location-vectors ((to-origin (* -1.0d0 (aref matrix 3 0))
                                       (* -1.0d0 (aref matrix 3 1))
                                       (* -1.0d0 (aref matrix 3 2))
                                       0.0d0)
                            (back (aref matrix 3 0) (aref matrix 3 1) (aref matrix 3 2)  0.0d0))
      (matrix-translate to-origin matrix translated-matrix)
      (matrix-catenate translated-matrix scale-matrix scaled-matrix)
      (matrix-translate back scaled-matrix result)
      )))




(defun matrix-rotate (amount &optional
                            (matrix (matrix-identity (transform-matrix)) matrix-p)
                            (result (transform-matrix) result-p))
  "rotate an optional argument matrix according to an initial location-vector argument 
   and store the result in the optional third result argument. the order of rotation, z, y, x is chosen
   so that an amount #(0.0d0 alpha beta) will correspond to a spherical rotation.
   if no argument matrix is provided an initial matrix is constructed.
   if no result matrix is provided, a new matrix is constructed.
   the result matrix is returned."
  (declare (type transform-matrix matrix result)
           (type location-vector amount)
           (optimize (speed 3) (safety 0)))
  (assert-type amount location-vector)
  (when matrix-p (assert-type matrix transform-matrix))
  (when result-p (assert-type result transform-matrix))
           ; (rt::print-time-and-memory
  (let ((angle 0.0d0)
        (sin 0.0d0)
        (cos 0.0d0))
    (declare (type double-float angle sin cos)
             (dynamic-extent angle cos sin)
             (optimize (speed 3) (safety 0)))
    (with-location-vectors ((to-origin (* -1.0d0 (aref matrix 3 0))
                                       (* -1.0d0 (aref matrix 3 1))
                                       (* -1.0d0 (aref matrix 3 2))
                                       0.0d0)
                            (back (aref matrix 3 0)
                                  (aref matrix 3 1)
                                  (aref matrix 3 2)
                                  0.0d0))

      (with-matrices (at-origin rx ry rz rotation)
        (matrix-translate to-origin matrix at-origin)
        (let ((rotated at-origin))
          (declare (type transform-matrix rotated))
          (setf angle (aref amount 2))
          (unless (= 0.0d0 angle)       ; z rotation
            (sin! angle sin)
            (cos! angle cos)
            (_matrix-set cos sin 0.0d0 0.0d0
                         (* -1.0d0 sin) cos 0.0d0 0.0d0
                         0.0d0 0.0d0 1.0d0 0.0d0
                         0.0d0 0.0d0 0.0d0 1.0d0
                         rotation)
            (matrix-catenate rotated rotation rz)
            (setf rotated rz))
          (setf angle (aref amount 1))
          (unless (= 0.0d0 angle)       ; y rotation
            (sin! angle sin)
            (cos! angle cos)
            (_matrix-set cos 0.0d0 (* -1.0d0 sin) 0.0d0
                         0.0d0 1.0d0 0.0d0 0.0d0
                         sin 0.0d0 cos 0.0d0
                         0.0d0 0.0d0 0.0d0 1.0d0
                         rotation)
            (matrix-catenate rotated rotation ry)
            (setf rotated ry))
          (setf angle (aref amount 0))
          (unless (= 0.0d0 angle)       ; x rotation
            (sin! angle sin)
            (cos! angle cos)
            (_matrix-set 1.0d0 0.0d0 0.0d0 0.0d0
                         0.0d0 cos sin 0.0d0
                         0.0d0 (* -1.0d0 sin) cos 0.0d0
                         0.0d0 0.0d0 0.0d0 1.0d0
                         rotation)
            (matrix-catenate rotated rotation rx)
            (setf rotated rx))
          (matrix-translate back rotated result))))
      result)) ;)



;;;
;;; this implementation from the ai repository

(defun matrix-inverse (matrix &optional (result (matrix (array-dimensions matrix)) result-p))
  "Find the inverse of a matrix."
  (declare (optimize (speed 3) (safety 0))
           (type double-matrix matrix result))
  (assert-type matrix double-matrix)
  (when result-p (assert-type result double-matrix))
  (matrix-copy matrix result)
  (let ((size (array-dimension matrix 0))
        (temp 0.0d0))
    (declare (type double-float temp)
             (type fixnum size)
             (dynamic-extent temp))
    (dotimes (i size result)
      (declare (type fixnum i))
      (setf temp (aref result i i))
      (dotimes (j size)
        (declare (type fixnum j))
        (setf (aref result i j)
              (if (= i j)
                (/ 1.0d0 (aref result i j))
                (/ (aref result i j) temp))))
      (dotimes (j size)
        (declare (type fixnum j))
        (unless (= i j)
          (setf temp (aref result j i)
                (aref result j i) 0.0d0)
          (dotimes (k size)
            (declare (type fixnum k))
            (setf (aref result j k)
                  (- (aref result j k)
                     (* temp (aref result i k))))))))))

#|


; unrolled operations are possible for other loop sizes,
; but are realized for 4x4 only

(defmacro _matrix-op (function m dimensions)
  "generate (setf (aref i j) (function i j)) for the nine values and the matrix."
  (let ((forms nil))
    (dotimes (i (first dimensions))
      (dotimes (j (second dimensions))
        (push `(setf (aref ,m ,i ,j) (,function ,i ,j)) forms)))
  `(progn ,@(reverse forms))))

(defun matrix-catenate (m1 m2 &optional (na (matrix)))
  "catenate two argument matrices and store the result in the optional third result argument.
   if no result matrix is provided, a new matrix is constructed.
   the result matrix is returned."
  (declare (type matrix m1 m2 na)
           (optimize (speed 3) (safety 0)))
  (macrolet ((ij-op (i j)
               `(the double-float
                  (+ (the double-float (+ (the double-float
                                            (* (the double-float (aref m1 ,i 0))
                                               (the double-float (aref m2 0 ,j))))
                                          (the double-float
                                            (* (the double-float (aref m1 ,i 1))
                                               (the double-float (aref m2 1 ,j))))))
                     (the double-float (+ (the double-float
                                            (* (the double-float (aref m1 ,i 2))
                                               (the double-float (aref m2 2 ,j))))
                                          (the double-float
                                            (* (the double-float (aref m1 ,i 3))
                                               (the double-float (aref m2 3 ,j))))))))))
    (_matrix-op ij-op na (4 4)))
  na)


;; a straight-forward implementation via determinants and cofactors

(defun matrix-inverse (m &optional (na (transform-matrix) result-p))
  "invert an argument matrix and store the result in an optional result matrix.
   if no result matrix is provided, a new matrix is constructed."
  (declare (type transform-matrix m na)
           (optimize (speed 3) (compilation-speed 0) (safety 0)))
  (assert-type m transform-matrix)
  (when result-p (assert-type na transform-matrix))
  (let ((md 0.0d0))
    (declare (type double-float md)
             (dynamic-extent md))
    (flet ((do-the-determinant (m)
             (declare (type transform-matrix m)
                      (optimize (speed 3) (compilation-speed 0) (safety 0)))
             (setf md
                   (* (aref m 3 3)
                      (- (+ (* (aref m 0 0) (* (aref m 1 1) (aref m 2 2)))
                            (+ (* (aref m 0 1) (* (aref m 1 2) (aref m 2 0)))
                               (* (aref m 0 2) (* (aref m 1 0) (aref m 2 1)))))
                         (+ (* (aref m 2 0) (* (aref m 1 1) (aref m 0 2)))
                            (+ (* (aref m 2 1) (* (aref m 1 2) (aref m 0 0)))
                               (* (aref m 2 2) (* (aref m 1 0) (aref m 0 1))))))))))
      (if (and (= 0.0d0 (aref m 0 3))
               (= 0.0d0 (aref m 1 3))
               (= 0.0d0 (aref m 2 3)))
        (do-the-determinant m)
        (with-matrices (reduced)
          (do-the-determinant (matrix-reduce m reduced)))))
    (if (= md 0.0d0)
      (error "matrix ~a has zero determinant, thus no inverse:~%" m)
      (with-matrices (cofactors)
        (matrix-cofactors m cofactors)
        (dotimes (y 4)
          (dotimes (x 4)
            (setf (aref na x y)		; transpose x y
                  (/ (aref cofactors y x) md))))))
    na))

(defun matrix-determinant
  (m)
  "compute the determinant of a matrix.
   uses the knowledge that the last column has only (3 3) non-zero.
   nb. matrix-inverse has its own version of this function in-lined."
  (declare (type transform-matrix m)
           (optimize (speed 3) (compilation-speed 0) (safety 0))
           (ftype (function (transform-matrix) transform-matrix)  matrix-reduce))
  (assert-type m transform-matrix)
  (flet ((the-determinant (m)
           (declare (type matrix m)
                    (optimize (speed 3) (compilation-speed 0) (safety 0)))
           (* (aref m 3 3)
              (- (+ (* (aref m 0 0) (* (aref m 1 1) (aref m 2 2)))
                    (+ (* (aref m 0 1) (* (aref m 1 2) (aref m 2 0)))
                       (* (aref m 0 2) (* (aref m 1 0) (aref m 2 1)))))
                 (+ (* (aref m 2 0) (* (aref m 1 1) (aref m 0 2)))
                    (+ (* (aref m 2 1) (* (aref m 1 2) (aref m 0 0)))
                       (* (aref m 2 2) (* (aref m 1 0) (aref m 0 1)))))))))
    (if (and (= 0.0d0 (aref m 0 3))
            (= 0.0d0 (aref m 1 3))
            (= 0.0d0 (aref m 2 3)))
      (the-determinant m)
      (with-matrices (reduced)
        (the-determinant (matrix-reduce m reduced))))))

        
(defun matrix-reduce
       (m &optional (result (transform-matrix)))
  "reduce matrix to only homogeneous factor non zero in last column"
  (declare (type transform-matrix m result)
           (optimize (speed 3) (compilation-speed 0) (safety 0)))
  ; (assert-type m transform-matrix) ; internal use only
  (let ((hf 0.0d0)
        (rf 0.0d0))
    (declare (double-float hf rf)
             (dynamic-extent hf rf))
    (setf hf (aref m 3 3))
    (dotimes (y 3)
      (setq rf (/ (aref m y 3) hf))
      (dotimes (x 3)
        (setf (aref result y x) (- (aref m y x) (* (aref m 3 x) rf))))
      (setf (aref result y 3) 0.0d0))
    (setf (aref result 3 3) hf)
    result))

(defun matrix-cofactors
  (m &optional (result (transform-matrix)))
  (declare (optimize (speed 3) (compilation-speed 0) (safety 0))
           (type transform-matrix m result)
           (ftype (function (fixnum fixnum) fixnum) mod))
  ; (assert-type m transform-matrix) ; internal use only
  (let ((xb 0) (yb 0))
    (declare (type fixnum xb yb))
    (dotimes (x 4)
      (dotimes (y 4)
        (setf xb (+ x 1) yb (+ y 1))
        (setf (aref result x y)
              (* (the double-float (if (evenp (+ x y)) 1.0d0 -1.0d0))
                 (- (+ (* (aref m (mod (+ yb 0) 4) (mod (+ xb 0) 4))
                          (* (aref m (mod (+ yb 1) 4) (mod (+ xb 1) 4))
                             (aref m (mod (+ yb 2) 4) (mod (+ xb 2) 4))))
                       (+ (* (aref m (mod (+ yb 0) 4) (mod (+ xb 1) 4))
                             (* (aref m (mod (+ yb 1) 4) (mod (+ xb 2) 4))
                                (aref m (mod (+ yb 2) 4) (mod (+ xb 0) 4))))
                          (* (aref m (mod (+ yb 0) 4) (mod (+ xb 2) 4))
                             (* (aref m (mod (+ yb 1) 4) (mod (+ xb 0) 4))
                                (aref m (mod (+ yb 2) 4) (mod (+ xb 1) 4))))))
                    (+ (* (aref m (mod (+ yb 2) 4) (mod (+ xb 0) 4))
                          (* (aref m (mod (+ yb 1) 4) (mod (+ xb 1) 4))
                             (aref m (mod (+ yb 0) 4) (mod (+ xb 2) 4))))
                       (+ (* (aref m (mod (+ yb 2) 4) (mod (+ xb 1) 4))
                             (* (aref m (mod (+ yb 1) 4) (mod (+ xb 2) 4))
                                (aref m (mod (+ yb 0) 4) (mod (+ xb 0) 4))))
                          (* (aref m (mod (+ yb 2) 4) (mod (+ xb 2) 4))
                             (* (aref m (mod (+ yb 1) 4) (mod (+ xb 0) 4))
                                (aref m (mod (+ yb 0) 4) (mod (+ xb 1) 4))))))))))))
  result)

|#

:de.setf.graphics

