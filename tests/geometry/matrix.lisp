;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)

(document :file
  (description "This file defines matrix tests for the 'de.setf.graphics' library.")
  
  (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
   "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `DSG:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


(defun counted-matrix (array &aux (count 1))
  "generate a matrix with 1.0 on the diagonal and cardinals as all other elements
   use coerce to generate new floats as the elements."
  (dotimes (i (array-dimension array 0))
    (dotimes (j (array-dimension array 1))
      ;; always coerce, _from a fixnum_ as some of the fpc tests modify the float value in-place
      (setf (aref array i j)
            (coerce (if (= i j) 1 count) 'double-float))
      (incf count)))
  array)

;; tests to verify that no garbage is generated

(dsu:test og/matrix/types
  (every #'identity
         (list (typep (matrix) 'array)
               (typep (location-vector) 'location-vector)
               (typep (transform-matrix) 'transform-matrix)))
  :value t :mode :silent)

;; (let ((m (transform-matrix))) (time (dotimes (x 100000) (matrix-identity m))))

(dsu:test og/matrix/copy
  (flet ((test-copy-matrix (c m r)
           (dotimes (x c) (matrix-copy m r)) r))
    (let ((m (matrix-identity (matrix '(4 4))))
          (r (matrix-random (matrix '(4 4)))))
      (nth-value 2 (dsu:time-and-memory (test-copy-matrix 10000 m r)))))
  :value 0 :mode :silent)
; ((g4-1.25 x10000) (mcl-5.0 16) (clisp-2.30 418) (acl-6.2 ))
; ((g2-2.5 (mcf-5.1b4 12ms 0))

(dsu:test og.location-vector.copy
  (flet ((test-copy-location-vector (c v1 v2)
           (declare (type location-vector v1 v2)
                    (optimize (speed 3) (safety 0)))
           (dotimes (z c)
             (dotimes (x 4) (setf (aref v2 x) (aref v1 x))))
           v2))
    (let ((l1 (make-location-vector :x 1.0d0 :y 2.0d0 :z 3.0d0 :h 4.0d0))
          (l2 (make-location-vector)))
      (values (equalp (test-copy-location-vector 1 l1 l2) l1)
              (nth-value 2 (dsu:time-and-memory (test-copy-location-vector 1000000 l1 l2))))))
  :values '(t 0) :mode :silent)

#+ignore
(defun test-m (m)
  (declare (type matrix m) (optimize (speed 3) (safety 0))
           (:explain :calls :types :boxing))
  (dotimes (x 3) (declare (type fixnum x)) (setf (aref m x 0) (aref m 0 x))) m)

#+ignore
(defun test-v (m)
  (declare (type location-vector m) (optimize (speed 3) (safety 0))
           (:explain :calls :types :boxing))
  (dotimes (x 4) (declare (type fixnum x)) (setf (aref m x) (aref m (- 3 x)))) m)

(dsu:test og.matrix.initialize
  (flet ((test-initialize-matrix (count m)
           (dotimes (x count)
             (matrix-set 1.0d0 2.0d0 3.0d0 4.0d0
                         5.0d0 1.0d0 7.0d0 8.0d0
                         9.0d0 10.0d0 1.0d0 12.0d0
                         13.0d0 14.0d0 15.0d0 1.0d0
                         m))))
    (let ((m (matrix-random (matrix '(4 4)))))
      (values (nth-value 2 (dsu:time-and-memory (test-initialize-matrix 100000 m)))
              (equalp m (counted-matrix (matrix '(4 4)))))))
  :values '(0 t) :mode :silent)

(dsu:test og.matrix.catenate.1
  (flet ((count-catenate (count m1 m2 mr)
           (dotimes (x count)
             (declare (type fixnum x count))
             (matrix-catenate m1 m2 mr))
           mr))
    (let* ((m1 (matrix-identity (transform-matrix)))
           (m2 (matrix-random (transform-matrix)))
           (mr (transform-matrix)))
      (values (nth-value 2 (dsu:time-and-memory (count-catenate 100 m1 m2 mr)))
              (equalp m2 mr))))
  :values '(0 t) :mode :silent)

#+ignore
(rt:deftest og.matrix.catenate.2
  (flet ((test-catenate (count size)
           (flet ((do-catenate (count m1 m2 m3)
                    (dotimes (x count) (matrix-catenate m1 m2 m3))
                    (when (<= (array-dimension m3 0) 4) m3)))
             (let* ((m1 (matrix size))
                    (m2 (matrix (reverse size)))
                    (m3 (matrix-random (matrix (list (array-dimension m1 0) (array-dimension m2 1))))))
               (let ((location 1.0d0))
                 (dotimes (i (array-dimension m1 0))
                   (dotimes (j (array-dimension m1 1))
                     (setf (aref m1 i j) location)
                     (setf location (+ location 1.0d0))))
                 (dotimes (i (array-dimension m2 0))
                   (dotimes (j (array-dimension m2 1))
                     (setf (aref m2 i j) location)
                     (setf location (+ location 1.0d0)))))
               ;; (format *trace-output* "~%dimensions: ~s count: ~s~%" size count)
               ;; (when (<= (array-dimension m1 0) 4) (print-matrix m1))
               ;; (when (<= (array-dimension m2 0) 4) (print-matrix m2))
               #+ccl (ccl:gc) #+allegro (excl:gc)
               (dsu:time-and-memory (do-catenate count m1 m2 m3))))))
    (mapcar #'(lambda (size)
                (let ((count 10000))
                  (multiple-value-bind (array time memory)
                                       (test-catenate count size)
                    (declare (ignore array))
                    (format *trace-output* "~%catenate ~s:~10t~d ms, ~d bytes;  ~d ms / pass; ~d ms / cell"
                            size time memory
                            (float (/ time count))
                            (float (/ time (* count (apply '* size)))))
                    memory)))
            '(; '(1024 1024)) ; needs a large memory space
              ;;(400 400)
              ;;(256 256)
              (30 30)                   ; "http://www.bagley.org/~doug/shootout/bench/matrix/"  
              (4 4)
              (3 2)
              (2 2))))
  (0 0 0 0))
    


(dsu:test og.matrix.translate
  (flet ((test-translate (count v m r)
           (dotimes (x count)
             (declare (type fixnum x count))
             (matrix-translate v m r))))
    (let ((v (make-location-vector :x 10.0d0 :y 10.0d0 :z 10.0))
          (m (matrix-identity (transform-matrix)))
          (r (transform-matrix)))
      (nth-value 2 (dsu:time-and-memory (test-translate 10000 v m r)))))
  :value 0 :mode :silent)

(dsu:test og.matrix.scale
  (flet ((test-scale (count amount matrix result)
           (dotimes (x count)
             (declare (type fixnum x count))
             (matrix-scale amount matrix result))))
    (let ((amount (make-location-vector :x 10.0d0 :y 10.0d0 :z 10.0d0))
          (matrix (matrix-identity (transform-matrix)))
          (result (transform-matrix)))
      (nth-value 2 (dsu:time-and-memory (test-scale 100 amount matrix result)))))
  :value 0 :mode :silent)

(dsu:test og.matrix-catenate-scale
  (flet ((test-catenate-scale (count m1 m2 m3)
           (dotimes (x count) (matrix-catenate m1 m2 m3))))
    (let ((m1 (matrix-scale (make-location-vector :x 2.0d0  :y 2.0d0  :z 2.0d0)))
          (m2 (matrix-scale (make-location-vector :x 3.0d0  :y 3.0d0  :z 3.0d0)))
          (m3 (matrix)))
      (nth-value 2 (dsu:time-and-memory (test-catenate-scale 100 m1 m2 m3)))))
 :value 0 :mode :silent)

(dsu:test og.matrix.rotate
  (flet ((test-rotate (count amount matrix result)
           (dotimes (x count)
             (matrix-rotate amount matrix result))))
    (let ((amount (make-location-vector :x 0.0d0 :y 0.0d0 :z (/ pi 2)))
          (matrix (matrix-identity (transform-matrix)))
          (result (transform-matrix)))
      (nth-value 2 (dsu:time-and-memory (test-rotate 10 amount matrix result)))))
  :value 0 :mode :silent)
                 
(dsu:test og.matrix.catenate-scale-translate
  (flet ((test-catenate-scale-translate (count scale translation result
                                               matrix-t matrix-s matrix-tr matrix-sr)
           (dotimes (x count)
             (matrix-catenate (matrix-translate translation (matrix-identity matrix-t) matrix-tr)
                              (matrix-scale scale (matrix-identity matrix-s) matrix-sr)
                              result))))
    (with-matrices ((matrix-t) (matrix-s) (matrix-tr) (matrix-sr))
      (let ((scale (make-location-vector :x 2.0d0 :y 2.0d0 :z 2.0d0))
            (translation (make-location-vector :x 3.0d0 :y 3.0d0 :z 3.0d0))
            (result (matrix)))
        (nth-value 2 (dsu:time-and-memory (test-catenate-scale-translate 10 scale translation result
                                                                         matrix-t matrix-s matrix-tr matrix-sr))))))
  :value 0 :mode :silent)

(dsu:test og.matrix.inverse.1
  (flet ((test-inverse (count m mr)
           (dotimes (i count)
             (matrix-inverse m mr))
           mr))
    (let* ((m (matrix-random (transform-matrix)))
           (mr (transform-matrix)))
      (nth-value 2 (dsu:time-and-memory (test-inverse 100 m mr)))))
  :value 0 :mode :silent)

(dsu:test og.matrix.inverse.2
  (flet ((test-inverse (count m mr)
           (dotimes (i count)
             (matrix-inverse m mr))
           mr))
    (let* ((m (matrix-random (matrix '(10 10))))
           (mr (matrix '(10 10))))
      (nth-value 2 (dsu:time-and-memory (test-inverse 100 m mr)))))
  :value 0 :mode :silent)



(dsu:test og.matrix.inverse.2
  (flet ((test-inverse-catenate (count m mi mr)
           (dotimes (i count)
             (matrix-catenate m (matrix-inverse m mi) mr))
           mr))
    (let* ((m (matrix-random (transform-matrix) 10.0d0))
           (mi (transform-matrix))
           (mr (transform-matrix)))
      (values (nth-value 2 (dsu:time-and-memory (test-inverse-catenate 10000 m mi mr)))
              (loop for i below (length (array-dimensions mr))
                    collect (or (< (abs (- 1.0d0 (aref mr i i))) short-float-epsilon)
                                (aref mr i i))))))
  :values '(0 (t t)) :mode :silent)


(dsu:test og.matrix.fill
  (every #'identity
            (list (equalp (matrix-fill (transform-matrix) 1 2 3 4)
                          #2a((1.0d0 0.0d0 0.0d0 0.0d0)
                              (0.0d0 2.0d0 0.0d0 0.0d0)
                              (0.0d0 0.0d0 1.0d0 0.0d0)
                              (3.0d0 4.0d0 0.0d0 1.0d0)))
                  (equalp (matrix-fill (transform-matrix) 1 2 3 4 5 6)
                          #2a((1.0d0 2.0d0 0.0d0 0.0d0)
                              (3.0d0 4.0d0 0.0d0 0.0d0)
                              (0.0d0 0.0d0 1.0d0 0.0d0)
                              (5.0d0 6.0d0 0.0d0 1.0d0)))
                  (equalp (matrix-fill (transform-matrix) 1 2 3 4 5 6 7 8 9 10 11 12)
                          #2a((1.0d0 2.0d0 3.0d0 0.0d0)
                              (4.0d0 5.0d0 6.0d0 0.0d0)
                              (7.0d0 8.0d0 9.0d0 0.0d0)
                              (10.0d0 11.0d0 12.0d0 1.0d0)))
                  (equalp (matrix-fill (transform-matrix) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
                          #2a((1.0d0 2.0d0 3.0d0 4.0d0)
                              (5.0d0 6.0d0 7.0d0 8.0d0)
                              (9.0d0 10.0d0 11.0d0 12.0d0)
                              (13.0d0 14.0d0 15.0d0 16.0d0)))))
  :value t :mode :silent)
                  



:eof
