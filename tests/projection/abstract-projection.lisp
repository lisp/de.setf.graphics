;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-


(in-package :de.setf.graphics.implementation)

(document :file
 (description "This file defines bstract test for the 'de.setf.graphics' library.")
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `DSG:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (history
  (copyright 2003-2005 "[james anderson](mailto:james.anderson@setf.de)")
  (delta 20050614 "collected abstract tests.")
  (delta 20050620 "generalized tests to permit use for any context"))

 (long-description "The `DSG:tests;projection;abstract-projection.lisp` file defines abstract tests for the
 scene geometry elements. This file prepares the context necessary to invoke those tests against a CLX
 device interface."))


(defparameter *initial-view-position* (make-point 64 64))
(defparameter *next-view-position* *initial-view-position*)
(defparameter *max-view-height* 0)
(defparameter *test-screen-size* (make-point 0 0))
(defparameter *test-view-size* (make-point 320 320))
(defparameter *test-view-placement* :tiled)
(defparameter *test-sleep* 0.0)
(defparameter *test-count* 10)
(defun minimum-test-count () (or *test-count* 10))
(defparameter *test-raster* nil)
(defparameter *test-clear-color* #@(|3| .1 .1 .1))
(defparameter *test-fill-color* #@(|3| .1 .1 .9))
(defparameter *test-stroke-color* #@(|3| .9 .1 .1))
(defvar *test-context* nil
  "binds a global value for use with regression tests, each of which runs its test
   wrapped within a with-projection-context ")
(defparameter *test-projection-scale* #@(|3| 1.0 1.0 1.0))

(defun identity-projection-context (&rest args)
  "an identity projection context."
  args)
(defparameter *ipc* #'identity-projection-context)

(defun test-raster ()
  (or *test-raster*
      (let ((data (make-array '(128 128) :element-type t :initial-element nil)))
        (dotimes (i 128)
          (dotimes (j 128)
            (setf (aref data i j)
                  (let ((intensity (round (* 255 (/ i 128)))))
                    (make-list 3 :initial-element intensity)))))
        (dotimes (x 128)
          (let ((y (+ 64 (round (* 63 (sin (* 4 pi (/ x 128))))))))
            (setf (aref data y x) (list (* x 2) 0 (- 255 (* x 2))))))
        (setq *test-raster*
              (make-instance 'raster :location (make-location-world :x 0.0d0 :y 0.0d0)
                             :sample-depth 32
                             :sample-data data)))))
;;; (test-raster)

(defgeneric scene-equalp (f1 f2)
  (:method ((f1 list) (f2 list))
            (if (eq (first f1) 'quote)
              (setf f1 (second f1)))
            (when (eq (first f2) 'quote)
              (setf f2 (second f2)))
            (or (and (null f1) (null f2))
                (and f1 f2
                     (scene-equalp (first f1) (first f2))
                     (scene-equalp (rest f1) (rest f2)))))
  (:method ((s1 symbol) (s2 symbol))
           (eq s1 s2))
  (:method ((s1 t) (s2 symbol))
           (and s2 (boundp s2)
                (let ((s2v (symbol-value s2)))
                  (and (not (eq s2v s2))
                       (scene-equalp s1 s2v)))))
  (:method ((s1 symbol) (s2 t))
           (and s1 (boundp s1)
                (let ((s1v (symbol-value s1)))
                  (and (not (eq s1v s1))
                       (scene-equalp s1v s1)))))
  (:method ((f1 t) (f2 t))
           (equalp f1 f2))
  ;; allow compiled function to match an expression
  (:method ((s1 function) (s2 list))
           (eql (first s2) 'function)))

(defgeneric initialize-test-context (context &key clear-p)
  (:method ((context t) &key (clear-p t))
           (clear-agent *test-clear-color*)
           (transform :projection :clear
                      :projection :scale *test-projection-scale*
                      :view :clear)
           (when clear-p (clear-view))))
  
(defun execute-graphics-tests (&key (pattern :og.**) (stream *trace-output*) (mode test:*test-unit-mode*))
  (dolist (test (test:find-tests pattern))
    (test:execute-test test :stream stream :mode mode)))


(defun render-form-and-test (name op form &key (clear-p t) (context *projection-context*))
  name
  (with-projection-context (context)
    (initialize-test-context context :clear-p clear-p)
    (let ((result (funcall op)))
      (flush-view)
      (cond ((eq context *ipc*)
             (scene-equalp result form))
            (t t)))))

(defmacro og::test (name form &rest args)
  (let* ((render-args (when (consp name) (rest name)))
         (name (if (consp name) (first name) name))
         (docstring nil)
         (op (gensym (string name))))
    (when (stringp form)
      (setf docstring form
            form (pop args)))
    `(test:test ,name
       ,@(when docstring (list docstring))
       (flet ((,op () ,form))
         (render-form-and-test ',name #',op ',form ,@render-args))
       :value t ,@args)))



;;; clear the og.projection tests
(setf (test:find-test "og.projection.**") nil)


;;;
;;; generic tests


;;;
;;; per geometric element

(og::test og.projection.arc.1
  (arc  #@(|3| 0.0 0.0 0.0)  0.5 0.0 #.pi :clockwise '((path-effect :paint)
                                                      (path-constituents :lines)
                                                      (stroke-agent #@(|3| 0.5 0.0 0.0)))))

(og::test og.projection.arc*2.1
  (arc*2 -0.5 0.0  0.5 #.pi #.pi :clockwise (lambda (render)
                                              (path-effect :paint)
                                              (path-constituents :surfaces)
                                              (fill-agent #@(|3| 0.5 0.0 0.0))
                                              (funcall render)
                                              (path-constituents :lines)
                                              (stroke-agent #@(|3| 0.0 0.5 0.0))
                                              (funcall render))))

(og::test og.projection.arc*3.1
  (arc*3 0.5 0.0 0.0  0.55 0.0 #.pi :clockwise '((path-effect :paint)
                                                 (path-constituents :lines)
                                                 (stroke-agent #@(|3| 0.0 0.0 0.5)))))


(og::test og.projection.circle.1
  (circle #@(|3| 0.1 0.0 0.0) 0.1 '((path-effect :paint)
                                    (stroke-agent #@(|3| 0.5 0.0 0.0)))))

(og::test og.projection.circle*2.1
  (circle*2  0.2 0.1 0.2 '((path-effect :paint)
                           (path-constituents :surfaces)
                           (fill-agent #@(|3| 0.0 0.5 0.0)))))

(og::test og.projection.circ3e*3.1
  (circle*3  0.5 0.2 0.0  0.5 '((path-effect :paint)
                                (path-constituents :lines)
                                (stroke-agent #@(|3| 0.0 0.0 0.5)))))


(og::test og.projection.line.1
  (line  #@(|3| 0.0 0.0 0.0) #@(|3| 0.1 0.0 0.0) '((path-effect :paint)
                                                   (stroke-agent #@(|3| 0.5 0.0 0.0)))))

(og::test og.projection.line*2.1
  (line*2 0.0 0.0 0.2 0.1 '((path-effect :paint)
                             (stroke-agent #@(|3| 0.0 0.5 0.0)))))

(og::test og.projection.line*3.1
  (line*3 0.0 0.0 0.0  0.5 0.2 0.0
          '((path-effect :paint) (path-constituents :lines) (stroke-agent #@(|3| 0.0 0.0 0.5)))))


(og::test og.projection.poly.1
  (progn
    (transform :projection :rotate 1.0 1.0 0.0)
    (poly '(#@(|3| 0.0 0.0 0.0) #@(|3| 0.0 0.1 0.0) #@(|3| 0.1 0.1 0.0) #@(|3| 0.1 0.0 0.0)
            #@(|3| 0.1 0.0 0.1) #@(|3| 0.1 0.1 0.1) #@(|3| 0.0 0.1 0.1) #@(|3| 0.0 0.0 0.1)
            #@(|3| 0.0 0.0 0.0))
          (lambda (render)
            (path-effect :paint) (path-constituents :surfaces) (fill-agent #@(|3| 0.5 0.0 0.0)) (funcall render)
            (path-constituents :lines) (stroke-agent #@(|3| 0.5 0.5 0.0)) (funcall render)))))

(defun render-test-cube ()
  (poly '(#@(|3| 0.0 0.0 0.0) #@(|3| 0.0 0.1 0.0) #@(|3| 0.1 0.1 0.0) #@(|3| 0.1 0.0 0.0)
          #@(|3| 0.1 0.0 0.1) #@(|3| 0.1 0.1 0.1) #@(|3| 0.0 0.1 0.1) #@(|3| 0.0 0.0 0.1)
          #@(|3| -0.5 -0.5 0.5) #@(|3| -0.5 0.5 0.5) #@(|3| 0.0 0.1 0.1)
          #@(|3| 0.1 0.1 0.1) #@(|3| 0.5 0.5 0.5) #@(|3| 0.5 -0.5 0.5) #@(|3| 0.1 0.0 0.1)
          #@(|3| 0.1 0.0 0.0) #@(|3| 0.5 -0.5 -0.5) #@(|3| 0.5 0.5 -0.5) #@(|3| 0.1 0.1 0.0)
          #@(|3| 0.0 0.1 0.0) #@(|3| -0.5 0.5 -0.5) #@(|3| -0.5 -0.5 -0.5) #@(|3| 0.0 0.0 0.0)
          #@(|3| 0.0 0.0 0.1))
        (lambda (render)
          (path-effect :paint) (path-constituents :surfaces) (fill-agent #@(|3| 0.5 0.0 0.0)) (funcall render)
          (path-constituents :lines) (stroke-agent #@(|3| 0.5 0.5 0.0)) (funcall render))))

(og::test og.projection.poly.2
  (progn
    (transform :projection :rotate 1.0 1.0 -0.2)
    (render-test-cube)))

(og::test og.projection.poly.3
  (let ((delta (degrees-to-radians 5))
        (base (degrees-to-radians 30)))
    (transform :projection :rotate base base base)
    (dotimes (x (floor (* (/ pi delta))))
      (clear-view)
      (render-test-cube) (flush-view )
      (transform :projection :rotate delta delta delta))))


(og::test og.projection.raster.0
  (progn
    (path-effect :paint)
    ;; test that the raster and other geometry transform to the same spots
    (line #@(|3| 1.0 -1.0 0.0) #@(|3| -1.0 1.0 0.0) '((stroke-agent #@(|3| 1.0 0.0 0.0))))
    (line #@(|3| -1.0 -1.0 0.0) #@(|3| 1.0 1.0 0.0) '((stroke-agent #@(|3| 0.0 1.0 0.0))))
    (raster #@(|3| 1.0 -1.0 0.0) #@(|3| -1.0 1.0 0.0) (test-raster) ())))

(og::test og.projection.raster.1
  (raster*2  -1.0 -1.0  0.0 0.0 (test-raster) ()))

(og::test og.projection.raster*2.1
  (raster*2 -0.5 -0.5 0.5 0.5 (test-raster) ()))

(og::test og.projection.raster*3.1
  (raster*3 -0.5 -0.25 -1.0 0.5 0.25 1.0 (test-raster) ()))


(og::test og.projection.rectangle.1
  (rectangle #@(|3| 0.0 0.0 0.0) #@(|3| 0.1 0.1 0.0) '((path-effect :paint)
                                                       (stroke-agent #@(|3| 0.5 0.0 0.0)))))

(og::test og.projection.rectangle*2.1
  (rectangle*2 0.0 0.0 0.2 0.1 '((path-effect :paint)
                                 (path-constituents :surfaces)
                                 (fill-agent #@(|3| 0.0 0.9 0.0)))))

(og::test og.projection.rectangle*3.1
  (rectangle*3 0.0 0.0 0.0  1.0 0.7 0.0 '((path-effect :paint)
                                          (path-constituents :lines)
                                          (stroke-agent #@(|3| 0.0 0.0 0.5)))))


(og::test og.projection.text.1
  (text  #@(|3| 0.0 0.0 0.0) "testing one" :times-bold-10 '((stroke-agent #@(|3| 1.0 0.0 0.0)))))

(og::test og.projection.text*2.1
  (text*2 0.2 0.5 "testing two" :times-plain-10 '((stroke-agent #@(|3| 0.0 1.0 0.0)))))

(og::test og.projection.text*3.1
  (text*3 0.5 -0.5 0.0 "testing three" :times-bold-10 '((stroke-agent #@(|3| 0.0 0.0 1.0)))))


(og::test og.projection.color*3.1
  (fill-view (color*3 0.1 0.0 0.2)))

(og::test og.projection.color*4.1
  (fill-view (color*4  0.0 0.2 0.3 0.1)))

(og::test og.projection.clear-view.1
  (clear-view))

(og::test og.projection.fill-view.1
  (fill-view  #@(|3| 0.0 0.1 0.2)))


(og::test og.projection.transform.1
  "test projection transforms - relative and assertion.
   should produce a multi-colored 'X'."
  (progn
    (line*3 0.1 0.1 0.0 0.0 0.0 0.0 '((stroke-agent #@(|3| 1.0 0.0 0.0))))
    (transform :projection :clear :scale (location-*  #@(|3| -1.0 1.0 1.0) *test-projection-scale*))
    (line*3 0.1 0.1 0.0 0.0 0.0 0.0 '((stroke-agent #@(|3| 0.0 1.0 0.0))))
    (transform :projection :scale (location-*  #@(|3| 1.0 -1.0 1.0) *test-projection-scale*))
    (line*3 0.1 0.1 0.0 0.0 0.0 0.0 '((stroke-agent #@(|3| 0.0 0.0 1.0))))
    (with-matrices ((lower-right 1.0 0.0 0.0 0.0
                                   0.0 -1.0 0.0 0.0
                                   0.0 0.0 1.0 0.0
                                   0.0 0.0 0.0 1.0))
      (transform :projection :set lower-right)
      (line*3 0.1 0.1 0.0 0.0 0.0 0.0 '((stroke-agent #@(|3| 1.0 1.0 1.0)))))
    t))

;;; there is no general test for this, as some systems implement a viewport transform
;;; based on rectangle coordinates in the window-system's window and some based
;;; on a transform. in the former case, the projection layer computes the effective
;;; port rectangles coordinate by transforming [#@(-1 -1) #@(1 1)] and then normalizes to
;;; #@(0 0), but that's not correct

#+until-viewports-work-for-opengl
(og::test og/projection/transform/2
  "test view transforms - relative and assertion.
   should produce a multi-colored 'X'."
  (progn
    (line*3 -1.0 -1.0 1.0 1.0 1.0 -1.0 '((stroke-agent #@(|4| 1.0 0.0 0.0 1.0))))
    (line*3 1.0 -1.0 1.0 -1.0 1.0 -1.0 '((stroke-agent #@(|4| 1.0 0.0 0.0 1.0))))
    (transform :view :clear :scale #@(|4| .75 .75 1.0))
    (line*3 -1.0 -1.0 0.0 1.0 1.0 0.0 '((color-mode :blend) (stroke-agent #@(|4| 0.0 1.0 0.0 0.9))))
    (line*3 1.0 -1.0 0.0 -1.0 1.0 0.0 '((color-mode :blend) (stroke-agent #@(|4| 0.0 1.0 0.0 0.9))))

    (transform :view :clear :scale #@(|3| 0.5 0.5 0.0 0.0) :translate #@(|2| 0.0 08.0))
    (line*3 -1.0 -1.0 0.0 1.0 1.0 0.0 '((stroke-agent #@(|3| 0.0 0.0 1.0))))
    (line*3 1.0 -1.0 0.0 -1.0 1.0 0.0 '((stroke-agent #@(|3| 0.0 0.0 1.0))))
    (with-matrices ((lower-right .5 0.0 0.0 0.0
                                 0.0 .5 0.0 0.0
                                 0.0 0.0 0.0 0.0
                                 128.0 0.0 0.0 0.0))
      (transform :view :set lower-right)
      (line*3 -1.0 -1.0 0.0 1.0 1.0 0.0 '((stroke-agent #@(|3| 0.0 0.0 0.0))))
      (line*3 1.0 -1.0 0.0 -1.0 1.0 0.0 '((stroke-agent #@(|3| 0.0 0.0 0.0)))))
    nil)
  nil)



;;;
;;; various combined tests

(defun test-color ()
  (fill-view #@(|3| 1.0d0 0.0d0 0.0d0))
  (line*2 1.0 1.0 -1.0 -1.0 '((stroke-agent #@(|3| 0.0 1.0 0.0))))
  (line*2 1.0 -1.0 -1.0 1.0 '((stroke-agent #@(|3| 0.0d0 0.0d0 1.0d0))))
  t
  )

(og::test og.projection.test-color.1
  (test-color))


(defun test-translated-rectangles (&key (count 10))
  (let ((colors (make-array count)))
    (dotimes (x count)
      (let ((value (/ x (float count 1.0))))
        (setf (aref colors x) (list 'color*3 value value value))))
    (with-projection-variables
      (lambda ()
        (path-constituents :surfaces))
      (dotimes (i count)
        (let* ((center (- (* i .1) 0.45))
               (min (- center 0.5))
               (max (+ center 0.5)))
              
          (with-projection-variables
            (lambda () )
            (fill-agent (aref colors i))
            (rectangle*2 min min max max))))))
  t)

(og::test og.projection.translated-rectangles-test.1
  (test-translated-rectangles))


(defun test-projection-variables ()
  (rectangle*3 -0.9d0 -0.9d0 0.0d0 0.9d0 0.9d0 0.0d0
               (lambda (geometry)
                 (path-constituents :surfaces)
                 ;; (color-mode :blend :src-alpha :one-minus-src-alpha)
                 (fill-agent 1.0 1.0 1.0 1.0)
                 (funcall geometry)
                 (color-mode :opaque)
                 (stroke-agent 1.0 0.0 0.0 1.0)
                 (path-constituents :lines)
                 (funcall geometry)
                 ))
  t)

(og::test og.projection.projection-variables-test.1
  (test-projection-variables))


(defun test-simple-transformation ()
  (let* ((from (make-location-world :x 10.0d0 :y 10.0d0))
         (to (make-location-world :x 200.0d0 :y 100.0d0))
         (radius*3 (location-/ (location-- to from)
                               (location-3 2.0 2.0 2.0 1.0)))
         (count 10)
         (radius (location-magnitude radius*3))
         (delta (make-location-world :x (/ radius count)
                                     :y (/ radius count)))
         (-delta (location-* delta #@(|2| 1.0 -1.0))))
    (stroke-agent 1.0 1.0 1.0)
    (path-effect :paint)
    (transform :projection :clear
               :translate #@(|3| -1.0 -.5 0.0)
               :scale #@(|3| .01 .01 .01))
    ;; (raster (location-+ from delta) (location-- to from) *test-raster*)
    (line from to '((fill-agent 1.0 0.0 0.0)))
    (stroke-agent 1.0 0.0 0.0)
    (rectangle from to '((fill-agent 0.0 1.0 0.0)
                         (path-constituents :lines)))
    (stroke-agent 0.0 1.0 0.0)
    (arc (location-/ (location-+ from to) (location-3 2.0 2.0 2.0 1.0))
         radius 0.0 pi :clockwise)
    (let* ((l (location-world (- (/ (+ (location-x from) (location-x to)) 2)
                                 radius)
                              (/ (+ (location-y from) (location-y to)) 2)))
           (l* (list l)))
      (dotimes (x count)
        (push (setf l (location-+ l delta)) l*)
        (push (setf l (location-+ l -delta)) l*))
      (poly l* '((path-constituents :lines) (stroke-agent 0.0 0.0 1.0))))
    t))


(og::test og.projection.test-simple-transformation.1
  (test-simple-transformation))


#+ignore
(og::test og.projection.drawing-modes.1
  (test-drawing-modes))

(defun test-lines (&key (count (minimum-test-count)) (flush-p t))
  (stroke-agent (random 1.0) (random 1.0) (random 1.0) .5s0)
  (transform :projection :scale .01 .01 .01)
  (dotimes (x count)
    (line*3 -90.0 -90.0 0.0 90.0 90.0 0.0)
    (when flush-p (flush-view)))
  t)

(og::test og.projection.test-lines.flushed
  (test-lines))

(og::test og.projection.test-lines.piped
  (test-lines :count 1000 :flush-p nil))


(defun test-lines-random (&key (count (minimum-test-count)) (flush-p t))
  (dotimes (i count)
    (dotimes (j count)
      (let ((x (random 1.0))
            (y (random 1.0))
            (z (random 1.0d0)))
        (stroke-agent x y z 1.0)
        (line*3 (- (/ i count) 1.0) (- (/ j count) 1.0) z
                (- (* x 2.0) 1.0)  (- (* y 2.0) 1.0) z)
        (when flush-p (flush-view))))
    ;; always flush at least once pre pass - 10.4 otherwise shows the end result only
    (unless flush-p (flush-view)))
  t)


(og::test og.projection.run-life.1
  (run-life :cycles (minimum-test-count) :sleep nil :initialize-p nil))


(defun test-sampler (&key (initialize-p t))
  (when (or (null *life-raster*) initialize-p)
    (initialize-life-raster))
  
  (sampler :break-p nil
           :clear-p #'(lambda ()
                        (stroke-agent 1.0 1.0 1.0)
                        (raster*2 -0.9 -0.9 0.9 0.9 *life-raster*)
                        ;; (line-pattern 1 #b1010101010101010)
                        (rectangle*2 -0.9 -0.9 0.9 0.9)))
  t)

(og::test og.projection.sampler.1
  (test-sampler))


(defun test-sampler-animation (&key (count (minimum-test-count)) (sleep *test-sleep*) (initialize-p t)
                                    (verbose-p nil))
  "run the sampler function with an iteratively rotating coordinate system.
   keyword parameters:
    count: specifies iterations
    sleep: specifies how long to sleep between iterations, or nil continuous."
  (when (or (null *life-raster*) initialize-p)
    (initialize-life-raster))
  (dotimes (i (or count most-positive-fixnum))
    (with-projection-variables
      (lambda ()
        (transform :projection :rotate (* i .1) 0.0 0.0))
      (when verbose-p (format *trace-output* "~d " i))
      (sampler :clear-p #'(lambda ()
                            (clear-view)
                            (stroke-agent 1.0 1.0 1.0)
                            (raster*2 -0.9 -0.9 0.9 0.9 *life-raster*)
                            (rectangle*2 -0.9 -0.9 0.9 0.9)))
      (flush-view)
      (next-generation *life-raster*)
      (when sleep (sleep sleep))))
  t)

(og::test og.projection.sampler.2
  (test-sampler-animation))


(og::test (og.version :clear-p nil)
  (text*2 -0.98d0 -0.98d0 (format nil "~a~%~a~%~a"
                              (lisp-implementation-type)
                              (lisp-implementation-version)
                              (type-of *projection-context*))
          :times-plain-10
          '((stroke-agent "fuchsia"))))


(find-rgba :fuchsia)
;;;
;;; pipelined v/s immediate performance
#+(or )
(progn

  (with-projection-context ((test-context)) 
    (with-monitoring-report (:stream #p"home:og-line-no-flush.html")
      (dsu:time-and-memory
       (test:execute-test  (test-lines-random :count 1000 :flush-p nil)))))
  
  (with-projection-context ((test-context)) 
    (with-monitoring-report (:stream #p"home:tmp;og-line-flush.html")
      ;; use time-and-memory to make it comparable with library-specific results
      (dsu:time-and-memory 
       (test-lines-random :count 1000 :flush-p t)))))
  



#|
(defun compute-view-position (position
                              &key (screen-size #@(1024 768))
                              (view-size *test-view-size*))
  (case position
    (:fixed *initial-view-position*)
    (:tiled (let ((screen-width (point-h screen-size))
                  (screen-height (point-v screen-size))
                  (x (point-h *next-view-position*))
                  (y (point-v *next-view-position*)))
              (cond ((< (+ x (point-h view-size)) (* screen-width .67))
                     (setf position *next-view-position*))
                    (t
                     (setf x (point-h *initial-view-position*)
                           y (+ y *max-view-height*)
                           *max-view-height* 0)
                     (cond ((< (+ y (point-v view-size)) (* screen-height .67))
                            (setf position (make-point x y)))
                           (t
                            (setf y (point-v *initial-view-position*))
                            (setf position (make-point x y))))))
              (setf *next-view-position*
                    (make-point (+ x (point-h view-size)) y))
              (setf *max-view-height*
                (max *max-view-height* (point-v view-size)))
              
              position))
    (t
     (etypecase position
       (integer position)))))


(let ((pathname (make-pathname :host "home"
                               :name (multiple-value-bind (sec min hour day month year) (decode-universal-time (get-universal-time))
                                       (format nil "functions-~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d" year month day hour min sec))
                               :type "dot"))
      (count nil)
      (de.setf.utility.implementation::*function-walk-depth-limit* 4))
  (setf count (de.setf.utility.implementation::graph-functions :stream pathname :function 'line*3
                             :packages `(:og.impl :og)
                             ;; :packages (list-all-packages)
                             :depth-limit 4
                             :options '(:qualifiers (de.setf.utility.implementation::calls de.setf.utility.implementation::relations de.setf.utility.implementation::other))))
  (ccl:set-mac-file-creator pathname (intern (make-string 4 :initial-element #\null) :keyword))
  (setf pathname (namestring (truename pathname)))
  (setf pathname (subseq pathname (1+ (position #\: pathname))))
  (bsd:system-command (print (format nil  "open  '/~a'" (substitute #\/ #\: pathname))))
  count)


|#
