;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-


(in-package :de.setf.graphics.implementation)

(document :file
 (description "This file defines a sampler-test for the 'de.setf.graphics' library.")

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
  (delta 20030918 "for quickdraw"))

 (long-description
  "the results for 3-coordinate access were 3.94 - 1.35 - 1.6 for class - structure - list representation"))


(defParameter +sin-values+ (make-array 100))

(dotimes (x 100)
  (setf (aref +sin-values+ x) (list (/ x 99.0d0) (sin (* pi 4 (/ x 99))))))

(defParameter +colors+ (make-array 16))
(defParameter +levels+ (make-array 16))
(flet ((next-level () (aref +levels+ (random (length +levels+)))))
  (dotimes (x (length +levels+))
    (setf (aref +levels+ x) (random 0.67)))
  (dotimes (x (length +colors+))
    (setf (aref +colors+ x)
          (list (next-level) (next-level) (next-level)))))

(defparameter *bottom-left* (make-location-world :x -1.0 :y -1.0 :z 0.0))
(defparameter *top-right* (make-location-world :x 1.0 :y 1.0 :z 0.0))
(defparameter *origin* (make-location-world :x 0.0 :y 0.0 :z 0.0))

(defun sampler (&key (display-time-p nil)
                     (arc-p t)
                     (rectangle-p t)
                     (axes-p t)
                     (polyline-p t)
                     (line-p t)
                     (clear-p t)
                     (frame-p t)
                     (break-p nil))
  (let* ((time (get-internal-run-time))
         (color (rgba 0.0 0.0 0.0))
         (stroke-properties `((path-constituents :lines) (stroke-agent ,color)))
         (fill-properties `((path-constituents :surfaces) (fill-agent ,color)))
         (x0 0.0d0)
         (y0 0.0d0)
         (x1 0.0d0)
         (y1 0.0d0)
         (z0 1.0d0)
         (radius 0.0d0)
         (intensity 0.0d0)
         (intensity2 0.0d0))
    (declare (type double-float x0 y0 x1 y1 z0 radius intensity))
         
    (flet ((clear-color ()
             (setf (rgba-r color) 0.0d0 (rgba-g color) 0.0d0 (rgba-b color) 0.0d0)
             color)
           (set-red (intensity) (setf (rgba-r color) intensity) color)
           (set-green (intensity) (setf (rgba-g color) intensity) color)
           (set-blue (intensity) (setf (rgba-b color) intensity) color)
           (set-gray (intensity)
             (setf (rgba-r color) intensity)
             (setf (rgba-g color) intensity)
             (setf (rgba-b color) intensity)
             color))
      (declare (dynamic-extent #'clear-color #'set-red #'set-green #'set-blue #'set-gray))
      
      (typecase clear-p
        ((member t)
          ;; initially fill with (0,0,0)
         (clear-view)
         (rectangle *bottom-left* *top-right* fill-properties))
        (function (funcall clear-p)))
      (when break-p (flush-view) (break))
      
      (when line-p
        ;; sweep inner triangles
        (flet ((sweep (x0 y0 x1 y1 count update-color)
                 (declare (optimize (speed 3) (safety 0))
                          (type fixnum count)
                          (type double-float x0 y0 x1 y1))
                 (setf count (min count (length +sin-values+)))
                 (let ((percent 0.0d0)
                       (dx 0.0d0)
                       (dy 0.0d0)
                       (intensity 0.0d0))
                   (declare (type double-float intensity dx dy percent)
                            (dynamic-extent intensity dx dy percent))
                   (setf dx (/ (- x1 x0) count)
                         dy (/ (- y1 y0) count)
                         percent (/ 1.0d0 count))
                   (with-location-vectors ((xyz))
                     (dotimes (i count)
                       (setf (aref xyz 0) (+ x0 (* i dx))
                             (aref xyz 1) (+ y0 (* i dy))
                             (aref xyz 2) (second (aref +sin-values+ i))
                             intensity (* percent i))
                       (funcall update-color intensity)
                       (line *origin* xyz stroke-properties))))))
          (clear-color)
          (sweep 0.0d0 (location-y *top-right*) (location-x *top-right*) 0.0d0 30 #'set-red)
          (clear-color)
          (sweep (location-x *top-right*) 0.0d0 0.0d0 (location-y *bottom-left*) 45 #'set-green)
          (clear-color)
          (sweep 0.0d0 (location-y *bottom-left*) (location-x *bottom-left*) 0.0d0 67 #'set-blue)
          (clear-color)
          (sweep (location-x *bottom-left*) 0.0d0 0.0d0 (location-y *top-right*) 100 #'set-gray)))
      (when break-p (flush-view) (break))
     
      (when arc-p
        (with-projection-variables
          (lambda ()
            (path-constituents :lines)
            (transform :projection :scale  .5d0 .5d0 .5d0 :translate .5d0 -.5d0 0.0d0))
          ;arc2
          
          (dotimes (x 20)
            (setf radius (/ .5d0 (+ 1.0d0 (* .33 x))))
            (when (zerop x)
              (arc*2 0.0d0 0.0d0 radius pi +2pi+ :counterclockwise
                     `((inflect :surfaces :fill .67 .67 .67))))
            (setf intensity (- .5d0 (/ .5d0 (1+ x))))
            (set-red intensity)
            (set-green intensity)
            (set-blue intensity)
            (arc*3 0.0d0 0.0d0 (* x -.1d0) radius pi +2pi+ :counterclockwise
                   stroke-properties))
          ;arc3

          (dotimes (x 20)
            (when (zerop x)
              (arc*3 0d0 (/ 0d0 (1+ x)) 0.0d0 (/ .5d0 (1+ x))
                      0.0d0 pi
                     :counterclockwise
                     #'(lambda (arg)
                         (path-constituents :surfaces)
                         (fill-agent .75 .1 .1)
                         (funcall arg))))
            (arc*3 0.0d0 (/ -.5d0 (1+ x)) (* x .1d0) (/ .5d0 (1+ x))
                   (if (evenp x) 0.0d0 pi) (if (evenp x) pi +2pi+)
                   :counterclockwise
                   (if (evenp x)
                     #'(lambda (arc) (inflect :stroke .33 .33 .33) (funcall arc))
                     #'(lambda (arc)
                        (inflect :surfaces :fill .67 .67 .67)
                        (funcall arc) 
                        (inflect :lines :stroke .5d0 .5d0 .5d0)
                        (funcall arc)))))
          (arc*2 0.0d0 0.0d0 0.0d0 0.0d0 +2pi+ :counterclockwise
                 `((stroke-agent 0.0d0 0.0d0 1.0d0)))))
      (when break-p (flush-view) (break))
      
      (when rectangle-p
        (let ((width 0.0d0)
              (height 0.0d0)
              (delta-x 0.0d0)
              (delta-y 0.0d0)
              (count 10))
          (declare (type double-float width height delta-x delta-y) (type fixnum count)
                   (dynamic-extent width height delta-x delta-y))
          (locally (declare (ftype (function (location-3) double-float) location-x location-y))
            (setf width (- (location-x *top-right*) (location-x *bottom-left*))
                  height (- (location-y *top-right*) (location-y *bottom-left*))
                  delta-x (/ width count)
                  delta-y (/ height count))
            (with-projection-variables
              (lambda ()
                (inflect 'path-constituents :lines)
                (transform  :projection :translate *bottom-left* 
                           ;  :scale (/ width 5) (/ height 5) 0.0d0
                            ))
              (dotimes (i count)
                (setf intensity (/ i (* count 1.0d0))
                      intensity2 (- 1.0d0 intensity)
                      x0 (* i delta-x) ;; 1.0d0)
                      y0 (* i delta-y) ;; 1.0d0)
                      x1 (+ (* delta-x 2) x0)
                      y1 (+ (*  delta-y 2) y0))
                (set-red intensity2)
                (set-blue intensity)
                (rectangle*2 x0 y0 x1 y1 stroke-properties)))))
        
        (with-projection-variables
          (lambda ()
            (transform :projection :translate -1.0d0 0.0d0 0.0d0
                       :scale (/ 1.0d0 7.0d0) (/ 1.0d0 7.0d0) 0.0d0)
            (inflect 'path-constituents :surfaces 'stroke-agent 0.0d0 0.0d0 0.0d0))
          (dotimes (i 7)
            (setf x0 (* 1.0d0 i)
                  x1 (+ 1.0d0 x0))
            (dotimes (j 7)
              (setf y0 (* 1.0d0 j)
                    y1 (+ 1.0d0 y0))
              (let ((choice (+ (* i 7) j)))
                (setf z0 (second (aref +sin-values+ i)))
                (cond ((= 0 (random 7))
                       (path-constituents :lines)
                       (stroke-agent (aref +colors+ (logand #xf choice))))
                      (t
                       (path-constituents :surfaces)
                       (fill-agent (aref +colors+ (logand #xf choice))))))
              (rectangle*3 x0 y0 z0 x1 y1 z0))))
        
        (with-projection-variables (lambda () (transform :projection :scale 0.5d0 0.5d0 0.0d0))
          (rectangle *bottom-left* *top-right*
                     (if (projection-variable-p 'color-mode)
                       '((path-constituents :surfaces)
                         (path-effect :paint)
                         (stroke-agent .75s0 .75s0 .75s0 .5s0)
                         (color-mode :blend :src-alpha :one-minus-src-alpha))
                       '((path-constituents :surfaces)
                         (path-effect :invert)
                         (stroke-agent 1.0s0 1.0s0 1.0s0 .5d0))))))
      (when break-p (flush-view) (break))

      (when polyline-p
        (let ((value (second (aref +sin-values+ (1- (length +sin-values+))))))
          (map nil #'(lambda (v) (rotatef (second v) value))
               +sin-values+))
        (with-projection-variables (lambda () (stroke-agent (rgba 1.0d0 1.0d0 0.0d0))
                       (path-constituents :lines))
          (poly +sin-values+)))
      (when break-p (flush-view) (break))      
      
      (when axes-p
        (stroke-agent (rgba .7 .7 .7))
        (line*2 (location-x *bottom-left*) 0.0d0 (location-x *top-right*) 0.0d0)
        (line*2 0.0d0 (location-y *bottom-left*) 0.0d0 (location-y *top-right*))
        (do ((x (location-x *bottom-left*) (+ x .1)))
            ((> x (location-x *top-right*)))
          (line*2 x 0.0d0 x .1))
        (do ((y (location-y *bottom-left*) (+ y .1)))
            ((> y (location-y *top-right*)))
          (line*2 0.0d0 y .1 y))
        ; text
        (text*2 0.0d0 -1.0d0 "(0.0,-1.0d0)" :times-plain-10)
        (text*3 0.0d0 .5d0 -1.0d0 "(0.0,.5,-1.0d0)" :times-plain-10 )
        )
      (when break-p (flush-view) (break))

      
      (when frame-p
        (rectangle *bottom-left* *top-right*
                   `((path-constituents :lines)
                     ;(when (projection-variable-p 'line-pattern) (line-pattern 1 #b1010101010101010))
                     )))
      (when break-p (flush-view) (break))
      
      (when display-time-p
        (format *trace-output* "~%display: ~s"
                (- (get-internal-run-time) time)))
      t
      )))


(defun axes (&key (left -1.0d0) (right 1.0d0) (top 1.0d0) (bottom -1.0d0) (textp t) (color (rgba .7 .7 .7)))
  (stroke-agent color)
  (line*2 left 0.0d0 right 0.0d0)
  (line*2 0.0d0 bottom 0.0d0 top)
  (do ((x left (+ x (/ (- right left) 20))))
      ((> x right))
    (line*2 x 0.0d0 x .1))
  (do ((y bottom (+ y (/ (- top bottom) 20))))
      ((> y top))
    (line*2 0.0d0 y .1 y))
  (when textp
    ; text
    (text*2 0.0d0 -1.0d0 "(0.0,-1.0d0)" :times-plain-10)
    (text*3 0.0d0 .5d0 -1.0d0 "(0.0,.5,-1.0d0)" :times-plain-10 )))

:de.setf.graphics
