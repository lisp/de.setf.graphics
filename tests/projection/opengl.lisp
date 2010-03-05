;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)

(document :file
 (description "This file defines tests for the OpenGL interface for the 'de.setf.graphics' library.")
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
  (copyright 2003 "[james anderson](mailto:james.anderson@setf.de)")
  (delta 20030921 ))

 (long-description "The `DSG:tests;projection;abstract-projection.lisp` file defines abstract tests for the
 scene geometry elements. This file prepares the context necessary to invoke those tests against an OpenGL
 device interface."))


(defparameter *gl-w*  nil)
(defparameter *gl-c* nil)

#+( or)                                 ; to start
(initialize-opengl-tests)


(defun initialize-opengl-tests ()
  (setq *gl-c* (make-instance 'opengl-context))
  (setf (context-get *gl-c* :window-title) "OpenGL"
        (context-get *gl-c* :view-size) *test-view-size*
        (context-get *gl-c* :view-position) (make-point 64 64)
        )
  
  (setq *gl-w* (make-instance *class.projection-context-window*
                 :window-title "OpenGL"
                 :view-size #@(256 256)
                 :view-position #@(100 100)
                 :window-layer 1))
  
  
  (setf (context-view *gl-c*) *gl-w*)
  (setf (view-context *gl-w*) *gl-c*)
  
  (define-font *gl-c* '("Times" :plain 10) :times-plain-10)
  (define-font *gl-c* '("Courier" :plain 10) :courier-plain-10)
  (define-font *gl-c* '("Times" :bold 10) :times-bold-10)
  *gl-c*)


(defmethod initialize-test-context ((context opengl-context) &key &allow-other-keys)
  (call-next-method)
  (context-log-message context 'initialize-test-state "~W" (context-state context)))


(defun opengl-test-context ()
  (or *gl-c*
      (initialize-opengl-tests)))



(defmacro og::test-opengl (name form &rest args)
  (let ((render-args (when (consp name) (rest name)))
         (name (if (consp name) (first name) name))
         (docstring nil))
    (when (stringp form)
      (setf docstring form
            form (pop args)))
    `(og::test (,name :context (opengl-test-context) ,@render-args)
       ,@(when docstring (list docstring))
       ,form
       ,@args)))

;;; direct ffi example from the opengl documentation

(og::test-opengl og.opengl.ffi
  (progn
    (assert (typep *gl-c* 'opengl-context))
    ;; (print (context-state))
    (#_glClearColor 0.0s0 0.0s0 0.0s0 0.0s0)
    (#_glClear GL:GL_COLOR_BUFFER_BIT)
    (#_glOrtho -1.0 1.0 -1.0 1.0 -1.0 1.0)
    (print (context-state))
    ;; clockwise fill
    (#_glColor3f 1.0s0 0.0s0 0.0s0)
    (#_GLPolygonMode GL:GL_FRONT GL:GL_FILL)
    (#_GLPolygonMode GL:GL_BACK GL:GL_POINT)
    (#_glBegin GL:GL_POLYGON)
    (#_glVertex2f -0.75s0 -0.75s0)
    (#_glVertex2f 0.75s0 -0.75s0)
    (#_glVertex2f 0.75s0  0.75s0)
    (#_glVertex2f -0.75s0 0.75s0)
    (#_glEnd)
    ;; counter-clockwise points
    (#_glColor3f 0.0s0 1.0s0 0.0s0)
    (#_glBegin GL:GL_POLYGON)
    (#_glVertex2f -0.5s0 -0.5s0)
    (#_glVertex2f -0.5s0 0.5s0)
    (#_glVertex2f 0.5s0  0.5s0)
    (#_glVertex2f 0.5s0 -0.5s0)
    (#_glEnd)
    ;; clockwise frame
    (#_glColor3f 0.0s0 0.0s0 1.0s0)
    (#_GLPolygonMode GL:GL_FRONT GL:GL_LINES)
    (#_glBegin GL:GL_POLYGON)
    (#_glVertex2f -0.25s0 -0.25s0)
    (#_glVertex2f 0.25s0 -0.25s0)
    (#_glVertex2f 0.25s0  0.25s0)
    (#_glVertex2f -0.25s0 0.25s0)
    (#_glEnd)
    (#_glFlush)
    t))
;; (test:execute-test :og.opengl.ffi)


;;;
;;; regression tests

;;; simple

(og::test-opengl og.opengl.context.elements.1
  (progn
    ;; w/o initialization - check what constituents are
    (line  #@(|3| -0.9 -0.9 0.0) #@(|3| 0.9 0.9 0.0) '((stroke-agent 1.0s0 0.0s0 0.0s0 .1s0)
                                                       (fill-agent 0.0s0 1.0s0 0.0s0 .1s0)
                                                       (clear-agent 0.0s0 0.0s0 1.0s0 .1s0)))
    (rectangle  #@(|3| -0.9 -0.9 0.0) #@(|3| 0.9 0.9 0.0) '((stroke-agent 0.0s0 1.0s0 0.0s0 .1s0)
                                                            (fill-agent 0.0s0 0.0s0 1.0s0 .1s0)
                                                            (clear-agent 1.0s0 0.0s0 0.0s0 .1s0)))
    (circle  #@(|3| 0.0 0.0 0.0) 0.9 '((stroke-agent 0.0s0 0.0s0 1.0s0 .1s0)
                                       (fill-agent 1.0s0 0.0s0 0.0s0 .1s0)
                                       (clear-agent 0.0s0 1.0s0 0.0s0 .1s0)))
    
    t))
;; (test:execute-test :og.opengl.context.elements.1)


(og::test-opengl og.opengl.context.elements.2
  (progn
    (line  #@(|3| -0.9 -0.9 0.0) #@(|3| 0.9 0.9 0.0) '((stroke-agent 1.0s0 0.0s0 0.0s0 .1s0)))
    (line  #@(|3| -0.9 0.9 0.0) #@(|3| 0.9 -0.9 0.0) '((stroke-agent #@(|3| 0 1 0))))
    (arc  #@(|3| 0.0 0.0 0.0) .3 #.pi 0.0 :clockwise '((stroke-agent #@(|3| 0.0 1.0 0.0))))
    (arc  #@(|3| 0.0 0.0 0.0) .3 0.0 #.pi :clockwise '((stroke-agent #@(|3| 1.0 0.0 0.0))))
    (arc  #@(|3| 0.0 0.0 0.0) .2 #.pi 0.0 :clockwise '((path-effect :clear)
                                                       (path-constituents :surfaces)))
    (arc  #@(|3| 0.0 0.0 0.0) .2 0.0 #.pi :clockwise '((stroke-agent #@(|3| 1.0 1.0 1.0))
                                                       (path-constituents :lines)
                                                       (path-effect :paint)))
    (arc  #@(|3| 0.0 0.0 0.0) .1 #.pi 0.0 :clockwise '((stroke-agent #@(|3| 0.0 0.0 1.0))
                                                       ))
    (arc  #@(|3| 0.0 0.0 0.0) .1 #.pi 0.0 :clockwise (lambda (geometry)
                                                       (fill-agent #@(|3| 1.0 0.0 1.0))
                                                       (path-constituents :surfaces)
                                                       (funcall geometry)
                                                       (stroke-agent #@(|3| 1.0 0.0 0.0))
                                                       (path-constituents :lines)
                                                       (funcall geometry)))
    t))
;; (test:execute-test :og.opengl.context.elements.2)



;;;
;;; timing comparison eliminating abstraction levels
;;; 926 : 842 : 839

;;; generic calls 


(defun gl-test-lines-random.generic (&key (count (minimum-test-count)) (flush-p t))
  (dotimes (i count)
    (dotimes (j count)
      (let ((x (random 1.0))
            (y (random 1.0))
            (z (random 1.0d0)))
        (stroke-agent x y z 1.0)
        ;; use gl-specifc but argument-generic interface
        (line*3 (- (/ i count) 1.0) (- (/ j count) 1.0) z
                (- (* x 2.0) 1.0)  (- (* y 2.0) 1.0) z)
        (when flush-p (opengl-flush-view))))
    ;; always flush at least once pre pass - 10.4 otherwise shows the end result only
    (unless flush-p (opengl-flush-view)))
  t)

(og::test-opengl og.opengl.context.lines-random.generic
  (with-projection-context (*gl-c*) (gl-test-lines-random.generic)))
;; (time (test:execute-test :og.opengl.context.lines-random.generic))


;;; strictly opengl calls

(defun gl-test-lines-random.opengl (&key (count (minimum-test-count)) (flush-p t))
  (dotimes (i count)
    (dotimes (j count)
      (let ((x (random 1.0))
            (y (random 1.0))
            (z (random 1.0d0)))
        (opengl-stroke-color*4 x y z 1.0)
        ;; use gl-specifc but argument-generic interface
        (opengl-line*3 (- (/ i count) 1.0) (- (/ j count) 1.0) z
                       (- (* x 2.0) 1.0)  (- (* y 2.0) 1.0) z)
        (when flush-p (opengl-flush-view))))
    ;; always flush at least once pre pass - 10.4 otherwise shows the end result only
    (unless flush-p (opengl-flush-view)))
  t)

(og::test-opengl og.opengl.context.lines-random.opengl
  (with-projection-context (*gl-c*) (gl-test-lines-random.opengl)))
;; (time (test:execute-test :og.opengl.contextlines-random.opengl))



;;; strictly opengl ff calls

(defun gl-test-lines-random.ffi (&key (count (minimum-test-count)) (flush-p t))
  (rlet ((%new-color :opengl-color4d :alpha 1.0d0))
    (dotimes (i count)
      (dotimes (j count)
        (let ((x (random 1.0d0))
              (y (random 1.0d0))
              (z (random 1.0d0)))
          (setf (rref %new-color opengl-color4d.red) x
                (rref %new-color opengl-color4d.green) y
                (rref %new-color opengl-color4d.blue) z)
          (#_GLColor4dv %new-color)
          (#_glBegin GL:GL_LINES)
          (#_glVertex3d (- (/ i count) 1.0) (- (/ j count) 1.0) z)
          (#_glVertex3d (- (* x 2.0) 1.0)  (- (* y 2.0) 1.0) z)
          (#_glEnd)
          )
        (when flush-p
          (#_GLFlush)
          (gl:aglSwapbuffers *opengl-glcontext*)
          ))
      (unless flush-p
        (#_GLFlush)
        (gl:aglSwapbuffers *opengl-glcontext*)
        )))
  t)

(og::test-opengl og.opengl.context.lines-random.ffi
  (with-projection-context (*gl-c*) (gl-test-lines-random.ffi)))
;; (time (test:execute-test :og.opengl.context.lines-random.ffi))






(defun gl-test-geometry ()
  (let* ((from (make-location-world :x 10.0d0 :y 10.0d0))
         (to (make-location-world :x 200.0d0 :y 100.0d0))
         (radius*3 (location-/ (location-- to from)
                               (location-3 2.0 2.0 2.0 1.0)))
         (count 10)
         (radius (location-magnitude radius*3))
         (delta (make-location-world :x (/ radius count)
                                     :y (/ radius count)))
         (-delta (location-* delta #@(|2| 1.0 -1.0)))
         (size (view-size (context-view *projection-context*))))
    (#_GLViewport 0 0 (point-h size) (point-v size))
    (transform :projection :translate -1.0 -1.0 0.0
               :scale (/ 2.0 (point-h size)) (/ 2.0 (point-v size)) 1.0)
    (clear-agent .75 .75 .75)
    (clear-view)
    (raster (location-+ from delta) (location-- to from) *test-raster*)
    (line from to '((stroke-agent 1.0 0.0 0.0)))
    (rectangle from to '((stroke-agent 0.0 1.0 0.0)
                         (path-effect :invert)))
    (path-constituents :lines)
    (arc (location-/ (location-+ from to) (location-3 2.0 2.0 2.0 1.0))
         radius
         0.0 pi :counterclockwise)
    (let* ((l (location-world (- (/ (+ (location-x from) (location-x to)) 2)
                                 radius)
                              (/ (+ (location-y from) (location-y to)) 2)))
           (l* (list l)))
      (dotimes (x count)
        (push (setf l (location-+ l delta)) l*)
        (push (setf l (location-+ l -delta)) l*))
      (poly l* '((stroke-agent 0.0s0 0.0s0 1.0s0) (path-constituents :lines)))))
  t)

(og::test-opengl og.opengl.context.geometry
  (with-projection-context (*gl-c*) (gl-test-geometry)))
;; (test:execute-test :og.opengl.context.geometry)

;;; run all tests for opengl and those from abstract-projection
;;;
;;; (test:find-tests :og.opengl.**)

(defun execute-graphics-opengl-tests ()
  (opengl-test-context)                 ; ensure that there is one
  (test:execute-test :og.opengl.**)
  (with-projection-context (*gl-c*) (test:execute-test :og.projection.**))
  (with-projection-context (*gl-c*)
    (test:execute-test :og.projection.sampler.1)
    (test:execute-test :og.version)))


;;; iff there already is one

(when (typep *gl-c* 'opengl-context)
  (execute-graphics-opengl-tests))


;; (with-projection-context (*gl-c*) (test:execute-test :og.projection.sampler.1 :break-on-signals t))


#|

;;;
;;; extended primitive tests

(defclass opengl-test-window (context-window)
  ((draw-function :initform nil :accessor window-draw-function)))

(defmethod ccl:view-draw-contents ((view opengl-test-window))
  (with-slots (draw-function) view
    (when draw-function (funcall draw-function view))))


(defun new-test-window (&key (view-size *test-view-size*)
                             (view-position #@(256 256))
                             (window-title "OpenGL")
                             (window-layer 1)
                             (window-show t)
                             (window-class opengl-test-window)
                             &aux w)
  (setf w (make-instance window-class :color-p t 
                         :view-size view-size
                         :view-position view-position
                         :window-title window-title
                         :window-layer window-layer
                         :window-show window-show))
  (ccl:process-allow-schedule)          ; let the default clear run
  w)

(defun test-gl-projection (function
                           &key (view-size *test-view-size*)
                           ((:window *gl-w*)
                            (or (when (and *gl-w* (ccl:wptr *gl-w*)) *gl-w*)
                                (setq *gl-w*
                                      (new-test-window :view-size view-size
                                                       :window-class 'opengl-test-window))))
                           (invalidate-p t)
                           (immediate-p t)
                           (stream *trace-output*))
  (format stream "~%projecting ~s -> ~s" *gl-c* *gl-w*)
  (cond (immediate-p
         (setf (context-view *gl-c*) *gl-w*)
         (call-with-projection-context function *gl-c*))
        (t
         (setf (window-draw-function *gl-w*)
               #'(lambda (view)
                   (setf (context-view *gl-c*) view)
                   (call-with-projection-context function *gl-c*)))
         (when invalidate-p (ccl:invalidate-view *gl-w*))))
  t)

(setq *display-function*
      #'(lambda (w &aux (time 0))
          w
          (when *display-time-p* (setf time (get-internal-run-time)))
          ; (glClearColor 0.0 0.0 0.0 1.0)
          (glClear (logior GL_COLOR_BUFFER_BIT gl_depth_buffer_bit))
          (let* ((color (list 'glcolor3d 0.0 0.0 0.0))
                 (color-properties (list color)))
            (dotimes (i 100)
              (let ((x (/ i 100.0)))
                (setf (second color) x)
                (line3 x 0.0 0.0 0.0 1.0 0.0 color-properties)))
            (setf (second color) 0.0)
            (dotimes (i 100)
              (let ((x (/ i 100.0)))
                (setf (third color) x)
                (line2 ( - x) 0.0 0.0 -1.0 color-properties)))
            
            ; rect2
            (let ((colors (make-array 10)))
              (dotimes (x 10)
                (setf (aref colors x)
                      (list (random 1.0) (random 1.0) (random 1.0))))
              (glpushattrib GL_POLYGON_BIT)
              (glpolygonMode GL_FRONT GL_FILL)
              (glpolygonMode GL_BACK GL_FILL)
              (glmatrixmode gl_modelview)
              (glpushmatrix)
              (gltranslated -1.0 0.0 0.0)
              (glscaled .1 .1 .1)
              (flet ((rect-props (glop)
                       (apply #'glcolor3d (aref colors (floor (random 10))))
                       (funcall glop)))
                (declare (dynamic-extent #'rect-props))
                (dotimes (x 10)
                  (dotimes (y 10)
                    (rect2 x y (1+ x) (1+ y) #'rect-props))))
              (glpopmatrix)
              (glpopattrib))
            (glpushattrib GL_POLYGON_BIT)
            (glpolygonMode GL_FRONT GL_LINE)
            (glpolygonMode GL_BACK GL_LINE)
            (dotimes (i 100)
              (let ((x (/ i 100.0)))
                (setf (fourth color) x)
                (rect2 (- x) (- x) (- (+ x .25)) (- (+ x .25)) color-properties)))
            (glpopattrib)

            ; rect3 blended and transformed
            (rect3 -1 -1 0 -2 0 0 `((glmatrixmode ,gl_modelview) (glpushmatrix)
                                    (glscaled .5 .5 .5) (gltranslated .5 -.5 0.0)
                                    (glpushattrib ,GL_POLYGON_BIT)
                                    (glpolygonMode ,GL_FRONT ,GL_FILL)
                                    (glpolygonMode ,GL_BACK ,GL_FILL)
                                    (glEnable ,GL_BLEND)
                                    (glBlendFunc ,GL_SRC_ALPHA ,GL_ONE_MINUS_SRC_ALPHA)
                                    (glColor4d .75 .75 .75 .5)
                                    (glop)
                                    (glDisable ,GL_BLEND)
                                    (glColor4d .25 .25 .25 1.0)
                                    (glpolygonMode ,GL_FRONT ,GL_LINE)
                                    (glpolygonMode ,GL_BACK ,GL_LINE)
                                    (glop)
                                    (glpopattrib)
                                    (glpopmatrix)))
            
            ;arc2
            (glpushattrib GL_POLYGON_BIT)
            (glpolygonMode GL_FRONT GL_LINE)
            (glpolygonMode GL_BACK GL_LINE)
            (dotimes (x 20)
              (arc2 .5 -.5 (/ .5 (1+ x))
                    pi (* 2 pi) `((glcolor3d .5 ,(+ .5 (/ .5 (1+ x))) .5))))
            (glpopattrib)

            ;arc3
            (glpushattrib GL_POLYGON_BIT)
            (glpolygonMode GL_FRONT GL_FILL)
            (glpolygonMode GL_BACK GL_FILL)
            (dotimes (x 20)
              (if (evenp x)
                (glcolor3d 1.0 1.0 1.0)
                (glcolor3d 0.0 0.0 0.0))
              (arc3 .5 (/ -.5 (1+ x)) 0.0 (/ .5 (1+ x))
                    (if (evenp x) 0.0 pi) (if (evenp x) pi (* 2 pi))))
            (glpopattrib)

            ;; polyline
            (let ((value (second (aref sin-values (1- (length sin-values))))))
              (map nil #'(lambda (v) (rotatef (second v) value))
                   sin-values))
            (glcolor3d 0.0 0.0 1.0)
            (glpushattrib GL_POLYGON_BIT)
            (glpolygonMode GL_FRONT GL_LINE)
            (glpolygonMode GL_BACK GL_LINE)
            (polyline2 sin-values)
            (glpopattrib)
            
            ; axes
            (glcolor3d .7 .7 .7)
            (line2 -1 0 1 0)
            (line2 0.0 -1.0 0.0 1.0)
            (do ((x -1.0 (+ x .1)))
                ((> x 1.0))
              (line2 x 0.0 x .1)
              (line2 0.0 x .1 x))
            ; text2
            (text2 0.0 -1.0 "(0.0,-1.0)" :times-bold-10)
            
            ; text3
            (text3 0.0 .5 -1.0 "(0.0,.5,-1.0)" :times-bold-10)
            
            ;; bounds
            (rect2 -1 -1 1 1 `((glpushattrib ,GL_POLYGON_BIT)
                               (glpolygonMode ,GL_FRONT ,GL_LINE)
                               (glpolygonMode ,GL_BACK ,GL_LINE)
                               (glEnable ,GL_LINE_STIPPLE)
                               (glLineStipple 1 #b1010101010101010)
                               (glop)
                               (glDisable ,GL_LINE_STIPPLE)
                               (glpopattrib)))

            (when *display-time-p*
              (format *trace-output* "~%display: ~s"
                      (- (get-internal-run-time) time)))
            )))


;;; ideosyncratics

(context-state *gl-c*)
(with-projection-context (*gl-c*) (time (opengl-clear)))
(with-projection-context (*gl-c*) (time (opengl-set-color *opengl-background-color*)))
(with-projection-context (*gl-c*) (time (opengl-set-color #.(location-vector 0.0 0.0 0.0 0.0))))
(with-projection-context (*gl-c*) (time (opengl-get-color *opengl-background-color*)))
(with-projection-context (*gl-c*) (rt::time-and-memory (opengl-fill-view *opengl-background-color*)))
(with-projection-context (*gl-c*) (gl:gldrawbuffer GL:GL_FRONT) (agl-error-check))
(with-projection-context (*gl-c*) (gl:gldrawbuffer GL:GL_BACK) (agl-error-check))
(with-projection-context (*gl-c*) (#_GLGetError))
(with-projection-context (*gl-c*)
  (let* ((color (rgba 0.0 0.0 0.0))
         (fill-properties `((path-constituents :surfaces) (fill-agent ,color))))
    (rectangle *bottom-left* *top-right* fill-properties)))

;;; specific things from abstract-projection, individually

(with-projection-context (*gl-c*) (test-translated-rectangles))
(with-projection-context (*gl-c*) (test-projection-variables))
(sample-projection *test-raster* *gl-c*)
(with-projection-context (*gl-c*) (raster #@(|3| -1.0 -1.0 0.0) #@(|3| 1.0 1.0 0.0) *test-raster* ()))
(context-state)

|#
