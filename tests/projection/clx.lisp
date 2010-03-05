;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)

(document :file
 (description "This file defines test for the CLX interface for the 'de.setf.graphics' library.")
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
  (delta 20030918 ))

 (long-description "The `DSG:tests;projection;abstract-projection.lisp` file defines abstract tests for the
 scene geometry elements. This file prepares the context necessary to invoke those tests against a CLX
 device interface."))


(defvar *clx-c* nil)


;;; the error "TCP-UNKNOWN-DOMAIN-NAME: Unknown host name. A bad endpoint name was supplied."
;;; means the host is not set
;;; the error "X-Error: Connection failure to X11.0 server 127.0.0.1 display 0: No protocol specified"
;;; means authorization information is required. either the .Xauthority file was not found,
;;; or no entry from it matched the specified host.
;;; nb. under osx, the server generates a files which does not support local host over tcp
;;; the entry night have been intended to be
;;;   localhost:0  MIT-MAGIC-COOKIE-1  ce437f51a50bbf13881d651f15ce3470
;;; which xlib::get-best-authorization matches to neither a "localhost" nor a "127.0.0.1" display host,
;;; as, in those cases, it  coerces the protocol to :local, despite that the entry protocol is :internet
;;; either use a proper host name, or disable display authentication.
;;;
;;; mcl defines #P"home:" to be the start-up directory, ofter the same as #P"ccl:". in this case,
;;; set the XAUTHORITY environment variable or set a link from #4P"home:.Xauthority" to "~/.Xauthority".
;;; in the latter case, xlib::homedir-file-pathname may need a patch to recognize osx.

;;; for my 'localhost', either or
(setq *clx-display-host* "yoda.setf.de")
(setq *clx-display-host* "192.168.1.25")
;;; or disable authorization
;;; xhost +192.168.1.25


;;; from clx-demos.lisp
#+(or )                                 ; to reset/restart
(progn
  (ignore-errors (xlib:close-display *clx-default-display*))
  (setq *clx-default-display* nil)
  (setq *clx-c* nil))

#+(or )                                 ; to start
(initialize-clx-tests)

(defun initialize-clx-tests ()
  (unless *clx-default-display*
    #+:cmu
    (setq *clx-default-display* (ext:open-clx-display))
    #+(or sbcl openmcl)
    (setf *clx-default-display* (xlib::open-default-display))
    #-(or cmu sbcl openmcl)
    ;; Portable method
    (setq *clx-default-display* (xlib:open-display *clx-display-host*
                                                   :display *clx-display-number*
                                                   :protocol :internet))
    )
  
  ;; no (setf (context-view *clx-c*) *clc-w*)
  (setq *clx-c* (make-instance 'clx-context :display *clx-default-display*
                               :view-size *test-view-size*))
  (setq *test-context* *clx-c*)
  )


(defmethod initialize-test-context ((context clx-context) &key &allow-other-keys)
  (let ((size (context-size context)))
    (call-next-method)
    (transform :view
               :translate (/ (point-h size) 2) (/ (point-v size) 2) 0.0
               :scale (/ (point-h size) 2) (/ (point-v size) -2) 0.0)
    (context-log-message context 'initialize-test-state "~W" (context-state context))
    ))


(defun clx-test-context ()
  (or *clx-c*
      (initialize-clx-tests)))


(defmacro og::test-clx (name form &rest args)
  (let ((render-args (when (consp name) (rest name)))
         (name (if (consp name) (first name) name))
         (docstring nil))
    (when (stringp form)
      (setf docstring form
            form (pop args)))
    `(og::test (,name :context (clx-test-context) ,@render-args)
       ,@(when docstring (list docstring))
       ,form
       ,@args)))



;;; just to check
#+mcl
(test:test og.clx.endian
  (ccl:rlet ((data :rect))
    (setf (ccl:%get-long data) #x01020304)
    (ecase (+ (* (ccl:%get-byte data 0) 1000)
              (* (ccl:%get-byte data 1) 100)
              (* (ccl:%get-byte data 2) 10)
              (ccl:%get-byte data 3))
      (1234 (not (find :clx-little-endian *features*)))
      (4321 (find :clx-little-endian *features*)))))
;; (test:execute-test :og.clx.endian)


(og::test-clx og.clx.ffi
  "test direct xlib interface operators"
  (progn
    (assert (typep *projection-context* 'clx-context))
    (let* ((context *projection-context*)
           (view (context-view context))
           (size (context-size context))
           (width (point-h size))
           (height (point-v size)))
      ;; (print (context-state))
      (xlib:clear-area view)
      ;; (sleep 1)
      (xlib:draw-rectangle view *clx-gcontext*
                           4 4 (- width 8) (- height 8)
                           t)
      (setf (xlib:gcontext-foreground *clx-gcontext*)
            (nth-value 1 (clx-fill-agent*3 0.0 1.0 1.0)))
      (xlib:draw-line view *clx-gcontext* 0 (point-v size) (point-h size) 0 nil)
      (setf (xlib:gcontext-foreground *clx-gcontext*)
            (nth-value 1 (clx-fill-agent*3 1.0 0.0 1.0)))
      (xlib:draw-line view *clx-gcontext* 0 0 (point-h size) (point-v size) nil)
      (xlib:display-force-output *clx-default-display*)
      (let* ((1w (floor (/ width 8)))
             (1h (floor (/ height 8))))
        (setf (xlib:gcontext-foreground *clx-gcontext*)
              (nth-value 1 (clx-fill-agent*3 1.0 0.0 0.0)))
        ;; clockwise
        (xlib:draw-lines view *clx-gcontext*
                         (list 1w (- height 1h)
                               1w 1h
                               (- width 1w) 1h
                               (- width 1w) (- height 1h))
                         :fill-p t)
        (setf (xlib:gcontext-foreground *clx-gcontext*)
              (nth-value 1 (clx-fill-agent*3 0.0 1.0 0.0)))
        ;; counter-clockwise
        (let ((1w (* 1w 2))
              (1h (* 1h 2)))
          (xlib:draw-lines view *clx-gcontext*
                           (list 1w (- height 1h)
                                 (- width 1w) (- height 1h)
                                 (- width 1w) 1h
                                 1w 1h)
                           :fill-p t))
        
        (setf (xlib:gcontext-foreground *clx-gcontext*)
              (nth-value 1 (clx-fill-agent*3 0.0 0.0 1.0)))
        ;; clockwise frame - needs an extra point
        (let ((1w (* 1w 3))
              (1h (* 1h 3)))
        (xlib:draw-lines view *clx-gcontext*
                         (list 1w (- height 1h)
                                 1w 1h
                                 (- width 1w) 1h
                                 (- width 1w) (- height 1h)
                                 1w (- height 1h))
                         :fill-p nil))
        ;; (print (context-state))
        t))))
;; (test:execute-test :og.clx.ffi)


(og::test-clx og.clx.context.size
  "check the view size accessors"
  (let ((width (xlib:drawable-width *context-view*))
        (height (xlib:drawable-height *context-view*))
        (size (context-size *projection-context*)))
    (and (eql width (point-h size))
         (eql height (point-v size)))))
           

(og::test-clx og.clx.context.line
  "draw progressive lines over the top-left quadrangle"
  (progn
    ;; reset the transforms to reinstate port coordinates 
    (transform :projection :clear :view :clear)
    (let* ((size (context-size *projection-context*))
           (width (point-h size))
           (height (point-v size))
           (c1 (/ width 2))
           (c2 height)
           (from (make-location-world :x 0.0d0 :y 0.0d0))
           (to (make-location-world :x c1 :y c2)))
      (line from to)
      (setf (location-x to) c2 (location-y to) c1)
      (line from to)
      (flush-view)
      (sleep *test-sleep*)
      (dotimes (i (floor c1) c1)
        (line*2 0 0 i c1)
        (line*2 0 0 c1 i)
        ;; let it appear sort of incrementally
        (flush-view)))))
;; (test:execute-test :og.clx.context.line :break-on-signals t)


(og::test-clx og.clx.context.rectangle
  "draw progressive rectangles over the top-left quadrangle"
  (progn
    ;; reset the transforms to reinstate port coordinates 
    (transform :projection :clear :view :clear)
    (let* ((size (context-size *projection-context*))
           (width (point-h size))
           (height (point-v size))
           (c1 (/ width 2))
           (c2 height)
           (from (make-location-world :x 0.0d0 :y 0.0d0))
           (to (make-location-world :x c1 :y c2)))
      (line from to)
      (setf (location-x to) c2 (location-y to) c1)
      (line from to)
      (flush-view)
      (sleep *test-sleep*)
      (dotimes (i (floor c1) c1)
        (rectangle*2 0 0 i c1 nil)
        (rectangle*2 0 0 c1 i nil)
        ;; let it appear sort of incrementally
        (flush-view)))))
;; (test:execute-test :og.clx.context.rectangle)


;;; run all tests for clx and those from abstract-projection
;;;
;;; (test:find-tests :og.clx.**)

(defun execute-graphics-clx-tests ()
  (clx-test-context)                    ; ensure there is one
  (test:execute-test :og.clx.**)
  (with-projection-context (*clx-c*) (test:execute-test :og.projection.**))
  (with-projection-context (*clx-c*)
    (test:execute-test :og.projection.sampler.1)
    (test:execute-test :og.version)))

;;; iff there already is one

(when (typep *clx-c* 'clx-context)
  (execute-graphics-clx-tests))

;; (setf (context-log *clx-c*) nil) ;;  *trace-output*)
;; (with-projection-context (*clx-c*) (test:execute-test :og.projection.poly.3 :break-on-signals t))
;; (with-projection-context (*clx-c*) (xlib:screen-root-depth *clx-screen*))
;; (with-projection-context (*clx-c*) (xlib:drawable-depth (context-view *clx-c*)))
;; (with-projection-context (*clx-c*) (test-color))
;; (with-projection-context (*clx-c*) (test:execute-test :og.projection.translated-rectangles-test.1))
;; (with-projection-context (*clx-c*) (test:execute-test :og.projection.sampler.1))
;; (with-projection-context (*clx-c*) (let ((*test-sleep* 0.05)) (test:execute-test :og.projection.sampler.2)))
;; (time (with-projection-context (*clx-c*) (initialize-test-context *clx-c*) (test-sampler-animation :count 100 :sleep nil)))
;; (with-projection-context (*clx-c*) (initialize-test-context *clx-c*) (run-life :cycles 256 :sleep nil :initialize-p t :size 512))


