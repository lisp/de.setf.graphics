;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)


(document "Implement a concrete port projection context based on CLX for the de.setf.graphics library"

  (copyright "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved" "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  (history
   (delta 20060605 "janderson@ravenpack.com" "polygon vertex compression to moved port locations only.
   try splitting excess length sequences. use (- display-max-request-length 3) to limit")
   (delta 20060516 "janderson@ravenpack.com" "reconciled to quickdraw/opengl; corrected text tp assert
foreground pixel.")
   (delta 20030905 "james.anderson@setf.de" "rewritten for dynamic projection contexts. generic
functions are defined for only those operations which take typed location, and determine which transform
should be applied.")
   (delta 19940523 "jaa@dtmg" "took out direct toolbox calls and replaced them with methods in order to
allow specializations (eg. for backing store).")
   (delta 19891113 "jaa@dtmg" "ported to clx/pcl; added polygon modes to arc function.")
   (delta 19880625 "jaa@dtmg" "added coercion to point data in order to allow 4-d coordinate results
from coordinate-transform also added functions for pictures, arcs.")
   (delta 19880614 "jaa@dtmg" "as OG-graphics-standards, a preliminary version which does not take
account of fill, invert, ... in polygon mode. this layer takes atomic parameters, rather than
og-elements, since it is intended to be parallel to the lower level graphics with the addition of only
coordinate system transformations."))
  (long-description
   "Defines the concrete implementation for graphics projection operations in terms of CLX.

 Provides methods for generic graphics operations (see abstract-projection.lisp): rendering, display
 properties, state. specializes port-transform-context for coordinate system support. It immplements
 a per-server X event handler which dispatches to context-*-event-handler functions, which it
 activates if the run-time supports threads."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;
;;; parameters

(defvar *clx-display* nil
  "binds the current display within a with-projection-context.")
(defvar *clx-default-display* nil
  "binds the last opened display.")
(defvar *clx-display-host* "")
(defvar *clx-display-number* 0)
(defvar *clx-display-protocol* :tcp)
(defvar *clx-screen* nil)
(defvar *clx-root* nil)
(defvar *clx-gcontext* nil)

(defparameter *clx-black-pixel* 0)
(defparameter *clx-white-pixel* 0)
(defparameter *clx-background-color* (short-location-vector))
(defparameter *clx-fill-color* (short-location-vector))
(defparameter *clx-stroke-color* (short-location-vector))
(defparameter *clx-background-pixel* 0)
(defparameter *clx-fill-pixel* 0)
(defparameter *clx-stroke-pixel* 0)
(defparameter *clx-foreground-pixel* 0)

(defparameter *clx-path-constituents* :lines
  "default clx path constituents.
   used as the initial value when establishing a projection context.")

(defparameter *clx-path-effect* :paint
  "default clx path paint effect. used as the initial value when establishing a projection context.")

(defparameter *clx-path-rule* :winding
  "default clx path rule.
   used as the initial value when establishing a projection context.")

;;;
;;; the clx context class

(defclass clx-context (port-transform-context single-sided-projection-context)
  ((gcontext
    :initform nil :initarg :gcontext
    :accessor context-gcontext
    :documentation
    "caches the clx gcontext during dynamic wxtent of a single call-with-.")
   (display
    :initarg :display
    :initform (when (xlib-open-display-p *clx-default-display*)
                *clx-default-display*)
    :reader get-context-display
    :writer (setf context-display))
   (arc-port :initform 'clx-arc-port :allocation :instance)
   (line-port :initform 'clx-line-port :allocation :instance)
   (raster-port :initform 'clx-raster-port :allocation :instance)
   (rectangle-port :initform 'clx-rectangle-port :allocation :instance)
   (text-port :initform 'clx-text-port :allocation :instance)
   (default-fonts
     :initform '((:courier-bold-10   ("courier" 10 :bold))
                 (:courier-plain-10  ("courier" 10 :plain))
                 (:courier-bold-12   ("courier" 12 :bold))
                 (:courier-plain-12  ("courier" 12 :plain))
                 (:fixed             ("fixed" 10))
                 (:small             ("fixed" 8))
                 (:times-bold-10     ("times" 10 :bold))
                 (:times-plain-10    ("times" 10 :plain))
                 (:times-bold-12     ("times" 12 :bold))
                 (:times-plain-12    ("times" 12 :plain)))
     :allocation :class)
   (event-mask
    :initform '(:exposure :key-press :key-release :button-press :button-release)
    :initarg :event-mask
    :accessor context-event-mask
    :documentation
    "binds a list of event keys or an event mas as acceptable to
     xlib:create-window, which controls which events are reported
     for the context's window.")
   (uri-scheme
    :initform :clx :allocation :class))
  (:documentation
   "projection context for clx-based x window system operations."))

(def-initialize-clone clx-context
  (display))


(defclass clx-controller (projection-controller)
  ((process
    :initform nil
    :reader get-controller-process)
   (display
    :initform (error "display required.") :initarg :display
    :reader controller-display)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-in-clx-context (function context &key (view (context-view context)))
  (declare (dynamic-extent function))
  (let* ((*clx-display* (context-display context))
         (*clx-screen* (xlib:display-default-screen *clx-display*))
         (*clx-root* (xlib:screen-root *clx-screen*))
         (*clx-black-pixel* (xlib:screen-black-pixel *clx-screen*))
         (*clx-white-pixel* (xlib:screen-white-pixel *clx-screen*))
         (*clx-foreground-pixel* *clx-white-pixel*)
         (*clx-background-pixel* *clx-black-pixel*)
         (*context-path-constituents* *clx-path-constituents*)
         (*context-path-effect* *clx-path-effect*)
         (*context-path-rule* *clx-path-rule*)
         (*clx-path-rule* *clx-path-rule*)
         (*clx-gcontext* (xlib:create-gcontext :drawable view
                                                :background *clx-background-pixel*
                                                :foreground *clx-foreground-pixel*
                                                :font (get-font context :fixed))))
    (setf (xlib:window-background view)
          (ecase (context-background-mode context)
            (:opaque *clx-background-pixel*)
            (:transparent :none)))
    ;; (print (cons context (context-background-mode context)))
    (setf (context-gcontext context) *clx-gcontext*)
    (xlib:with-gcontext (*clx-gcontext* :foreground *clx-white-pixel*
                                        :background *clx-black-pixel*)
      (unwind-protect (funcall function)
        (xlib:display-force-output *clx-display*)))))

(defmethod call-with-projection-context
           ((function t) (context clx-context)
            &key (view (context-view context))
            &allow-other-keys)
  (flet ((do-next-method ()
           (call-next-method)))
    (declare (dynamic-extent #'do-next-method))
    (call-in-clx-context #'do-next-method context :view view)))


(defmethod initialize-instance :after ((context clx-context) &key)
  ;; set-up initial fonts
  (context-initialize-fonts context)
  ;; ensure an event process
  (context-controller context))

(defmethod context-controller ((context clx-context))
  (or (call-next-method)
      (set-context-controller (clx-context-controller (context-display context))
                              context)))

(defun clx-context-controller (display)
  (or (getf (xlib:display-plist display) 'projection-controller)
      (setf (getf (xlib:display-plist display) 'projection-controller)
            (make-instance 'clx-controller
              :display display))))
  

(defmethod initialize-instance :after ((instance clx-controller) &key)
  (setf (slot-value instance 'process)
        (make-clx-controller-process instance)))


(defvar *clx-display-processes* nil)

(defun make-clx-controller-process (controller)
  (declare (ignorable controller))
  #+(or ccl sb-threads)
  (de.setf.utility.lock:run-in-thread #'clx-controller-loop
                                      :name (format nil "CLX events [~a:~a]"
                                                    (xlib:display-host (controller-display controller))
                                                    (xlib:display-display (controller-display controller)))
                                      :parameters (list controller)))


(defun clx-controller-loop (controller)
  ;; nb. can't go downinto the scheduler, as it ultimately recursively blocks.
  ;; also can't listen (above) to hang, as that blocks the socket for writes
  ;; (xlib::process-block "wait clx event" #'clx-event-predicate display controller)
  ;; (xlib::process-wakeup (xlib::current-process))
  ;; so ... poll    
  (loop (sleep .5)
        (unless (xlib-open-display-p (controller-display controller))
          (return))
        (clx-controller-process-events controller 0)))


(defun clx-controller-process-events (controller &optional timeout)
  "First, process all pending display events for the controller's display. Then execute pending internal
 tasks. The given timeout value determines whether to wait for available display events or, if nil,
 to return immediately. A non-null value is passed from clx-controller-loop, to cause
 an event thread to hand. A non-threaded configuration should pass nil to poll and return immediately."

  (let ((display (controller-display controller)))
    (flet ((process-clx-event (&rest event-slots
                                     &key event-key event-window
                                     &allow-other-keys)
             (declare (dynamic-extent event-slots))
             ;; dispatch the event
             (let ((context (view-projection-context event-window)))
               (when context
                 (apply #'clx-controller-dispatch-event controller context event-key event-slots))
               ;; then return t if nothing else is there
               (not (xlib:event-listen display 0)))))
      (declare (dynamic-extent #'process-clx-event))

      (when (or (xlib:event-listen display timeout)
                (plusp (length (controller-tasks controller))))
        (xlib:process-event display
                            :timeout 0
                            :handler #'process-clx-event
                            :discard-p t)
        (controller-run-tasks controller)))))


(defgeneric clx-controller-dispatch-event (controller context event &key &allow-other-keys)
  (:method ((controller clx-controller) (context clx-context) (event (eql :exposure))
            &key window)
           (context-expose-event-handler controller context window))
  (:method ((controller clx-controller) (context clx-context) (event (eql :button-press))
            &key x y window)
           (context-click-event-handler controller context (make-point x y) window))
  (:method ((controller clx-controller) (context clx-context) (event (eql :button-release))
            &key x y window)
           (context-button-up-event-handler controller context (make-point x y) window))
  (:method ((controller clx-controller) (context clx-context) (event (eql :key-press))
            &key code modifiers)
           (context-key-event-handler controller context
                                      (xlib:keycode->character (controller-display controller)
                                                               code modifiers)
                                      nil))
  (:method ((controller clx-controller) (context clx-context) (event (eql :key-release))
            &key code modifiers)
           (context-key-up-event-handler controller context
                                         (xlib:keycode->character (controller-display controller)
                                                                  code modifiers)
                                         nil)))


(defmethod context-display ((context clx-context))
  ;; resolve the display lazily in order to recover from closed windows.
  (let ((display (get-context-display context)))
    (cond ((xlib-open-display-p display)
           display)
          (t
           ;; open and set-up the display
           (setf display
                 (etypecase display
                   (xlib:display        ;; re-use the old display parameters
                    (xlib:open-display (xlib:display-host display)
                                       :protocol *clx-display-protocol*
                                       :display (xlib:display-display display)
                                       :authorization-name (xlib:display-authorization-name display)
                                       :authorization-data (xlib:display-authorization-data display)))
                   (list
                    (destructuring-bind (&optional (host *clx-display-host*)
                                                   &key ((:display display-number) *clx-display-number*)
                                                   (protocol *clx-display-protocol*))
                                        display
                      (apply #'xlib:open-display host
                             :display display-number
                             :protocol protocol
                             (rest display))))))
           (setf (context-display context)
                 (setq *clx-default-display* display))
           display))))

(defmethod (setf context-view) :before ((view xlib:window) (context clx-context))
  (with-accessors ((display context-display)) context
    (if display
      (unless (eq display (xlib:drawable-display view))
        (error "window not on display: ~s: ~s" view display))
      (setf display (xlib:drawable-display view)))))
  


(defgeneric clx-font-designator (font-spec)
  (:method ((designator string))
           "a string is used as-is."
           designator)
  (:method ((font-spec cons))
           (multiple-value-bind (family point-size style-names)
                                (compute-font-parameters font-spec)
             (let ((weight (cond ((find :bold style-names))
                                 ((find :plain style-names) :medium)
                                 ((find :medium style-names))
                                 ((find :light style-names))
                                 (t :medium)))
                   (slant (cond ((find :italic style-names) :i)
                                (t :r)))
                   (width (cond ((find :narrow style-names) :condensed)
                                (t :normal))))
               (format nil "~(-*-~a-~a-~a-~a-*-*-~d-*~)"
                       family weight slant width (* point-size 10))))))
;;; (clx-font-designator '("times" 8 :bold :italic))
;;; (clx-font-designator '("courier" 10 :bold))

(defmethod define-font ((context clx-context) font-spec name)
  (setf (get-font context name)
        (xlib:open-font (context-display context) (clx-font-designator font-spec)))
  name)




(defmethod view-projection-context ((view xlib:window))
  (getf (xlib:window-plist view) 'projection-context))

(defmethod (setf view-projection-context) ((context projection-context) (view xlib:window))
  (setf (getf (xlib:window-plist view) 'projection-context) context))

(defmethod (setf view-projection-context) ((context null) (view xlib:window))
  (remf (xlib:window-plist view) 'projection-context)
  nil)


(defmethod context-make-view ((context clx-context)
                                &key
                                (window-title (or (context-get context :window-title) "CLX"))
                                (view-position (or (context-get context :view-position) #@(32 32)))
                                (view-size (or (context-get context :view-size) #@(128 128)))
                                (view-class nil)
                                (view-parent nil)
                                (color-p nil)
                                (icon-name (or (context-get context :icon-name) "CLX OG"))
                                (resource-class (or (context-get context :resource-class) 'og))
                                (resource-name (or (context-get context :resource-name) "OG"))
                                (show-p t))
  (let* ((display (context-display context)))
    (assert-type display xlib:display)
    (when view-class (warn "view-class ignored: ~s." view-class))
    (when color-p (warn "color-p ignored: ~s." color-p))

    (let* ((screen (xlib:display-default-screen display))
           (black (xlib:screen-black-pixel screen))
           (white (xlib:screen-white-pixel screen))
           (width (point-h view-size))
           (height (point-v view-size))
           (x (point-h view-position))
           (y (point-v view-position))
           (window (xlib:create-window :parent (or view-parent
                                                   (xlib:screen-root screen))
                                       :x x :y y :width width :height height
                                       :background black
                                       :border white
                                       :border-width 1
                                       :colormap (xlib:screen-default-colormap screen)
                                       :bit-gravity :center
                                       :event-mask (context-event-mask context))))
      (xlib:set-wm-properties window
                              :name window-title
                              :icon-name icon-name
                              :resource-name resource-name
                              :resource-class resource-class
                              :x (xlib:drawable-x window)
                              :y (xlib:drawable-y window)
                              :width (xlib:drawable-width window)
                              :height (xlib:drawable-height window)
                              :min-width (xlib:drawable-width window)
                              :min-height (xlib:drawable-height window)
                              :input :off :initial-state :normal
                              :command `(context-expose-event-handler
                                         ',(controller-id (context-controller context))
                                         ',(context-name context)
                                         ',(xlib:window-id window))
                              )
      (when show-p (xlib:map-window window))
      (xlib:display-force-output display)
      (setf (context-get context :view-size) view-size)
      (setf (context-get context :view-position) view-position)
      (setf (view-projection-context window) context)
      window)))

(defmethod context-update-for-view ((context clx-context) (view xlib:window))
  (setf (context-get context :view-size)
        (make-point (xlib:drawable-width view) (xlib:drawable-height view)))
  (setf (context-get context :view-position)
        (make-point (xlib:drawable-x view) (xlib:drawable-y view))))

(defmethod context-close-view ((context clx-context) (view xlib:window))
  (xlib:destroy-window view)
  (xlib:display-force-output (context-display context))
  (setf (view-projection-context view) nil)
  (call-next-method))

(defmethod context-size ((context clx-context))
  (let ((view (context-view context)))
    (make-point (xlib:drawable-width view)
                (xlib:drawable-height view))))

(defmethod view-size ((view xlib:window))
  (make-point (xlib:drawable-width view)
              (xlib:drawable-height view)))

(defmethod (setf view-size) (size (view xlib:window))
  (let ((width (point-h size))
        (height (point-v size)))
    (setf (xlib:drawable-width view) width
          (xlib:drawable-height view) height)
    size))

(defmethod context-position ((context clx-context))
  (let ((view (context-view context)))
    (make-point (xlib:drawable-x view)
                (xlib:drawable-y view))))

(defmethod view-position ((view xlib:window))
  (make-point (xlib:drawable-x view)
              (xlib:drawable-y view)))

(defmethod (setf view-position) (position (view xlib:window))
  (let ((width (point-h position))
        (height (point-v position)))
    (setf (xlib:drawable-x view) width
          (xlib:drawable-y view) height)
    position))

(defmethod context-view-p ((context clx-context) (window xlib:window))
  (ignore-errors
   (and (xlib-open-display-p (xlib:window-display window))
        (xlib:drawable-width window)
        t)))

(defgeneric xlib-open-display-p (display)
  (:method ((datum t)) nil)
  (:method ((display xlib:display))
           (and (typep (xlib::display-output-stream display) 'stream)
                (open-stream-p (xlib::display-output-stream display))
                ;; allegro can indicate that it is open, just not for output
                (output-stream-p (xlib::display-output-stream display))
                #+digitool
                (let* ((stream (xlib::display-output-stream display))
                       (conn (ccl::stream-io-buffer stream)))
                  (and ;; (not (ccl:%null-ptr-p (ccl::ot-conn-inbuf conn)))
                       ;; (not (ccl:%null-ptr-p (ccl::ot-conn-inptr conn)))
                       (not (ccl:%null-ptr-p (ccl::ot-conn-outbuf conn)))
                       (not (ccl:%null-ptr-p (ccl::ot-conn-outptr conn))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; geometric elements
;;;
;;; these functions draw the basic geometric figures.
;;; NB: they do not take structure objects, rather they require the explicit
;;; points - either as instances or as spread coordinates. all locations are absolute.
;;; the instance

;;;
;;; line

(defun clx-line-port (drawable location1 location2 &optional variables)
  "draw a line given the endpoints"
  (declare (type location-vector location1 location2))
  #+og.assert-types (progn (assert-type drawable xlib:drawable)
                           (assert-types (location1 location2) location-vector)
                           (assert-type variables (or list function)))
  (let ((x1 (%round (aref location1 0))) (y1 (%round (aref location1 1)))
        (x2 (%round (aref location2 0))) (y2 (%round (aref location2 1))))

    (context-log-message *projection-context* 'clx-line-port ":x1 ~s :y1 ~s :x2 ~s :y2 ~s"
                   x1 y1 x2 y2)
    
    (flet ((line-geometry ()
             (clx-assert-foreground-pixel *clx-stroke-pixel*)
             (xlib:draw-line drawable *clx-gcontext* x1 y1 x2 y2 nil)
             ;;x1 y1 x2 y2
             ))
      (declare (dynamic-extent #'line-geometry))
      (call-with-projection-variables #'line-geometry variables))))



;;;
;;; rectangle

(defun clx-rectangle-port (drawable location1 location2 &optional variables)
  (declare (type location-vector location1 location2))
  #+og.assert-types (progn (assert-type drawable xlib:drawable)
                           (assert-types (location1 location2) location-vector)
                           (assert-type variables (or list function)))
  (let ((x1 (%round (aref location1 0))) (y1 (%round (aref location1 1)))
        (x2 (%round (aref location2 0))) (y2 (%round (aref location2 1))))
    (when (< x2 x1) (rotatef x1 x2))
    (when (< y2 y1) (rotatef y2 y1))
    (let ((width (- x2 x1))
          (height (- y2 y1))
          (draw-rectangle-fill-p nil))
      (flet ((rectangle-geometry ()
               (xlib:draw-rectangle drawable *clx-gcontext*
                                    x1 y1 width height
                                    draw-rectangle-fill-p))
             (g&p (render pixel fill)
               (clx-assert-foreground-pixel pixel)
               (setf draw-rectangle-fill-p fill)
               (when fill (unless (eq :winding *clx-path-rule*)
                            (setf (xlib:gcontext-fill-rule *clx-gcontext*)
                                  (setf *clx-path-rule* :winding))))
               (funcall render)))
        (declare (dynamic-extent #'rectangle-geometry))
        (labels ((rectangle-properties ()
                 (case *context-path-effect*
                   (:clear
                    (g&p #'rectangle-geometry *clx-background-pixel* :winding))
                   ((:eofill :fill)
                    )
                   (:paint
                    (ecase *context-path-constituents*
                      (:lines (g&p #'rectangle-geometry *clx-stroke-pixel* nil))
                      (:surfaces (g&p #'rectangle-geometry *clx-fill-pixel* :winding))
                      (:points (unless (= *clx-stroke-pixel* *clx-foreground-pixel*)
                                 (setf (xlib:gcontext-foreground *clx-gcontext*)
                                       (setf *clx-foreground-pixel* *clx-stroke-pixel*)))
                               (xlib:draw-line drawable *clx-gcontext* x1 y1 x1 y1 nil)
                               (xlib:draw-line drawable *clx-gcontext* x1 y2 x1 y2 nil)
                               (xlib:draw-line drawable *clx-gcontext* x2 y1 x2 y1 nil)
                               (xlib:draw-line drawable *clx-gcontext* x2 y2 x2 y2 nil))))
                   (:invert
                    (let ((boole (xlib:gcontext-function *clx-gcontext*)))
                      (unwind-protect
                        (let ((*context-path-effect* :paint))
                          (setf (xlib:gcontext-function *clx-gcontext*) boole-c2)
                          (rectangle-geometry))
                        (setf (xlib:gcontext-function *clx-gcontext*) boole))))
                   (:fill-stroke
                    (g&p #'rectangle-geometry *clx-fill-pixel* :winding)
                    (g&p #'rectangle-geometry *clx-stroke-pixel* nil)))))
          (declare (dynamic-extent #'rectangle-properties))
          (call-with-projection-variables #'rectangle-properties variables))))))


;;;
;;; polygon

(defun clx-poly-port (port-location-list &optional properties)
  "render a list of port locations in a clx gcontext."
  (let ((draw-line-geometry-fill-p nil))
    (flet ((draw-line-geometry ()
             (xlib:draw-lines (context-view *projection-context*)
                              *clx-gcontext* port-location-list
                              :fill-p draw-line-geometry-fill-p))
           (g&p (render pixel fill-rule)
             (unless (= pixel *clx-foreground-pixel*)
               (setf (xlib:gcontext-foreground *clx-gcontext*)
                     (setf *clx-foreground-pixel* pixel)))
             (setf draw-line-geometry-fill-p (not (null fill-rule)))
             (when fill-rule (unless (eq fill-rule *clx-path-rule*)
                               (setf (xlib:gcontext-fill-rule *clx-gcontext*)
                                     (setf *clx-path-rule* fill-rule))))
             (funcall render)))
      (declare (dynamic-extent #'draw-line-geometry))
      (flet ((clx-poly-geometry ()
               (case *context-path-effect*
                 (:clear (g&p #'draw-line-geometry *clx-background-pixel* :winding))
                 (:paint
                  (ecase *context-path-constituents*
                    (:lines
                     (g&p #'draw-line-geometry *clx-stroke-pixel* nil))
                    (:points
                     (unless (= *clx-stroke-pixel* *clx-foreground-pixel*)
                       (setf (xlib:gcontext-foreground *clx-gcontext*)
                             (setf *clx-foreground-pixel* *clx-stroke-pixel*)))
                     (let ((locations port-location-list)
                           (x nil) (y nil))
                       (loop (unless (setf x (pop locations) y (pop locations)) (return))
                             (xlib:draw-line (context-view *projection-context*)
                                             *clx-gcontext* x y x y nil))))
                    (:surfaces
                     (g&p #'draw-line-geometry *clx-fill-pixel* *clx-path-rule*))))
                 (:invert
                  (let ((boole (xlib:gcontext-function *clx-gcontext*)))
                      (unwind-protect
                        (let ((*context-path-effect* :paint))
                          (setf (xlib:gcontext-function *clx-gcontext*) boole-c2)
                          (g&p #'draw-line-geometry *clx-fill-pixel* :winding))
                        (setf (xlib:gcontext-function *clx-gcontext*) boole))))
                 #+ignore
                 (:fill-stroke (g&p #'draw-line-geometry *clx-fill-pixel* :winding)
                               (g&p #'draw-line-geometry *clx-stroke-pixel* nil)))))
        (declare (dynamic-extent #'clx-poly-geometry))
        (call-with-projection-variables #'clx-poly-geometry properties)))))


(defgeneric clx-poly (locations &optional variables)

  (:method ((locations sequence) &optional (properties nil))
           "transform a setf of points to port coordinates and render a polygon.
   this is intended to be calssed as part of display list processing, which means that the view/port
   are already focused."
           (let ((get-next-location nil)
                 (location-count (length locations))
                 (last-index -1))
             (declare (type fixnum last-index location-count))
             (when (> location-count 0)
               (flet ((next-list-location ()
                        (locally (declare (type list locations))
                          (pop locations)))
                      (next-vector-location ()
                        (locally (declare (type vector locations))
                          (when (< (incf last-index) location-count)
                            (aref locations last-index)))))
                 (declare (dynamic-extent #'next-list-location #'next-vector-location))
                 (etypecase locations
                   (cons (setf get-next-location #'next-list-location))
                   (vector (setf get-next-location #'next-vector-location)))
                 (with-location-vectors ((port-location-vector))
                   (let* ((port-x 0.0d0) (port-y 0.0d0) (location nil)
                          (port-coordinate-count (* location-count 2)) 
                          (port-location-list (make-list port-coordinate-count))
                          (port-location port-location-list))
                     (declare (dynamic-extent port-location-list port-x port-x)
                              (type double-float port-x port-y)
                              (type fixnum port-coordinate-count))
                     (loop (typecase (setf location (funcall get-next-location))
                             (null (return))
                             (fixnum (setf (first port-location) (point-h location)
                                           (second port-location) (point-v location)))
                             (location-port (setf (first port-location) (round (location-x location))
                                                  (second port-location) (round (location-y location))))
                             (t (location-transform *context-transform* location port-location-vector)
                                (setf port-x (aref port-location-vector 0) port-y (aref port-location-vector 1))
                                (setf (first port-location) (%round port-x)
                                      (second port-location) (%round port-y))))
                           (setf port-location (nthcdr 2 port-location)))
                     (clx-poly-port port-location-list properties)))))))
  #+ignore ;; directional compression
  ;; taking the example of spx for 2005/06, there were 361511 points.
  ;; directional compression rendered 862 @ 2.75/112882808
  ;; point compression rendered 25924 @ 2.82/112816032
  (:method ((locations function) &optional (properties nil))
           "transform a setf of points to port coordinates and render a polygon.
accept a generator function as the locations.
this is intended to be calssed as part of display list processing, which means that the view/port
are already focused."
           (let ((port-location-list nil)
                 (delta-x 0) (delta-y 0)             ; accumulated deltas
                 (last-x nil) (last-y nil)
                 (location-count 0)
                 (port-location-count 0))
             (with-location-vectors ((port-location-vector))
               (let* ((port-x 0.0d0) (port-y 0.0d0))
                 (declare (dynamic-extent port-x port-x)
                          (type double-float port-x port-y))
                 (flet ((clx-poly-generator (location &aux (x 0) (y 0))
                          (typecase location
                            (fixnum (setf x (point-h location) y (point-v location)))
                            (location-port (setf x (round (location-x location))
                                                 y (round (location-y location))))
                            (t (location-transform *context-transform* location port-location-vector)
                               (setf port-x (aref port-location-vector 0)
                                     port-y (aref port-location-vector 1))
                               (setf x (%round port-x)
                                     y (%round port-y))))
                          (incf location-count)
                          ;; compute the delta from the last plotted point. if in just 1 direction
                          ;; don't draw it, just keep pushing the same way.
                          (cond ((null last-x)
                                 (push x port-location-list) (push y port-location-list)
                                 (incf port-location-count)
                                 (setf delta-x 0 delta-y 0
                                       last-x x last-y y))
                                (t
                                 ;; (push y port-location-list) (push x port-location-list)
                                 (let ((d-x (- x last-x))
                                       (d-y (- y last-y)))
                                   (cond ((or (and (zerop delta-x) (zerop d-x))
                                              (and (zerop delta-y) (zerop d-y)))
                                          (incf delta-x d-x)
                                          (incf delta-y d-y))
                                         (t
                                          (push last-x port-location-list) (push last-y port-location-list)
                                          (incf port-location-count)
                                          (setf delta-x d-x delta-y d-y))))
                                 (setf last-x x last-y y)))))
                   (declare (dynamic-extent #'clx-poly-generator))
                   (funcall locations #'clx-poly-generator))))
             (when (or (zerop delta-x) (zerop delta-y))
               ;; add the deferrred most recent point
               (push (+ last-x delta-x) port-location-list)
               (push (+ last-y delta-y) port-location-list))
             (setf port-location-list (nreverse port-location-list))
             (clx-poly-port port-location-list properties)
             (values port-location-count location-count)))
  ;; #+ignore ;; point compression
  (:method ((locations function) &optional (properties nil))
           "transform a setf of points to port coordinates and render a polygon.
accept a generator function as the locations.
this is intended to be calssed as part of display list processing, which means that the view/port
are already focused."
           (let ((port-location-list nil)
                 (last-x most-negative-fixnum)
                 (last-y most-negative-fixnum)
                 (location-count 0)
                 (port-location-count 0)
                 (location-limit (- (xlib:display-max-request-length
                                     (context-display *projection-context*))
                                    3)))
             (with-location-vectors ((port-location-vector))
               (let* ((port-x 0.0d0) (port-y 0.0d0))
                 (declare (dynamic-extent port-x port-x)
                          (type double-float port-x port-y))
                 (flet ((clx-poly-generator (location &aux (x 0) (y 0))
                          (typecase location
                            (fixnum (setf x (point-h location) y (point-v location)))
                            (location-port (setf x (round (location-x location))
                                                 y (round (location-y location))))
                            (t (location-transform *context-transform* location port-location-vector)
                               (setf port-x (aref port-location-vector 0)
                                     port-y (aref port-location-vector 1))
                               (setf x (%round port-x)
                                     y (%round port-y))))
                          (incf location-count)
                          ;; draw moved locations only
                          (cond ((and (= x last-x) (= y last-y))
                                 ;; skip it
                                 )
                                (t
                                 (push x port-location-list) (push y port-location-list)
                                 (incf port-location-count)
                                 (setf last-x x last-y y)
                                 ;; attempt to limit overflow
                                 (when (>= port-location-count location-limit)
                                   (setf port-location-list (nreverse port-location-list))
                                   (clx-poly-port port-location-list properties)
                                   (setf port-location-count 1
                                         port-location-list (list y x)))))))
                   (declare (dynamic-extent #'clx-poly-generator))
                   (funcall locations #'clx-poly-generator))))
             (when (= port-location-count 1)
               ;; make sure that there are two locations
               (push last-x port-location-list)
               (push last-y port-location-list)
               port-location-count)
             (setf port-location-list (nreverse port-location-list))
             (clx-poly-port port-location-list properties)
             (values port-location-count location-count))))

(defmethod context-poly ((context clx-context) locations &optional variables)
  (clx-poly locations variables))



;;;
;;; arc

(defun clx-arc-port (drawable location radius start-radians end-radians direction &optional variables)
  "draw a rectangle given the port location"
  (declare (type location-vector location))
  #+og.assert-types (progn (assert-type drawable xlib:drawable)
                           (assert-type location location-vector)
                           (assert-types (radius start-radians end-radians) number)
                           (assert-type variables (or sequence function)))
  (ecase direction
    (:clockwise )
    (:counterclockwise (rotatef start-radians end-radians)))
  (unless (typep start-radians 'double-float)
    (setf start-radians (float start-radians 1.0d0)))
  (unless (typep end-radians 'double-float)
    (setf end-radians (float end-radians 1.0d0)))
  
  (let* ((x (%round (aref location 0))) (y (%round (aref location 1)))
         (width (* radius 2))          ; should have 2-d size
         (height width)
         (arc-geometry-fill-p nil))
    (flet ((arc-geometry ()
             (xlib:draw-arc drawable *clx-gcontext*
                            (- x radius) (- y radius) width height
                            start-radians end-radians
                            arc-geometry-fill-p))
           (g&p (render pixel fill)
             (unless (= pixel *clx-foreground-pixel*)
               (setf (xlib:gcontext-foreground *clx-gcontext*)
                     (setf *clx-foreground-pixel* pixel)))
             (setf arc-geometry-fill-p fill)
             (funcall render)))
      (declare (dynamic-extent #'arc-geometry))
      (flet ((clx-arc-properties ()
               (case *context-path-effect*
                 (:clear (g&p #'arc-geometry *clx-background-pixel* :winding))
                 (:paint
                  (ecase *context-path-constituents*
                    (:lines
                     (g&p #'arc-geometry *clx-stroke-pixel* nil))
                    (:points
                     (unless (= *clx-stroke-pixel* *clx-foreground-pixel*)
                       (setf (xlib:gcontext-foreground *clx-gcontext*)
                             (setf *clx-foreground-pixel* *clx-stroke-pixel*)))
                     (let ((x1 (+ x (* (cos start-radians) radius)))
                           (y1 (+ y (* (sin start-radians) radius))))
                       (xlib:draw-line drawable *clx-gcontext* x1 y1 x1 y1 nil)
                       (setf x1 (+ x (* (cos end-radians) radius))
                             y1 (+ y (* (sin end-radians) radius)))
                       (xlib:draw-line drawable *clx-gcontext* x1 y1 x1 y1 nil)))
                    (:surfaces
                     (g&p #'arc-geometry *clx-fill-pixel* :winding))))
                 (:invert
                  (let ((boole (xlib:gcontext-function *clx-gcontext*)))
                      (unwind-protect
                        (let ((*context-path-effect* :paint))
                          (setf (xlib:gcontext-function *clx-gcontext*) boole-c2)
                          (g&p #'arc-geometry *clx-fill-pixel* :winding))
                        (setf (xlib:gcontext-function *clx-gcontext*) boole)))
                  #+ignore
                  (:fill-stroke
                   (g&p #'arc-geometry *clx-fill-pixel* :winding)
                   (g&p #'arc-geometry *clx-stroke-pixel* nil))))))
        (declare (dynamic-extent #'clx-arc-properties))
        (call-with-projection-variables #'clx-arc-properties variables)))))


;;;
;;; text

(defun clx-text-port (drawable location text font &optional variables)
  #+og.assert-types (progn (assert-type drawable xlib:drawable)
                           (assert-type location location-vector)
                           (assert-type text string)
                           (assert-type variables (or list function)))
  (let ((x (%round (aref location 0))) (y (%round (aref location 1)))) 
    (cond ((and (typep x 'xlib:int16) (typep y 'xlib:int16))
           (flet ((text-geometry ()
                    (clx-assert-foreground-pixel *clx-stroke-pixel*)
                    (xlib:draw-glyphs drawable *clx-gcontext* x y text)))
             (declare (dynamic-extent #'geometry))
             (if (and font (setf font (get-font *projection-context* font)))
               (let ((old-font (xlib:gcontext-font *clx-gcontext*)))
                 (unwind-protect
                   (progn (setf (xlib:gcontext-font *clx-gcontext*) font)
                          (call-with-projection-variables #'text-geometry variables))
                   (setf (xlib:gcontext-font *clx-gcontext*) old-font)))
               (call-with-projection-variables #'text-geometry variables))))
          (t
           (context-log-message *projection-context* :warn
                                "not INT16: #@(~s ~s)." x y)))))


;;;
;;; raster

(defun clx-raster-port (drawable location1 location2 raster &optional variables)
  #+og.assert-types (progn (assert-type drawable xlib:drawable)
                           (assert-types (location1 location2) location-vector)
                           (assert-type variables sequence))
  (let  ((x1 (%round (aref location1 0))) (y1 (%round (aref location1 1)))
         (x2 (%round (aref location2 0))) (y2 (%round (aref location2 1))))
    (let* ((size (context-size *projection-context*))
           (w (location-x size))
           (h (location-y size)))
      (context-log-message *projection-context* 'clx-raster-port ":x1 ~s :y1 ~s :x2 ~s :y2 ~s :viewSize (~a, ~a)"
                           x1 y1 x2 y2 w h))
    ;; (context-log-message *projection-context* "~W" (context-state *projection-context*))
    (when (< x2 x1) (rotatef x1 x2))
    (when (< y2 y1) (rotatef y1 y2))
    (let* ((offset (port-offset raster))
           (image (sample-projection raster *projection-context*))
           (width (- x2 x1))
           (height (- y2 y1))
           (x (+ x1 (point-h offset)))
           (y (+ y1 (point-v offset))))
      (context-log-message *projection-context* 'clx-raster-port ":x ~s :y ~s :w ~s :h ~s"
                   x y width height)

      (flet ((raster ()
               (xlib:put-image drawable *clx-gcontext* image
                               :x x
                               :y y
                               :width width
                               :height height)))
        (declare (dynamic-extent #'raster))
        (call-with-projection-variables #'raster variables)))))

(defmethod sample-projection ((raster raster) (context clx-context))
  (let ((image (context-get context 'xlib:image raster)))
    ;; instantiate a image with the respective new data sequence if none is
    ;; present in the cache or the size is wrong
    (let* ((size (sample-size raster))
           (width (point-h size))
           (height (point-v size))
           (depth (sample-depth raster)))
      (unless (and (typep image 'xlib:image)
                   (= width (xlib:image-width image))
                   (= height (xlib:image-height image)))
        (let* ((data (make-array (list height width)  ;; rows x pixels-per-row
                                 :element-type (ecase depth
                                                 (8 'xlib:card8)
                                                 (24 'xlib:pixel)
                                                 (32 'xlib:pixel))
                                 :initial-element 0)))
          (setf image (xlib:create-image :data data :format :z-pixmap
                                         :depth (min depth (xlib:drawable-depth
                                                            (context-view context)))
                                         :bits-per-pixel (ecase depth
                                                           (8 8)
                                                           (24 32)
                                                           (32 32))))
          (context-log-message context 'sample-projection "new data: ~s: ~s: ~s."
                               raster image (type-of data))
          (setf (context-get context 'xlib:image raster) image)))
      (unless (test-projection-generation raster context)
        ;; (re)fill the data
        (let* ((sample-data (sample-data raster))
               (sample 0)
               (image-data (xlib:image-z-pixarray image)))
          (dotimes (i height)
            (dotimes (j width)
              (setf sample (sample-filter raster (aref sample-data i j) i j))
              (setf (aref image-data i j) sample))))))
    image))



;;;
;;; attributes
;;;
;;; colors require parallel state as they cannot be read out (ie there's no CGContextGetFillColor)

(defMacro with-clx-intensity-variables (variables &rest body)
  `(progn ,@(mapcar #'(lambda (var)
                        `(etypecase ,var
                           (short-float)
                           (double-float (setf ,var (float ,var 1.0s0)))
                           (integer (setf ,var (/ ,var 65535.0s0)))
                           (number (setf ,var (float ,var 1.0s0)))))
                    variables)
          (locally (declare (type short-float ,@variables)))
          ,@body))


(defmethod define-color ((context clx-context) (color t))
  (with-coerced-variables ((short-location-vector color))
    (or (context-get context :color color)
        (let* ((clx-color (xlib:make-color :red (aref color 0) :green (aref color 1) :blue (aref color 2)))
               (pixel-id (xlib:alloc-color (xlib:screen-default-colormap
                                            (xlib:display-default-screen
                                             (context-display context)))
                                           clx-color)))
          (setf color (location-vector-copy color (short-location-vector)))
          (setf (context-get context :color color) pixel-id)))))
    
(defun clx-call-with-foreground-pixel (function pixel)
  (declare (dynamic-extent function))
  (if (eql pixel *clx-foreground-pixel*)
    (funcall function)
    (let ((old-pixel *clx-foreground-pixel*)
          (*clx-foreground-pixel* pixel))
      (unwind-protect (progn (setf (xlib:gcontext-foreground *clx-gcontext*) pixel)
                             (funcall function))
        (setf *clx-foreground-pixel* old-pixel)
        (setf (xlib:gcontext-foreground *clx-gcontext*) old-pixel)))))

(defun clx-assert-foreground-pixel (pixel)
  (unless (eql pixel *clx-foreground-pixel*)
    (setf (xlib:gcontext-foreground *clx-gcontext*) pixel
          *clx-foreground-pixel* pixel)))

;;;
;;; utilities


(defmethod context-clear-view ((context clx-context))
  (unless (eq (context-background-mode context) :transparent)
    (setf (xlib:window-background (context-view context))
          *clx-background-pixel*))
  (xlib:clear-area (context-view context)))

(defmethod context-fill-view ((context clx-context) &optional (agent *context-fill-agent*))
  (let ((size (context-size context)))
    (flet ((do-fill ()
             (xlib:draw-rectangle (context-view context)
                                  (context-gcontext context)
                                  0 0
                                  (point-h size) (point-v size)
                                  :fill)))
      (declare (dynamic-extent #'do-fill))
      (unwind-protect
        (progn (context-save-fill-agent context)
               (context-set-fill-agent context agent)
               (clx-call-with-foreground-pixel #'do-fill *clx-stroke-pixel*))
        (restore-projection-variable)))))

(defmethod context-flush-view ((context clx-context))
  (xlib:display-force-output (context-display context))
  )
  


;;;
;;; agent management

(defun clx-fill-agent*3 (r g b)
  (declare (optimize (speed 3) (safety 0)))
  (with-clx-intensity-variables (r g b)
    (with-coerced-variables ((short-location-vector *clx-fill-color*))
      (cond ((and (= (aref *clx-fill-color* 0) r)
                  (= (aref *clx-fill-color* 1) g)
                  (= (aref *clx-fill-color* 2) b))
             (values *clx-fill-color* *clx-fill-pixel*))
            (t
             (setf (aref *clx-fill-color* 0) r
                   (aref *clx-fill-color* 1) g
                   (aref *clx-fill-color* 2) b)
             (values *clx-fill-color*
                     (setf *clx-fill-pixel*
                           (define-color *projection-context* *clx-fill-color*))))))))

(defmethod context-fill-agent*3 ((context clx-context) r g b)
  (clx-fill-agent*3 r g b))

(defmethod context-fill-agent*4 ((context clx-context) r g b &optional a)
  (declare (ignore a))
  (clx-fill-agent*3 r g b))

(defmethod context-set-fill-agent ((context clx-context) color &optional g b a)
  (declare (ignore a))
  (etypecase color
    (number
     (clx-fill-agent*3 color g b))
    (cons
     (destructuring-bind (r g b &optional a) (if (symbolp (first color)) (rest color) color)
       (declare (ignore a))
       (clx-fill-agent*3 r g b)))
    ((or vector location-3)
     (with-coerced-variables ((short-location-vector *clx-fill-color*))
       (location-vector-copy color *clx-fill-color*)
       (setf *clx-fill-pixel* (define-color context *clx-fill-color*))
       *clx-fill-color*))))


(defun clx-restore-fill-agent (color pixel)
  (location-vector-copy color *clx-fill-color*)
  (setq *clx-fill-pixel* pixel)
  (return-short-location-vector color))

(defmethod context-save-fill-agent ((context clx-context))
  (push-projection-variable #'clx-restore-fill-agent
                            (location-vector-copy *clx-fill-color* (get-short-location-vector))
                            *clx-fill-pixel*))



(defun clx-stroke-agent*3 (r g b)
  (declare (optimize (speed 3) (safety 0)))
  (with-clx-intensity-variables (r g b)
    (with-coerced-variables ((short-location-vector *clx-stroke-color*))
      (unless (and (= (aref *clx-stroke-color* 0) r)
                   (= (aref *clx-stroke-color* 1) g)
                   (= (aref *clx-stroke-color* 2) b))
        (setf (aref *clx-stroke-color* 0) r
              (aref *clx-stroke-color* 1) g
              (aref *clx-stroke-color* 2) b)
        (setf *clx-stroke-pixel* (define-color *projection-context* *clx-stroke-color*)))
      *clx-stroke-color*)))

(defmethod context-stroke-agent*3 ((context clx-context) r g b)
  (clx-stroke-agent*3 r g b))

(defmethod context-stroke-agent*4 ((context clx-context) r g b &optional a)
  (declare (ignore a))
  (clx-stroke-agent*3 r g b))

(defmethod context-set-stroke-agent ((context clx-context) color &optional g b a)
  (declare (ignore a))
  (etypecase color
    (number
     (clx-stroke-agent*3 color g b))
    (cons
     (destructuring-bind (r g b &optional a) (if (symbolp (first color)) (rest color) color)
       (declare (ignore a))
       (clx-stroke-agent*3 r g b)))
    ((or vector location-3)
     (with-coerced-variables ((short-location-vector *clx-stroke-color*))
       (location-vector-copy color *clx-stroke-color*)
       (setf *clx-stroke-pixel* (define-color context *clx-stroke-color*))
       *clx-stroke-color*))))

(defun clx-restore-stroke-agent (color pixel)
  (location-vector-copy color *clx-stroke-color*)
  (setq *clx-stroke-pixel* pixel)
  (return-short-location-vector color))

(defmethod context-save-stroke-agent ((context clx-context))
  (push-projection-variable #'clx-restore-stroke-agent
                            (location-vector-copy *clx-stroke-color* (get-short-location-vector))
                            *clx-stroke-pixel*))




(defun clx-clear-agent*3 (r g b)
  (declare (optimize (speed 3) (safety 0)))
  (with-clx-intensity-variables (r g b)
    (with-coerced-variables ((short-location-vector *clx-background-color*))
      (unless (and (= (aref *clx-background-color* 0) r)
                   (= (aref *clx-background-color* 1) g)
                   (= (aref *clx-background-color* 2) b))
        (setf (aref *clx-background-color* 0) r
              (aref *clx-background-color* 1) g
              (aref *clx-background-color* 2) b)
        (setf *clx-background-pixel* (define-color *projection-context* *clx-background-color*))
        (setf (xlib:gcontext-background *clx-gcontext*) *clx-background-pixel*))
      *clx-background-color*)))
  
(defmethod context-clear-agent*3 ((context clx-context) r g b)
  (clx-clear-agent*3 r g b))

(defmethod context-clear-agent*4 ((context clx-context) r g b &optional a)
  (declare (ignore a))
  (clx-clear-agent*3 r g b))

(defmethod context-set-clear-agent ((context clx-context) color &optional g b a)
  (declare (ignore a))
  (etypecase color
    (number
     (clx-clear-agent*3 color g b))
    (cons
     (destructuring-bind (r g b &optional a) (if (symbolp (first color)) (rest color) color)
       (declare (ignore a))
       (clx-clear-agent*3 r g b)))
    ((or vector location-3)
     (with-coerced-variables ((short-location-vector *clx-background-color*))
       (location-vector-copy color *clx-background-color*)
       (setf *clx-background-pixel* (define-color context *clx-background-color*))
       *clx-background-color*))))

(defun clx-restore-clear-agent (color pixel)
  (location-vector-copy color *clx-background-color*)
  (setq *clx-background-pixel* pixel)
  (setf (xlib:gcontext-background *clx-gcontext*) pixel)
  (return-short-location-vector color))
  
(defmethod context-save-clear-agent ((context clx-context))
  (push-projection-variable #'clx-restore-clear-agent
                            (location-vector-copy *clx-background-color* (get-short-location-vector))
                            *clx-background-pixel*))




;;;
;;; path constituents

(defmethod context-normalized-path-constituents
           ((context clx-context) (front symbol) (back t))
  "normalize path constituents to the front spec only."
  front)


;;;
;;; color mode

(defmethod context-set-color-mode ((context clx-context) mode &rest args)
  "core graphics supports a alpha combination factor for images?"
  (declare (ignore mode args))
  ;; no support for color modes
  )

(defmethod context-save-color-mode ((context clx-context))
  ;; no support for color modes
  )



;;;
;;; debugging

(defmethod projection-context-state ((context clx-context))
  (flet ((construct-state ()
           (let ((properties nil))
             (setf (getf properties :background-pixel) *clx-background-pixel*)
             (setf (getf properties :foreground-pixel) *clx-foreground-pixel*)
             (setf (getf properties :fill-pixel) *clx-fill-pixel*)
             (setf (getf properties :stroke-pixel) *clx-stroke-pixel*)
             (setf (getf properties :size) (location-list (context-size context)))
             (list :gcontext (context-gcontext context)
                   :view (context-view context)
                   :projection-transform (matrix-list *context-projection-transform*)
                   :too (matrix-list (context-projection-transform context))
                   :view-transform (matrix-list *context-view-transform*)
                   :too (matrix-list (context-view-transform context))
                   :transform (matrix-list (context-transform context))
                   :properties properties))))
    (declare (dynamic-extent #'construct-state))
    (call-in-clx-context #'construct-state context)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(xlib:clear-area (context-view *clx-c*))
(setf (xlib:gcontext-foreground *clx-gcontext*) *clx-white-pixel*)
(setf (xlib:gcontext-foreground *clx-gcontext*) *clx-black-pixel*)
(setf (xlib:gcontext-background *clx-gcontext*) *clx-black-pixel*)
(xlib:draw-rectangle (context-view *clx-c*) *clx-gcontext*
                     0 0 100 200 t)
(xlib:draw-line (context-view *clx-c*) *clx-gcontext* 10 10 200 200 nil)
(xlib:display-force-output *clx-display*)
(setf (xlib:window-background (context-view *clx-c*)) *clx-black-pixel*)
(xlib:display-force-output (context-display *clx-c*))
(xlib:with-gcontext (*clx-gcontext* :foreground *clx-white-pixel*
                                    :background *clx-black-pixel*)
  (xlib:draw-line (context-view *clx-c*) *clx-gcontext* 10 10 200 200 nil)
  (xlib:display-force-output *clx-display*))

(defclassFun (draw-self og-pixarray)
          (drawable &aux )
  (declare (object-variable screen-pixels pixels location end colormap))
  (setf raw-pixels (cdr (assoc visual-id screen-pixels :test #'=)))
  (when (null raw-pixels)
    (setf size (og:coordinate-- end location))
    (let ((depth (xlib:drawable-depth drawable)))
      (cond ((= depth 1)
	     (setf raw-pixels (dither-pixarray pixels size colormap)))
	    ((= depth 8)
	     (let* ((drawable-colormap (xlib:create-colormap (gethash visual-id
								      (xlib::display-resource-id-map
								       (xlib:window-display drawable)))
							     drawable t))
		    (cm-entries (slot-value (slot-value drawable-colormap 'xlib::visual-info)
					    'xlib::colormap-entries))
		    (pixel-list nil))
	       (dotimes (cm-index (min cm-entries (first (array-dimensions colormap))))
		 (push cm-index pixel-list)
		 (setf (aref colormap cm-index 3) cm-index)
		 (push (xlib:make-color :red (/ (aref colormap cm-index 0) 255)
					:green (/ (aref colormap cm-index 1) 255)
					:blue (/ (aref colormap cm-index 2) 255))
		       pixel-list))
	       (do ((cm-index (min cm-entries (first (array-dimensions colormap))) (1+ cm-index)))
		   ((>= cm-index (first (array-dimensions colormap))))
		 (setf (aref colormap cm-index 3) (closest-pixel cm-index colormap cm-entries)))
	       (og::with-display (xlib:window-display drawable)
		 (xlib:store-colors drawable-colormap (nreverse pixel-list)
				    :red-p t :green-p t :blue-p t)
		 (setf (xlib:window-colormap drawable) drawable-colormap))
	       (setf raw-pixels (make-array (list (og:coordinate-y size) (og:coordinate-x size))
					    :element-type '(unsigned-byte 8)))
	       (dotimes (y (og:coordinate-y size))
		 (dotimes (x (og:coordinate-x size))
		   (setf (aref raw-pixels y x) (aref colormap (aref pixels y x) 3))))))
	    (t
	     (error "Cannot display pixarray [~s] to depth ~d." self depth))))
    (setf screen-pixels (acons visual-id raw-pixels screen-pixels)))
  (graphics-pixmap drawable raw-pixels location end))

|#


:de.setf.graphics
