;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-


(in-package :de.setf.graphics.implementation)


(document
  (description "open gl graphics implementation for the de.setf.graphics library")

  (copyright "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
             "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

  (history
   (delta 20060424 "janderson@ravenpack.com"
          "opengl-raster*3. with scaleing based on projection/unprojected corners to
   compute the scale.")
   (delta 20030921 "james.anderson@setf.de" "the opengl context initialization code derives from
   the agentsheets code, but treats the view as an argument to the port-based context operations. full-screen
   operations ignore the port."))

  (long-description
   "<p>
  this file implements the abstract operations for the OpenGl library.
  the implementation depends on an interface to the agentsheets libraries.</p>
 <p>
  the functions exhibit several forms:
  <dl>
   <dt>context-<i>operation</i></dt>
   <dd>include OPENGL-CONTEXT as the intial specializer. they are invoked from
   interface functions (see abstract-projection.lisp) to perform operations for
   the given context.</dd>
   <dt>opengl-</i>operation</i></dt>
   <dd>are the opengl-specific operator implimentations.
   the active glcontextref is a static component of the graphics state.
   it is maintained by the opengl-specific method for
   call-with-projection-context, which means that it does not appear as an
   argument to the foreign-functiona calls, which means that it does not appear
   as a parameter to the opengl-specific methods.</dd>
   <dt>gl-</i>operation</i></dt><dd>are internal functions and utilities which
   are specific to opengl. they may or may not depend on the current gl
   context.</dd>
   </dl></p>
  <p>
  the operations all expect world-locations and integer values are treated as
  such. the type of the respective first argument determines if and how the
  values are coerced.</p>"))

;;;
;;; records

#+digitool
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defrecord opengl-matrix
    (data (:array :double-float 16)))
  (defrecord opengl-color4d
    (:variant ((red double-float)
               (green double-float)
               (blue double-float)
               (alpha double-float))
              ((data (:array :double-float 4)))))
  (defrecord opengl4iv
    (data (:array :long 4)))
  (defrecord opengl4fv
    (data (:array :float32 4)))
  (defrecord opengl-3dv
    (:variant ((:x :double-float)
               (:y :double-float)
               (:z :double-float))
              ((data (:array :double-float 3)))))
  (defrecord opengl4dv
    (data (:array :double-float 4)))
  (defrecord GLModelTransform
    (data (:array :double-float 16)))
  (defrecord GLProjectionTransform
    (data (:array :double-float 16)))
  (defrecord GLViewportTransform
    (:variant ((x :long)
               (y :long)
               (width :long)
               (height :long))
              ((data (:array :long 4)))))
  (defrecord opengl-transform-state
    (:model GLModelTransform)
    (:projection GLProjectionTransform)
    (:viewport GLViewportTransform)))
  
#-digitool
(nyi-error 'opengl-matrix)

(defstruct (opengl-path-properties (:include path-properties))
  "encapsulate geometry primitives, effect, mode, etc arguments for given path
   constituency."
  polygon-primitive
  line-primitive
  front-mode
  back-mode
  agent
  )
  
(defstruct opengl-image data width height depth)


;;;
;;; parameters


(defparameter *opengl-glcontext* nil
  "binds the current graphics context's internal opengl context.
   note that the opengl function operate on a global, static graphics context.")

(defparameter *class.opengl-view*' ccl::opengl-view)
(defparameter *class.opengl-window* 'ccl::opengl-window)

(defparameter *opengl-fill-color* (location-vector))
(defparameter *opengl-stroke-color* (location-vector))
(defparameter *opengl-background-color* (location-vector))
(defparameter *opengl-color* nil
  "cached the last color value passed through #_GLColor")
(defparameter *inverting-white-color* (make-location-vector :x 1.0d0 :y 1.0d0 :z 1.0d0 :h 1.0d0))

(defparameter *opengl-path-combined-constituents* :lines
  "specifies the combined mode given the front and back polygon settings")
(defparameter *opengl-color-mode* :opaque)
(defparameter *opengl-color-source-factor* GL:GL_ONE)
(defparameter *opengl-color-destination-factor* GL:GL_ZERO)

(defparameter *opengl-path-constituents* :lines
  "default opengl path constituents.
   used as the initial value when establishing a projection context.")

(defparameter *opengl-path-effect* :paint
  "default opengl paint effect.
   used as the initial value when establishing a projection context.")

(defparameter *opengl-path-rule* :winding
  "default opengl path rule.
   used as the initial value when establishing a projection context.")

(defvar *opengl-path-properties-map*
  (make-hash-table :test #'equalp)
  "binds an hashtable which maps path constituent designators to the
   opengl-specific path-property instances.
   the initial content contains generated settings for the combinations
   present in *path-constituents*")

(flet ((gl-polymode (constituents)
         (ecase constituents
           (:lines GL:GL_LINE)
           (:points GL:GL_POINT)
           (:surfaces GL:GL_FILL)))
       (gl-poly-primitive (constituents)
         (ecase constituents
           (:lines GL:GL_LINE_STRIP)
           (:points GL:GL_POINTS)
           (:surfaces GL:GL_POLYGON)))
       (gl-line-primitive (constituents)
         (ecase constituents
           (:lines GL:GL_LINES)
           (:points GL:GL_POINTS)
           (:surfaces GL:GL_LINES)))
       (gl-poly-agent (constituents)
         (ecase constituents
           (:lines :stroke)
           (:points :stroke)
           (:surfaces :fill))))
  (clrhash *opengl-path-properties-map*)
  (dolist (name *path-constituents*)
    (destructuring-bind (front back) name
      (let ((properties (make-opengl-path-properties
                         :name name
                         :polygon-primitive (gl-poly-primitive front)
                         :line-primitive (gl-line-primitive front)
                         :front-mode (gl-polymode front)
                         :back-mode (gl-polymode back)
                         :agent (gl-poly-agent front))))
        (setf (gethash name *opengl-path-properties-map*) properties)
        (setf (gethash front *opengl-path-properties-map*) properties)))))

(defun gl-path-properties-list ()
  (loop for properties being each hash-value of *opengl-path-properties-map*
        collect properties))


;;;
;;; the opengl context class

(defclass opengl-context (view-context double-sided-projection-context)
  ((%glcontext
    :initform nil
    :reader context-glcontext
    :documentation "bound to an *opengl-context* within a with-context form.")
   (full-screen-p
    :initform nil :initarg :full-screen-p
    :reader context-full-screen-p)
   (default-fonts
     :initform '((:courier-plain-10  ("courier" 10 :plain))
                 (:courier-bold-10   ("courier" 10 :bold))
                 (:courier-plain-12  ("courier" 12 :plain))
                 (:courier-bold-12   ("courier" 12 :bold))
                 (:fixed             ("monaco" 10 :plain))
                 (:small             ("monaco"  8  :plain))
                 (:times-bold-10     ("times" 10 :bold))
                 (:times-plain-10    ("times" 10 :plain))
                 (:times-bold-12     ("times" 12 :bold))
                 (:times-plain-12    ("times" 12 :plain)))
     :allocation :class))
  (:documentation
   "the projection context for opengl operations.
    it specializes caching-context to operate on opengl views.
    the coordinate-system management is passed through to the abstract class' implementation.
    general display lists are managed per glcontext on a view-independant basis.
    see find-display-list, define-font, and with-open-display-list. a per-view context display list is keyed by view.
    the display methods is specialized to generate a display list by delegating to  view-draw-display-list-contents"))


(defmethod initialize-instance ((instance opengl-context) &key)
  (call-next-method)
  (initialize-%glcontext instance))

#+digitool
(defmethod initialize-%glcontext ((context opengl-context))
  (with-slots (%glcontext full-screen-p) context
    (let ((%attributes (make-vector AGL_RGBA
                                    AGL_DOUBLEBUFFER 
                                    AGL_DEPTH_SIZE 32
                                    (if full-screen-p AGL_FULLSCREEN AGL_NONE)
                                    AGL_NONE)))
      ;; create context
      (rlet ((%GDhandleArray :handle))
        (%put-ptr %GDhandleArray (#_getMainDevice))
        (let ((%AGLPixelFormat (gl:aglChoosePixelFormat %GDhandleArray 1 %attributes)))
          (gl:agl-error-check "after pixel format")
          (#_disposePtr %attributes)
          (setf %glcontext (gl:aglCreateContext %AGLPixelFormat (%null-ptr)))
          ;; texture sharing works only when FIRST creating all contexts and THEN creating textures:
          ;; http://developer.apple.com/qa/qa2001/qa1031.html
          (gl:agl-error-check "context")
          (gl:aglDestroyPixelFormat %AGLPixelFormat)
          (gl:agl-error-check))))))

#-digitool
(nyi-error 'initialize-%glcontext)

(defmethod (setf context-view) ((new-value view) (context opengl-context))
  (unless (eq (get-context-view context) new-value)
    (unless (view-window new-value)     ; nb. for a window this is reflexive
      (error "view must be present in a window: ~s." new-value))
    (prog1 (call-next-method)
      (initialize-context-view context new-value))))
    
(defmethod (setf context-view) ((new-value null) (context opengl-context))
  (let ((existing (get-context-view context)))
    (unless (eq existing new-value)
      (unless (context-full-screen-p context)
        (error "context not full-screen: ~s." context))
      (prog1 (call-next-method)
        (initialize-context-view context new-value)))))

(defmethod terminate-context ((context opengl-context))
  "Make sure to call this method only if the aglContext is no longer used"
  (with-slots (%glcontext) context
    (when (and %glcontext (macptrp %glcontext) (not (%null-ptr-p %glcontext)))
      (gl:aglSetDrawable %glcontext (%null-ptr))
      (gl:agl-error-check)
      (gl:aglSetCurrentContext (%null-ptr))
      (gl:agl-error-check)
      (gl:aglDestroyContext %glcontext)
      (gl:agl-error-check))
    (setf %glcontext nil)))

(defmethod context-make-view ((context opengl-context)
                              &rest initargs
                              &key (view-parent nil)
                              (view-class (if view-parent *class.opengl-view* *class.opengl-window*))
                              &allow-other-keys)
  (format *trace-output* "~%context view class: ~s." view-class)
  (prog1 (apply #'call-next-method context
                :view-class view-class
                initargs)))

#+digitool
(defmethod context-update-for-view ((context opengl-context) (view ccl:view))
  ;; inside of with-opngl-context only
  (unless (and (eql (context-get context :view-size) (ccl:view-size view))
               (eql (context-get context :view-position) (ccl:view-position view)))
    (ccl:aglUpdateContext (context-glcontext context))))
  

(defGeneric initialize-context-view (context new-value)
  (:documentation
   "initializes the context's glcontext, esp the drawable, for the given view or window. view initialization includes setting up the buffer rectangle")
  (:method ((context opengl-context) (null null))
           )
  (:method ((context opengl-context) (window window))
           "where the entire window content is one glContext, set only the drawable"
           (with-slots (%glcontext) context
             (gl:aglSetDrawable %glcontext (#_GetWindowPort (wptr window)))
             (gl:agl-error-check "initialize-context-view#aglSetDrawable")
             (opengl-initialize-fonts context)
             ))
  (:method ((context opengl-context) (view view))
           "where a view, a part of a window, is mediated by a glContext, set
            the drawable and set the glContext's buffer rectangle."
           (with-slots (%glcontext) context
             (let ((window (view-window view))
                   (%old-glcontext nil))
               (unwind-protect
                 (progn (unless (eq %old-glcontext %glcontext)
                          (gl:aglSetCurrentContext %glcontext)
                          (gl:agl-error-check "initialize-context-view#aglSetCurrentContext"))
                        (gl:aglSetDrawable %glcontext (#_GetWindowPort (wptr window)))
                        (gl:agl-error-check "initialize-context-view#aglSetDrawable")
                        ;; adjust the buffer rect to view
                        (gl:aglEnable %glcontext AGL_BUFFER_RECT)
                        (gl:agl-error-check "initialize-context-view#aglEnable(AGL_BUFFER_RECT)")
                        (rlet ((%rect :opengl4iv
                                      (data 0) (point-h (view-position view))
                                      (data 1) (- (point-v (view-size window))
                                                  (point-v (view-position view))
                                                  (point-v (view-size view)))
                                      (data 2) (point-h (view-size view))
                                      (data 3) (point-v (view-size view))))
                          (gl:aglSetInteger %glcontext AGL_BUFFER_RECT %rect)
                          (gl:agl-error-check "initialize-context-view#aglSetInteger(AGL_BUFFER_RECT)")
                          (opengl-initialize-fonts context)
                          ;; ?! (#_DisposePtr %rect)
                          ))
                 (unless (unless (eq %old-glcontext %glcontext)
                           (gl:aglSetCurrentContext %old-glcontext)
                           (gl:agl-error-check "initialize-context-view#aglSetCurrentContext"))))))))

(defmethod context-view-p ((context opengl-context) (view view))
  #+digitool
  (when (ccl:wptr view) t))

;;;
;;; macros

(defmacro in-opengl-context ((context) &rest body)
  "generate a closure for the body and invoke call-with-opengl-view"
  #+ignore
  (flet ((check-opengl-context-parameter (parm)
           (or (member parm '(view context %glcontext))
               (and (consp parm) (consp (first parm))
                    (member (caar parm) '(:view :context :%glcontext)))
               (error "an invalid parameter was specified: ~s." parm))))
    (mapc #'check-opengl-context-parameter parameters))
  (let ((fn (gensym)))
    `(flet ((,fn () ,@body))
       (declare (dynamic-extent (function ,fn)))
       (call-in-opengl-context (function ,fn) ,context))))


(defun call-in-opengl-context (function context
                                 &key (view (context-view context))
                                      (flush-p t)
                                      (swap-p t))
  "assert the contexts's graphics context and execute the body.
   if the view is not current, then make it the context's current view,
   which initializes it and the graphics context as a side-effect.
   upon conclusion, restores the initial context and, according to flush-p and
   swap-p, flushes operations and/or swaps the buffers.
   checks for both existing values for *context-view* and *opengl-glcontext*,
   in order to avoid rebinding/resetting.
   also suppresses swapping if the context is already active.
   does _not_ rebind *projection-context* or any other dynamic variables!"
  (declare (dynamic-extent function))
  (flet ((set-context-view (view)
           (if (eq view (context-view context))
             view
             (setf (context-view context) view)))
         (finish (flush-p swap-p)
           ;; don't need to do both, as swapping flushes.
           (cond (swap-p (gl:aglSwapbuffers *opengl-glcontext*))
                 (flush-p (GL:GLFlush)))))
    (if (eq view *context-view*)
      ;; if the view is currently bound, everything is set up
      (unwind-protect (funcall function)
        (finish flush-p nil))
      ;; otherwise rebind the view and check the gl context
      (let* ((*context-view* view)
             (old-context (gl:aglGetCurrentContext))
             (%glcontext (context-glcontext context))
             (*context-path-constituents* *opengl-path-constituents*)
             (*context-path-effect* *opengl-path-effect*)
             (*context-path-rule* *opengl-path-rule*)
             (*context-path-properties* (gl-path-properties *context-path-constituents*)))
        (set-context-view view)
        (let ((*opengl-glcontext* %glcontext))
          (if (eq old-context %glcontext)
            ;; if the glcontext is current call the function with the view
            ;; and the glcontext (possibly re)bound
            (unwind-protect (funcall function)
              ;; but defer the swap
              (finish flush-p nil))
            ;; otherwise make the gl context current
            (unwind-protect
              (progn (gl:aglSetCurrentContext %glcontext)
                     (gl:agl-error-check)
                     (funcall function))
              (finish flush-p swap-p)
              (gl:aglSetCurrentContext old-context)
              (gl:agl-error-check))))))))


(defmethod call-with-projection-context
           ((function function) (context opengl-context)
            &key &allow-other-keys)
  "initialize the transforms fill the view with identity model andprojection.
   then delegate to the next method."
  (in-opengl-context (context)
    ;; if establishing this context for the first time, initialize it:
    ;; model/projection/view transforms
    (unless (member context *projection-context-stack*)
      (opengl-initialize-transforms))
    (call-next-method)))


(defmacro with-open-display-list ((&key id name (view '*context-view*)
                                        (context '*projection-context*))
                                  &rest body
                                  &environment env)
  "generate a new display list id, start it, execute the body to generate its content,
   and return the new id upon completion. if a name is provided, bind the display list
   within the active opengl view."
  (let ((list-id (gensym "DISPLAY-LIST"))
        (context-var (if (and (symbolp context) (eq context (macroexpand-1 context env)))
                       context (gensym "CONTEXT-"))))
    `(let* (,@(unless (eq context context-var) `((,context-var ,context)))
            (,list-id ,(if view
                         `(context-display-list ,context-var ,view
                                                ,(or name :view-display-list)
                                                :if-does-not-exist :create)
                         (if name
                           `(context-display-list ,context-var nil ,name
                                                :if-does-not-exist :create)
                           (if id
                             id
                             '(#_GLGenLists 1))))))
       (#_glNewList ,list-id GL:GL_COMPILE)
       ,@body
       (#_glEndList)
       ,list-id)))


(defmacro with-opengl-color (color &rest body)
  (let ((%old-color (gensym)))
    `(rlet ((,%old-color :opengl4dv))
       (#_glGetDoublev GL:GL_CURRENT_COLOR ,%old-color)
       (unwind-protect (progn (opengl-set-color ,color)
                              ,@body)
         (#_GLColor4dv ,%old-color)))))

(defmacro with-opengl4dv ((record location-vector) &rest body)
  `(with-coerced-variables ((location-vector ,location-vector))
     (rlet ((,record :opengl4dv
                     (data 0) (aref ,location-vector 0)
                     (data 1) (aref ,location-vector 1)
                     (data 2) (aref ,location-vector 2)
                     (data 3) (aref ,location-vector 3)))
       ,@body)))
       

(defmacro with-double-float-intensity-variables (variables &rest body)
  `(progn ,@(mapcar #'(lambda (var)
                        `(etypecase ,var
                           (double-float)
                           (short-float (setf ,var (float ,var 1.0d0)))
                           (integer (setf ,var (/ ,var 65535.0d0)))
                           (number (setf ,var (float ,var 1.0d0)))))
                    variables)
          (locally (declare (type double-float ,@variables)))
          ,@body))

(defun context-display-list (context owner name &key (if-does-not-exist :error))
  "returns the id associated with the (owner x name) key. if none is present,
   then if-does-not-exist processing either returns nil, creates a new id,
   or signals an error."
  (or (context-get context owner name)
      (ecase if-does-not-exist
        ((nil) nil)
        (:create (setf (context-get context owner name) (#_GLGenLists 1)))
        (:error (error "no display list: ~s: ~s: ~s." context owner name)))))

(defun (setf context-display-list) (display-list-id context owner name)
  (setf (context-get context owner name) display-list-id))

(defmethod define-font ((context opengl-context) font-spec name)
  (unless (context-display-list context :font name :if-does-not-exist nil)
    (in-opengl-context (context)
      (opengl-define-font context font-spec name))))

#+ccl
(defun opengl-define-font (context font-spec name)
  "define an opengl glyph list from the given font specification.
   extract the name and size from the spec, but compute the style mask separately.
   then map the family to an id and generate the glyph list."
  (multiple-value-bind (family font-size) (compute-font-parameters font-spec)
    (setf (get-font context name)
          (let ((font (#_GLGenLists 224))
                (font-family-id (ccl::with-pstrs ((%name family))
                                  (#_FMGetFontFamilyFromName %name)))
                (font-style-mask (compute-font-style-mask font-spec)))
            (when (eql font-family-id #$kInvalidFontFamily)
              (error "invalid font family: ~s." family))
            (gl:aglUseFont (context-glcontext context) 
                           #+ignore (font-number-from-name family) 
                           font-family-id
                           font-style-mask   ;; bitset: {bold, italic, underline, outline, shadow, condense, extend}
                           Font-Size  
                           32  ;; starting at the space
                           224  ;; number of characters in set
                           Font)
            (gl:agl-error-check "define-font#aglUseFont")
            font))))


(defun opengl-initialize-fonts (context)
  (loop for (id designator) in (context-default-fonts context)
        do (unless (ignore-errors (opengl-define-font context designator id))
             (warn "cannot load font: ~s: ~s."
                   context designator))))


(defGeneric initialize-display-lists (context view)
  (:documentation
   "generate inital display lists prior to displaying. invoked by an opengl-context when its view set.")
  (:method :around ((context opengl-context) (view view))
           (in-opengl-context (context)
             (opengl-initialize-transforms)
             (call-next-method)))
  (:method ((context opengl-context) (view view))
           "the general method does nothing except accept next method calls."
           ))

(defGeneric invalidate-display-list (context owner name)
  (:method ((context opengl-context) owner name)
           (let ((list-id (context-display-list context owner name :if-does-not-exist nil)))
             (when list-id
               (setf (context-display-list context owner name)
                     (- (abs list-id)))))))

(defGeneric opengl-draw-contents (opengl-context what)
  (:documentation
   "draw the given subject in the opengl context. if the object is associated with a display list, that list is used. if the list is defined, but invalid, generatio is delegated to the object.")
  (:method ((context opengl-context) (what t))
           (let* ((name (context-name context))
                  (list-id (context-display-list context what name :if-does-not-exist nil)))
             (cond (list-id
                    (unless (plusp list-id)
                      (setf list-id (abs list-id))
                      (opengl-initialize-transforms)
                      (with-open-display-list (:id list-id)
                        (opengl-draw-contents what name)))
                    (opengl-initialize-transforms)
                    (#_GLCallList list-id))
                   (t
                    (opengl-initialize-transforms)
                    (opengl-draw-contents what name))))))



;;
;; additional gl primitives

#+mcl
(defun opengl-call-string-lists (string font)
  (#_GLPushAttrib GL:GL_LIST_BIT)
  (#_GLListBase (- Font 32))
  (ccl::with-cstr (&String String)
    (#_GLCallLists (length String) GL:GL_UNSIGNED_BYTE &String))
  (#_GLPopAttrib))

#-mcl
(nyi-error 'opengl-call-string-lists)


(defGeneric opengl-call-display-list (id)
  (:method ((opengl-list-id integer))
           (#_GLCallList opengl-list-id))
  (:method ((list-designator t))
           (opengl-call-display-list
            (context-display-list *projection-context* *context-view* list-designator))))

(defun opengl-initialize-transforms ()
  "initialize the transforms fill the view with identity model and projection."
  (let ((size (view-size *context-view*)))
    (#_GLmatrixmode GL:GL_projection)
    (#_GLloadidentity)
    (#_GLmatrixmode GL:GL_modelview)
    (#_GLloadidentity)
    (#_GLViewport 0 0 (point-h size) (point-v size))))


;;;
;;; geometric elements
;;;
;;; these functions draw the basic geometric figures.
;;; NB: they do not take structure objects, rather they require the explicit
;;; points - either as instances or as spread coordinates.
;;; all locations are generic: locations, location vectors, or numbers

;;;
;;; geometric objects : lines

(defun opengl-line*3 (x1 y1 z1 x2 y2 z2 &optional aspects)
  "draw a line given the 3d location in object/world coordinates"
  (declare (optimize (speed 3) (safety 0)))
  #+og.assert-types (progn (assert-types (x1 y1 z1 x2 y2 z2) number)
                           (assert-type aspects (or sequence function)))
  ;(print (list :opengl-line*3 (#_GLGetError )))
  (etypecase y1
    (double-float (with-coerced-variables ((double-float x1 z1 x2 y2 z2))
                    (flet ((render-line ()
                             (opengl-set-color *opengl-stroke-color*)
                             (#_glBegin (opengl-path-properties-line-primitive *context-path-properties*))
                             (#_glVertex3d x1 y1 z1) (#_glVertex3d x2 y2 z2)
                             (#_glEnd))
                           #+ignore (render-line ()  x1 y1 z1 x2 y2 z2))
                 (declare (dynamic-extent #'render-line))
                 ;; makes little difference to avoid the call
                 (if aspects
                   (call-with-projection-variables #'render-line aspects)
                   (render-line)))))
    (short-float (with-coerced-variables ((short-float x1 z1 x2 y2 z2))
                   (flet ((render-line ()
                            (opengl-set-color *opengl-stroke-color*)
                            (#_glBegin (opengl-path-properties-line-primitive *context-path-properties*))
                            (#_glVertex3f x1 y1 z1) (#_glVertex3f x2 y2 z2)
                            (#_glEnd)))
                     (declare (dynamic-extent #'render-line))
                     (call-with-projection-variables #'render-line aspects))))
    (number (with-coerced-variables ((integer x1 z1 x2 y2 z2))
               (flet ((render-line ()
                        (opengl-set-color *opengl-stroke-color*)
                        (#_glBegin (opengl-path-properties-line-primitive *context-path-properties*))
                        (#_glVertex3i x1 y1 z1) (#_glVertex3i x2 y2 z2)
                        (#_glEnd)))
                 (declare (dynamic-extent #'render-line))
                 (call-with-projection-variables #'render-line aspects)))))
  
  ;;(print (list :opengl-line*3.end (#_GLGetError )))
  )

(defmethod context-line*3 ((context opengl-context) x1 y1 z1 x2 y2 z2 &optional aspects)
  "draw a line given two extreme 3d vertices"
  (opengl-line*3 x1 y1 z1 x2 y2 z2 aspects))

(defmethod context-line*2 ((context opengl-context) x1 y1 x2 y2 &optional aspects)
  "draw a line given two extreme 2d vertices"
  (opengl-line*3 x1 y1 0.0d0 x2 y2 0.0d0 aspects))

(defmethod context-line ((context opengl-context) l1 l2 &optional aspects)
  "draw a line given the end locations (2-d or 3-d) in object/world coordinates"
  (declare (optimize (speed 3) (safety 0)))
  (if (and (location-vector-p l1) (location-vector-p l2))
    (locally (declare (type location-vector l1 l2))
      (flet ((render-line ()
               (opengl-set-color *opengl-stroke-color*)
               (#_glBegin (opengl-path-properties-line-primitive *context-path-properties*))
               (#_glVertex3d (aref l1 0) (aref l1 1) (aref l1 2))
               (#_glVertex3d (aref l2 0) (aref l2 1) (aref l2 2))
               (#_glEnd)))
        (declare (dynamic-extent #'render-line))
        (call-with-projection-variables #'render-line aspects)))
    (with-location-coordinates (((x1 y1 z1) l1) ((x2 y2 l2) l2))
      (opengl-line*3 x1 y1 z1 x2 y2 l2 aspects))))



;;;
;;; geometric objects : rectangles

(defun opengl-rectangle*3 (x1 y1 z1 x2 y2 z2 &optional aspects)
  "draw a rectangle given the 3d location in object/world coordinates"
  (declare (optimize (speed 3) (safety 0)))
  #+og.assert-types (progn (assert-types (x1 y1 z1 x2 y2 z2) number)
                           (assert-type aspects (or sequence function)))
  (labels ((gl-rectangle-renderer ()
             (declare (dynamic-extent #'gl-rectangle-renderer))
             (flet ((generate-rectangle ()
                      (#_GLBegin (opengl-path-properties-polygon-primitive *context-path-properties*))
                      (typecase y1
                        (integer
                         (with-coerced-variables ((integer x1 z1 x2 y2 z2))
                           (#_GLVertex3i x1 y1 z1)
                           (#_GLVertex3i x2 y1 z1)
                           (#_GLVertex3i x2 y2 z2)
                           (#_GLVertex3i x1 y2 z2)
                           (#_GLVertex3i x1 y1 z1)))
                        (double-float
                         (with-coerced-variables ((double-float x1 z1 x2 y2 z2))
                           (#_GLVertex3d x1 y1 z1)
                           (#_GLVertex3d x2 y1 z1)
                           (#_GLVertex3d x2 y2 z2)
                           (#_GLVertex3d x1 y2 z2)
                           (#_GLVertex3d x1 y1 z1)))
                        (short-float
                         (with-coerced-variables ((short-float x1 z1 x2 y2 z2))
                           (#_GLVertex3f x1 y1 z1)
                           (#_GLVertex3f x2 y1 z1)
                           (#_GLVertex3f x2 y2 z2)
                           (#_GLVertex3f x1 y2 z2)
                           (#_GLVertex3f x1 y1 z1))))
                      (#_GLEnd)))
               (ecase *context-path-effect*
                 (:clear
                  (opengl-set-color *opengl-background-color*)
                  (generate-rectangle))
                 (:paint
                  (ecase (opengl-path-properties-agent *context-path-properties*)
                    (:fill (opengl-set-color *opengl-fill-color*))
                    (:stroke (opengl-set-color *opengl-stroke-color*)))
                  (generate-rectangle))
                 (:invert
                  (let ((*context-path-effect* :paint))
                    (gl-call-with-inverted-blend #'gl-rectangle-renderer)))
                 #+ignore
                 (:fill-stroke
                  (let ((*context-path-effect* :paint))
                    (let ((*context-path-properties* (gethash :surfaces *opengl-path-properties-map*)))
                      (gl-rectangle-renderer))
                    (let ((*context-path-properties* (gethash :lines *opengl-path-properties-map*)))
                      (gl-poly-renderer))))))))
    (declare (dynamic-extent #'gl-rectangle-renderer))
    (if aspects
      (call-with-projection-variables #'gl-rectangle-renderer aspects)
      (gl-rectangle-renderer))))

(defmethod context-rectangle*3 ((context opengl-context) x1 y1 z1 x2 y2 z2 &optional aspects)
  "draw a rectangle given two extreme 3d vertices"
  (opengl-rectangle*3 x1 y1 z1 x2 y2 z2 aspects))


(defmethod context-rectangle*2 ((context opengl-context) x1 y1 x2 y2 &optional aspects)
  "draw a rectangle given two extreme 2d vertices"
  (opengl-rectangle*3 x1 y1 0.0d0 x2 y2 0.0d0 aspects))

(defmethod context-rectangle ((context opengl-context) l1 l2 &optional aspects)
  "draw a rectangle given the end locations (2-d or 3-d) in object/world coordinates"
  (declare (optimize (speed 3) (safety 0)))
  (with-location-coordinates (((x1 y1 z1) l1) ((x2 y2 l2) l2))
    (opengl-rectangle*3 x1 y1 z1 x2 y2 l2 aspects)))



;;
;; geometric objects : vertex sequences

(defgeneric opengl-poly (locations &optional variables)
  (:documentation
   "render a sequence of points as a polygon/polyline in an opengl context.")

  (:method ((locations sequence) &optional (aspects nil))
           #+og.assert-types (progn (assert-types (aspects) (or sequence function))
                                    (assert-types (locations) sequence))
           (let* ((get-next-location nil)
                  (location-count (length locations))
                  (last-index -1)
                  (next-locations locations))
             (declare (type fixnum last-index location-count))
             (when (> location-count 0)
               (flet ((next-list-location ()
                        (locally (declare (type list locations))
                          (pop next-locations)))
                      (next-vector-location ()
                        (locally (declare (type vector locations))
                          (when (< (incf last-index) location-count)
                            (aref locations last-index)))))
                 (declare (dynamic-extent #'next-list-location #'next-vector-location))
                 (etypecase locations
                   (cons (setf get-next-location #'next-list-location))
                   (vector (setf get-next-location #'next-vector-location)))
                 (labels ((gl-poly-renderer (&aux (location nil))
                            (declare (dynamic-extent #'gl-poly-renderer))
                            (flet ((generate-poly ()
                                     (setf last-index -1 next-locations locations)
                                     (#_GLBegin (opengl-path-properties-polygon-primitive *context-path-properties*))
                                     (unwind-protect
                                       (loop (typecase (setf location (funcall get-next-location))
                                               (null (return))
                                               ((satisfies location-vector-p)
                                                (locally (declare (type location-vector location))
                                                  (with-mutable-double-floats (x y z)
                                                    (setf x (aref location 0) y (aref location 1) z (aref location 2))
                                                    (#_glVertex3d x y z))))
                                               (t
                                                (with-location-coordinates (((x y z) location))
                                                  (typecase y
                                                    (integer
                                                     (with-coerced-variables ((integer x z))
                                                       (#_GLVertex3i x y z)))
                                                    (double-float
                                                     (with-coerced-variables ((double-float x z))
                                                       (#_GLVertex3d x y z)))
                                                    (short-float
                                                     (with-coerced-variables ((short-float x z))
                                                       (#_GLVertex3f x y z))))))))
                                       (#_GLEnd))))
                              (ecase *context-path-effect*
                                (:clear
                                 (opengl-set-color *opengl-background-color*)
                                 (generate-poly))
                                (:paint
                                 (ecase (opengl-path-properties-agent *context-path-properties*)
                                   (:fill (opengl-set-color *opengl-fill-color*))
                                   (:stroke (opengl-set-color *opengl-stroke-color*)))
                                 (generate-poly))
                                (:invert
                                 (let ((*context-path-effect* :paint))
                                   (gl-call-with-inverted-blend #'gl-poly-renderer)))
                                #+ignore
                                (:fill-stroke
                                 (let ((*context-path-effect* :paint))
                                   (let ((*context-path-properties* (gethash :surfaces *opengl-path-properties-map*)))
                                     (gl-poly-renderer))
                                   (let ((*context-path-properties* (gethash :lines *opengl-path-properties-map*)))
                                     (gl-poly-renderer))))))))
                   (declare (dynamic-extent #'gl-poly-renderer))
                   (call-with-projection-variables #'gl-poly-renderer aspects))))))

  (:method ((locations function) &optional (aspects nil))
           #+og.assert-types (progn (assert-types (aspects) (or sequence function))
                                    (assert-types (locations) function))
           (labels ((gl-poly-renderer ()
                      (declare (dynamic-extent #'gl-poly-renderer))
                      (flet ((generate-poly ()
                               (flet ((gl-poly-vertex (location)
                                        (cond ((location-vector-p location)
                                               (locally (declare (type location-vector location))
                                                 (with-mutable-double-floats (x y z)
                                                    (setf x (aref location 0) y (aref location 1) z (aref location 2))
                                                    (#_glVertex3d x y z))))
                                              (t
                                               (with-location-coordinates (((x y z) location))
                                                 (typecase y
                                                   (integer
                                                    (with-coerced-variables ((integer x z))
                                                      (#_GLVertex3i x y z)))
                                                   (double-float
                                                    (with-coerced-variables ((double-float x z))
                                                      (#_GLVertex3d x y z)))
                                                   (short-float
                                                    (with-coerced-variables ((short-float x z))
                                                      (#_GLVertex3f x y z)))))))))
                                 (declare (dynamic-extent #'gl-poly-vertex))
                                 (unwind-protect
                                   (progn
                                     (#_GLBegin (opengl-path-properties-polygon-primitive *context-path-properties*))
                                     (funcall locations #'gl-poly-vertex))
                                   (#_GLEnd)))))
                        (ecase *context-path-effect*
                          (:clear
                           (opengl-set-color *opengl-background-color*)
                           (generate-poly))
                          (:paint
                           (ecase (opengl-path-properties-agent *context-path-properties*)
                             (:fill (opengl-set-color *opengl-fill-color*))
                             (:stroke (opengl-set-color *opengl-stroke-color*)))
                           (generate-poly))
                          (:invert
                           (let ((*context-path-effect* :paint))
                             (gl-call-with-inverted-blend #'gl-poly-renderer)))
                          #+ignore
                          (:fill-stroke
                           (let ((*context-path-effect* :paint))
                             (let ((*context-path-properties* (gethash :surfaces *opengl-path-properties-map*)))
                               (gl-poly-renderer))
                             (let ((*context-path-properties* (gethash :lines *opengl-path-properties-map*)))
                               (gl-poly-renderer))))))))
             (declare (dynamic-extent #'gl-poly-renderer))
             (call-with-projection-variables #'gl-poly-renderer aspects))))


(defmethod context-poly ((context opengl-context) locations &optional variables)
  "render a polygon through opengl.
   this is intended to be called as part of display list processing, which means
   that the view/port are already focused."
  (opengl-poly locations variables))


;;
;; draw geometric figures : arcs

  
(defun opengl-arc*3 (wx wy wz radius start end direction &optional aspects)
  #+og.assert-types (progn (assert-types (wx wy wz radius start end) number)
                           (assert-type aspects (or sequence function)))
   (with-coerced-variables ((double-float start end))
    (ecase direction
      (:clockwise (when (> end start) (decf end +2pi+))) ;;  (rotatef end start)))
      (:counterclockwise (when (< end start) (incf end +2pi+)))) ;; (rotatef end start))))
    ;; (print (list direction end start))
    (rotatef end start)

    (let* ((scalar-radius 0.0d0))
      (declare (type double-float scalar-radius)
               ;(dynamic-extent scalar-radius);; they overwrite 0.0
               )
      (etypecase radius
        ;; a scalar radius is used literally
        (number (setf scalar-radius radius))
        ;; a location vector has the magnitude calculated "in-place"
        (location-vector
         (locally (declare (type location-vector radius))
           (location-magnitude radius)
           (setf scalar-radius (aref radius 3))))
        ;; any other location returns the magnitude value
        ((or location-2 cons)
         (setf scalar-radius (location-magnitude radius))))
      (let* ((count (compute-arc-vertex-count scalar-radius start end))
             (d-theta (/ (- end start) count)))
        (declare (type double-float scalar-radius d-theta)
                 ; (dynamic-extent scalar-radius d-theta) ;; they overwrite 0.0
                 (type fixnum count))
        (labels ((gl-arc-renderer ()
                   (declare (dynamic-extent #'gl-arc-renderer))
                   (flet ((generate-arc ()
                            (let ((t-cos 0.0d0)
                                  (t-sin 0.0d0)
                                  (xi 0.0d0)
                                  (yi 0.0d0)
                                  (theta 0.0d0))
                              (declare (type double-float t-cos t-sin xi yi theta)
                                       (dynamic-extent t-cos t-sin xi yi theta))
                              (#_GLBegin (opengl-path-properties-polygon-primitive *context-path-properties*))
                              (dotimes (i (+ count 1))
                                (setf theta (+ start (* i d-theta)))
                                ;; theta)
                                (sin! theta t-sin)
                                (cos! theta t-cos)
                                (setf xi (+ wx (* scalar-radius t-cos))
                                      yi (+ wy (* scalar-radius t-sin)))
                                ;; (print (list xi yi wz))
                                (#_GLVertex3d xi yi wz))
                              (#_GLEnd))))
                     (ecase *context-path-effect*
                       (:clear
                        (opengl-set-color *opengl-background-color*)
                        (generate-arc))
                       (:paint
                        (ecase (opengl-path-properties-agent *context-path-properties*)
                          (:fill (opengl-set-color *opengl-fill-color*))
                          (:stroke (opengl-set-color *opengl-stroke-color*)))
                        (generate-arc))
                       (:invert
                        (let ((*context-path-effect* :paint))
                          (gl-call-with-inverted-blend #'gl-arc-renderer)))
                       #+ignore
                       (:fill-stroke
                        (let ((*context-path-effect* :paint))
                          (let ((*context-path-properties* (gethash :surfaces *opengl-path-properties-map*)))
                            (gl-arc-renderer))
                          (let ((*context-path-properties* (gethash :lines *opengl-path-properties-map*)))
                            (gl-arc-renderer))))))))
          (declare (dynamic-extent #'gl-arc-renderer))
          (call-with-projection-variables #'gl-arc-renderer aspects))))))


(defmethod context-arc*3 ((context opengl-context) wx wy wz radius start end direction &optional aspects)
  (opengl-arc*3 wx wy wz radius start end direction aspects))

(defmethod context-arc*2 ((context opengl-context) wx wy radius start end direction &optional aspects)
  (opengl-arc*3 wx wy 0.0d0 radius start end direction aspects))

(defmethod context-arc ((context opengl-context) location radius start end direction &optional aspects)
  (with-location-coordinates (((wx wy wz) location))
     (opengl-arc*3 wx wy wz radius start end direction aspects)))



;;;
;;; text

(defun opengl-text*3 (x y z string font aspects)
  #+og.assert-types (progn (assert-types (x y z) number)
                           (assert-type string string)
                           (assert-type aspects (or sequence function)))
  (typecase font
    (fixnum )
    (t (setf font (get-font *projection-context* font))))
  (flet ((render-text ()
           (opengl-set-color *opengl-stroke-color*)
           (etypecase y
             (integer
              (with-coerced-variables ((integer y z))
                (#_GLrasterpos3i x y z)))
             (double-float
              (with-coerced-variables ((double-float y z))
                (#_GLrasterpos3d x y z)))
             (short-float
              (with-coerced-variables ((short-float y z))
                (#_GLrasterpos3f x y z))))
           (when (plusp (length string))
             (opengl-call-string-lists string font))))
    (declare (dynamic-extent #'render-text))
    (call-with-projection-variables #'render-text aspects)))

(defmethod context-text*3 ((context opengl-context) xw yw zw text font &optional aspects)
  (opengl-text*3 xw yw zw text font aspects))

(defmethod context-text*2 ((context opengl-context) xw yw text font &optional aspects)
  (opengl-text*3 xw yw 0.0 text font aspects))

(defmethod context-text ((context opengl-context) location text font &optional aspects)
  (with-location-coordinates (((x y z) location))
    (opengl-text*3 x y z text font aspects)))



;;;
;;; raster

(defun opengl-raster*3 (x1 y1 z1 x2 y2 z2 raster &optional aspects (break-p nil))
  #+og.assert-types (progn (assert-types (x1 y1 z1 x2 y2 z2) number)
                           (assert-type raster raster)
                           (assert-type aspects (or sequence function)))
  (let* ((image (sample-projection raster *projection-context*)))
    (flet ((render-raster ()
             (opengl-set-color *opengl-fill-color*)
             (multiple-value-bind (sx1 sy1 sz1)
                                  (gl-model-to-screen x1 y1 z1)
               (multiple-value-bind (sx2 sy2 sz2)
                                    (gl-model-to-screen x2 y2 z2)
                 (when (< sx2 sx1) (rotatef sx1 sx2))
                 (when (< sy2 sy1) (rotatef sy1 sy2))
                 (when (< sz2 sz1) (rotatef sz1 sz2))
                 (multiple-value-bind (mx my mz)
                                      (gl-screen-to-model sx1 sy1 sz1)
                   ;; (print (list mx my mz))
                   (#_GLrasterpos3d mx my mz))
                 
                 (let* ((dx (- sx2 sx1)) (dy (- sy2 sy1))
                        (width (opengl-image-width image))
                        (height (opengl-image-height image))
                        (depth (opengl-image-depth image))
                        (xscale (/ dx width)) (yscale (/ dy height)))
                   (#_GLPixelZoom (float xscale 1.0s0) (float yscale 1.0s0))
                   ;; (break "before draw pixels")
                   (when break-p (break "before draw pixels"))
                   ;; save and restore the front mode
                   (rlet ((%polymode opengl4iv))
                     (#_GLGetIntegerv GL:GL_POLYGON_MODE %polymode)
                     (unwind-protect
                       (progn (#_GLPolygonMode GL:GL_FRONT GL:GL_FILL)
                              (#_GLDrawPixels width height
                               (ecase depth
                                 (8 GL:GL_COLOR_INDEX)
                                 (16 GL:GL_COLOR_INDEX)
                                 (32 GL:GL_RGBA))
                               (ecase depth
                                 (8 GL:GL_UNSIGNED_BYTE)
                                 (16 GL:GL_UNSIGNED_SHORT)
                                 (32 GL:GL_UNSIGNED_BYTE))
                               (opengl-image-data image)))
                       (#_GLPolygonMode GL:GL_FRONT (rref %polymode (opengl4iv.data 0)))
                       ;; always flush raster data
                       (#_GLFlush))))))))
      (declare (dynamic-extent #'render-raster))
      (call-with-projection-variables #'render-raster aspects))))

(defmethod context-raster*3 ((context opengl-context) x1 y1 z1 x2 y2 z2 raster &optional aspects)
  (opengl-raster*3 x1 y1 z1 x2 y2 z2 raster aspects))

(defmethod context-raster*2 ((context opengl-context) x1 y1 x2 y2 raster &optional aspects)
  "draw a sample array given the 2d coordinates"
  (opengl-raster*3 x1 y1 0.0 x2 y2 0.0 raster aspects))

(defmethod context-raster ((context opengl-context) l1 l2 raster &optional aspects)
  "draw a sample array given the location and size (2-d or 3-d)"
  (with-location-coordinates (((x1 y1 z1) l1) ((x2 y2 z2) l2))
    (opengl-raster*3 x1 y1 z1 x2 y2 z2 raster aspects)))

(defmethod terminate-relation ((owner raster) (key (eql 'opengl-image)) (image opengl-image))
  (let ((data (opengl-image-data image)))
    (when (and data (ccl:macptrp data) (not (ccl:%null-ptr-p data)))
      (#_DisposePtr data)
      (ccl:%setf-macptr data (ccl:%null-ptr)))))


(defmethod sample-projection ((raster raster) (context opengl-context))
  (let ((image (context-get context 'opengl-image raster)))
    (unless (opengl-image-p image)
      ;; instantiate a new image if none is present in the cache
      (let* ((size (sample-size raster))
             (width (point-h size))
             (height (point-v size))
             (depth (sample-depth raster)))
        (setf image
              (make-opengl-image :data nil
                                 :width width :height height :depth depth))
        (context-log-message context 'sample-projection "new image: ~s: ~s."
                             raster image)
        (setf (context-get context 'opengl-image raster) image)))
    (let* ((data (opengl-image-data image))
           (size (sample-size raster))
           (width (point-h size))
           (height (point-v size))
           (depth (sample-depth raster))
           (bytes-per-pixel (/ depth 8))
           (bytes-per-row (* width bytes-per-pixel)))
      ;; allocate new data for a new image instance
      (if (and data (ccl:macptrp data) (not (ccl:%null-ptr-p data)))
        (unless (= (ccl::pointer-size data) (* width height (/ depth 8)))
          (error "data does not match dimensions."))
        (let ((length (* height bytes-per-row)))
          (setf data (#_NewPtr length))
          (context-log-message context 'sample-projection "new data: ~s: ~s: ~s."
                               raster image data)
          (setf (opengl-image-data image) data)))
      (unless (test-projection-generation raster context)
        ;; (re)fill the data
        (let* ((sample-data (sample-data raster))
               (sample 0)
               (data-offset 0))
          (dotimes (i height)
            (setf data-offset (* i bytes-per-row))
            (dotimes (j width)
              (setf sample (sample-filter raster (aref sample-data i j) i j))
              ;(when (zerop j) (format *trace-output* " ~8,'0x" sample))
              (case depth
                (8 (%put-byte data sample data-offset))
                (16 (%put-word data sample data-offset))
                (32 (%put-long data sample data-offset)))
              (incf data-offset bytes-per-pixel)))
          ;; (format *trace-output* "~%image ~d bytes from ~s" length data)
          )))
    image))





;;; general utilities
;;;

(defun opengl-clear ()
  (with-coerced-variables ((location-vector *opengl-background-color*))
    (with-short-location-coordinates ((*opengl-background-color* r g b a))
      (#_GLClearColor r g b a)
      (#_GLClearDepth 0.0d0)
      (#_GLClear (logior GL:GL_COLOR_BUFFER_BIT GL:GL_DEPTH_BUFFER_BIT)))))

(defmethod context-clear-view ((context opengl-context))
  (opengl-clear))


(defun opengl-fill-view (color)
  (with-coerced-variables ((location-vector color))
    (with-short-location-coordinates ((color r g b a))
      (#_GLClearColor r g b a)
      (#_GLClear GL:GL_COLOR_BUFFER_BIT))))

(defmethod context-fill-view ((context opengl-context) &optional (fill *opengl-fill-color*))
  (typecase fill
    ((or vector sequence location-2)
     (opengl-fill-view fill))
    (raster )
    ))

(defun opengl-flush-view ()
  (GL:GLFlush)
  (gl:aglSwapbuffers *opengl-glcontext*)
  ;; #+ccl (agl-error-check)
  )

(defmethod context-flush-view  ((context opengl-context))
  (opengl-flush-view))

;;;
;;; properties
;;;
;;; colors: opengl maintains one color only. this is used to fill or stroke as specified by the pathmode
;;; the special bindings maintain values for fill, stroke, and backround 4dv equivalents, which are then
;;; applied as needed when rendering

(defmethod context-color*3 ((context opengl-context) r g b)
  `(color*3 ,r ,g ,b))

(defmethod context-color*4 ((context opengl-context) r g b a)
  `(color*4 ,r ,g ,b ,a))


;;; colors require parallel state as the mode-specific color cannot be read out
;;; (ie. there's no CGContextGetFillColor v/s CGContextGetLineColor)

(defun opengl-set-color (color)
  "intended to mediate switches among fill/stroke/background colors by checking
   if the color is current. if so, nothing happens. if not, then the update is
   cached and the color is set."
  ;;(declare (optimize (speed 3) (safety 0)))
  ;(unless t ; (equal color *opengl-color*)
  (setq *opengl-color* color)
  (with-coerced-variables ((location-vector color))
    ;; cache the last-used color and only set if it changes
    (with-mutable-double-floats (r g b a)
      (setf r (aref color 0)
            g (aref color 1)
            b (aref color 2)
            a (aref color 3))
      (rlet ((%new-color :opengl-color4d :red r :green g :blue b :alpha a))
        #+ignore (setf (rref %new-color opengl-color4d.red) r
              (rref %new-color opengl-color4d.green) g
              (rref %new-color opengl-color4d.blue) b
              (rref %new-color opengl-color4d.alpha) a)
        (#_GLColor4dv %new-color)))))

(defun opengl-get-color (&optional (color (location-vector)))
  ;;(declare (optimize (speed 3) (safety 0)))
  (with-coerced-variables ((location-vector color))
    (rlet ((%current-color :opengl-color4d))
      (#_glGetDoublev GL:GL_CURRENT_COLOR %current-color)
      #+mcl
      (with-mutable-double-floats (r g b a)
        (copy-record-to-double-coordinates %current-color r g b a)        
        (setf (aref color 0) r
              (aref color 1) g
              (aref color 2) b
              (aref color 3) a))
      #-mcl
      (setf (aref color 0) (rref %current-color :opengl-color4d.r)
            (aref color 1) (rref %current-color :opengl-color4d.g)
            (aref color 2) (rref %current-color :opengl-color4d.b)
            (aref color 3) (rref %current-color :opengl-color4d.a)))
    color))

(defun gl-call-with-color (function color)
  (rlet ((%saved-color :opengl-color4d))
    (#_glGetDoublev GL:GL_CURRENT_COLOR %saved-color)
    (unwind-protect (progn (opengl-set-color color)
                           (funcall function))
      (#_GLColor4dv %saved-color))))


;;;
;;; mode-specific operators
;;;
;;; fill

(defun opengl-fill-color*4 (r g b &optional (a 1.0d0))
  (declare (optimize (speed 3) (safety 0)))
  (with-double-float-intensity-variables (r g b a)
    (with-coerced-variables ((location-vector *opengl-fill-color*))
      (setf (aref *opengl-fill-color* 0) r
            (aref *opengl-fill-color* 1) g
            (aref *opengl-fill-color* 2) b
            (aref *opengl-fill-color* 3) a)
      (when (eq *opengl-color* *opengl-fill-color*)
        (setq *opengl-color* nil))
      *opengl-fill-color*)))

(defmethod context-fill-color*3 ((context opengl-context) r g b)
  (opengl-fill-color*4 r g b 1.0d0))

(defmethod context-fill-color*4 ((context opengl-context) r g b &optional (a 1.0d0))
  ; optional to allow call from fill-color w/o length check
  (opengl-fill-color*4 r g b a))

(defmethod context-set-fill-agent ((context opengl-context) color &optional g b (a 1.0d0))
  (etypecase color
    (number
     ;; default the color here as the interface funtion does not
     (opengl-fill-color*4 color g b (if a a 1.0d0)))
    (cons
     (apply #'opengl-fill-color*4 (if (symbolp (first color)) (rest color) color)))
    ((or vector location-3)
     (with-coerced-variables ((location-vector *opengl-fill-color*))
       (location-vector-copy color *opengl-fill-color*)
       (when (eq *opengl-color* *opengl-fill-color*)
         (setq *opengl-color* nil))
       *opengl-fill-color*))))

(defmethod context-save-fill-agent ((context opengl-context))
  (flet ((reset-fill-and-release-vector (context color)
           (setf (context-fill-agent context) color)
           (return-location-vector color)))
    (with-coerced-variables ((location-vector *opengl-fill-color*))
      (push-projection-variable #'reset-fill-and-release-vector context
                               (location-vector-copy *opengl-fill-color* (get-location-vector))))))


;;;
;;; stroke

(defun opengl-stroke-color*4 (r g b &optional (a 1.0d0))
  (declare (optimize (speed 3) (safety 0)))
  (with-double-float-intensity-variables (r g b a)
    (with-coerced-variables ((location-vector  *opengl-stroke-color*))
      (setf (aref *opengl-stroke-color* 0) r
            (aref *opengl-stroke-color* 1) g
            (aref *opengl-stroke-color* 2) b
            (aref *opengl-stroke-color* 3) a)
      (when (eq *opengl-color* *opengl-stroke-color*)
        (setq *opengl-color* nil))
      *opengl-stroke-color*)))

(defmethod context-color*3 ((context opengl-context) r g b)
  (opengl-stroke-color*4 r g b 1.0d0))

(defmethod context-color*4 ((context opengl-context) r g b a)
  (opengl-stroke-color*4 r g b a))

(defmethod context-set-stroke-agent ((context opengl-context) color &optional g b (a 1.0d0))
  (etypecase color
    (number
     (opengl-stroke-color*4 color g b (if a a 1.0d0)))
    (cons
     (apply #'opengl-stroke-color*4 (if (symbolp (first color)) (rest color) color)))
    ((or vector location-3)
     (with-coerced-variables ((location-vector *opengl-stroke-color*))
       (location-vector-copy color *opengl-stroke-color*)
       (when (eq *opengl-color* *opengl-stroke-color*)
        (setq *opengl-color* nil))
       *opengl-stroke-color*))))

(defmethod context-save-stroke-agent ((context opengl-context))
  (flet ((reset-stroke-and-release-vector (context color)
           (context-set-stroke-agent context color)
           (return-location-vector color)))
    (with-coerced-variables ((location-vector *opengl-stroke-color*))
      (push-projection-variable #'reset-stroke-and-release-vector context
                               (location-vector-copy *opengl-stroke-color* (get-location-vector))))))


;;;
;;; background

(defun opengl-set-background-color*4 (r g b &optional (a 1.0d0))
  (declare (optimize (speed 3) (safety 0)))
  (with-double-float-intensity-variables (r g b a)
    (with-coerced-variables ((location-vector *opengl-background-color*))
      (setf (aref *opengl-background-color* 0) r
            (aref *opengl-background-color* 1) g
            (aref *opengl-background-color* 2) b
            (aref *opengl-background-color* 3) a)
      (when (eq *opengl-color* *opengl-background-color*)
        (setq *opengl-color* nil))
      *opengl-background-color*)))
  
(defmethod context-set-background-color*3 ((context opengl-context) r g b)
  (opengl-set-background-color*4 r g b 1.0d0))

(defmethod context-set-background-color*4 ((context opengl-context) r g b &optional (a 1.0d0))
  (opengl-set-background-color*4 r g b a))

(defmethod context-set-clear-agent ((context opengl-context) color &optional g b (a 1.0d0))
  (etypecase color
    (number
     (opengl-set-background-color*4 color g b (if a a 1.0d0)))
    (cons
     (apply #'opengl-set-background-color*4 (if (symbolp (first color)) (rest color) color)))
    ((or vector location-3)
     (with-coerced-variables ((location-vector *opengl-background-color*))
       (location-vector-copy color *opengl-background-color*)
       (when (eq *opengl-color* *opengl-background-color*)
        (setq *opengl-color* nil))
       *opengl-background-color*))))

(defmethod context-save-clear-agent ((context opengl-context))
  (flet ((reset-background-and-release-vector (context color)
           (setf (context-clear-agent context) color)
           (return-location-vector color)))
    (with-coerced-variables ((location-vector *opengl-background-color*))
      (push-projection-variable #'reset-background-and-release-vector context
                                (location-vector-copy *opengl-background-color* (get-location-vector))))))


;;;
;;; path constituents and polygon mode

(defgeneric gl-path-properties (designator)
  (:method ((mode t))
           (or (gethash mode *opengl-path-properties-map*)
               (error "invalid path mode: ~s." mode)))
  (:method ((properties opengl-path-properties))
           properties))

(defgeneric gl-set-path-mode (mode)
  (:method ((mode t))
           (let ((properties (gl-path-properties mode)))
             (typecase properties
               (opengl-path-properties
                (gl-set-path-mode properties))
               (t
                (cerror "ignore and continue." "invalid path-mode: ~s." mode)))))
  (:method ((properties opengl-path-properties))
           (setq *context-path-constituents* (path-properties-name properties))
           (setq *context-path-properties* properties)
           (#_GLPolygonMode GL:GL_FRONT (opengl-path-properties-front-mode properties))
           (#_GLPolygonMode GL:GL_BACK (opengl-path-properties-back-mode properties))
           (#_GLGetError )))

(defmethod context-set-path-constituents ((context opengl-context) designator)
  (declare (ignore front-mode back-mode))
  (call-next-method)
  (gl-set-path-mode designator))

(defmethod context-save-path-constituents ((context opengl-context))
  ;; save just the properties as that recoreds the designator as well
  (push-projection-variable #'gl-set-path-mode
                            *context-path-properties*))


;;;
;;; color mode

(defun gl-call-with-inverted-blend (function)
  "set and enable blending such that the source factor is the inverted destination, 
   the source color is saturated, and the destination is ignored."
  (unwind-protect (let ((*opengl-color* *inverting-white-color*))
                    (#_GLPushAttrib (logior GL:GL_CURRENT_BIT GL:GL_COLOR_BUFFER_BIT))
                    (#_GLEnable GL:GL_BLEND)
                    (#_GLBlendFunc GL:GL_ONE_MINUS_DST_COLOR GL:GL_ZERO)
                    (#_GLColor4d 1.0d0 1.0d0 1.0d0 1.0d0)
                    (funcall function))
    (#_glPopAttrib)))

(defparameter *opengl-blending-factors* nil)
(let ((factors '(GL:GL_ZERO GL:GL_ONE GL:GL_DST_COLOR GL:GL_SRC_COLOR 
                 GL:GL_ONE_MINUS_DST_COLOR GL:GL_ONE_MINUS_SRC_COLOR GL:GL_SRC_ALPHA
                 GL:GL_ONE_MINUS_CONSTANT_ALPHA_EXT GL:GL_ONE_MINUS_CONSTANT_COLOR_EXT
                 GL:GL_DST_ALPHA  GL:GL_ONE_MINUS_DST_ALPHA  GL:GL_ONE_MINUS_SRC_ALPHA
                 GL:GL_SRC_ALPHA_SATURATE)))
  (flet ((factor-keyword (gl-name)
           (intern (substitute #\- #\_ (subseq (string gl-name) 3)) :keyword)))
    (setq *opengl-blending-factors* nil)
    (dolist (factor factors)
      (push (cons factor (symbol-value factor)) *opengl-blending-factors*))
    (dolist (factor factors)
      (push (cons (symbol-value factor) (symbol-value factor)) *opengl-blending-factors*))
    (dolist (factor factors)
      (push (cons (factor-keyword factor) (symbol-value factor)) *opengl-blending-factors*))))

(defparameter *opengl-destination-factors*
  `((,GL:GL_ONE . ,GL:GL_ZERO)
    (,GL:GL_ZERO . ,GL:GL_ONE)
    (,GL:GL_SRC_COLOR . ,GL:GL_DST_COLOR)
    (,GL:GL_SRC_ALPHA . ,GL:GL_ONE_MINUS_SRC_ALPHA)
    (,GL:GL_ONE_MINUS_DST_COLOR . ,GL:GL_ONE_MINUS_SRC_COLOR)
    (,GL:GL_DST_ALPHA . ,GL:GL_ONE_MINUS_DST_ALPHA)))
    

(defun gl-set-color-mode (mode src-factor-value dst-factor-value)
  (setq *opengl-color-mode* mode
        *opengl-color-source-factor* src-factor-value
        *opengl-color-destination-factor* dst-factor-value)
  (ecase mode
    ((:source :opaque)
     (#_GLDisable GL:GL_BLEND))
    ((:blend :alpha)
     (#_GLEnable GL:GL_BLEND)
     (#_GLBlendFunc src-factor-value dst-factor-value)))
  (#_GLGetError))

(defmethod context-set-color-mode ((context opengl-context) mode &rest args)
  (declare (dynamic-extent args))
  (destructuring-bind (&optional (source-factor GL:GL_SRC_ALPHA) destination-factor)
                      args
    (ecase mode
      ((:source :opaque)
       (gl-set-color-mode :opaque GL:GL_ONE GL:GL_ZERO))
      (:blend
       (gl-set-color-mode :blend
                          (or (setf source-factor
                                    (rest (assoc source-factor *opengl-blending-factors* :test #'eql)))
                              (error "invalid source factor: ~s." source-factor))
                          (if destination-factor
                            (or (rest (assoc destination-factor *opengl-blending-factors* :test #'eql))
                                (error "invalid destination factor: ~s." destination-factor))
                            (rest (assoc source-factor *opengl-destination-factors*)))))
      (:alpha
       (gl-set-color-mode :alpha
                          GL:GL_SRC_ALPHA
                          GL:GL_ONE_MINUS_SRC_ALPHA)))))

(defmethod context-save-color-mode ((context opengl-context))
  (push-projection-variable #'gl-set-color-mode
                           *opengl-color-mode*
                           *opengl-color-source-factor*
                           *opengl-color-destination-factor*))
  


;;;
;;; geometric transforms
;;;
;;; opengl has model and projection transforms and a viewport (not a transform)
;;; operations on two transforms are implemented through the initialize,
;;; scale/translate/rotate, catenate operations on transform matrices.
;;; the viewport transform uses the get/set viewport opengl interface and
;;; performs its own matrix manipulation.
;;; nb. opengl angles are degrees.

(defun opengl-modify-transform (matrix-mode op args)
  "modify the model or projection transform."
  (flet ((call-with-glmatrix (function transform-args)
           (rlet ((opengl-matrix :opengl-matrix))
             (etypecase (first transform-args)
               (number (let ((arg 0))
                         (do ((i 0 (1+ i)))
                             ((>= i 16))
                           (unless (setf arg (pop transform-args)) (setf arg 0.0d0))
                           (with-coerced-variables ((double-float arg))
                             (setf (rref opengl-matrix (opengl-matrix.data i)) arg)))))
               (array (let ((matrix (first args))
                            (i 0))
                        (declare (type fixnum i))
                        (assert-type matrix transform-matrix)
                        (locally (declare (type transform-matrix matrix))
                          (setf (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 0 0) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 0 1) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 0 2) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 0 3) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 1 0) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 1 1) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 1 2) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 1 3) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 2 0) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 2 1) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 2 2) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 2 3) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 3 0) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 3 1) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 3 2) i (+ 1 i)
                                (rref opengl-matrix (opengl-matrix.data i)) (aref matrix 3 3))))))
             (funcall function opengl-matrix))))
    (ecase op
      (:scale (destructuring-bind (x &optional y z) args
                (spread-optional-coordinates_ x y z)
                (with-coerced-variables ((double-float x y z))
                  (#_GLMatrixMode matrix-mode)
                  (#_GLScaleD x y z))))
      (:translate (destructuring-bind (x &optional y z) args
                    (spread-optional-coordinates_ x y z)
                    (with-coerced-variables ((double-float x y z))
                      ;; (print (list op matrix-mode x y z))
                      (#_GLMatrixMode matrix-mode)
                      (#_GLTranslateD x y z))))
      (:rotate (destructuring-bind (theta &optional (x 0.0d0) (y 0.0d0) (z 1.0d0)) args
                 (spread-optional-coordinates_ x y z)
                 (with-coerced-variables ((double-float theta x y z))
                   (setf theta (radians-to-degrees theta))
                   (#_GLMatrixMode matrix-mode)
                   (#_GLRotateD theta x y z))))
      (:catenate (call-with-glmatrix #'(lambda (glmatrix)
                                         (#_GLMatrixMode matrix-mode)
                                         (#_GLMultMatrixD glmatrix))
                                     args))
      (:set
       (call-with-glmatrix #'(lambda (glmatrix)
                               (#_GLMatrixMode matrix-mode)
                               (#_GLLoadIdentity)
                               (when (eql matrix-mode GL:GL_PROJECTION)
                                 ;; add in transform from (-1,-1)x(1,1) to (0,0)x(1,1)
                                 (rlet ((init-matrix :opengl-matrix))
                                   (setf (rref init-matrix (opengl-matrix.data 0)) 2.0
                                         (rref init-matrix (opengl-matrix.data 1)) 0.0
                                         (rref init-matrix (opengl-matrix.data 2)) 0.0
                                         (rref init-matrix (opengl-matrix.data 3)) 0.0
                                         (rref init-matrix (opengl-matrix.data 4)) 0.0
                                         (rref init-matrix (opengl-matrix.data 5)) 2.0
                                         (rref init-matrix (opengl-matrix.data 6)) 0.0
                                         (rref init-matrix (opengl-matrix.data 7)) 0.0
                                         (rref init-matrix (opengl-matrix.data 8)) 0.0
                                         (rref init-matrix (opengl-matrix.data 9)) 0.0
                                         (rref init-matrix (opengl-matrix.data 10)) 1.0
                                         (rref init-matrix (opengl-matrix.data 11)) 0.0
                                         (rref init-matrix (opengl-matrix.data 12)) -1.0
                                         (rref init-matrix (opengl-matrix.data 13)) -1.0
                                         (rref init-matrix (opengl-matrix.data 14)) 0.0
                                         (rref init-matrix (opengl-matrix.data 15)) 1.0)
                                   (#_GLMultMatrixD init-matrix)))
                               (#_GLMultMatrixD glmatrix))
                           args))
      (:clear (#_GLMatrixMode matrix-mode)
              (#_GLLoadIdentity)))))


(defmethod context-set-model-transform ((context opengl-context) op &rest args)
  (declare (dynamic-extent args))
  (opengl-modify-transform GL:GL_MODELVIEW op args))

(defmethod context-save-model-transform ((context opengl-context))
  (flet ((reset-model-transform-and-release (context matrix)
           (context-set-model-transform context :set matrix)
           (return-matrix matrix)))
    (let ((matrix (get-matrix)))
      ;; save the current context view matrix
      (opengl-get-transform-matrix GL:GL_MODELVIEW_MATRIX matrix)
      (push-projection-variable #'reset-model-transform-and-release
                                 context matrix))))


(defmethod context-set-projection-transform ((context opengl-context) op &rest args)
  (declare (dynamic-extent args))
  (opengl-modify-transform GL:GL_PROJECTION op args))

(defmethod context-save-projection-transform ((context opengl-context))
  (flet ((reset-projection-transform-and-release (context matrix)
           (context-set-projection-transform context :set matrix)
           (return-matrix matrix)))
    (let ((matrix (get-matrix)))
      ;; save the current context view matrix
      (opengl-get-transform-matrix GL:GL_PROJECTION_MATRIX matrix)
      (push-projection-variable #'reset-projection-transform-and-release
                                context matrix))))

(defun opengl-get-transform-matrix (type matrix)
  (declare (type transform-matrix matrix))
  (assert-type matrix transform-matrix)
  (let ((i 0))
      (declare (type fixnum i))
      (rlet ((opengl-matrix :opengl-matrix))
        (#_GLGetDoublev type opengl-matrix)
        (setf (aref matrix 0 0) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 0 1) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 0 2) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 0 3) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 1 0) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 1 1) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 1 2) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 1 3) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 2 0) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 2 1) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 2 2) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 2 3) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 3 0) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 3 1) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 3 2) (rref opengl-matrix (opengl-matrix.data i)) i (+ 1 i)
              (aref matrix 3 3) (rref opengl-matrix (opengl-matrix.data i)))))
  matrix)



(defmethod context-set-view-transform ((context opengl-context) op &rest args)
  "accept an operation indicator and appropriate arguments.
   operations like :clear require no arguments.
   operations like :scale accept either a location or spread coordinates.
   operations like :set accept a mtrix."
  (flet ((call-with-transform (function transform-args)
           (etypecase (first transform-args)
             (number (if (> (length transform-args) 3)
                       (with-matrices ((view-transform))
                         (apply #'matrix-fill view-transform transform-args)
                         (funcall function view-transform))
                       (with-location-vectors ((location-vector))
                         (apply #'location-vector-set location-vector transform-args)
                         (funcall function location-vector))))
             (location-vector (funcall function (first transform-args)))
             (location-2 (with-location-vectors (location-vector)
                           (location-vector-copy (first transform-args) location-vector)
                           (funcall function location-vector)))
             (transform-matrix (let ((matrix (first transform-args)))
                                 (funcall function matrix)))))
         (copy-viewport-to-transform (%viewport matrix)
           ;; extract the 2d matrix elements.
           ;; normalize width and height to account for ndc coordinates with centered origin
           (matrix-fill matrix
                        (float (rref %viewport GLViewportTransform.width) 1.0d0)
                        (float (rref %viewport GLViewportTransform.height) 1.0d0)
                        (float (rref %viewport GLViewportTransform.x) 1.0d0)
                        (float (rref %viewport GLViewportTransform.y) 1.0d0)))
         (copy-transform-to-viewport (matrix %viewport)
           (declare (type transform-matrix matrix))
           ;; extract the 2d matrix elements.
           ;; normalize width and height to account for ndc coordinates with centered origin
           ;; (terpri)(print-matrix matrix *trace-output*)

           (setf (rref %viewport GLViewportTransform.width) (%round (aref matrix 0 0))
                 (rref %viewport GLViewportTransform.height) (%round (aref matrix 1 1))
                 (rref %viewport GLViewportTransform.x) (max (%round (aref matrix 3 0)) 0)
                 (rref %viewport GLViewportTransform.y) (max (%round (aref matrix 3 1)) 0))

           ))
    (case op
      (:scale
       (call-with-transform
        #'(lambda (transform)
            (rlet ((%viewport :GLViewportTransform))
              (#_GLGetIntegerv GL:GL_VIEWPORT %viewport)
              (with-matrices ((viewport-matrix) (new-matrix))
                (copy-viewport-to-transform %viewport (matrix-initialize viewport-matrix))
                (etypecase transform
                  (location-vector
                   (matrix-scale transform viewport-matrix new-matrix))
                  (transform-matrix
                   (matrix-catenate viewport-matrix transform new-matrix)))
                (copy-transform-to-viewport new-matrix %viewport)
                (#_GLViewport (rref %viewport GLViewportTransform.x)
                   (rref %viewport GLViewportTransform.y)
                   (rref %viewport GLViewportTransform.width)
                   (rref %viewport GLViewportTransform.height)))))
        args))
      (:translate
       (call-with-transform
        #'(lambda (transform)
            (rlet ((%viewport :GLViewportTransform))
              (#_GLGetIntegerv GL:GL_VIEWPORT %viewport)
              (with-matrices ((viewport-matrix) (new-matrix))
                (copy-viewport-to-transform %viewport (matrix-initialize viewport-matrix))
                (etypecase transform
                  (location-vector
                   (matrix-translate transform viewport-matrix new-matrix))
                  (transform-matrix
                   (matrix-catenate viewport-matrix transform new-matrix)))
                (copy-transform-to-viewport new-matrix %viewport)
                (#_GLViewport (rref %viewport GLViewportTransform.x)
                              (rref %viewport GLViewportTransform.y)
                              (rref %viewport GLViewportTransform.width)
                              (rref %viewport GLViewportTransform.height)))))
        args))
      (:rotate
       (warn "NYI: viewport transform :rotate. post-catenated onto the perspective matrix instead."))
      (:catenate
       (call-with-transform
        #'(lambda (transform)
            (rlet ((%viewport :GLViewportTransform))
              (#_GLGetIntegerv GL:GL_VIEWPORT %viewport)
              (with-matrices ((viewport-matrix) (new-matrix))
                (copy-viewport-to-transform %viewport (matrix-initialize viewport-matrix))
                (etypecase transform
                  (location-vector
                   ;; use it as (sx sy tx ty) to initialize a matrix
                   (with-matrices ((added-matrix))
                     (matrix-fill added-matrix
                                  (aref transform 0) (aref transform 1) (aref transform 2) (aref transform 3))
                     (matrix-catenate viewport-matrix added-matrix new-matrix)))
                  (transform-matrix
                   (matrix-catenate viewport-matrix transform new-matrix)))
                (copy-transform-to-viewport new-matrix %viewport)
                (#_GLViewport (rref %viewport GLViewportTransform.x)
                              (rref %viewport GLViewportTransform.y)
                              (rref %viewport GLViewportTransform.width)
                              (rref %viewport GLViewportTransform.height)))))
        args))
      (:set
       (call-with-transform
        #'(lambda (transform)
            (let ((size (view-size *context-view*)))
              (rlet ((%viewport :GLViewportTransform :x 0 :y 0 :width (point-h size) :height (point-v size)))
              (etypecase transform
                (location-vector
                 (with-matrices ((new-matrix))
                   (matrix-fill new-matrix
                                (aref transform 0) (aref transform 1) (aref transform 2) (aref transform 3))
                   (copy-transform-to-viewport new-matrix %viewport)))
                (transform-matrix
                 (copy-transform-to-viewport transform %viewport)))
              (#_GLViewport (rref %viewport GLViewportTransform.x)
                            (rref %viewport GLViewportTransform.y)
                            (rref %viewport GLViewportTransform.width)
                            (rref %viewport GLViewportTransform.height)))))
        args))
      (:clear (let ((size (view-size *context-view*)))
                (#_GLViewport 0 0 (point-h size) (point-v size))
                ;; (print (context-state))
                )))
    (#_GLGetError)))

(defmethod context-save-view-transform ((context opengl-context))
  (flet ((reset-view-transform (x y w h)
           (#_GLViewport x y w h)))
    (rlet ((opengl-matrix :GLViewportTransform))
      (#_GLGetIntegerv GL:GL_VIEWPORT opengl-matrix)
      (push-projection-variable #'reset-view-transform
                                (rref opengl-matrix GLViewportTransform.x)
                                (rref opengl-matrix GLViewportTransform.y)
                                (rref opengl-matrix GLViewportTransform.width)
                                (rref opengl-matrix GLViewportTransform.height)))))



(defun opengl-model-transform-list (%transform)
  (list (list (rref %transform (GLModelTransform.data 0))
              (rref %transform (GLModelTransform.data 1))
              (rref %transform (GLModelTransform.data 2))
              (rref %transform (GLModelTransform.data 3)))
        (list (rref %transform (GLModelTransform.data 4))
              (rref %transform (GLModelTransform.data 5))
              (rref %transform (GLModelTransform.data 6))
              (rref %transform (GLModelTransform.data 7)))
        (list (rref %transform (GLModelTransform.data 8))
              (rref %transform (GLModelTransform.data 9))
              (rref %transform (GLModelTransform.data 10))
              (rref %transform (GLModelTransform.data 11)))
        (list (rref %transform (GLModelTransform.data 12))
              (rref %transform (GLModelTransform.data 13))
              (rref %transform (GLModelTransform.data 14))
              (rref %transform (GLModelTransform.data 15)))))

(defun opengl-projection-transform-list (%transform)
  (list (list (rref %transform (GLProjectionTransform.data 0))
              (rref %transform (GLProjectionTransform.data 1))
              (rref %transform (GLProjectionTransform.data 2))
              (rref %transform (GLProjectionTransform.data 3)))
        (list (rref %transform (GLProjectionTransform.data 4))
              (rref %transform (GLProjectionTransform.data 5))
              (rref %transform (GLProjectionTransform.data 6))
              (rref %transform (GLProjectionTransform.data 7)))
        (list (rref %transform (GLProjectionTransform.data 8))
              (rref %transform (GLProjectionTransform.data 9))
              (rref %transform (GLProjectionTransform.data 10))
              (rref %transform (GLProjectionTransform.data 11)))
        (list (rref %transform (GLProjectionTransform.data 12))
              (rref %transform (GLProjectionTransform.data 13))
              (rref %transform (GLProjectionTransform.data 14))
              (rref %transform (GLProjectionTransform.data 15)))))

(defun opengl-viewport-transform-list (%transform)
  (list (rref %transform (GLViewportTransform.data 0))
        (rref %transform (GLViewportTransform.data 1))
        (rref %transform (GLViewportTransform.data 2))
        (rref %transform (GLViewportTransform.data 3))))

(defmacro with-opengl-transform-state ((variable) &rest body)
  `(rlet ((,variable opengl-transform-state))
     (#_glGetDoublev GL:GL_MODELVIEW_MATRIX (rref ,variable opengl-transform-state.model))
     (#_glGetDoublev GL:GL_PROJECTION_MATRIX (rref ,variable opengl-transform-state.projection))
     (#_glGetIntegerv GL:GL_VIEWPORT (rref ,variable opengl-transform-state.viewport))
     ,@body))


(defgeneric print-view-transforms (context)
  (:method ((context ccl::opengl-simple-view))
            (aglSetCurrentContext (aglcontext context))
            (agl-error-check)
            (with-opengl-transform-state (x-state)
              (format *trace-output* "~%model: ~s~%projection: ~s~%viewport: ~s"
                      (opengl-model-transform-list (rref x-state opengl-transform-state.model))
                      (opengl-projection-transform-list (rref x-state opengl-transform-state.projection))
                      (opengl-viewport-transform-list (rref x-state opengl-transform-state.viewport)))))
  (:method ((context opengl-context))
           (with-projection-context (context)
             (with-opengl-transform-state (x-state)
              (format *trace-output* "~%model: ~s~%projection: ~s~%viewport: ~s"
                      (opengl-model-transform-list (rref x-state opengl-transform-state.model))
                      (opengl-projection-transform-list (rref x-state opengl-transform-state.projection))
                      (opengl-viewport-transform-list (rref x-state opengl-transform-state.viewport)))))))

(defun gl-model-to-screen (x y z)
  "compute screen coordinates from model coordinates on the basis of the current context's
   model, projection, and view transforms.
   results are not rounded, as the reverse process will require doubles."
  (with-opengl-transform-state (x-state)
    (with-coerced-variables ((double-float x y z))
      (rlet ((result opengl-3dv))
        (when (= GL:GL_TRUE (#_GLUProject x y z
                             (rref x-state opengl-transform-state.model)
                             (rref x-state opengl-transform-state.projection)
                             (rref x-state opengl-transform-state.viewport)
                             result (ccl:%inc-ptr result 8) (ccl:%inc-ptr result 16)))
          (values (rref result opengl-3dv.x)
                  (rref result opengl-3dv.y)
                  (rref result opengl-3dv.z)))))))

(defun gl-screen-to-model (x y z)
  "compute model coordinates from screen coordinates on the basis of the current context's
   model, projection, and view transforms."
  (with-opengl-transform-state (x-state)
    (with-coerced-variables ((double-float x y z))
      (rlet ((result opengl-3dv))
        (let ((status (#_gluUnproject x y z
                       (rref x-state opengl-transform-state.model)
                       (rref x-state opengl-transform-state.projection)
                       (rref x-state opengl-transform-state.viewport)
                       result (ccl:%inc-ptr result 8) (ccl:%inc-ptr result 16))))
          (cond ((= GL:GL_TRUE status)
                 (values (rref result opengl-3dv.x) (rref result opengl-3dv.y) (rref result opengl-3dv.z)))
                (t
                 (error "reverse transform failed."))))))))


(defun call-with-transformed-mouse (view x y z  tracking-function
                                    &optional (return-p #+ccl #'(lambda () (not (ccl:mouse-down-p)))
                                                        #-ccl (error "return predicate required.")))
  "given a model point, project it to the screen, then displace the transformed point
   to follow the mouse, and for each movement, transform the translated point back to model
   space and apply the tracking function to the model coordinates."
  (with-opengl-transform-state (x-state)
    (rlet ((result opengl-3dv))
      ;; first get the screen coordinate of the origin
      (cond ((/= GL:GL_TRUE (#_gluProject x y z
                                     (rref x-state opengl-transform-state.model)
                                     (rref x-state opengl-transform-state.projection)
                                     (rref x-state opengl-transform-state.viewport)
                                     result (ccl:%inc-ptr result 8) (ccl:%inc-ptr result 16)))
             (warn "can't project origin to screen.")
             #+ccl (ccl:ed-beep)
             nil)
            (t
             (let ((mouse (ccl:view-mouse-position view))
                   (new-mouse (make-point 0 0))
                   (sx (rref result opengl-3dv.x))
                   (sy (rref result opengl-3dv.y))
                   (sz (rref result opengl-3dv.z)))
               (loop (when (funcall return-p) (return))
                     (setf new-mouse (ccl:view-mouse-position view))
                     (unless (= mouse new-mouse)
                       (when (= GL:GL_TRUE (#_GLUUnproject (incf sx (- (point-h new-mouse) (point-h mouse)))
                                                      (incf sy (- (point-v new-mouse) (point-v mouse)))
                                                      sz
                                                      (rref x-state opengl-transform-state.model)
                                                      (rref x-state opengl-transform-state.projection)
                                                      (rref x-state opengl-transform-state.viewport)
                                                      result (ccl:%inc-ptr result 8) (ccl:%inc-ptr result 16)))
                         (funcall tracking-function
                                  (- (rref result opengl-3dv.x) x)
                                  (- (rref result opengl-3dv.y) y)
                                  (- (rref result opengl-3dv.z) z))
                         (setf x (rref result opengl-3dv.x)
                               y (rref result opengl-3dv.y)
                               z (rref result opengl-3dv.z)))
                       (setf mouse new-mouse)))))))))

#+ignore
(defmethod TRACK-MOUSE-SPIN ((camera orthographic-camera) (Gain t))
  "could rotate the view around the view origin."
  #+ccl (ccl:ed-beep)
  nil)


;;;
;;; debugging

(defmethod projection-context-state ((context opengl-context))
  (flet ((compute-state (&key &allow-other-keys)
           (let ((viewport nil)
                 (model-transform (opengl-get-transform-matrix GL:GL_MODELVIEW_MATRIX
                                                               (transform-matrix)))
                 (projection-transform (opengl-get-transform-matrix GL:GL_PROJECTION_MATRIX
                                                                    (transform-matrix)))
                 (properties nil))
             (rlet ((%viewport :GLViewportTransform)
                    (%modes :opengl4iv)
                    (%color :opengl-color4d))
               (#_GLGetIntegerv GL:GL_VIEWPORT %viewport)
               (setf (getf viewport :y) (rref %viewport GLViewportTransform.y)
                     (getf viewport :x) (rref %viewport GLViewportTransform.x)
                     (getf viewport :height) (rref %viewport GLViewportTransform.height)
                     (getf viewport :width ) (rref %viewport GLViewportTransform.width))
               (#_GLGetIntegerv GL:GL_POLYGON_MODE %modes)
               (setf (getf properties :front-mode) (rref %modes (opengl4iv.data 0))
                     (getf properties :back-mode) (rref %modes (opengl4iv.data 1)))
               (#_GLGetDoublev GL:GL_COLOR_CLEAR_VALUE %color)
               (setf (getf properties :clear-color)
                     `(color*4 ,(rref %color opengl-color4d.red)
                               ,(rref %color opengl-color4d.green)
                               ,(rref %color opengl-color4d.blue)
                               ,(rref %color opengl-color4d.alpha)))
               
               (#_GLGetDoublev GL:GL_CURRENT_COLOR %color)
               (setf (getf properties :color)
                     `(color*4 ,(rref %color opengl-color4d.red)
                               ,(rref %color opengl-color4d.green)
                               ,(rref %color opengl-color4d.blue)
                               ,(rref %color opengl-color4d.alpha))))
             (list* :model-transform (matrix-list model-transform)
                    :projection-transform (matrix-list projection-transform)
                    :viewport viewport
                    :properties properties))))
    (declare (dynamic-extent #'compute-state))
    ;; use in-opengl-context to get unchanged dynamic state
    (call-in-opengl-context #'compute-state context
                              :flush-p nil :swap-p nil)))

#|

transform state xamples:

(defparameter *Window* (make-instance 'opengl-window))

(maphash #'(lambda (k p) (print (cons k p))) *opengl-path-properties-map*)

(defparameter *path-combined-constituents-map*
  `((:erase  (t . :erase))
    (:fill   (t . :fill))
    (:fill-stroke   (t . :fill-stroke))
    (:invert (t . :invert))
    (:point  (:erase . :erase) (:fill . :fill) (:stroke . :stroke)
             (:invert . :invert) (:point . :point) (:fill-stroke :fill-stroke))
    (:stroke  (:erase . :erase) (:fill . :fill) (:stroke . :stroke)
              (:invert . :invert) (:point . :stroke) (:fill-stroke :fill-stroke)))
  "specifies the combined path mode given front and back modes.")

(defparameter *path-map-normalization-map*
  '((:erase . :erase)
    (:eofill . :fill)
    (:eofill-stroke . :fill-stroke)
    (:fill . :fill)
    (:fill-stroke . :fill-stroke)
    (:invert . :invert)
    (:point . :point)
    (:stroke  . :stroke)))
|#

#| avoided as it conflicts with the macro for universal names

;;; repair the agentsheets vector reader-maro

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolic-vector-reader (Stream Char)
    (declare (ignore Char))
    (let ((elements nil))
      (loop
        (let ((Char (peek-char t Stream nil #\} t)))
          (case Char
            (#\} (read-char stream) (return `(make-vector ,@(reverse elements))))
            (t (push (read Stream) elements)))))))
  
  
  (set-macro-character #\{ 'symbolic-vector-reader)
  (set-syntax-from-char #\} #\))
  )

(set-syntax-from-char #\} #\} *readtable* (copy-readtable nil))
(set-syntax-from-char #\{ #\{ *readtable* (copy-readtable nil))
|#

:EOF
