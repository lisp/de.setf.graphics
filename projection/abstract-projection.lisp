;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)

(document "Define the rendering projection interface and abstract operators for the de.setf.graphics library."

  (copyright "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
             "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  (history
   (delta 20061008 "janderson" "added projection-controller to the abstract implementation
   as basis for event handling for digitool and clx bases.")
   (delta 20050617 "janderson" "changed context-get signature to make the owner an optional
   third parameter")
   (delta 20050512 "janderson@ravenpack.com" "reformulated and documented the abstract interface;
   projection operators for functions include the operator name as the first arg.")
   (delta 20040530 "james.anderson@setf.de" "settled on a compromise between the customary context-as-initial-dispatching arg and
   the wide-bound-implementation-functions mechanisms which retains the display-list / display-model
   duality: the api functions are defined at three levels
   - the abstract implementation function `_op_` delegates to an
     implementation function `context-_op_`. it uses the dynamic binding
     of the *projection-context* variable as the initial argument.
   - the implementation function context-op dispatches on at least
     the initial context argument, handles coordinate system and property
     domain transforms, and delegates further to graphics-engine-specific
     functions. `(setf context-_op_)` functions pepend the new value (or
     initial component thereof) before the context's dispatching argument.
   - the implementation function as expressed in the base library's coordinate
     and property system. these may or may not require the context as an
     initial argument depending on whether the base library expects a static
     or a function-specific context. 
   any operations require an initial context establishment - independent of
   individual function calls. this avoids the repeated context establishment
   around primitive rendering operations.")
   (delta 20030905 "james.anderson@setf.de"))
  
  (long-description
   "This file  defines abstract functions for rendering operations. these serve both as descriptions of
display models and as instructions for rendering. in the first case the operation names serve as node
labels for a list-based scene graph and the properties are the leaves. in the second case the operations
names designate functions which are specialized for each display context and the properties are the
arguments. the forms are interpreted in the presence of a dynamically bound display contrext, which is
used to dispatch the operator's specialized implementation. if no binding is found for a given
operation, the operation is ignored.</p>
 <p> the interface supports two kinds of model components
   <ul>
    <li>operations: rendering functions for shapes, surfaces, etc.</li>
    <li>inflections: displa state variables, which can be manipulated to
    change the effect of operations.</li>
    </ul>
   each component is supported in the most general form by the abstract
   interface and transformed, translated, and/or constrained as appropriate
   when interpreted for a given specific context.
   <dl>
    <dt>graphics operations</dt>
    <dd><ul>
     <li><code>arc</code></li>
     <li><code>arc*2</code></li>
     <li><code>arc*3</code></li>
     <li><code>circle</code></li>
     <li><code>circle*2</code></li>
     <li><code>circle*3</code></li>
     <li><code>line</code></li>
     <li><code>line*2</code></li>
     <li><code>line*3</code></li>
     <li><code>poly</code></li>
     <li><code>raster</code></li>
     <li><code>raster*2</code></li>
     <li><code>raster*3</code></li>
     <li><code>rectangle</code></li>
     <li><code>rectangle*2</code></li>
     <li><code>rectangle*3</code></li>
     <li><code>text</code></li>
     <li><code>text*2</code></li>
     <li><code>text*3</code></li>
     <li><code>color*3</code>
      could compute a library-specific color datum.
      not necessary for rendering: the respective library operators should just take a
      color vector, which is what the generic operator could resource and return.
      as it is, it returns a list and the operators accept that as well</li>
     <li><code>color*4</code>
      ditto</li>
     <li><code>clear-view</code> erase the current drawable target of the projection context</li>
     <li><code>fill-view</code> fill or paint the projection context</li>
     <li><code>flush-view</code> fill or paint the projection context</li>
     <li><code>inflect</code>
      just sugar.
      assert display state variable values</li>
     <li><code>transform</code> assert coordinate system transform</li>
     </ul></dd>
    <dt>variable / property inflections</dt>
    <dd><ul>
     <li><code>color-mode</code></li>
     <li><code>drawing-mode</code> would be for composites, but not NYI</li>
     <li><code>fill-agent</code></li>
     <li><code>stroke-agent</code></li>
     <li><code>clear-agent</code></li>
     <li><code>path-agent</code></li>
     <li><code>path-constituents</code></li>
     <li><code>path-effect</code></li>
     <li><code>path-rule</code></li>
     <li><code>projection-transform</code></li>
     <li><code>view-transform</code></li>
     </ul></dd>
    </dl>
   </p>"))


;;;
;;;

(deftype  projection-variable-specification ()
  "a projection variable corresponds to a component of a graphics subsystem's display state.
   this includes display properties such as font, color, and polyline drawing mode.
   the interface accepts the union of known component variables and operates on only
   those which are appropriate for the respective context."
  '(or sequence function))

(defstruct path-properties name)

;;;
;;; parameters

(defparameter *if-not-implemented-hook* 'warn
  "when non-null, binds a function which is called with a format string and arguments to
   indicate that a projection was not implemented for those arguments.
   the default value is warn")
      

(defparameter *projection-context* nil
  "binds the current dynamically bound projection context.
   {call-}with-projection-context rebinds this as specified.")

(defparameter *projection-context-stack* nil
  "binds a list of the current projection contexts.
   {call-}with-projection-context extends this for each call.")

(defparameter *projection-variables* (make-array 0 :adjustable t :fill-pointer t)
  "binds a fill vector which orders the names of the state variables. 
   this is used to map names to positions in the flags which indicate which variables
   are to be saved in a given context before modification.")

(defparameter *projection-variable-stack* (make-array 0 :adjustable t :fill-pointer t)
  "binds a fill vector stack onto which restore operations are pushed.
   each 'frame' comprises a sequence of spread forms followed by an element count.
   a count of zero indicates the end of the frame.")

(defparameter *projection-generation* nil
  "binds a symbol which identifies the projection generation.
   used by call-with-projection-context-timed to track resource usage.")

(defconstant *all-projection-variables* -1
  "binds all-variables flag value.")

(defconstant *no-projection-variables* 0
  "binds the no-variables flag value.")

(defparameter *context-dynamic-projection-variables* *all-projection-variables*
  "specifies the initial value for *dynamic-projection-variables* to be established upon entry to
   call-with-projection-variables. the default value is *all-projection-variables*.")

(defparameter *dynamic-projection-variables* *all-projection-variables*
  "binds a flag set which indicates which variables are to be saved upon initial modification within
   a display variable context. upon invocation of call-with-projection-variables, *dynamic-projection-variables*
   is bound to *context-dynamic-projection-variables*. it may be modified by assignment or as
   a side-effect the first assertion of a new value for a given variable within the context. once modified,
   each variable becomes 'static' within the context and is restored to its original when the context
   terminates.")

(defparameter *context-view* nil
  "binds the window-system or graphics library 'view' instance when one is associated with a projection context.")

(defparameter *context-default-z* 0.0d0
  "used as the default z value fo 2d coordinates")

(defparameter *context-default-alpha* 1.0s0
  "used as the default alpha value")

(defparameter *context-zero* 0.0d0
  "used as the zero value when extending location dimensions.
   the initial value is double float zero.")

(defparameter *context-one* 1.0d0
  "used as the one value.
   the initial value is double float one.")

(defparameter *context-ink* nil
  "binds the datum which represents the current effective drawing color for the active context.
   may combine the active color with blending and/or inversion effects.")

(defparameter *context-fill-agent* nil
  "binds the current fill instance for the active context.")

(defParameter *context-stroke-agent* nil
  "binds the current stroke instance for the active context.")

(defParameter *context-clear-agent* nil
  "binds the current background color for the active context.")

(defParameter *context-path-effect* :paint
  "binds the current effect - :clear, :paint, :invert, for the active context.")

(defParameter *context-path-constituents* :lines
  "binds the current path mode - :lines :surfaces :points, for surfaces in the active
   context. if the context qualifies the constituency (eg. front v/s back faces) this
   my be a combined value, eg (:surfaces :lines).")

(defParameter *context-path-properties* (make-path-properties :name :lines)
  "binds the properties associated with the current constituents.
   the initial value is a stub for :lines.")

(defParameter *context-path-rule* :winding
  "binds the current path rule (winding, even-odd) for the active context.")

(defparameter *context-projection-variables-cache*
  (make-hash-table :test 'eq
                   #+ccl :weak #+ccl :key
                   #+allegro :weak-keys #+allegro t
                   )
  "binds an eq hash-table which is weak on key to cache variable
   assertion functions. those which are present in source will be
   retained. those which are dynamically generated will be retained
   only so long as the expression is referenced.")


(defparameter *path-elementary-constituents*
  '(:lines :surfaces :points))

(defParameter *path-constituents*
  (loop for front in *path-elementary-constituents*
        append (loop for back in *path-elementary-constituents*
                    collect (list front back))))

(defParameter *unit-circle-vertex-count* 100
  "specifies the number of segments to generate for the polyline for a unit circle.
   arc generation uses this to vcompute the segment count of an arc of a given radius and angle.")


#+ignore
(defparameter *double-sided-constituents*
  '(:lines/lines :lines/surfaces :lines/points
    :points/lines :points/surfaces :points/points 
    :surfaces/lines :surfaces/surfaces :surfaces/points))

(defparameter *path-constituents-map* (make-hash-table :test 'equal)
  "this hash table maps front modes to the default (front back) mode,
   and (front back) modes to the default single-sided mode.")

;; the mappings are from elementary to compound with identical modes and
;; from a compound mode to the front-side elementary mode
#+ignore
(loop for (key . value) in '(((:lines :lines) . :lines) ((:lines :surfaces) . :lines) ((:lines :points) . :lines)
                             ((:points :lines) . :lines) ((:points :surfaces) . :surfaces) ((:points :points) . :points)
                             ((:surfaces :lines) . :surfaces) ((:surfaces :surfaces) . :surfaces) ((:surfaces :points) . :surfaces)
                             (:lines . (:lines :lines)) 
                             (:points . (:points :points))
                             (:surfaces . (:surfaces :surfaces)))
      do (setf (gethash key *path-constituents-map*) value))

(dolist (pc *path-elementary-constituents*)
  (setf (gethash pc *path-constituents-map*) (list pc pc))
  (dolist (back-pc *path-elementary-constituents*)
    (setf (gethash (list pc back-pc) *path-constituents-map*) pc)))
;;(maphash #'(lambda (k v) (print (list k v))) *path-constituents-map*)


(defParameter *path-effects* '(:clear :paint :invert))

(defParameter *path-agents* '(:fill :clear :stroke)
  "binds a list of the possible uses for agents (colors, bitmaps) with path
   operations. used when inflecting a geometry.")
(defParameter *path-rules* '(:even-odd :winding))
(defParameter *blend-factors* '(:src-alpha :one-minus-src-alpha))

(defParameter *context-drawing-mode* nil
  "binds the current composite drawing mode for the active context.
   any variable note presented is taken from the current constituent value.
   the initial value is nil.")

(defparameter *class.projection-context-window* 'context-window)

(defvar *projection-registry*
  (make-hash-table :test 'eq
                   #+ccl :weak #+ccl :key
                   #+allegro :weak-keys #+allegro t
                   ))

;;;
;;; utilities

(defun no-op (&rest args)
  "provide a no-op function to use when a context does not support an
   operation."
  (declare (dynamic-extent args)
           (ignore args))
  nil)

(defun compute-arc-vertex-count (radius start end)
  (if (= start end)
    1
    (max 3
         (ceiling (* *unit-circle-vertex-count* radius (abs (- end start)))
                  +2pi+))))

(defun find-projection-context (designator)
  (gethash designator *projection-registry*))

(defun (setf find-projection-context) (context designator)
  (if context
    (setf (gethash designator *projection-registry*) context)
    (remhash designator *projection-registry*))
  context)

(defun find-projection-controller (designator)
  (gethash designator *projection-registry*))

(defun (setf find-projection-controller) (datum designator)
  (if datum
    (setf (gethash designator *projection-registry*) datum)
    (remhash designator *projection-registry*))
  datum)

;;;
;;; abstract context definitions

(defclass projection-context ()
  ((name
    :initform (gensym "CONTEXT-") :initarg :name
    :reader context-name
    :documentation
    "binds a name for use to associate the context with things in its own cache
     and for external (eg window manager) references.")
   (controller
    :initarg :controller :initform nil
    :reader context-controller :writer set-context-controller
    :documentation
    "binds an instance to which events are delegated for things like
     exposure and mouse gestures.")
   (projection-task
    :initform (make-instance 'projection-task
                :function 'context-draw-contents)
    :reader context-projection-task
    :documentation
    "binds a cached task instance to use when deferring the context's projection operation.")
   (cache
    :initform (make-hash-table :test 'equalp)
    :reader context-cache)
   (log
    :initform nil :initarg :log
    :accessor context-log)
   (default-fonts
     :initform nil :initarg :default-fonts
     :accessor context-default-fonts
     :allocation :class
     :documentation
     "binds a context-class specific alist of (id . designator) specifiers for
      fonts which are always bound when the context is opened.")
   (uri-scheme
    :initform nil :allocation :class
    :reader context-uri-scheme)
   (uri
    :initarg :uri :initform nil
    :reader context-uri
    :documentation "binds a uri designator for the context."))
  (:documentation
   "the projection-context class is an abstract protocol class from which all projection contexts derive.
    it binds at least
    - a name
    - a hash table to support the accessor context-get, which retrieves or caches a value according to id (and
      optionally owner) key(s);
    - a cached task for deferring operations;
    it may also bind
    - a uri, which distinguished its class upon instantiation;
    - a log."))

(def-initialize-clone projection-context
  (name))

(defclass projection-view ()
  ((projection-context
    :initarg :projection-context
    :accessor view-projection-context))
  (:documentation
   "a projection-view is a view which supports operations with a projection-context.
    the class is a protocol class and is specialized for the respective window systems.
    it servers, for example, with clx, to associate events with the respective context."))

(defclass projection-controller (de.setf.utility.lock:synchronized-object)
  ((id
    :initform (gensym "CONTROLLER-") :initarg :id
    :reader controller-id)
   (timestamp
    :initform 0
    :accessor controller-timestamp
    :documentation
    "binds a timestamp which distinguishes processing generations. each tasks generation
     is synced with the controller when it is run. thus any task which is already synced
     is redundant.")
   (tasks
    :initform nil
    :accessor controller-tasks))
  (:documentation
   "a projection-controller handles processing/scheduling for events."))

(defclass projection-task ()
  ((function
    :initarg :function :initform (error "function required.")
    :accessor task-function)
   (timestamp
    :initform (get-universal-time)
    :accessor task-timestamp
    :documentation
    "binds a timestamp which distinguishes processing generations. each tasks generation
     is synced with the controller when it is run. thus any task which is already synced
     is redundant.")))

(defmethod controller-id ((controller null)) nil)

(defgeneric terminate-relation (owner relation owned)
  (:documentation
   "this is called when some owner instance is either directly terminated,
    or some context of its use is terminated and the owned instance is no longer to be used.
    for example, when a projection context is terminated, all projections can be terminated.")
  (:method ((owner t) (relation t) (owned t))
           ;; don't warn (warn "anomalous termination: ~s ~s ~s." owner relation owned)
           nil)
  (:method ((owner null) (relation t) (owned t))
           ;; anonymous items are ignored
           ))

(defgeneric terminate-context (context)
  (:documentation
   "called when a context is no longer reachable to dispose of any static resources.
    asserted by an after method on initialize-instance")
  (:method ((context projection-context))
           "the general method just removes the registration"
           (setf (find-projection-context (context-name context)) nil)
           (maphash #'(lambda (k v)
                        (let ((owner (if (consp k) (first k) nil))
                              (id (if (consp k) (rest k) k)))
                        (terminate-relation owner id v)))
                    (context-cache context))))


(defgeneric (setf context-get) (value context id &optional owner)
  (:documentation
   "cache a value in a given context as named by the combined argument owner and id keys. a new combined key is constructed with the argument owner and id. for any non-elementary values, a termination function is registered, which when called delegates to (terminate-relation owner id value). ")
  (:method (value (context projection-context) id &optional (owner nil owner-p))
           (with-slots (cache) context
             ;; use the argument value, otherwise the original value remains reachable
             #+ignore ;; this has no effect, since the etry itself will keep the value reachable
             (unless (or (null owner) (numberp value) (characterp value))
               #+digitool
               (terminate-when-unreachable value
                                           #'(lambda (v) (terminate-relation owner id v))))
             (if owner-p
               (setf (gethash (cons owner id) cache) value)
               (setf (gethash id cache) value)))))

(defgeneric context-get (context id &optional owner)
  (:documentation
   "retrieve a value from a given context as named by the combined argument owner and id keys.")
  (:method ((context projection-context) id &optional (owner nil owner-p))
           (with-slots (cache) context
             (if owner-p
               (let ((key (cons owner id)))
                 (declare (dynamic-extent key))
                 (gethash key cache))
               (gethash id cache)))))

(defgeneric get-font (context name)
  (:method ((context projection-context) name)
           (or (context-get context :font name)
               (error "undefined font: ~s: ~s." context name)))
  (:method ((context projection-context) (id (eql t)))
           "the default font is, in general, :fixed."
           (get-font context :fixed)))

(defgeneric (setf get-font) (font-instance context name)
  (:method (font-instance (context projection-context) name)
           (setf (context-get context :font name) font-instance)))

(defgeneric define-font (context definition name)
  (:documentation
   "resolve the font specification for the respective context and bind the name for use with get-font."))


(defmethod initialize-instance :after ((instance projection-context) &key cache)
  "arrange for the context to be terminated prior to its gc.
  a given platform's implementation should effect a call to terminate-context."
  #+digitool (terminate-when-unreachable instance #'terminate-context)
  (setf (find-projection-context (context-name instance)) instance)
  (loop for (key value) on cache by #'cddr
        do (setf (context-get instance key) value))
  )

(defgeneric terminate-controller (context)
  (:documentation
   "called when a controller is no longer reachable to dispose of any static resources.
    asserted by an after method on initialize-instance")
  (:method ((controller projection-controller))
           "the general method just removes the registration"
           (setf (find-projection-controller (controller-id controller)) nil)
           ))

(defmethod initialize-instance :after ((instance projection-controller) &key)
  "arrange for the context to be terminated prior to its gc.
  a given platform's implementation should effect a call to terminate-context."
  #+digitool (terminate-when-unreachable instance #'terminate-controller)
  (setf (find-projection-controller (controller-id instance)) instance)
  )


(defparameter *class.projection-context* 'projection-context
  "binds the concrete class designator to use when instantiating
   these, but specification is initargs only.
   this must be a symbol as it may be used as the constructor
   function designator")

(defgeneric projection-context-p
  (datum)
  (:method ((datum t)) nil)
  (:method ((datum projection-context)) t))

(defgeneric projection-context
  (designator &rest args)
  (:method ((class-designator (eql t)) &rest initargs)
           (apply *class.projection-context* *class.projection-context*
                  initargs))
  (:method ((instance projection-context) &key &allow-other-keys)
           instance)
  (:method ((initargs list) &rest args)
           "given a list, spread it and recurse."
           (apply *class.projection-context*
                  (first initargs)
                  (nconc args (rest initargs))))
  (:method ((keyword symbol) &rest rest-initargs)
           (if (keywordp keyword)
             (apply #'make-instance
                    *class.projection-context*
                    keyword
                    rest-initargs)
             (projection-context (apply #'make-instance keyword rest-initargs))))
  (:method ((class class) &rest initargs)
           (declare (dynamic-extent initargs))
           (projection-context (apply #'make-instance class initargs)))
  (:method ((designator string) &rest initargs)
           "treat the designator as an uri: extract the 'protocol', locate a class and delegate to it."
           (let* ((key (subseq designator 0 (or (position #\: designator) (length designator))))
                  (class (labels ((find-subclass (class)
                                    (if (string-equal key (context-uri-scheme (class-prototype class)))
                                      class
                                      (some #'find-subclass (class-direct-subclasses class)))))
                           (or (find-subclass (find-class 'projection-context))
                               (error "invalid projection context designator: ~s." designator)))))
             (apply #'projection-context class
                    :uri designator
                    initargs))))
                  
(defun make-projection-context (&rest initargs)
  (declare (dynamic-extent initargs))
  (projection-context initargs))

(defparameter *class.projection-view* nil
  "binds the concrete class designator to use when instantiating
   these, but specification is initargs only.
   this must be a symbol as it may be used as the constructor
   function designator.
   the initial value, NIL, must be replaced by the application
   before attempting implicit instantiation.")

(defgeneric projection-view-p
  (datum)
  (:method ((datum t)) nil)
  (:method ((datum projection-view)) t))

(defgeneric projection-view
  (designator &rest args)
  (:method ((class-designator (eql t)) &rest initargs)
           (apply *class.projection-view* *class.projection-view*
                  initargs))
  (:method ((instance projection-view) &key &allow-other-keys)
           instance)
  (:method ((initargs list) &rest args)
           "given a list, spread it and recurse."
           (apply *class.projection-view*
                  (first initargs)
                  (nconc args (rest initargs))))
  (:method ((keyword symbol) &rest rest-initargs)
           (if (keywordp keyword)
             (apply #'make-instance
                    *class.projection-view*
                    keyword
                    rest-initargs)
             (projection-view
              (apply #'make-instance keyword rest-initargs)))))

(defun make-projection-view (&rest initargs)
  (declare (dynamic-extent initargs))
  (projection-view initargs))

(defgeneric projection-context-state (context)
  (:method ((context null)) nil)
  (:documentation
   "return a property list which describes the context's current state."))

(defun context-state (&optional (context *projection-context*))
   "return a property list which describes the context's current state.
    accepts a single, optional argument, the context.
    the default is the current *projection-context*."
   (projection-context-state context))



(defclass view-context (projection-context)
  ((view
    :initarg :view :initform nil
    :writer (setf context-view) :writer set-context-view
    :reader get-context-view
    :documentation
    "binds the view or drawable into which the context projects.")
   (background-mode
    :initarg :background-mode :initform :opaque
    :accessor context-background-mode
    :type (member :opaque :transparent)
    :documentation
    "binds a boolean to indicate whether the context's view is to be
     transparent or opaque."))
  (:documentation
   "binds a view for the context"))

(def-initialize-clone view-context
  ;; view is not copied --> the context must be itself opened
  (background-mode))

(defmethod initialize-instance :after ((instance view-context) &key view-size view-position window-title)
  (when view-size
    (setf (context-get instance :view-size) view-size))
  (when view-position
    (setf (context-get instance :view-position) view-position))
  (when window-title
    (setf (context-get instance :window-title) window-title)))

(defclass dimensioned-context (projection-context)
  ((position
    :initform #@(0 0) :initarg :position
    :accessor context-position)
   (size
    :initform #@(128 128) :initarg :size
    :accessor context-size)))

(def-initialize-clone dimensioned-context
  (position
   size))

(defclass context-window (#+digitool ccl:window)
  ((context
    :initform nil :initarg :context
    :accessor view-context
    :documentation
    "binds a projection context."))
  (:default-initargs
    :color-p t 
    :view-size #@(256 256)
    :view-position #@(100 100)
    :window-show t))

(defgeneric context-view-p (context view)
  (:method ((context t) (view t)) nil))


(defun front-context-window ()
  #+digitool (ccl:front-window :class 'context-window))

(defgeneric context-view (context)
  (:method ((context view-context))
           (with-slots (view) context
             (if (context-view-p context view)
               view
               (setf view (context-make-view context))))))

(defgeneric context-close-view (context view)
  (:method ((context view-context) (view t))
           (when (eq view (get-context-view context))
             (set-context-view nil context))
           view)
  #+DIGITOOL
  (:method ((context view-context) (window ccl:window))
           (when (ccl:wptr window)
             (ccl:window-close window)
             (setf (ccl:wptr window) nil))
           (call-next-method)))

(defgeneric context-size (context)
  (:method ((context null)) #@(0 0))
  (:method ((context function)) #@(0 0))
  (:method ((context view-context))
           (view-size (context-view context))))

(defgeneric context-position (context)
  (:method ((context null)) #@(0 0))
  (:method ((context function)) #@(0 0))
  (:method ((context view-context))
           (view-position (context-view context))))

(defmethod (setf context-position) (position (context view-context))
  (with-slots (view) context
    (when (context-view-p context view)
      (setf (view-position view) position))))
      
(defmethod (setf context-size) (size (context view-context))
  (with-slots (view) context
    (when (context-view-p context view)
      (setf (view-size view) size))))

#+digitool
(defmethod (setf view-size) (size (view ccl:view))
  (ccl:set-view-size view size))

#+digitool
(defmethod (setf view-position) (position (view ccl:view))
  (ccl:set-view-position view position))



(defgeneric context-make-view (context &key view-position view-size view-parent &allow-other-keys)
  #+digitool
  (:method ((context view-context)
            &key
            (window-title (or (context-get context :window-title)
                              (string (type-of context))))
            (view-position (or (context-get context :view-position) #@(40 40)))
            (view-size (or (context-get context :view-size) #@(128 128)))
            (window-show 0)
            (window-layer 1)
            (view-parent nil)
            (view-class (or (context-get context :view-class) *class.projection-context-window*))
            (color-p t))
           (let ((v (make-instance view-class
                      :color-p color-p
                      :view-size view-size
                      :view-position view-position
                      :window-title window-title
                      :window-layer window-layer
                      :window-show window-show)))
             (when view-parent
               (ccl:set-view-container v view-parent))
             (setf (context-get context :view-size) view-size)
             (setf (context-get context :view-position) view-position)
             (ccl:process-allow-schedule)          ; let the default clear run
             v)))

(defgeneric context-update-for-view (context view)
  #+digitool
  (:method ((context view-context) (view ccl:view))
           (setf (context-get context :view-size) (ccl:view-size view))
           (setf (context-get context :view-position) (ccl:view-position view))))

(defclass projection-variable-context (projection-context)
  ()
  (:documentation
   "enables variable saving and restoring"))

(defclass single-sided-projection-context (projection-variable-context)
  ()
  (:documentation
   "interprets display variable operations for a context with single-sided
    rendering."))

(defclass double-sided-projection-context (projection-variable-context)
  ()
  (:documentation
   "interprets display variable operations for a context with double-sided
    rendering."))

(defclass delegate-context (projection-context)
  ()
  (:documentation
   "a marker for classes which implement the abstract interface by delegation.
    for each interface function includes a method is generated for this classto delegate to the result of applying #'effective-context to the argument."))

(defgeneric effective-context (delegate-context)
  (:documentation
   "concrete delegates must implement a method of one argument, the delegate, which returns the effective context."))

;;;
;;;

(defmacro with-short-float-intensity-variables (variables &rest body)
  `(progn ,@(mapcar #'(lambda (var)
                        `(etypecase ,var
                           (short-float)
                           (double-float (setf ,var (float ,var 1.0s0)))
                           (integer (setf ,var (/ ,var 65535.0s0)))
                           (number (setf ,var (float ,var 1.0s0)))))
                    variables)
          (locally (declare (type short-float ,@variables)))
          ,@body))

(defmacro with-mutable-double-floats (bindings &rest body)
  (flet ((binding-variable (binding)
           (if (consp binding) (first binding) binding)))
    `(let ,(mapcar #'(lambda (binding)
                       (if (consp binding) binding (list binding 0.0d0)))
                   bindings)
       (declare (type double-float ,@(mapcar #'binding-variable bindings))
                (dynamic-extent ,@(mapcar #'binding-variable bindings)))
       ,@body)))

(defun radians-to-degrees (r)
  (* r #.(/ 180.0 pi)))

(defun degrees-to-radians (d)
  (* d #.(/ pi 180.0)))

(defmacro with-fixnum-intensity-variables (variables &rest body)
  `(progn ,@(mapcar #'(lambda (var)
                        `(unless (integerp ,var) (setf ,var (round (* ,var 65535.0d0)))))
                    variables)
          (locally (declare (type fixnum ,@variables)))
          ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun boa-argument-list (lambda-list)
    "extract the required, optional and rest or keywords arguments
     from a lambda list for use in a macro which generates mediating
     functions. returns two values, the list of arguments and a boolean
     to indicate if the last argument binds a rest value."
    (let ((aux-p nil) (rest-arg nil) (key-p nil) (optional-p nil))
      (values
       (reduce #'append
               (mapcar #'(lambda (parameter)
                           (flet ((parameter-argument (parameter)
                                    (cond (optional-p (list parameter))
                                          (rest-arg (if (eq rest-arg t)
                                                      (list (setf rest-arg parameter))
                                                      (list parameter)))
                                          (key-p (unless rest-arg
                                                   (list (if (consp parameter)
                                                           (first parameter)
                                                           (intern (string parameter) :keyword))
                                                         parameter)))
                                          (aux-p nil)
                                          (t (list parameter)))))
                             (etypecase parameter
                               (cons (parameter-argument (first parameter)))
                               (symbol (case parameter
                                         (&optional (setf optional-p t) nil)
                                         (&rest (setf rest-arg t) nil)
                                         (&key (setf key-p t) nil)
                                         (&aux (setf aux-p t) nil)
                                         (t (parameter-argument parameter)))))))
                       lambda-list))
       rest-arg))))

(defmacro def-projection-operator (designator lambda-list &rest options)
  "define a projection operation.
   the definition comprises a special variable which binds the implementation
   context and a pair of interface functions. the first is synonymous with the
   scene model constituent and delegated to a respective generic function,
   passing a dynamically bound context variable. the second, the generic
   function, specialized the first parameter on the context and implements the
   projection operation in terms of context-specific primitives."
  (when (consp designator)
    (error "setf operation designators not allowed: ~s" designator))
  (labels ((context-specializer (method)
             (if (consp designator)
               (destructuring-bind ((datum (param specializer) &rest rest) &rest body)
                                   (nthcdr (or (position-if (complement #'symbolp) method)
                                               (error "invalid method clause: ~s." method))
                                           method)
                 (declare (ignore datum param rest body))
                 specializer)
               (destructuring-bind (((param specializer) &rest rest) &rest body)
                                   (nthcdr (or (position-if (complement #'symbolp) method)
                                               (error "invalid method clause: ~s." method))
                                           method)
                 (declare (ignore param rest body))
                 specializer)))
           (find-specialized-method (specializer methods)
             (dolist (m methods)
               (when (and (listp (second m))
                          (equal specializer (context-specializer m)))
                 (return m))))
           (construct-call (call-operator function-designator specialization-variable arguments)
             ;; this was intended to account for operator and (setf operator), but the
             ;; context of use has too many variations...
             (if (consp function-designator)
               (ecase call-operator
                 (funcall `(setf (,(second function-designator) ,specialization-variable ,@(rest arguments))
                                  ,(first arguments)))
                 (apply `(apply #',function-designator ,(first arguments) ,specialization-variable ,@(rest arguments))))
               (ecase call-operator
                 (funcall (if (symbol-package function-designator)
                            `(,function-designator ,specialization-variable ,@arguments)
                          `(funcall ,function-designator ,specialization-variable ,@arguments)))
                 (apply `(apply #',function-designator ,specialization-variable ,@arguments))))))
    (let* ((name (if (symbolp designator) designator (second designator)))
           (model-operator (assoc :model-operator options))
           (methods (remove :method options :key #'first :test (complement #'eq)))
           (specialization-variable (assoc :specialization-variable options))
           (delegation-accessor (assoc :delegation-accessor options))
           (documentation (assoc :documentation options))
           (gf-name (intern (concatenate 'string (string 'context-) (string name))
                            *package*))
           (gf-designator (if (symbolp designator) gf-name `(setf ,gf-name)))
           (list-operator 'list)
           (call-operator 'funcall)
           (specialization-parameter (gensym "CONTEXT-")))
      (multiple-value-bind (arguments rest-arg) (boa-argument-list lambda-list)

        (when rest-arg (setf list-operator 'list* call-operator 'apply))
        (setf options (set-difference options methods))
        (if specialization-variable
          (setf options (remove specialization-variable options)
                specialization-variable (second specialization-variable))
          (setf specialization-variable '*projection-context*))
        (if delegation-accessor
          (setf options (remove delegation-accessor options)
                delegation-accessor (second delegation-accessor))
          (setf delegation-accessor 'effective-context))
        (if model-operator
          (setf options (remove model-operator options)
                model-operator (second model-operator))
          (setf model-operator designator))
        
        (unless (find-specialized-method '(eql t) methods)
          (push `(:method ,(if (consp designator)
                             `(,(first lambda-list) (,specialization-parameter (eql t)) ,@(rest lambda-list))
                             `((,specialization-parameter (eql t)) ,@lambda-list))
                          (,list-operator ',designator ,@arguments))
                methods))
        (unless (find-specialized-method 't methods)
          (push `(:method ,(if (consp designator)
                             `(,(first lambda-list) (,specialization-parameter t) ,@(rest lambda-list))
                             `((,specialization-parameter t) ,@lambda-list))
                          (when *if-not-implemented-hook*
                            (funcall *if-not-implemented-hook*
                                     "no method for operation: ~s: ~s."
                                     ',designator (,list-operator ,specialization-parameter ,@arguments))))
                methods))
        (unless (find-specialized-method 'null methods)
          (push `(:method ,(if (consp designator)
                             `(,(first lambda-list) (,specialization-parameter null) ,@(rest lambda-list))
                             `((,specialization-parameter null) ,@lambda-list))
                          (declare (ignore ,@arguments))
                          nil)
                methods))
        (unless (find-specialized-method 'function methods)
          (push `(:method ((,specialization-parameter function) ,@lambda-list)
                          ,(ecase call-operator
                            (funcall `(funcall ,specialization-parameter ',name ,@arguments))
                            (apply `(apply ,specialization-parameter ',name ,@arguments))))
                methods))
        (unless (find-specialized-method 'symbol methods)
          (push `(:method ((,specialization-parameter symbol) ,@lambda-list)
                          "a generated method which interprets an fbound
                           symbolic context as a function designator and
                           otherwise delegates to the default method."
                          (if (fboundp ,specialization-parameter)
                            ,(ecase call-operator
                               (funcall `(funcall ,specialization-parameter ,@arguments))
                               (apply `(apply ,specialization-parameter ,@arguments)))
                            (call-next-method)))
                methods))
        (unless (find-specialized-method 'delegate-context methods)
          (push `(:method ,(if (consp designator)
                             `(,(first lambda-list) (,specialization-parameter delegate-context) ,@(rest lambda-list))
                             `((,specialization-parameter delegate-context) ,@lambda-list))
                          ,(construct-call call-operator gf-designator `(,delegation-accessor ,specialization-parameter) arguments))
                methods))

        ;; compile-in a monitoring method if a function is specified at compile-time
        (when *projection-monitor-function*
          (push `(:method :around ,(if (consp designator)
                                     `(,(first lambda-list) (,specialization-parameter t) ,@(rest lambda-list))
                                     `((,specialization-parameter t) ,@lambda-list))
                          (declare (ignore ,@arguments))
                          (if (and *projection-generation* *projection-monitor-function*)
                            (multiple-value-bind (values time space)
                                                 (funcall *projection-monitor-function* #'call-next-method)
                              (let ((operator-resources (get *projection-generation* ',designator)))
                                (incf (getf operator-resources :time 0) time)
                                (incf (getf operator-resources :space 0) space)
                                (incf (getf operator-resources :count 0) 1)
                                (setf (get *projection-generation* ',designator) operator-resources)
                                (apply #'values values)))
                            (call-next-method)))
              methods))
                              
        `(progn (defgeneric ,gf-designator
                  ,(if (consp designator) (list* (first lambda-list) 'context (rest lambda-list)) (cons 'context lambda-list))
                  ,@methods
                  ,@options)
                ;; for documentary purposes
                (setf (getf (symbol-plist ',designator) 'generic-function) ',gf-designator)
                ;; link the context-free function to the specializable one
                (defun ,model-operator ,lambda-list
                  ,@(when documentation (rest documentation))
                  ,@(when rest-arg `((declare (dynamic-extent ,rest-arg))))
                  ,(construct-call call-operator gf-designator specialization-variable arguments)))))))


(defmacro def-projection-variable (name)
  (let ((bit-variable (intern (concatenate 'string #.(string :+projection-state-) (string name) #.(string :-bit+))))
        (mask-variable (intern (concatenate 'string #.(string :+projection-state-) (string name) #.(string :-mask+)))))
    `(progn
       (defparameter ,bit-variable (or (position ',name *projection-variables*)
                                       (vector-push-extend ',name *projection-variables*)))
       (defparameter ,mask-variable (ash 1 ,bit-variable))
       (def-projection-operator ,(intern (concatenate 'string #.(string :save-) (string name))) ()
         (:method ((context delegate-context)) nil)))))


(defmacro def-projection-variable-op (name lambda-list &rest options)
  "generate a save method for the variable. by default generate a no-op for delegates."
  (flet ((arg1-specializer (method)
           (destructuring-bind (((param specializer) &rest rest) &rest body)
                               (nthcdr (or (position-if (complement #'symbolp) method)
                                           (error "invalid method clause: ~s." method))
                                       method)
             (declare (ignore param rest body))
             specializer)))
    (multiple-value-bind (arguments rest-arg) (boa-argument-list lambda-list)
      (declare (ignore rest-arg))
      (unless (find-if #'(lambda (clause)
                           (and (eq (second clause) :before)
                                (eq (arg1-specializer clause) 'projection-variable-context)))
                       options)
        (let ((mask-variable (intern (concatenate 'string #.(string :+projection-state-) (string name) #.(string :-mask+)))))
          (push `(:method :before ((context projection-variable-context) ,@lambda-list)
                          (declare (ignore ,@arguments))
                          (when (logtest ,mask-variable *dynamic-projection-variables*)
                            (setf *dynamic-projection-variables* (logxor ,mask-variable *dynamic-projection-variables*))
                            (,(intern (concatenate 'string #.(string :context-save-) (string name))) context)))
                options)))
      `(prog1
         (def-projection-operator ,(intern (concatenate 'string (string :set-) (string name)) *package*)
           ,lambda-list ,@options)
         (defun (setf ,(intern (concatenate 'string (string :context-) (string name)) *package*))
                (,(first lambda-list) .context. ,@(rest lambda-list))
           (,(intern (concatenate 'string (string :context-set-) (string name)) *package*)
            .context. ,@arguments))
         (setf (fdefinition ',name)
               (fdefinition ',(intern (concatenate 'string (string :set-) (string name)) *package*)))))))


(defgeneric call-with-projection-context (function context &key &allow-other-keys)
  (:documentation
   "call argument function in the context of a given graphic interface.
    the base method binds the function parameters for simple geometric operations.")
  (:argument-precedence-order context function)
  (:method ((function function) (*projection-context* t) &key &allow-other-keys)
           "the general method binds the dynamic *projection-context*, extends
            the stack, and applies the function to the argument value."
           (let ((*projection-context-stack*
                  (cons *projection-context* *projection-context-stack*)))
             (declare (dynamic-extent *projection-context-stack*))
             (funcall function *projection-context*)))
  (:method ((function t) (context view-context) &key &allow-other-keys)
           "if the context is a view-context, bind the respective view and delegate to the next method."
           (let ((*context-view* (context-view context)))
             (context-update-for-view context *context-view*)
             (call-next-method)))
  (:method ((function t) (context delegate-context) &rest args)
           "if the context delegates, then recurse with the effective context."
           (declare (dynamic-extent args))
           (apply #'call-with-projection-context (effective-context context) args)))
         

(defmacro with-projection-context ((&optional context &rest args) &rest body &environment env)
  "if the context is specified as a variable, then reuse it for the binding, otherwise
   gensym a new binding variable."
  (let ((context-variable (if (and context (symbolp context) (eq context (macroexpand context env)))
                            context
                            (gensym "CONTEXT-")))
        (function (gensym "BODY-")))
   `(flet ((,function (,context-variable)
              (declare (ignorable ,context-variable))
              ,@body))
       (declare (dynamic-extent #',function))
       (call-with-projection-context #',function ,context ,@args))))

(defparameter *monitoring-output* *trace-output*)

(defmacro projection-list (&rest expressions)
  "collect the projection list results in a null context. this defers their side-effects"
  `(let ((*projection-context* nil))
     (list ,@expressions)))


;;;
;;; some monitoring

(defmacro with-monitoring-report ((&key (stream '*monitoring-output*)) form)
  `(let* ((*projection-generation* (gensym "PROJECTION"))
          (result (multiple-value-list ,form)))
     (report-monitoring *projection-generation* ,stream :form ',form :result-values result)
     (apply #'values result)))

(defgeneric report-monitoring (generation destination &key form result-values)
  (:method ((generation t) (destination null) &key &allow-other-keys)
           nil)
  (:method ((generation symbol) (destination stream) &rest args)
           (apply #'write-monitoring-report-as generation destination :text/text args))
  (:method ((generation symbol) (destination pathname) &rest args)
           (let ((type (pathname-type destination)))
             (cond ((string-equal "html" type)
                    (apply #'write-monitoring-report-as generation destination :text/html args)
                    (open-document destination))
                   ((string-equal "csv" type)
                    (apply #'write-monitoring-report-as generation destination :text/csv args)
                    (open-document destination))
                   ((string-equal "lisp" type)
                    (apply #'write-monitoring-report-as generation destination :text/lisp args)
                    (ed destination))
                   (t
                    (apply #'write-monitoring-report-as generation destination :text/text args)
                    (ed destination))))))

(defgeneric write-monitoring-report-as (generation destination mime-type &key form result-values)
  (:method ((report null) (destination t) (type t) &key form result-values)
           (warn "no result found: ~s.~@[~% ~W~@[~%=>~{~4t~s~}~]~]" *projection-generation* form result-values))
  (:method ((generation symbol) (destination t) (as t) &rest args)
           (apply #'write-monitoring-report-as (compute-monitoring-results generation) destination as args))
  (:method ((report list) (destination pathname) (as t) &rest args)
           (with-open-file (stream destination
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (apply #'write-monitoring-report-as report stream as args)))
  (:method ((report list) (destination stream) (as (eql :text/lisp)) &key form result-values)
           (pprint form destination)
           (pprint result-values destination)
           (pprint report destination))
  (:method ((report list) (destination stream) (as (eql :text/csv)) &key form result-values)
           (format destination "OPERATOR,COUNT,TIME,SPACE,'~s','~s'" form result-values)
           (dolist (operator report)
             (destructuring-bind (&key name count time space) operator
               (format destination "~&~a,~s,~s,~s" name count time space))))
  (:method ((report list) (destination stream) (as (eql :text/html)) &key form result-values)
           (format destination "<html><head><title>~a</title></head>" (pathname-name destination))
           (format destination "~%<body>")
           (format destination "~%<p><pre style='background-color: #e0e0e0; margin: 4px;'>~%~W" form)
           (format destination "~%=>")
           (dolist (result result-values)
             (format destination "~%&nbsp;~s" result))
           (format destination "~%</pre></p>")
           (format destination "~%<p style='border: groove gray; margin: 4px;'><table>")
           (format destination "~%<tr><th>OPERATOR</th><th>COUNT</th><th>TIME(ms)</th><th>SPACE</th><th>OPS/S</th><th>BYTES/OP</th></tr>")
           (format destination "~%<tr><td colspan='6'><hr /></tr>")
           (dolist (operator report)
             (destructuring-bind (&key name count time space) operator
               (flet ((ops-per-second ()
                        (if (zerop time) 0 (/ count (/ time 1000.0))))
                      (bytes-per-op ()
                        (if (zerop count) 0 (float (/ space count) 1.0))))
                 (format destination "~% <tr><td>~a</td><td align='right'>~s</td><td align='right'>~s</td><td align='right'>~s</td><td align='right'>~$</td><td align='right'>~$</td></tr>"
                         name count time space (ops-per-second) (bytes-per-op)))))
           (format destination "~%<tr><td colspan='6'><hr /></tr>")
           (format destination "</table></p></body></html>"))
  (:method ((report list) (destination stream) (as (eql :text/text)) &key form result-values)
           (let ((column-width 12)
                 (name-width (+ (reduce #'max report 
                                        :key #'(lambda (result) (length (symbol-name (getf result :name)))))
                                2)))
             (flet ((column (column) (+ name-width (* column-width (1- column)))))
               (let ((rule (make-array (column 6) :initial-element #\- :element-type 'character)))
                 (format destination "~%~a~%~@[~W~:[~;~:*~%=>~{~4t~s~}~]~%~a~]"
                         rule form result-values rule)
                 (format destination "~%OPERATOR~vtCOUNT~vtTIME~vtSPACE~vtOPS/S~vtBYTES/S"
                         (column 1) (column 2) (column 3) (column 4) (column 5))
                 (format destination "~%~a" rule)
                 (dolist (operator report)
                   (destructuring-bind (&key name count time space) operator
                     (flet ((ops-per-second ()
                              (if (zerop time) 0 (/ count (/ time 1000.0))))
                            (bytes-per-op ()
                              (if (zerop count) 0 (float (/ space count) 1.0))))
                       (format destination "~&~a~vt~s~vt~s~vt~s~vt~$~vt~$"
                               name (column 1) count (column 2) time (column 3) space
                               (column 4) (ops-per-second) (column 5) (bytes-per-op)))))
                 (format destination "~%~a" rule))))))
  
(defun compute-monitoring-results (generation)
  (sort (loop for (operator results) on (symbol-plist generation) by #'cddr
              collect (list* :name operator results))
        #'>
        :key #'(lambda (result) (getf result :time))))

#+digitool
(defun open-document (pathname)
  (ccl:set-mac-file-creator pathname (intern (make-string 4 :initial-element #\null) :keyword))
  (setf pathname (namestring (truename pathname)))
  (setf pathname (subseq pathname (1+ (position #\: pathname))))
  (asdf:run-shell-command "open ~a" pathname))

;;; (setf (get 'tmp 'line*2) '(:time 100 :space 200))
;;; (write-monitoring-report-as 'tmp #p"macintosh hd;tmp;monitor-result.html" :text/html)
;;; (ed #p"og:test.html")
;;; (open-document #p"og:test.html")
;;; 


;;;
;;; manage the context state uniformly, in terms of "projection variables". these are intended to
;;; cover the union of various context implementation's rendering properties and express them
;;; all in common terms. the abstract rendering interface provides tools to manages a frame-oriented
;;; stack of variables, with provisions to add entries, mark frame boundaries, and roll-back a frame,
;;; thereby returning context's property state to that at the creation of the frame.

(defun push-projection-context ()
  "push a frame marker (a zero count) onto the variable stack.
   this signifies to restore-display-state that the frame is ended."
  (vector-push-extend 0 *projection-variable-stack*))

(defun push-projection-variable (&rest form &aux (count 0))
  "push an variable operator and a sequence of argument values onto the variable stack.
   follow this operation specification with a count. the count, values and operator
   are used to restore-display-state to construct a function call which restores
   the respective variable's values."
  (declare (dynamic-extent form))
  (dolist (element form)
    (vector-push-extend element *projection-variable-stack*)
    (incf count))
  (vector-push-extend count *projection-variable-stack*))

;;(pushnew :restore-display-state.debug *features*) (setq *features* (remove :restore-display-state.debug *features*))

(defun restore-projection-variable (&aux count)
  "construct function calls by collecting the operator/values sequences present
   in a frame and invoking the indicated operation. when the count is 0, there is
   no operator present, which indicates the end of the frame."
  (labels ((pop-and-call (count elements)
             ;; construct an argument list incrementally and call the
             ;; restore function
             #+restore-display-state.debug
             (format *trace-output* "~%rds: ~d ~s" count elements)
             (if (> count 1)
               (let ((elements (cons (vector-pop *projection-variable-stack*) elements)))
                 (declare (dynamic-extent elements))
                 (pop-and-call (1- count) elements))
               (apply (vector-pop *projection-variable-stack*) elements))))
    ;; (declare (inline pop-and-call)) not appreciated by sbcl
    (when (> (length *projection-variable-stack*) 0)
      (setf count (vector-pop *projection-variable-stack*))
      (when (plusp count)
        (pop-and-call count nil)))))

(defun restore-display-state ()
  "iterate over the variable stack, restoring each combination in
   the frame. when restore-display-variable returns nil, a zero count
   has indicated the end of the frame."
  #+restore-display-state.debug
  (format *trace-output* "~%rds: pvs before: ~s" *projection-variable-stack*)
  (loop (unless (restore-projection-variable)
          (return)))
  #+restore-display-state.debug
  (format *trace-output* "~%rds: pvs after: ~s" *projection-variable-stack*)
  )

(defmacro with-projection-variables (variable-op &rest body)
  "execute a sequence of forms in a projection variable context. the forms are
   executed in an implicit tagbody and any asserted projection variables are
   unwound to their original values when the context completes. it accept an
   optional form which evaluates to a function of one argument. if present,
   this function is called with one argument, a function of no arguments
   which executes the body."
  (let ((body-function-name (gensym "BODY-")))
    (etypecase variable-op

      ;; if initial variable assertions are present, intepret them as a function designator
      (cons (let ((variable-function-name (gensym "VARIABLES-")))
              (cond ((and (eq (first variable-op) 'function)
                          (eql (length variable-op) 2)
                          (symbolp (second variable-op)))
                     ;; variables asserted by means of a named function designator
                     `(flet ((,body-function-name () ,@body))
                        (declare (dynamic-extent #',body-function-name))
                        (call-with-projection-variables #',body-function-name variable-op)))
                    ((or (eq (first variable-op) 'lambda )
                         (and (eq (first variable-op) 'function)
                              (eql (length variable-op) 2)
                              (consp (second variable-op))
                              (eq (first (second variable-op)) 'lambda)))
                     ;; allow either (lambda (params) . body) or (function (lambda (params) . body))
                     (when (eq (first variable-op) 'function)
                       ;; normalize the (function ...) form
                       (setf variable-op (second variable-op)))
                     (destructuring-bind (lambda v-parameters . v-body) variable-op
                       (declare (ignore lambda))
                       (when (or (null v-parameters)
                                 (find (first v-parameters) lambda-list-keywords))
                         (push body-function-name v-parameters)
                         (setf v-body `(,@v-body (funcall ,body-function-name))))
                       `(flet ((,body-function-name () ,@body)
                               (,variable-function-name ,v-parameters ,@v-body))
                          (declare (dynamic-extent #',body-function-name #',variable-function-name))
                          (call-with-projection-variables #',body-function-name #',variable-function-name))))
                    (t
                     (error "invalid variable assertion form: ~s.~%a function designator is required."
                            variable-op)))))
          
      ;; if no variables were specified, then generate a call which will assert no changes, but
      ;; will still effect a restore.
      (null `(flet ((,body-function-name () ,@body))
               (declare (dynamic-extent #',body-function-name))
               (call-with-projection-variables #',body-function-name nil))))))


(defgeneric call-with-projection-variables (display-operations variable-assertions)
  (:argument-precedence-order variable-assertions display-operations)
  (:documentation
   "execute an argument display operation in a projection variable context.
    the display operation should be a function of no argument. the variable
    assertions may be a function of one argument, null, or a sequence of
    variable assertion operations.
    if it is a sequence, it is compiled to a function and cached weakly on the
    form.")

  (:method ((op function) (variables function))
           (declare (dynamic-extent op variables))
           (let ((*dynamic-projection-variables* *context-dynamic-projection-variables*))
             ;; bind an initial set of flags to indicate those variables which should
             ;; be saved upon modification, mark a variable frame boundary, and
             ;; invoke the assertion/operation function.
             (unwind-protect (progn (push-projection-context)
                                    (funcall variables op))
               (restore-display-state))))
  
  (:method ((op function) (variables null))
           "if no variable assertions are specified just invoke the operation.
            _do not push a context_"
           (declare (dynamic-extent op))
           (funcall op))

  (:method ((op function) (variables cons))
           (let ((variable-function
                  (gethash variables *context-projection-variables-cache*)))
             (unless variable-function
               (let* ((op-parameter (gensym "BODY-")))
                 (setf variable-function
                       (setf (gethash variables *context-projection-variables-cache*)
                             (compile nil `(lambda (,op-parameter)
                                             ,@variables
                                             (funcall ,op-parameter)))))))
             (call-with-projection-variables op variable-function))))



(defgeneric context-projection-variable-p (context function)
  (:documentation
   "returns true if the argument function is specialized for the given context")
  (:method ((context t) (function symbol))
           (and (fboundp function)
                (context-projection-variable-p context (fdefinition function))))
  (:method ((context t) (function t))
           nil)
  (:method ((context projection-context) (function generic-function))
           (flet ((method-qualifies? (method)
                    (let ((specializer (first (method-specializers method))))
                      (and (typep specializer 'class)
                           (not (eq t (class-name specializer)))
                           (typep context specializer)))))
             (declare (dynamic-extent #'method-qualifies?))
             (find-if #'method-qualifies? (generic-function-methods function)))))

(defun projection-variable-p (variable)
  (context-projection-variable-p *projection-context* variable))


;;;
;;; geometric object operators
;;; include _generic_ functions also for operators with default implementions in terms of
;;; other operators (eg circle and arc) in order to permit specializations if the context
;;; supports them directly.

(def-projection-operator arc (location radius start end direction &optional properties))
(def-projection-operator arc*2 (x y radius start end direction &optional properties))
(def-projection-operator arc*3 (x y z radius start end direction &optional properties))
(def-projection-operator circle (location radius &optional properties)
  (:method ((context projection-context) location radius &optional (properties nil))
         (context-arc context location radius +2pi+ *context-zero* :clockwise properties)))  
(def-projection-operator circle*2 (x y radius &optional properties)
  (:method ((context projection-context) x y radius &optional (properties nil))
           (context-arc*2 context x y radius +2pi+ *context-zero* :clockwise properties)))
(def-projection-operator circle*3 (x y z radius &optional properties)
  (:method ((context projection-context) x y z radius &optional (properties nil))
           (context-arc*3 context x y z radius +2pi+ *context-zero* :clockwise properties)))
(def-projection-operator line (l1 l2 &optional properties)
  (:model-operator line2))         ; see definitions for geometric class constructor
(def-projection-operator line*2 (x1 y1 x2 y2 &optional properties))
(def-projection-operator line*3 (x1 y1 z1 x2 y2 z2 &optional properties))
(def-projection-operator poly (vertices &optional properties))
(def-projection-operator raster (location1 location2 raster &optional properties))
(def-projection-operator raster*2 (x1 y1 x2 y2 raster &optional properties))
(def-projection-operator raster*3 (x1 y1 z1 x2 y2 z2 raster &optional properties))
(def-projection-operator rectangle (l1 l2 &optional properties)
  (:model-operator rectangle2))         ; see definitions for geometric class constructor
(def-projection-operator rectangle*2 (x1 y1 x2 y2 &optional properties))
(def-projection-operator rectangle*3 (x1 y1 z1 x2 y2 z2 &optional properties))
(def-projection-operator text (location text font &optional properties))
(def-projection-operator text*2 (x y text font &optional properties))
(def-projection-operator text*3 (x y z text font &optional properties))

(defgeneric context-path-agent (context generic-agent)
  (:documentation
   "resolve the generic agent for the given projection context.
    the default method returns the agent unchanged.")
  (:method ((context t) (agent t))
           agent))

(defun call-with-path-properties (geometry-operator &optional (drawing-mode *context-drawing-mode*))
  "execute a geometric operation in the context of a composite argument drawing mode.
   the geometry operator is a function of four arguments, which designate the context's
   current effective clear and stroke agents (eg., black and white ink), its path constituents
   (:surfaces :lines :points), its fill rule (:winding :even-odd), and its rendering effect
   (:clear :invert :paint). these values are collected from the respective elements of the
   drawing-mode argument sequence. the element '/ forcesa call to the geomotry operator
   in the midst of the list. otherwise it is called after the mode proerties have been
   asserted. a property which is not one of the distinguished keywords is expected to be
   understood by the projection context as an abstract path agent.
   the mode is a list of generic rendering "
  (if drawing-mode
    (let* ((path-agent nil)
           (path-constituents *context-path-constituents*)
           (path-effect *context-path-effect*)
           (path-rule *context-path-rule*)
           (modifications #x1000))
      (labels ((do-call ()
                 (funcall geometry-operator
                          (cond (path-agent path-agent)
                                ((eq path-effect :clear) *context-clear-agent*)
                                ((eq path-constituents :surfaces) *context-fill-agent*)
                                (t *context-stroke-agent*))
                          path-constituents path-effect path-rule)
                 (setf modifications 0))
               (assert-property (property)
                 (typecase property
                   (keyword (case property
                              ((:surfaces :lines :points)       ; constituents
                               (unless (eq path-constituents property)
                                 (unless (zerop (logand modifications #x100)) (do-call))
                                 (setf modifications (logior modifications #x100)
                                       path-constituents property)))
                              ((:winding :even-odd)     ; fill rule
                               (unless (eq path-rule property)
                                 (setf modifications (logior modifications #x010)
                                       path-rule property)))
                              ((:clear :invert :paint)    ; paint effect
                               (unless (eq path-effect property)
                                 (unless (zerop (logand modifications #x01)) (do-call))
                                 (setf modifications (logior modifications #x001)
                                       path-effect property)))
                              ((/ :/)              ; intermediate rendering
                               (do-call))))
                   (cons (apply (first property) (rest property)))
                   ;;; anything else is a path-agent
                   (t (setf path-agent (context-path-agent *projection-context* property))))))
      (declare (dynamic-extent #'assert-property))
      (etypecase drawing-mode
        (sequence (map nil #'assert-property drawing-mode))
        (keyword (assert-property *context-drawing-mode*)))
      ;; if some property has been asserted and remains outstanding, then
      ;; ensure that a rendering occurs to manifest it.
      (unless (zerop modifications) (do-call))))
    ;; w/o local inflections, just call the operator on the 
    (funcall geometry-operator
             (cond  ((eq *context-path-effect* :clear) *context-clear-agent*)
                    ((eq *context-path-constituents* :surfaces) *context-fill-agent*)
                    (t *context-stroke-agent*))
             *context-path-constituents*
             *context-path-effect*
             *context-path-rule*)))


;;;
;;; operators to manage display variables: colors (agents), effect, blend, ...
;;; the specific-dimensionality colors are treated as operators, while the "stroke" agent,
;;; in general, display variable is handled as a variable.


;;; make the color values self evaluating

(def-projection-operator color*3 (r g b)
  (:method ((context t) r g b)
           "the general method returns a tagged list of the arguments."
           (list 'color*3 r g b)))

(def-projection-operator color*4 (r g b a)
  (:method ((context t) r g b a)
           "the general method returns a tagged list of the arguments."
           (list 'color*4 r g b a)))


;;; note each variable and define the manipulation function

(def-projection-variable color-mode)
(def-projection-variable-op color-mode (mode &rest args))

(def-projection-variable drawing-mode)
(def-projection-variable-op drawing-mode (mode))

(def-projection-variable fill-agent)
(def-projection-variable-op fill-agent (color &optional g b a)
  (:argument-precedence-order color context)
  (:method ((context t) (color string) &optional g b a)
    (declare (ignore g b a))
    (context-set-fill-agent context (find-rgba color)))
  (:method ((context t) (color string) &optional g b a)
    (declare (ignore g b a))
    (context-set-fill-agent context (find-rgba color))))

(def-projection-variable stroke-agent)
(def-projection-variable-op stroke-agent (color &optional g b a)
  (:argument-precedence-order color context)
  (:method ((context t) (color symbol) &optional g b a)
    (declare (ignore g b a))
    (context-set-stroke-agent context (find-rgba color)))
  (:method ((context t) (color string) &optional g b a)
    (declare (ignore g b a))
    (context-set-stroke-agent context (find-rgba color))))

(def-projection-variable clear-agent)
(def-projection-variable-op clear-agent (color &optional g b a)
  (:argument-precedence-order color context)
  (:method ((context t) (color symbol) &optional g b a)
    (declare (ignore g b a))
    (context-set-clear-agent context (find-rgba color)))
  (:method ((context t) (color string) &optional g b a)
    (declare (ignore g b a))
    (context-set-clear-agent context (find-rgba color))))

(defun path-agent (agent-name &rest args)
  (declare (dynamic-extent args))
  (apply (ecase agent-name (:fill #'fill-agent) (:stroke #'stroke-agent) (:clear #'clear-agent))
         args))

(def-projection-variable path-constituents)
(def-projection-variable-op path-constituents (designator))

(def-projection-variable path-effect)
(def-projection-variable-op path-effect (effect))

(def-projection-variable path-rule)
(def-projection-variable-op path-rule (rule))

(defun inflect (&rest args)
  (declare (dynamic-extent args))
  (loop (unless args (return))
        (setf args (do-inflection (pop args) args))))

(defgeneric do-inflection (option args)
  (:documentation
   "recognize  a display variable from an initial argument, extract the
    related arguments and dispatch to the appropriate variable operator.
    this treats the instruction sequence as a quasi plist - one in which
    each property comprises a variable number of values.")
  (:method ((option cons) args)
           (path-constituents option)
           args)
  (:method ((option symbol) args)
           (labels ((variable-operator-p (datum)
                      (and (symbolp datum) (not (keywordp datum))))
                    (call-with-subseq (function list
                                                &optional (length (position-if #'variable-operator-p args)))
                      "extract the immediatly apparent argument subsequence from
                     a variable plist. if a symbol is present, this ends with
                     the preceeding element. otherwise it is the entire list.
                     spread the arguments in a temporary dynamic list and apply
                     the respective inflection operator."
                      ;; (print (list list length))
                      (cond (length
                             (locally (declare (type fixnum length)
                                               (type cons list))
                               (let ((subseq (make-list length)))
                                 (declare (dynamic-extent subseq))
                                 (mapl #'(lambda (from to) (setf (car to) (car from)))
                                       list subseq)
                                 (setf list (nthcdr length list))
                                 (funcall function subseq))
                               ;; return the remaining list
                               list))
                            (t
                             (funcall function list)
                             nil))))
             (cond ((keywordp option)  ;; the  operator is implicit in the value
                    (cond ((member option *path-agents*)
                           ;; call here adding the option back into the args
                           (flet ((set-agent (args) (apply #'path-agent option args)))
                             (declare (dynamic-extent #'set-agent))
                             (call-with-subseq #'set-agent args)))
                          ((gethash option *path-constituents-map*)
                           (path-constituents option)
                           args)
                          ((member option *path-effects*)
                           (path-effect option)
                           args)
                          ((member option *path-rules*)
                           (path-rule option)
                           args)
                          (t
                           (error "invalid projection variable value: ~s . ~s"
                                  option args))))
                   (t 
                    ;; color-mode, drawing-mode,
                    ;; fill-agent, stroke-agent, clear-agent, path-agent,
                    ;; path-constituents, path-effect, path-rule
                    (call-with-subseq
                     ;; call here with the option as the operator
                     #'(lambda (sub-args) (apply option sub-args))
                     args))))))


;;; 
;;; general state management for variable contexts

(defgeneric normalize-path-constituents (mode-designator)
  (:method ((mode-designator string))
           (or (find mode-designator *path-elementary-constituents*
                     :test #'string-equal)
               (error "invalid path mode: ~s" mode-designator)))
  (:method ((mode symbol))
           (if (and (boundp mode)
                    (not (eq mode (symbol-value mode))))
             (normalize-path-constituents (symbol-value mode))
             (normalize-path-constituents (symbol-name mode)))))


(defgeneric context-normalized-path-constituents (context front-mode back-mode)
  (:method ((context projection-variable-context) front-mode (back-mode null))
           (gethash (normalize-path-constituents front-mode) *path-constituents-map*))
  (:method ((context projection-variable-context) front-mode back-mode)
           ;; the default method uses the modes as a key to retrieve normalized values
           (let ((key (list (normalize-path-constituents front-mode)
                            (normalize-path-constituents back-mode))))
             (declare (dynamic-extent key))
             (gethash key *path-constituents-map*)))
  (:method ((context projection-variable-context) (combined cons) (back-mode null))
           "delegate to what may be a context-specific method for the individual components.
            in any case, the spread method should be used to normalize the values."
           (destructuring-bind (front back) combined
             (context-normalized-path-constituents context front back))))

(defmethod context-set-path-constituents ((context projection-variable-context) designator)
  (setq *context-path-constituents* designator))

(defmethod context-save-path-constituents ((context projection-variable-context))
  (push-projection-variable #'(lambda (saved-value) (setf  *context-path-constituents* saved-value)) *context-path-constituents*))



(defmethod context-set-path-rule ((context projection-variable-context) rule)
  (labels ((normalize-rule (rule)
             (or (etypecase rule
                   (keyword (find rule *path-rules*))
                   (string (find rule *path-rules* :test #'string-equal))
                   (symbol (when (and (boundp rule)
                                      (not (eq rule (setq rule (symbol-value rule)))))
                             (normalize-rule rule))))
                 (error "invalid path rule: ~s" rule))))
    (setq *context-path-rule* (normalize-rule rule))))

(defmethod context-save-path-rule ((context projection-variable-context))
  (push-projection-variable #'(lambda (saved-rule) (setf  *context-path-rule* saved-rule)) *context-path-rule*))


(defmethod context-set-path-effect ((context projection-variable-context) effect)
  (labels ((normalize-effect (effect)
             (or (etypecase effect
                   (keyword (find effect *path-effects*))
                   (string (find effect *path-effects* :test #'string-equal))
                   (symbol (when (and (boundp effect)
                                      (not (eq effect (setq effect (symbol-value effect)))))
                             (normalize-effect effect))))
                 (error "invalid path effect: ~s" effect))))
    (setq *context-path-effect* (normalize-effect effect))))

(defmethod context-save-path-effect ((context projection-variable-context))
  (push-projection-variable #'(lambda (saved-effect) (setf  *context-path-effect* saved-effect)) *context-path-effect*))


;;;
;;; coordinates

;;; support in opengl.
(def-projection-variable model-transform)
(def-projection-variable-op model-transform (op &rest parameters))

(def-projection-variable projection-transform)
(def-projection-variable-op projection-transform (op &rest parameters))

(def-projection-variable view-transform)
(def-projection-variable-op view-transform (op &rest parameters))


(defun transform (&rest args &aux target)
  (declare (dynamic-extent args))
  (flet ((do-transform (transformer)
           (let ((combination-op (pop args))
                 (arg-count (position-if #'keywordp args)))
             (if arg-count
               (locally (declare (type fixnum arg-count))
                 (let ((sub-args (make-list arg-count)))
                   (declare (dynamic-extent sub-args))
                   (mapl #'(lambda (from to) (setf (car to) (car from)))
                         args sub-args)
                   (setf args (nthcdr arg-count args))
                   ;; (print (cons combination-op sub-args))
                   (apply transformer combination-op sub-args)))
               (apply transformer combination-op (shiftf args nil))))))
    (loop (unless args (return))
          (case (first args)
            (:clear (pop args)
                    (view-transform :clear)
                    (projection-transform :clear))
            (:view (setf target (pop args)) (do-transform #'view-transform))
            (:projection (setf target (pop args)) (do-transform #'projection-transform))
            (t (ecase (or target (first args))
                 (:view (do-transform #'view-transform))
                 (:projection (do-transform #'projection-transform))))))))

;;;
;;; font names

(defun compute-font-parameters (font-spec)
  "given a fontspec (name size :bold :italic :underline :outline :shadow :condense :extend),
   compute the make-opengl-font parameters."
  (let ((name nil)
        (size nil)
        (style-names nil))
    (dolist (element font-spec)
      (typecase element
        (string (when name (error "Font Spec: ~s contains two names." font-spec))
                (setf name element))
        (integer (when size (error "Font Spec: ~s contains two sizes." font-spec))
                 (setf size element))
        (t
         (setf style-names (cons element style-names)))))
    (unless (and name size)
      (error "Font Spec: ~s contains lacks name and/or size." font-spec))
    (values name size (sort style-names #'string-lessp))))

(defun compute-font-style-mask (font-spec)
  (reduce #'logior font-spec
          :initial-value 0
          :key #'(lambda (element)
                   (etypecase element
                     (symbol
                      #-digitool 0
                      #+digitool
                      (or (rest (assoc element ccl::*style-alist*))
                          (error "Font Spec: ~s contains unknown option." font-spec)))
                     (number 0)
                     (string 0)))))
;; (compute-font-style-mask '("times" 0 :plain :italic))
         

(defgeneric font-spec-name (font-spec)
  (:method ((font-spec list))
           (multiple-value-bind (name size style-names)
                                (compute-font-parameters font-spec)
             (intern (format nil "~:@(~a-~{~a~^+~}-~a~)" name style-names size) :keyword))))

(defmethod context-initialize-fonts ((context projection-context))
  (loop for (id designator) in (context-default-fonts context)
        do (multiple-value-bind (font error)
                                (ignore-errors (define-font context designator id))
             (unless font
               (warn "cannot load font: ~s: ~s: ~a" context designator error)))))



;;;
;;; events


(defgeneric delegate-projection-task (task controller &rest args)
  (declare (dynamic-extent args))
  (:documentation
   "run a context's projection task. this is either a reified task, in which case the context's controller queues it for
    later action, or it is a function, in which case it is run immediately.")
  (:method ((task projection-task) (context projection-controller) &rest args)
           (de.setf.utility.lock:with-object-locked (context)
             (push (cons task args) (controller-tasks context))))
  (:method ((task projection-task) (context projection-context) &rest args)
           (apply #'delegate-projection-task task (og:context-controller context) args))
  (:method ((task function) (context projection-context) &rest args)
           (apply task context args)))

(defgeneric context-project (context &rest args)
  (:documentation
   "present a context's contents. the default method delegates to the context's controller,
    which, by default, queues a deferred draw operation.
    a context specialization may, alternatively, implement rendering as a directly executed
    method, or it may implement context-")
  (declare (dynamic-extent args))
  (:method ((context projection-context) &rest args)
           (declare (dynamic-extent args))
           (apply #'delegate-projection-task
                  (context-projection-task context) (context-controller context)
                  context
                  args)))

(defgeneric context-draw-contents (context &key destination)
  ;; this is a placeholder for some other redisplay method
  (:method ((context t) &key &allow-other-keys) nil))



(defgeneric controller-run-tasks (controller)
  (:method ((controller projection-controller))
           (let ((task.args nil)
                 (timestamp (de.setf.utility.lock:with-object-locked (controller)
                              (shiftf (controller-timestamp controller) nil))))
             (when timestamp
               (unwind-protect
                 (loop (unless (setf task.args (de.setf.utility.lock:with-object-locked (controller)
                                                 (pop (controller-tasks controller))))
                         (return))
                       (destructuring-bind (task . args) task.args
                         (unless (eql (task-timestamp task) timestamp)
                           (setf  (task-timestamp task) timestamp)
                           (apply (task-function task) args))))
                 (de.setf.utility.lock:with-object-locked (controller)
                   (setf (controller-timestamp controller)
                         (get-universal-time))))))))
;; (enqueue-projection-task (make-instance 'projection-task :function #'(lambda () (print :here))) (ccl:top-inspect-form))


(defgeneric context-click-event-handler (controller context where view)
  (:method ((controller null) (context t) (where t) (view t))
           )
  (:method ((controller t) (context null) (where t) (view t))
           )
  (:method ((controller t) (designator symbol) (where t) (view t))
           (context-click-event-handler controller (find-projection-context designator) where view))
  (:method ((designator symbol) (context t) (where t) (view t))
           (context-click-event-handler (find-projection-controller designator) context where view))
  (:method ((controller projection-controller) (context projection-context) (where t) (view t))
           "the desfault method does nothing"
           nil)
  )

(defgeneric context-button-up-event-handler (controller context where view)
  (:method ((controller null) (context t) (where t) (view t))
           )
  (:method ((controller t) (context null) (where t) (view t))
           )
  (:method ((controller t) (designator symbol) (where t) (view t))
           (context-button-up-event-handler controller (find-projection-context designator) where view))
  (:method ((designator symbol) (context t) (where t) (view t))
           (context-button-up-event-handler (find-projection-controller designator) context where view))
  (:method ((controller projection-controller) (context projection-context) (key t) (view t))
           "the default method does nothing"
           nil)
  )

(defgeneric context-key-event-handler (controller context key view)
  (:method ((controller null) (context t) (key t) (view t))
           )
  (:method ((controller t) (context null) (key t) (view t))
           )
  (:method ((controller t) (designator symbol) (key t) (view t))
           (context-key-event-handler controller (find-projection-context designator) key view))
  (:method ((designator symbol) (context t) (key t) (view t))
           (context-key-event-handler (find-projection-controller designator) context key view))
  (:method ((controller projection-controller) (context projection-context) (key t) (view t))
           "the default method does nothing"
           nil)
  )

(defgeneric context-key-up-event-handler (controller context key view)
  (:method ((controller null) (context t) (key t) (view t))
           )
  (:method ((controller t) (context null) (key t) (view t))
           )
  (:method ((controller t) (designator symbol) (key t) (view t))
           (context-key-up-event-handler controller (find-projection-context designator) key view))
  (:method ((designator symbol) (context t) (key t) (view t))
           (context-key-up-event-handler (find-projection-controller designator) context key view))
  (:method ((controller projection-controller) (context projection-context) (key t) (view t))
           "the desfault method does nothing"
           nil)
  )

(defgeneric context-expose-event-handler (controller context window)
  (:method ((controller null) (context t) (window t))
           )
  (:method ((controller t) (context null) (window t))
           )
  (:method ((controller t) (designator symbol) (window t))
           (context-expose-event-handler controller (find-projection-context designator) window))
  (:method ((designator symbol) (context t) (window t))
           (context-expose-event-handler (find-projection-controller designator) context window))
  (:method ((controller projection-controller) (context projection-context) (window t))
           "the default method enqueues a refresh task"
           (context-project context :destination window))
  )


;;;
;;; utilities

(def-projection-operator clear-view ())

(def-projection-operator flush-view ())

(def-projection-operator fill-view (&optional color))

(defun axis (&key (size *context-one*) (location (make-location-world))
                  (label-offset 4))
  (with-location-vectors ((x-axis (+ (location-x location) size)
                                  (location-y location)
                                  (location-z location))
                          (y-axis (location-x location)
                                  (+ (location-y location) size)
                                  (location-z location))
                          (z-axis (location-x location)
                                  (location-y location)
                                  (+ (location-z location) size)))
    (line location x-axis)
    (line location y-axis)
    (line location z-axis)
    (with-port-coordinates* (((x y) x-axis)) *projection-context*
      (when label-offset
        (incf x label-offset)
        (incf y label-offset)
        (text*2 x y "x" t)))
    (with-port-coordinates* (((x y) y-axis)) *projection-context*
      (when label-offset
        (incf x label-offset)
        (incf y label-offset)
        (text*2 x y "y" t)))
    (with-port-coordinates* (((x y) z-axis)) *projection-context*
      (when label-offset
        (incf x label-offset)
        (incf y label-offset)
        (text*2 x y "z" t)))))


(defgeneric context-log-message (context status control &rest args)
  (:method ((context null) (status t) (control t) &rest args)
           (declare (ignore args))
           nil)
  (:method ((context projection-context) (status t) (control t) &rest args)
           (declare (dynamic-extent args))
           (apply #'context-log-message (context-log context) status control args))
  (:method ((destination stream) (status t) (control t) &rest args)
           (declare (dynamic-extent args))
           (format destination "~&;;; ~a: " status)
           (apply #'format destination control args)))

#|
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


:de.setf.graphics
