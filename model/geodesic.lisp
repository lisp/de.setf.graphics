;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-


(in-package :de.setf.graphics.implementation)


(document "geodesic descriptions to generate geometries for the 'de.setf.graphics' library."
  (copyright
   "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
   "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")
  (history
   (delta 19940501 "jaa" "extracted from geodesic-render. now used to describe the sensors
   themselves in addition to the projections."))

  (long-description
   "This originated in the kernel of an acoustic simulator, where it was used to generate
 seonsor configurations. The surface geometry is useful for other things on its own.

<dt ID='DymaxionDescription'>Dymaxion Description:</dt>
<dd> a geodesic sphere is basis of both the geodesic sensors and, consequently,
of the dymaxion projections of simulation results. The sphere incorporates two
aspects: the kernel - the twenty-sided dodecahedron; and the surface
vertices - derived by triangular subdivision of the kernel faces.

 the simulation sensor uses the surfaces vertices to determine the direction
for sensor rays. the dymaxion projection, in turn, uses the position of the
rays respective vertex on a kernel surface to derive the planar projection.

 in both cases a particular arrangement of kernel surfaces applies. for the
sensor it determines the subset of the sphere surface which is actually sensed.
for the dymaxion projection it specifies the planar arrangement.

 this arrangement is specified recursively. the root is an initial
face and its immediate neighbors. this central face and its neighbors are
identified by an index which corresponds to the position in the polygon set
which describes the kernel dodecahedron.
 (See ray-sensor.lisp to find out what this order happens to be. it follows
 from the four primary geodesic faces and their five successive rotations.)
All further dependent faces are identified according to their relation to the
previous face. for any given face there is always one previous face (face 0),
one face one vertex away in the clockwise direction (face 1), and one face
at the distance of two vertices (face 2). for example, the arrangement:
<code>
                                                  ___
                                               /_\\ //_\
                                ___   ___   ___\ //_\   ___    
                                \ //_\\ //_\\ //_\\ //_\\ //_\
                                                  /_\\ /___
                                                  \ //_\\ /
</code>
could be described as:<br/>
<code>'(1 (12 (2 (1 (2 (1 (2 (1 (1 (1 (2 (2 (2))))) (2 (1 (2)) (2 (2 (1 (1 (1))))))))))))))</code><br/>
where the numbers of the first two faces (<code> 1</code>, <code> 12</code>) are arbitrary.

this description form is sufficient for all tiling arrangements of dodecahedron
faces which are connected. it can also, therefore, describe arrangements in
which a face appears more than once. for the projection this causes a sensed
value to appear at multiple locations on the projection surface, which is
improves scene coherence. for the sensor itself this redundance is unnecessary
and the faces are included once only.

subclasses of spherical and world coordinates are used in order to simplify
geodesic projection. they cache the respective surface and surface location.
"))

(defparameter +tau+ (/ (+ 1 (sqrt 5)) 2))
(defparameter +icosa-edge-length+ (/ 2 (* (expt 5 .25) (sqrt +tau+))))


(defclass map-polygon (polyline child)
  ((id
    :initform nil :initarg :id
    :accessor map-polygon-id)
   (parent
    :accessor map-polygon-parent)
   (neighbors
    :initform nil :initarg :neighbors
    :accessor map-polygon-neighbors)
   (center
    :initform nil :initarg :center
    :accessor map-polygon-center)
   (normal
    :initform nil :initarg :normal
    :accessor map-polygon-normal)
   (transformations
    :initform nil :initarg :transformations
    :accessor map-polygon-transformations))
  (:documentation
   "subtype of polygon for intersection functions"))

(defclass geodesic (component-node polyline triad)
  ((d-value :initarg :end :accessor end)
   (t-value :initarg :offset :accessor offset)
   (principle-faces :initarg :principle-faces :accessor principle-faces)
   (midpoints :initarg :midpoints :accessor midpoints))
  (:documentation
   " a specialized component which renders faces differently"))

(defstruct (cartesian-geodesic (:include location-world)
                               (:conc-name location-))
  face face-coordinate)

(defstruct (spherical-geodesic (:include location-spherical)
                               (:conc-name location-))
  face face-coordinate)

(defmethod cartesian->spherical
           ((c cartesian-geodesic) &optional (s (make-spherical-geodesic)))
  (setf (location-face s)
        (location-face c)
        (location-face-coordinate s)
        (location-face-coordinate c))
  (call-next-method))

(defmethod spherical->cartesian
           ((s spherical-geodesic) &optional (c (make-cartesian-geodesic)))
  (setf (location-face c)
        (location-face s)
        (location-face-coordinate c)
        (location-face-coordinate s))
  (call-next-method s c))


(defmethod initialize-instance :after
           ((instance map-polygon) &key)
  (let ((locations (locations instance)))
    (let ((first (first locations))
          (last-cons (last locations)))
      (when (not (equalp first (first last-cons)))
        (setf (rest last-cons) (list first))))))

(defmethod initialize-instance :after
           ((instance geodesic) &key)
  (with-slots ((location m-value) (end d-value) (offset t-value))
              instance
    (when (null offset) (setf offset (make-location-spherical)))
    (when (null location) (setf location (make-location-world)))
    (destructuring-bind (&key faces principle-faces vertices midpoints)
                        (generate-geodesic-geometry location offset)
      (setf (locations instance) vertices
            (children instance) faces
            (principle-faces instance) principle-faces
            (midpoints instance) midpoints
            end (first vertices)))))

(defmethod polygon-unique-points
           ((instance map-polygon))
  (subseq (locations instance) 0 3))

(defparameter *inside-out-matrix*
  (matrix-set 0.0d0 1.0d0 0.0d0 0.0d0
              1.0d0 0.0d0 0.0d0 0.0d0
              0.0d0 0.0d0 1.0d0 0.0d0
              0.0d0 0.0d0 0.0d0 1.0d0
              (transform-matrix)))

(defun turn-inside-out
       (geodesic)
  (dolist (face (children geodesic))
    (setf (map-polygon-transformations face)
          (mapcar #'(lambda (xform) (matrix-catenate xform *inside-out-matrix*))
                  (map-polygon-transformations face)))))

(defmethod project ((instance geodesic) view)
  "distinguish faces which are mapped from those which aren't. the
mapped faces are distance sorted and filled in order to approximate hidden
surfaces. the unmapped faces are rendered first as wireframes."
  (with-location-vectors ((l1) (l2) (l3))
    (flet ((compute-depth (face)
             (location-transform *projection-context* (map-polygon-center face) l1)
             (aref l1 2)))
      (declare (dynamic-extent #'compute-depth))
      (let* ((faces (sort (copy-list (children instance)) #'> :key #'compute-depth))
             (size (aref (location-magnitude (location-difference (location instance) (end instance) l1)) 3))
             (viewed-size (max (location-magnitude
                                (location-scale view (location-world size size 0.0d0) l1))
                               (location-magnitude
                                (location-scale view (location-world 0.0d0 size size) l2))
                               (location-magnitude
                                (location-scale view (location-world size 0.0d0 size) l3)))))
        
        (dolist (face faces)
          (poly (locations face) (if (map-polygon-transformations face)
                                   '(:erase :frame)
                                   '(:frame)))
          (when (= 1 (map-polygon-id face))
            (project (map-polygon-up-vector-projection face view)))
          (when (and (> viewed-size 32) (map-polygon-transformations face))
            (text (format nil "[~d]" (map-polygon-id face)) (map-polygon-center face) t)))))))


(defun geodesic-principle-face
       (geodesic &optional (id 1) (mapped-p t))
  (or (find-if #'(lambda (face)
                   (and (= id (map-polygon-id face))
                        (or (not mapped-p)
                            (map-polygon-transformations face))))
               (children geodesic))
      (error "requested face [~d] missing or unmapped" id)))

(defun map-polygon-up-vector-projection
       (face view)
  (let* ((center (map-polygon-center face))
         (normal (map-polygon-normal face))
         (direction (spherical->cartesian normal))
         (view-up (view-parameters-up (view-parameters view)))
         (vertex (location-sum center view-up (make-location-world))))
    (setf vertex (line-polygon-intersection
                  (make-line :location (location-sum direction center
                                                       (make-location-world))
                             :end vertex)
                  (list (ray-polygon-a face)
                        (ray-polygon-b face)
                        (ray-polygon-c face)
                        (ray-polygon-d face))))
    (make-line :location center :end vertex)))


(document "Face Parameter Derivation Functions"
  "Determine the face normal and the face center from the face vertices.
Note that the normal is a spherical, not a coordinate vector, as it is used to
determine rotation transformations, and is derived from the cross product,
rather than from the face center, as the latter method introduced inaccuracies
into the projection. This is even though the center point itself appears quite
adequate to determine face-to-origin translations...
 Neighbor id's are determined by checking for shared vertices.")


(defun generate-geodesic-vertex
       (location
        &key origin rotation
             (matrix (progn (check-type origin location-3)
                            (check-type rotation location-geodesic)
                            (matrix-translate
                             origin
                             (matrix-rotate
                              (make-location-world :x 0
                                                     :y (location-alpha rotation)
                                                     :z (location-beta rotation)))))))
  (location-transform matrix location location))

(defun generate-geodesic-geometry
       (&optional (origin (make-cartesian-geodesic)) (rotation (make-location-spherical))
        &aux (matrix (matrix-translate
                      origin
                      (matrix-rotate
                       (make-location-world :x 0
                                              :y (location-alpha rotation)
                                              :z (location-beta rotation))))))
  (flet ((make-rotated-coordinate
             (&key x y z &aux (c (make-cartesian-geodesic :x x :y y :z z)))
           (generate-geodesic-vertex c :matrix matrix)))
    (let* ((tau.5 (sqrt +tau+))
           (five.25 (expt 5 .25))
           (factor-a (/ tau.5 five.25))
           (factor-b (/ 1 (* five.25 tau.5)))
           (p1 (make-rotated-coordinate :x 0 :y factor-a :z factor-b))
           (p2 (make-rotated-coordinate :x factor-b :y 0 :z factor-a))
           (p3 (make-rotated-coordinate :x factor-a :y factor-b :z 0))
           
           (p4 (make-rotated-coordinate :x 0 :y factor-a :z (- factor-b)))
           (p5 (make-rotated-coordinate :x (- factor-b) :y 0 :z (- factor-a)))
           (p6 (make-rotated-coordinate :x (- factor-a) :y factor-b :z 0))
           
           (p7 (make-rotated-coordinate :x 0 :y (- factor-a) :z (- factor-b)))
           (p8 (make-rotated-coordinate :x factor-b :y 0 :z (- factor-a)))
           (p9 (make-rotated-coordinate :x factor-a :y (- factor-b) :z 0))
           
           (p10 (make-rotated-coordinate :x 0 :y (- factor-a) :z factor-b))
           (p11 (make-rotated-coordinate :x (- factor-b) :y 0 :z factor-a))
           (p12 (make-rotated-coordinate :x (- factor-a) :y (- factor-b) :z 0))
           
           (icosa-vertices (list p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12))

           ;; NB: the clockwise vertex order must be maintained as this will
           ;; be used to order neighbor faces when describing projections...
           (icosa-faces
            (generate-geodesic-polygons
             (list (make-polyline :locations (list p1 p2 p3))
                   (make-polyline :locations (list p4 p5 p6))
                   (make-polyline :locations (list p7 p8 p9))
                   (make-polyline :locations (list p10 p11 p12))
                   
                   (make-polyline :locations (list p6 p1 p4))
                   (make-polyline :locations (list p5 p7 p12))
                   (make-polyline :locations (list p9 p8 p3))
                   (make-polyline :locations (list p2 p11 p10))
                   
                   (make-polyline :locations (list p12 p6 p5))
                   (make-polyline :locations (list p7 p9 p10))
                   (make-polyline :locations (list p3 p8 p4))
                   (make-polyline :locations (list p1 p11 p2))
                   
                   (make-polyline :locations (list p10 p12 p7))
                   (make-polyline :locations (list p9 p3 p2))
                   (make-polyline :locations (list p4 p8 p5))
                   (make-polyline :locations (list p6 p11 p1))
                   
                   (make-polyline :locations (list p2 p10 p9))
                   (make-polyline :locations (list p3 p4 p1))
                   (make-polyline :locations (list p5 p8 p7))
                   (make-polyline :locations (list p12 p11 p6)))))
           (icosa-principle-faces
            (subseq icosa-faces 0 4))
           
           (icosa-edges
            (list (make-line :location p1 :end p2)
                  (make-line :location p2 :end p3)
                  (make-line :location p3 :end p1)
                  (make-line :location p1 :end p11)
                  (make-line :location p11 :end p2)
                  
                  (make-line :location p7 :end p5)
                  (make-line :location p5 :end p12)
                  (make-line :location p12 :end p7)
                  (make-line :location p7 :end p8)
                  (make-line :location p8 :end p5)
                  
                  (make-line :location p6 :end p4)
                  (make-line :location p4 :end p1)
                  (make-line :location p1 :end p6)
                  (make-line :location p6 :end p5)
                  (make-line :location p5 :end p4)
                  
                  (make-line :location p9 :end p10)
                  (make-line :location p10 :end p7)
                  (make-line :location p7 :end p9)
                  (make-line :location p9 :end p2)
                  (make-line :location p2 :end p10)
                  
                  (make-line :location p3 :end p8)
                  (make-line :location p8 :end p4)
                  (make-line :location p4 :end p3)
                  (make-line :location p3 :end p9)
                  (make-line :location p9 :end p8)
                  
                  (make-line :location p12 :end p11)
                  (make-line :location p11 :end p10)
                  (make-line :location p10 :end p12)
                  (make-line :location p12 :end p6)
                  (make-line :location p6 :end p11)))
           
           (icosa-midpoints
            (mapcar #'(lambda (line &aux (p1 (location line))
                                    (p2 (end line)))
		        (make-cartesian-geodesic :x (/ (+ (location-x p1)
                                                           (location-x p2)) 2)
				                  :y (/ (+ (location-y p1)
                                                           (location-y p2)) 2)
				                  :z (/ (+ (location-z p1)
                                                           (location-z p2)) 2)))
	            icosa-edges)))
      
      ;; The geodesic caches the complete set of vertices.
      (list :vertices icosa-vertices
            :midpoints icosa-midpoints
            :principle-faces icosa-principle-faces
            :faces icosa-faces))))

(defun generate-geodesic-polygons
       (faces)
  "Face Definitions:
     The icosa faces are derived from the exisiting icosahedraon definition
    (see ray-sensor.lisp), by augmenting the geometry information with an id
    used to specify the projection arrangement, center and normal parametersor
    use in deriving the face-specific projection transformation, 
    and with a,b,c,d surface parameters for intersection calculations."
  (flet ((derive-normal (tri)
           ;; nb. the order here is important, otherwise the vector
           ;; points the wrong way...
	   (let* ((points (locations tri))
		  (v1 (location-- (first points) (second points)))
		  (v2 (location-- (second points) (third points))))
	     (cartesian->spherical (location-cross v1 v2))))
         
         (derive-center (tri)
	   (let* ((points (locations tri))
                  (p1 (first points))
	          (p2 (second points))
	          (p3 (third points)))
	     (let* ((midp (location-normalize (location-+ p1 p2)
                                                (make-cartesian-geodesic) 2))
		    (through-center (location-- p3 midp
                                                  (make-cartesian-geodesic))))
	       (location-+ midp
                             (location-normalize through-center
                                                   through-center
                                                   (/ (location-distance p3 midp)
                                                      (* .5 (tan (/ pi 6)))))
                             (make-cartesian-geodesic)))))
         (derive-neighbors (tri trilist)
           ;; neighbor faces share exactly two vertices.
           (let ((ppoints (polygon-unique-points tri)))
             (remove-if-not
              #'(lambda (test-tri)
                  (let ((tpoints (polygon-unique-points test-tri)))
                    (= (length (intersection ppoints tpoints))
                       2)))
              trilist))))

    (let* ((i 0)
	   (map-faces (map 'list
			   #'(lambda (p &aux (ppoints  (locations p))
				        (center (derive-center p))
				        (normal (derive-normal p)))
			       (make-instance 'map-polygon :id (incf i)
                                              :parent nil
                                              :neighbors nil
                                              :center center
                                              :normal normal
                                              :transformations nil
                                              :locations ppoints))
			   faces)))
      
     ;; Derive the neighbor list and order it in the clockwise direction to match projection description
     ;; convention. This is done by making sure that the order agrees with that of the faces vertices,
     ;; as they are already clockwise.
      (mapc #'(lambda (cf)
	        (setf (map-polygon-neighbors cf)
		      (sort (derive-neighbors cf map-faces)
			    #'(lambda (nfp1 nfp2)
			        (ecase nfp1
				  (1 (ecase nfp2 (2 t) (3 t)))
				  (2 (ecase nfp2 (1 nil) (3 nil)))
				  (3 (ecase nfp2 (1 nil) (2 t)))))
			    :key #'(lambda (nf)
                                    "the key function returns 1, 3, and 2, for succesive neighbors."
                                    (apply #'+ (remove nil
                                                       (mapcar #'(lambda (v)
                                                                   (position v (polygon-unique-points cf)))
                                                               (polygon-unique-points nf))))))))
	    map-faces)
      (mapc #'init-polygon map-faces)
      map-faces)))

(defmethod extract-geodesic-map
           ((self geodesic))
  (let* ((face-paths
          (mapcar #'default-path-to-root (children self)))
         (face-path-ids
          (mapcar #'(lambda (path) (mapcar #'map-polygon-id path))
                  (remove-if-not #'map-polygon-transformations
                                 (remove nil face-paths) :key #'first)))
         (face-nodes nil)
         (tree nil))
    (setf face-path-ids
          (sort face-path-ids #'< :key #'length))
    (dolist (path face-path-ids)
      (let* ((face (first path))
             (parent (second path))
             (node (list face)))
        (cond ((null parent)
               (setf tree node) 
               (push node face-nodes))
              (t
               (push node face-nodes)
               (push node
                     (rest (or (find parent face-nodes :test #'=
                                     :key #'(lambda (node) (first node)))
                               (error "parent missing"))))))))
    tree))

(defmethod arrange-geodesic
       ((map geodesic) spec &optional (map-origin (make-location-world)))
  (dolist (p (children map))
    (setf (map-polygon-transformations p) nil
	  (map-polygon-parent p) nil))
  (let* ((root (find (first spec) (children map)
                     :key #'map-polygon-id :test #'=))
         (complete-transform (individual-face-transform root map-origin)))
    (setf (map-polygon-transformations root) (list complete-transform))
    (mapc #'(lambda (n)
	      (traverse-spec-tree root (find (first n)
                                             (children map)
					     :key #'map-polygon-id :test #'=)
				  complete-transform (rest n)))
	  (rest spec))
    root))

(defmethod map-polygon-neighbor
           ((face map-polygon) id
            &optional parent
            &aux (neighbors (map-polygon-neighbors face)))
  (check-type id (or integer (member + ++ @ - --)))
  (if (numberp id)
    (or (find id (map-polygon-neighbors face)
              :test #'= :key #'map-polygon-id)
        (error "polygon [~d] does not neighbor [~d]."
               id (map-polygon-id face)))
    (elt (map-polygon-neighbors face)
         (mod (+ (cdr (assoc id
                             '((@ . 0) (+ . 1) (++ . 2) (- . -1) (-- . -2))))
                 (if parent (position parent neighbors) 0))
              (length neighbors)))))

(defun traverse-spec-tree
       (parent node parent-transform children)
  "calculate the face-specific transformation based on the face position
and rotation, and the preceeding face transformation."
  (let ((node-transform (complete-face-transform parent node
                                                 parent-transform)))
    (setf (map-polygon-parent node) parent)
    (push node-transform (map-polygon-transformations node))
    (dolist (child-spec children)
      (assert (consp child-spec))
      (traverse-spec-tree node
                          (map-polygon-neighbor node (first child-spec)
                                                parent)
			  node-transform
			  (rest child-spec)))
    node))

(document "Dymaxion Transformation Derivation:"
  "The respective face projection transformations are derived by composing the
projection necessary to place the face in the view surface with the translation
and rotation transformation necessary to bring the correct edge into contact
with its neighbor(s).
 in order to visualize what's happening here, please keep in mind that the
the icosa vertices are derived from the corners of three rectangles which
intersect at the origin: one in each of the x, y, and z planes. 


 The components of each face's transformation are determined as follows:
<ul><li> the transformation to the projection surface is a translation and
rotation derived from the face center and normal.</li>
    <li> the transformation within the surface is a translation of a shared
vertex to the origin, a rotation into conformance with the preceeding neighbor
face, and a translation to the shared vertex location.</li></ul>

note that 1) alpha is around the y-axis (z-axis zero), beta is around the
x-axis (z-axis 0); 2) beta in spherical coordinate has a sign opposite to that
of transformation rotations; and 3) beta must be rotated prior to alpha,
otherwise, the beta rotation is not actually around the x-axis.")


(defun at-origin-transformation
       (face)
  (let ((normal (map-polygon-normal face)))
    (matrix-catenate
     (matrix-catenate (matrix-rotate (make-location-world
                                      :x 0
                                      :y (- (location-alpha normal))
                                      :z 0))
                      (matrix-rotate (make-location-world
                                      :x (location-beta normal)
                                      :y 0
                                      :z 0)))
     (matrix-rotate (make-location-world :x 0 :y pi :z 0)))
     ))
(defun at-origin-transformation
       (face)
  (let ((normal (map-polygon-normal face)))
     (matrix-catenate (matrix-rotate (make-location-world
                                      :x 0
                                      :y (- (location-alpha normal))
                                      :z 0))
                      (matrix-rotate (make-location-world
                                      :x  (location-beta normal)
                                      :y 0
                                      :z 0)))))

(defun individual-face-transform
       (root map-origin)
  (let* ((center (map-polygon-center root))
	 (to-origin-transform
          (matrix-translate (location-normalize center
                                                  (make-location-world)
                                                  -1)))
	 (at-origin-transform
          (matrix-catenate (at-origin-transformation root)
                           (matrix-translate map-origin))))
    (matrix-catenate to-origin-transform at-origin-transform)))

(defun complete-face-transform
       (parent node parent-transform)
  (let* ((center (map-polygon-center node))
         (to-origin-transform (matrix-translate
                               (location-normalize center
                                                     (make-location-world)
                                                     -1)))
         (at-origin-rotation-transform
          (at-origin-transformation node))
         (to-origin-and-flat
          (matrix-catenate to-origin-transform at-origin-rotation-transform))
         (complete-transform
          (matrix-catenate to-origin-and-flat
                           (at-origin-translation-transform node parent
                                                            to-origin-and-flat
                                                            parent-transform))))
    complete-transform))

(defun at-origin-translation-transform
       (face parent face-transform parent-transform)
  #|calculate the transformation necessary to get a neighbor to abut with its parent.
    the supplied face takes a point only to the x,y plane relative to the origin.
    an additional translation and rotation are required to position it relative to the parent.|#
  (flet ((polygon-shared-vertices (p1 p2)
	   (remove-if-not #'(lambda (p) (find p (polygon-unique-points p1)))
			  (polygon-unique-points p2))))
    (let ((shared-vertices (polygon-shared-vertices face parent)))
      (if (/= (length shared-vertices) 2)
	  (error "faces ~d and ~d do not share vertices." (map-polygon-id face) (map-polygon-id parent)))
      (let ((f1-xformed (location-transform face-transform (first shared-vertices)
                                              (make-location-world)))
	    (f2-xformed (location-transform face-transform (second shared-vertices)
                                              (make-location-world)))
	    (p1-xformed (location-transform parent-transform (first shared-vertices)
                                              (make-location-world)))
	    (p2-xformed (location-transform parent-transform (second shared-vertices)
                                              (make-location-world))))
	(let* ((xlation (location-- p1-xformed f1-xformed))
	       (fvec (location-- (location-+ f2-xformed xlation) p1-xformed))
	       (pvec (location-- p2-xformed p1-xformed))
	       (rotation (acos (/ (location-dot fvec pvec)						
				  (* (location-distance p1-xformed p2-xformed)
				     (location-distance f1-xformed f2-xformed)))))
	       (cross (location-cross fvec pvec)))
	  (when (complexp rotation) ; (cerror "take real part..." "complex angle ~a" rotation)
	    (setf rotation (realpart rotation)))
	  (if (minusp (location-z cross))
	      (setf rotation (- rotation)))
	  (matrix-catenate (matrix-catenate (matrix-translate (location-normalize f1-xformed
										    (make-location-world) -1))
					    (matrix-rotate (make-location-world :x 0 :y 0
									      :z rotation)))
			   (matrix-translate p1-xformed)))))))



(defun default-path-to-root
       (face &optional (root-face-p #'(lambda (face)
                                        (= (map-polygon-id face) 1)))
        &aux (traversed-faces nil) (generated-paths (list (list face))))
  (labels ((extend-path-to-root (path)
             (setf generated-paths
                   (nconc generated-paths
                          (remove nil
                                  (mapcar #'(lambda (neighbor)
                                              (when (and (not (find neighbor traversed-faces))
                                                         (map-polygon-transformations neighbor))
                                                (push neighbor traversed-faces)
                                                (cons neighbor path)))
                                          (map-polygon-neighbors (first path))))))))
    (do ((path (pop generated-paths) (pop generated-paths)))
        ((null path))
      (if (funcall root-face-p (first path))
        (return (nreverse path))
        (extend-path-to-root path)))))

(defun default-face-transform
       (face)
  (let* ((path-to-root (default-path-to-root face))
         (parent (second path-to-root)))
    (if parent
      (complete-face-transform parent face
                               (first (map-polygon-transformations parent)))
      (if path-to-root
        ;the face is the root itself...
        (individual-face-transform face (make-location-world))))))


;(mapcar #'map-polygon-id (default-path-to-root (elt (children *geodesic*) 0)))


#+(or )
(progn
  (defmethod draw-self
             ((self geodesic) view)
    "distinguish faces which are mapped from those which aren't. the
mapped faces are distance sorted and filled in order to approximate hidden
surfaces. the unmapped faces are rendered first as wireframes."
    (let* ((faces (children self))
           (filled-faces (sort (remove-if-not #'map-polygon-transformations
                                              faces)
                               #'>
                               :key #'(lambda (face &aux (center (map-polygon-center face)))
                                        (location-z
                                         (location-transform view center
                                                             #@(world 0 0 0)))))))
      (when (/= (length faces) (length filled-faces))
        (with-display-attributes (list :polymode '(:frame))
          (:view view)
          (dolist (face faces)
            (when (not (find face filled-faces))
              (draw-self face view)))))
      (when filled-faces
        (with-display-attributes (list :polymode '(:erase :frame))
          (:view view) ;(break)
          (dolist (face filled-faces)
            (draw-self face view))))))
  
  (mapcar #'map-polygon-id
          (default-path-to-root (elt (children *geodesic*) 18)))
  (initialize-map-tree
   '(1 (12 (2 (1 (2 (1 (2 (1 (1 (1 (2 (2 (2))))) (2 (1) (2 (2 (1 (1 (1)))))))))))))))
  (initialize-map-tree
   '(1 (12 (2 (1 (2 (1 (2 (1 (1 (1 (2 (2 (2))))))))))))))
  (initialize-map-tree
   '(1 (12) (14) (18)))
  )

;;(generate-geodesic-polygons)


:de.setf.graphics

