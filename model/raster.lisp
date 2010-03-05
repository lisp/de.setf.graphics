;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)

(document "This file defines a bitmap scene model element."
 
 (copyright "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
             "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (history
  (copyright 19880619 "james anderson dtmg mit")
  (delta 19880619 "new")
  (copyright 20060706 "[james anderson](james.anderson@setf.de)")
  (delta 20060706 "corrected index precedence to treat data as arranged
   in scanline order. this means that, in x-y terms, y/height is dimension 0
    and x/width is dimension 1. double-checked against the clx, opengl,
    quickdraw and core-graphics sample-projection implementation, that they
    treat the indices correctly.")))


;;;

(defvar *standard-gif-colors*
  (let ((list nil)
        (basics '(#x00 #x33 #x66 #x99 #xcc #xff))
        (extras '(#x11 #x22 #x44 #x55 #x77 #x88 #xaa #xbb #xdd #xee)))
    (dolist (r basics)
      (dolist (g basics)
        (dolist (b basics)
          (push (list r g b) list))))
    (dolist (i (append basics extras))
      (pushnew (list i 0 0) list :test #'equalp)
      (pushnew (list 0 i 0) list :test #'equalp)
      (pushnew (list 0 0 i) list :test #'equalp)
      (pushnew (list i i i) list :test #'equalp))
    (make-array 256 :initial-contents (nreverse list)))
  "see
   http://www.hanover.edu/chem/work/Netscape.colors.html
   http://www.google.com/search?client=safari&rls=en&q=standard+256+color+map&ie=UTF-8&oe=UTF-8")

(defvar *braille-cells*
  (let ((table (make-hash-table :test 'eql)))
    (dolist (entry '(((#\a #\A #\1) 1 0 0 0 0 0) ((#\b #\B #\2) 1 0 1 0 0 0) ((#\c #\C #\3) 1 1 0 0 0 0)
                     ((#\d #\D #\4) 1 1 0 1 0 0) ((#\e #\E #\5) 1 0 0 1 0 0) ((#\f #\F #\6) 1 1 1 0 0 0)
                     ((#\g #\G #\7) 1 1 1 1 0 0) ((#\h #\H #\8) 1 0 1 1 0 0) ((#\i #\I #\9) 0 1 1 0 0 0)
                     ((#\j #\J #\0) 0 1 1 1 0 0) ((#\k #\K) 1 0 0 0 1 0) ((#\l #\L) 1 0 1 0 1 0)
                     ((#\m #\M) 1 1 0 0 1 0) ((#\n #\N) 1 1 0 1 1 0) ((#\o #\O) 1 0 0 1 1 0)
                     ((#\p #\P) 1 1 1 0 1 0) ((#\q #\Q) 1 1 1 1 1 0) ((#\r #\R) 1 0 1 1 1 0)
                     ((#\s #\S) 0 1 1 0 1 0) ((#\t #\T) 0 1 1 1 1 0) ((#\u #\U) 1 0 0 0 1 1)
                     ((#\v #\V) 1 0 1 0 1 1) ((#\w #\W) 0 1 1 1 0 1) ((#\x #\X) 1 1 0 0 1 1)
                     ((#\y #\Y) 1 1 0 1 1 1) ((#\z #\Z) 1 0 0 1 1 1) ((#\space) 0 0 0 0 0 0)
                     ((#\( #\)) 0 0 1 1 1 1) ((#\!) 0 0 1 1 1 0) ((#\.) 0 0 1 1 0 1)
                     ((#\,) 0 0 1 0 0 0) ((#\` #\?) 0 0 1 0 1 1) ((#\') 0 0 0 1 1 1)
                     ((#\_ #\-) 0 0 0 0 1 1) ((t) 1 1 1 1 1 1)))
      (destructuring-bind (characters . contents) entry
        (let ((data (make-array '(3 2))))
          (dotimes (i 6) (setf (row-major-aref data i) (pop contents)))
          (dolist (char characters)
            (setf (gethash char table) data)))))
    table))

(defparameter *braille-foreground-color* '(224 224 224))

(defparameter *braille-background-color* '(32 32 32))


;;;
;;;

(defclass raster (located-node dyad)
  ((d-value
    :initform nil :initarg :sample-data
    :accessor sample-data
    :documentation
    "a 2d array which contains the sample data.")
   (sample-depth
    :initform 8 :initarg :sample-depth
    :accessor sample-depth
    :documentation
    "the number of bits used to reperesent a pixel.")
   (port-offset
    :initform (make-point 0 0) :initarg :port-offset
    :accessor port-offset
    :documentation
    "a port location which specifies the offset relative to the transformed
     port location to which the raster data is to be copied.")
   (generation
    :initform 0
    :accessor sample-generation
    :documentation
    "binds an integer which tracks generations for the raster data.
     this distinguishes state of the raster content from the instance
     identity.")
   (projection-generations
    :initform nil
    :accessor sample-projection-generations))
  (:documentation
   "a raster comprises an object location and size, a sample offset and size,
    a port offset, and a 2d sampled data array. it is projected onto a view by
    transforming the location to the port, offsetting it and transferring the
    specified region of the sample. a cached implementation-specific
    representation, is created and cached if that is supported.
    the data are stored in row-major order, that is scanline pixels are
    adjacent."))

(defmethod sample-size ((sample raster))
  "return a point to specify the size of the raster.
   this is _not_ the same as the array dimensions, as the order is reversed."
  (let ((data (sample-data sample)))
    (if data
      (make-point (array-dimension data 1) (array-dimension data 0))
      (make-point 0 0))))

(defgeneric sample-filter (raster value x y)
  (:documentation
   "compute the pixel value for a sample at the given location. 
    the general method rounds floats and leaves integer values unmodified.")
  (:method ((raster raster) (data float) (x t) (y t))
           (round data))
  (:method ((raster raster) (data integer) (x t) (y t))
           data)
  (:method ((raster raster) (data list) (x t) (y t))
           (destructuring-bind (r g b . rest) data
             (declare (ignore rest))
             (+ (ash r 16) (ash g 8) b)))
  (:method ((raster raster) (filter function) x y)
           (funcall filter (aref (sample-data raster) x y))))

(defmethod projection-generation ((raster raster) (context t))
  (getf (sample-projection-generations raster) context))

(defmethod set-projection-generation ((raster raster) (context t))
  (setf (getf (sample-projection-generations raster) context)
        (sample-generation raster)))

(defmethod test-projection-generation ((raster raster) (context t))
  (eql (getf (sample-projection-generations raster) context)
        (sample-generation raster)))

(defmethod print-object-slots d-value ((instance raster) (stream t))
  (let ((*print-array* nil))
    (format stream ":sample-data ~s"
            (when (slot-boundp instance 'd-value)
              (slot-value instance 'd-value)))))

;;;
;;; braille glyph support

(defun copy-braille-cell (designator data row column &key (cells *braille-cells*))
  "set the locations in the array as per the braille cell.
   if no glyph is present use 't.
   the array must be an array of sequence elements each with at least r,g,b elements."
  (let ((cell (or (gethash designator cells)
                   (gethash t cells)
                   #((1 1) (1 1) (1 1)))))
    (dotimes (i 3)
      (dotimes (j 2)
        (let ((entry (aref data (+ row i) (+ column j)))
              (color (ecase (aref cell i j) 
                       (0 *braille-background-color*)
                       (1 *braille-foreground-color*))))
          (replace entry color))))))

#|
;;; single glyphs
(let ((data (make-array (list 4 8))))
  (dotimes (i (array-total-size data)) (setf (row-major-aref data i) (list 0 0 0 0)))
  (copy-braille-cell #\y data 0 0)
  (inspect data))


(let ((data (make-array (list 4 (* (hash-table-count *braille-cells*) 4)))))
  (dotimes (i (array-total-size data)) (setf (row-major-aref data i) (list 0 0 0 0)))
  (let ((i 0) (start (char-code #\A)) (end (char-code #\Z)))
    (do ((c start (1+ c))) ((> c end))
      (copy-braille-cell (char-upcase (code-char c)) data 0 i)
      (incf i 4)
      (copy-braille-cell (char-downcase (code-char c)) data 0 i)
      (incf i 4))
    (copy-braille-cell #\- data 0 i) (incf i 4)
    (copy-braille-cell #\_ data 0 i) (incf i 4)
    (copy-braille-cell #\space data 0 i) (incf i 4)
    (copy-braille-cell #\$ data 0 i) (incf i 4)
    )
  (let* ((raster (make-instance 'og.impl::raster
                  :location (og:location-world 0 0 0)
                  :sample-depth 32
                  :sample-data data))
         (gif (og.impl::make-gif raster)))
    (skippy::write-gif gif #P"ECONORAVEN:DATA;braille.gif")))
    
|#


  

(defmethod make-gif ((raster raster))
  "transform a raster instance into a gif instance.
   compute the color map naively by eliminating the duplicates from the data.
   if there are more than 256 entries, then use the closest entry from the standard table. 
   this should use a histogram rather than order of appearance."
  (let* ((size (sample-size raster))
         (data-vector (make-array (* (point-h size) (point-v size))
                                  :displaced-to (sample-data raster)))
         (colors (remove-duplicates data-vector :test #'equalp :from-end t))
         (color-map nil))
    ;; if there are too many colors in the image, then ignore them and replace them 
    ;; with standard gif colors. whichever colrs serve, use them to build the
    ;; color-map.
    (unless (<= (length colors) 256)
      (setf colors *standard-gif-colors*))
    (let ((vector (make-array (* 256 3) :initial-element 0)))
      (do ((i 0 (1+ i))) ((>= i (min 256 (length colors))))
        (let ((color (aref colors i)))
          (setf (aref vector (* i 3)) (elt color 0))
          (setf (aref vector (+ 1 (* i 3))) (elt color 1))
          (setf (aref vector (+ 2 (* i 3))) (elt color 2))))
      (setf color-map vector))

    ;; now translate the image into a sequence of indices in the color map
    ;; for this use the colors themselves as the registry - the color map is in
    ;; the order.
    (flet ((color-index (pixel)
             (or (and (<= (length colors) 256)
                      (position pixel colors :test #'equalp))
                 (let ((distance most-positive-fixnum)
                       (index -1))
                   (dotimes (i 256)
                     (let* ((color (aref colors i))
                            (color-distance (reduce  #'+ (mapcar #'abs (mapcar #'- color pixel)))))
                       (when (< color-distance distance)
                         (setf distance color-distance
                               index i))))
                   (cond ((minusp index)
                          (warn "no color map entry for ~s." pixel)
                          0)
                         ((>= index (length color-map)) (break "bad color"))
                         (t
                          index))))))
      (make-instance 'skippy::gif
        :width (point-h size) :height (point-v size)
        :bpp 8
        :color-table color-map
        :image-data (map 'vector #'color-index data-vector)))))

;;; (inspect (make-gif *test-raster*))
;;; (skippy::write-gif (make-gif *test-raster*) "ECONORAVEN:DATA;test-raster.gif")

;;; 

#|

;;; tests for gif encoding
(skippy::write-gif
 (make-instance 'skippy::gif :width 256 :height 256 :bpp 8
                :color-table (make-array 768 :initial-element 0)
                :image-data (make-array (* 256 256) :initial-element 0))
 #p"ECONORAVEN:DATA;black.gif")

(let* ((width 256) (height 256) (size (* width height))
       (data (make-array size))
       (colors (make-array 768)))
  (dotimes (x 256)
    (setf (aref colors (* x 3)) x
          (aref colors (+ (* x 3) 1)) x
          (aref colors (+ (* x 3) 2)) x))
  (dotimes (x width)
    (dotimes (y height) 
      (setf (aref data (+ (* y width) x)) x)))
  (skippy::write-gif
   (make-instance 'skippy::gif :width width :height height :bpp 8
                  :color-table colors
                  :image-data data)
   #p"ECONORAVEN:DATA;gradient-h.gif"))

(let* ((width 256) (height 256) (size (* width height))
       (data (make-array size))
       (colors (make-array 768)))
  (dotimes (x 256)
    (setf (aref colors (* x 3)) x
          (aref colors (+ (* x 3) 1)) x
          (aref colors (+ (* x 3) 2)) x))
  (dotimes (x width)
    (dotimes (y height) 
      (setf (aref data (+ (* y width) x)) (floor (* 256 (/ (+ (* y width) x) size))))))
  (skippy::write-gif
   (make-instance 'skippy::gif :width width :height height :bpp 8
                  :color-table colors
                  :image-data data)
   #p"ECONORAVEN:DATA;gradient-v.gif"))

(let* ((width 256) (height 256) (size (* width height))
       (colors (make-array 768)))
  (dotimes (x 256)
    (setf (aref colors (* x 3)) x
          (aref colors (+ (* x 3) 1)) x
          (aref colors (+ (* x 3) 2)) x))
  (skippy::write-gif
   (make-instance 'skippy::gif :width width :height height :bpp 8
                  :color-table colors
                  :image-data (make-array size
                                          :initial-contents (loop for x from 0 below size collect (random 256))))
   #p"ECONORAVEN:DATA;random.gif"))
|#


;;; scene model elements and operators
;;; date from object lisp...

#+(or) 
(progn

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (export '(og-pixarray og-pixarray-p
	      og-pixarray-location og-pixarray-end
	      og-pixarray-name og-pixarray-pixels og-pixarray-colormap og-pixarray-screen-pixels
              
              draw-self draw-region
              make-self
              mapbitmap list->bitmap bitmap->list)))

  (eval-when (eval compile load)
    (defgraphicElement (og-pixarray (:print-function print-og-pixarray))
      name				; pathname for persistent storage
      colormap				; (n 3) array of rgb
      pixels				; (w h) array of display-independent pixels
      screen-pixels			; ((visual . (w h) array)* ) of display visual dependent pixels
      ))
  
;;;; ---------------------------------------------------------------------------
;;;; drawing methods


;;;; ---------------------------------------------------------------------------
;;;; editing method

(defclassFun (edit-self og-pixarray)
          (where drawable context &optional (parameters (make-og-pixarray))
           &aux (the-bitmap (slot-default parameters og-pixarray handle))
                (loc (slot-default parameters og-pixarray location))
                (size (subtract-points (slot-default parameters og-pixarray end)
                                       loc)))
  (when the-bitmap
    (if (typep drawable *dialog*)
      (ask drawable
        (add-dialog-items (oneof *bitmap-edit-dialog-item*
                                 :dialog-item-bitmap the-bitmap
                                 :dialog-item-position loc)))
      (oneof (list *dialog* *og-context*)
             :window-position (ask drawable (local-to-global loc))
             :window-size size
             :window-type :single-edge-box
             :window-show t
             :dialog-items (list (oneof *bitmap-edit-dialog-item*
                                        :dialog-item-bitmap the-bitmap
                                        :dialog-item-position (make-point 0 0)))))))


;;;; ---------------------------------------------------------------------------

(defOBJECT *bitmap-edit-dialog-item* *og-graphic* *user-dialog-item*)

(defOBFUN (exist *bitmap-edit-dialog-item*)
          (init-list
           &aux (bitmap (getf init-list :dialog-item-bitmap)))
  (when bitmap
    (usual exist self (init-list-default init-list
					 :dialog-item-size (subtract-points
							    (rref bitmap bitmap.bounds.bottomright)
							    (rref bitmap bitmap.bounds.topleft))))
    (have 'dialog-item-window)
    (have 'dialog-item-window-original-bitmap)
    (have 'dialog-item-bitmap bitmap)))

(defOBFUN (add-self-to-dialog *bitmap-edit-dialog-item*)
          (dialog
           &aux (my-bits dialog-item-bitmap))
  (usual add-self-to-dialog self dialog)
  (setq dialog-item-window
        (oneof *og-context*
               :window-position (make-point 0 0)
               :window-size (dialog-item-size)
               :window-show nil))
  (setq dialog-item-window-original-bitmap
        (ask dialog-item-window
          (copy-record (rref wptr grafport.portbits) :bitmap)))
  (ask dialog-item-window (set-bits my-bits)))

(defOBFUN (remove-self-from-dialog *bitmap-edit-dialog-item*)
          (&aux (its-bits dialog-item-window-original-bitmap))
  (usual remove-self-from-dialog self)
  (ask dialog-item-window
    (with-port wptr
      (_SetPBits :ptr its-bits))
    (window-close))
  (setq dialog-item-window-original-bitmap
        (dispose-record dialog-item-window-original-bitmap)))

(defOBFUN (dialog-item-draw *bitmap-edit-dialog-item*)
          (&aux (dialog my-dialog)
                (location (dialog-item-position)))
  (if (and dialog-item-window dialog)
    (ask dialog-item-window (show-bits dialog location))))

(defOBFUN (dialog-item-click-event-handler *bitmap-edit-dialog-item*)
          (where &aux s tool new-item location dialog)
  (cond ((double-click-p)
         (setq s (self))
         (ask my-dialog
           (remove-dialog-items s)
           (if (null (dialog-items))
             (window-close))))
        ((setq tool (ask my-dialog (property :drawing-tool 'segment-tool)))
         (apply tool (list where)))))

(defOBFUN (set-content *bitmap-edit-dialog-item*)
          (&rest new-elements)
  ;; intercept set-content and draw it into the bitmap window
  (if (keywordp (car new-elements))
    (setq new-elements (cdr new-elements)))
  (ask dialog-item-window
    (mapc #'draw-element new-elements))
  (dialog-item-draw))

;;;; ---------------------------------------------------------------------------
;;;; constructor function

(defclassFun (make-self og-pixarray) (definition &aux pixarray-pixels pixarray-colormap
                                                   pixarray-location pixarray-end
						   (pixarray-name nil)
						   (keys (cddr definition)))
  (declare (object-variable location end pixels screen-pixels colormap attributes properties))
  (setf pixarray-location (or (first definition) location))
  (setq pixarray-end (or (second definition) end))
  (typecase (third definition)
    (cons
     (rlet ((br :rect))
       (points-to-rect  pixarray-end pixarray-location br)
       (setq pixarray-pixels
             #+:ccl (make-bitmap 0 (subtract-points (rref br rect.bottomright) (rref br rect.topleft)))
	     #+:clx (make-bitmap :bounds br)))
     (list->bitmap pixarray-pixels (third definition)))
    ((or string pathname)
     (setq pixarray-name (third definition))
     (let ((image #+:ccl (read-mac-resource :bitmap pixarray-name)
		  #+:clx (read-gif pixarray-name)))
       (setq pixarray-pixels (gif-pixels image)
	     pixarray-colormap (getf (second (assoc "GIF87a" image :test #'equal)) :color-map))))
    (array
     (setq pixarray-pixels (third definition)
	   pixarray-colormap (fourth definition))))    
  (make-og-pixarray :location pixarray-location
		    :end pixarray-end
		    :pixels pixarray-pixels
		    :screen-pixels nil
		    :colormap pixarray-colormap
		    :name pixarray-name
		    :attributes (or (getf keys :attributes) attributes)
		    :properties (or (getf keys :properties) properties)))

;;;; ---------------------------------------------------------------------------
;;;; print function

(defun print-og-pixarray (bitmap &optional (stream *standard-output*)
                                          (print-level *print-level*))
  (declare (special *print-level* *standard-output*)
           (ignore print-level))
  (write-string "#g(og-pixarray " stream)
  (print-coordinate (og-pixarray-location bitmap) stream)
  (write-string " " stream)
  (print-coordinate (og-pixarray-end bitmap) stream)
  #|(if (file-stream-p stream)
      (if (og-pixarray-name bitmap)
	  (progn
	    (write-mac-resource (og-pixarray-pixels bitmap) :bitmap
				:stream (og-pixarray-name bitmap))
	    (write (og-pixarray-name bitmap) :stream stream))
	  (write (bitmap->list (og-pixarray-handle bitmap)) :stream stream))
      (format stream " #| ~a (~s) |#" (og-pixarray-pixels bitmap)
              (og-pixarray-name bitmap)))|#
  (format stream " #| ~a (~s) |#" (og-pixarray-pixels bitmap)
	  (og-pixarray-name bitmap))
  (format stream " ~@{~*~@[~1:* ~s ~s~]~}"
	  :attributes (og-pixarray-attributes bitmap)
	  :properties (og-pixarray-properties bitmap))
  (write-char #\) stream))


;;;; ---------------------------------------------------------------------------
;;;; other bitmap operations

(defun mapbitmap (bitmap-handle row-op byte-op &optional bounds)
  ; maps byt-by-byte across the bitmap, in row major order
  (with-pointers ((the-bitmap bitmap-handle))
    (let* ((bitrect (rref the-bitmap bitmap.bounds))
           (bits (rref the-bitmap bitmap.baseaddr))
           (storage-rowbytes (rref the-bitmap bitmap.rowbytes))
           rowbytes
           rowbase
           byte-offset
           row-offset
           rows)
      (rlet ((oprect :rect
                     :left (if bounds (max (rref bounds rect.left)
                                           (rref bitrect rect.left))
                               (rref bitrect rect.left))
                     :top (if bounds (max (rref bounds rect.top)
                                           (rref bitrect rect.top))
                               (rref bitrect rect.top))
                     :right (if bounds (min (rref bounds rect.right)
                                            (rref bitrect rect.right))
                                (rref bitrect rect.right))
                     :bottom (if bounds (min (rref bounds rect.bottom)
                                            (rref bitrect rect.bottom))
                                (rref bitrect rect.bottom))))
        (setq rowbytes (1+ (floor (/ (1- (- (rref oprect rect.right)
                                            (rref oprect rect.left))) 8))))
        (setq rows (1- (- (rref oprect rect.bottom) (rref oprect rect.top))))
        (setq byte-offset (floor (/ (- (rref oprect rect.left)
                                       (rref bitrect rect.left)) 8)))
        (setq row-offset (- (rref oprect rect.top) (rref bitrect rect.top)))
        (do ((y (rref oprect rect.top) (1+ y)))
            ((>= y (rref oprect rect.bottom)))
          (setq rowbase (* (+ y row-offset) storage-rowbytes))
          (do ((x 0 (1+ x)))
              ((>= x rowbytes))
            (apply byte-op
                   (list x y (%inc-ptr bits (+ rowbase byte-offset x)))))
          (apply row-op
                 (list y (%inc-ptr bits rowbase)))))))
  bitmap-handle)

(defun bitmap->list (bitmap-handle &aux row-list bitmap-list)
  (mapBitmap bitmap-handle
             #'(lambda (y row-pointer)
                 (if (null bitmap-list)
                   (setq bitmap-list (list row-list))
                   (setf (cdr (last bitmap-list))
                         (list row-list)))
                 (setq row-list nil))
             #'(lambda (x y byte-pointer &aux the-byte)
                 (setq the-byte (%get-byte byte-pointer))
                 (if (null row-list)
                   (setq row-list (list the-byte))
                   (setf (cdr (last row-list))
                         (list the-byte)))))
  bitmap-list)

(defun list->bitmap (bitmap-handle bitmap-list &aux row-list)
  ; nb. bitmap arg is still first as it is required.
  (setq row-list (pop bitmap-list))
  (mapBitmap bitmap-handle
             #'(lambda (y row-pointer)
                 (setq row-list (pop bitmap-list)))
             #'(lambda (x y byte-pointer &aux the-byte)
                 (setq the-byte (pop row-list))
                 (if (not the-byte) (setq the-byte 0))
                 (%put-byte byte-pointer the-byte)))
  bitmap-handle)


;;;; ----------------------------------------------------------------------------
;;;; mac toolbox packing unpacking

#+:quickdraw
(defun unpack-bitmap (source destination width height
                      &aux (bytes (1+ (floor (/ (1- width) 8))))
                           (max-count 127)
                           chunks last-count)
  ;; unpacks from the source data to the destination data
  ;; NB: these are NOT bitmaps, but the baseaddr field of the bitmap record
  (multiple-value-setq (chunks last-count) (truncate bytes max-count))
  (%stack-block ((source-ptr 4)
                 (destination-ptr 4))
    (%put-ptr source-ptr source)
    (%put-ptr destination-ptr destination)
    (do ((y 0 (1+ y))) ((>= y height))
      (if (> chunks 0)		; for most images 127 bytes suffices.
        (do ((chunk 0 (1+ chunk))) ((>= chunk chunks))
          (_UnpackBits :ptr source-ptr :ptr destination-ptr :word max-count)))
      (_UnpackBits :ptr source-ptr :ptr destination-ptr :word last-count)))
  destination)

#-:quickdraw
(defun unpack-bitmap (source destination width height
                      &aux (bytes (1+ (floor (/ (1- width) 8))))
                           (max-count 127)
                           count-byte row-byte the-byte)
  ;; NB: this is the equivalent of the toolbox call
  (flet ((get-byte (&aux the-byte)
           (setq the-byte (%get-byte source))
           (setq source (%inc-ptr source 1)))
         (put-byte (the-byte)
           (%put-byte destination the-byte)
           (setq destination (%inc-ptr destination 1))))
    (do ((y 0 (1+ y))) ((>= y height))
      (setq row-byte 0)
      (loop (if (> row-byte bytes) (return))
            (setq count-byte (get-byte))
            (if (> count-byte 127)
              (do ((count-byte count-byte (1+ count-byte)))
                  ((>= count-byte 256))
                (put-byte (get-byte))
                (setq row-byte (1+ row-byte)))
              (progn (setq the-byte (get-byte))
                     (do ((count-byte count-byte (1- count-byte)))
                         ((< count-byte 0))
                       (put-byte the-byte)
                       (setq row-byte (1+ row-byte)))))))))


;;;; ---------------------------------------------------------------------------

(defun intensity (r g b)
  (ash (+ (* r 11) (* g 16) (* b 5)) -5)) ; .34 .5 .16

(defun intensity (r g b)
  (+ (ash r -2) (ash r -4)            ; .31
     (ash g -1) (ash g -4) (ash g -6) ; .59
     (ash b -3)))                     ; .11


(defun array-sequence (array &aux (h (first (array-dimensions array))) (w (second (array-dimensions array)))
			          (seq (make-array (* h (/ w 8)) :element-type '(unsigned-byte 8)))
				  (i 0)
				  (pa (make-array (list h (/ w 8)) :element-type '(unsigned-byte 8)
						  :displaced-to array)))
  (dotimes (y h)
    (dotimes (x (/ w 8))
      (setf (aref seq i) (aref pa y x)) (incf i)))
  seq)
  
(defun dither-pixarray (pixels size cmap &aux (width (og:coordinate-x size))
			                      (height (og:coordinate-y size)))
  (let ((intensity-pixels (make-array (list height width) :initial-element 0))
	(bit-pixels (make-array (list height width) :element-type 'bit))
	(p-i 0)
	(error 0))
    (dotimes (i 256)
      (setf (aref cmap i 3) (intensity (aref cmap i 0) (aref cmap i 1) (aref cmap i 2))))
    (dotimes (y height)
      (dotimes (x width)
	(setf p-i (incf (aref intensity-pixels y x) (aref cmap (aref pixels y x) 3)))
	(cond ((< p-i 128)
	       (setf error p-i)
	       (setf (aref bit-pixels y x) 0))
	      (t
	       (setf error (- p-i 255))
	       (setf (aref bit-pixels y x) 1)))
	(if (< x (1- width))
	    (incf (aref intensity-pixels y (1+ x)) (floor (* error 7/16))))
	(when (< y (1- height))
	  (incf (aref intensity-pixels (1+ y) x) (floor (* error 5/16)))
	  (if (> x 0)
	      (incf (aref intensity-pixels (1+ y) (1- x)) (floor (* error 3/16))))
	  (if (< x (1- width))
	      (incf (aref intensity-pixels (1+ y) (1+ x)) (floor (* error 1/16)))))))
    (array-sequence bit-pixels)))
)

:de.setf.graphics

