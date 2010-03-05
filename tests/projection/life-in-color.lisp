;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; 

;;; ****************************************************************
;;; Conway's Game of Life ******************************************
;;; ****************************************************************

(in-package :de.setf.graphics.implementation)

;;; Don't know where/when got this. --mk
;;; 20031003.jaa fixed print function

(defstruct (world (:print-function
                   (lambda (world stream depth)
                     (declare (ignore depth))
                     (print-unreadable-object (world stream :identity nil :type t)
                       (princ (world-numdots world) stream)))))
  size
  current
  numdots
  next
  numchanged
  (xmin 1000000)    ; Initialize the region to ridiculous numbers.
  (xmax -1)
  (ymin 1000000)
  (ymax -1))

(defparameter *life-raster* nil)
(defparameter *life-cycles* 1000)
(defparameter *life-sleep* nil)  ; (defparameter *life-sleep* .01)


(defclass life-raster (raster)
  ((world )
   (sample-depth :initform 32)))

(defmethod initialize-instance :after ((instance life-raster) &key)
  (with-slots (world) instance
    (setf world (make-life-world (sample-data instance)))))

(defmethod sample-filter ((raster life-raster) data (i t) (j t))
  (aref #(0 #xffffff #x4040a0 #xa0a0ff) data))

(defmethod sample-filter ((raster life-raster) (filter function) (i t) (j t))
  (funcall filter (aref #((0 0 0) (1 1 1) (.25 .25 .25) (.75 .75 1)) (aref (sample-data raster) i j))))


(defmethod next-generation ((raster life-raster))
  (with-slots (world) raster
    (incf (sample-generation raster))
    (values (next-cycle world) (world-numdots world))))

(defmethod initialize-sample ((raster life-raster))
  (with-slots (world) raster
    (let ((size (sample-size raster))
          (data (world-current world)))
      (dotimes (i (point-v size))
        (dotimes (j (point-h size))
          (setf (aref data i j) (random 3)))))
    (initialize-world world)))

(defun initialize-life-raster (&optional (size 64))
  (let* ((world-source (make-array (list size size) :element-type '(unsigned-byte 8))))
    (dotimes (i size) (dotimes (j size) (setf (aref world-source i j) (random 3))))
    (setq *life-raster* (make-instance 'life-raster :sample-data world-source))))


(defun setnext (world i j)
  (let* ((current (world-current world))
         (next (world-next world))
         (neighbors (count-neighbors current i j)))
    (declare (type (simple-array (unsigned-byte 8)) current next)
             (type (unsigned-byte 8) neighbors))
    ;; set the next population pattern
    (if (zerop (aref current i j))
	(cond ((not (= neighbors 3))
	       ;; current = 0, surrounding cells != 3
	       (setf (aref next i j) 0))	
	      (t (setf (aref next i j) neighbors)
		 ;; current = 0, surrounding cells = 3
                 (incf (world-numchanged world))
		 (incf (world-numdots world))))
      (cond ((or (= neighbors 2)
		 (= neighbors 3))
	     ;; current > 0, surrounding cells = 2,3
	     (setf (aref next i j) neighbors))	
	    (t (setf (aref next i j) 0)
               (incf (world-numchanged world))
               (decf (world-numdots world)))))
    ;; reset the bounds, if necessary
    (unless (zerop (aref next i j))
      (when (< i (world-xmin world)) (setf (world-xmin world) i))
      (when (> i (world-xmax world)) (setf (world-xmax world) i))
      (when (< j (world-ymin world)) (setf (world-ymin world) j))
      (when (> j (world-ymax world)) (setf (world-ymax world) j)))))

(defun count-neighbors (array i j)
  (flet ((neighbor (i j) (if (zerop (aref array i j)) 0 1)))
    (+ (neighbor (1- i) (1- j))
       (neighbor i      (1- j))
       (neighbor (1+ i) (1- j))
       (neighbor (1- i) j)
       (neighbor (1+ i) j)
       (neighbor (1- i) (1+ j))
       (neighbor i      (1+ j))
       (neighbor (1+ i) (1+ j)))))

(defun next-cycle (world)
  "compute the next world cycle and return t when some cell has changed."
  (let* ((lim (world-size world))
         (current (world-current world))
         (next (world-next world))
         (xlb (max 1 (1- (world-xmin world))))
         (xub (min (- lim 2) (1+ (world-xmax world))))
         (ylb (max 1 (1- (world-ymin world))))
         (yub (min (- lim 2) (1+ (world-ymax world)))))
    (declare (type (simple-array (unsigned-byte 8)) current next))
    (setf (world-numchanged world) 0)
    (dotimes (i (1+ (- xub xlb)))
      (dotimes (j (1+ (- yub ylb)))
        (setnext world (+ i xlb) (+ j ylb))))
    (dotimes (y lim)
      (dotimes (x lim)
        (setf (aref current x y) (aref next x y)))))
  (not (zerop (world-numchanged world))))

(defun print-world (world generations)
  (let ((lim (world-size world))
        (current (world-current world)))
    (dotimes (y lim)
      (dotimes (x lim)
        (if (zerop (aref current y x))
          (princ " ")
          (princ (aref current y x))))
      (terpri))
    (format t "~&~d Generations, ~d Organisms."
            generations (world-numdots world))))

(defun propagate (world cycles)
  (print-world world cycles)
  (do ()
      ((or (zerop (world-numdots world)) (zerop (world-numchanged world)))
       (format t "~2&POPULATION 0 ... ~d generations" cycles))
    (next-cycle world)
    (incf cycles)
    (print-world world cycles)))


(defun make-life-world (source)
  (let* ((life (make-world
                :size (first (array-dimensions source))
                :current source
                :next (make-array (array-dimensions source) :element-type '(unsigned-byte 8)
                                  :initial-element 0)
                :numdots 0
                :numchanged 0)))
    (initialize-world life)
    life))

(defun initialize-world (life)
  (dotimes (i (first (array-dimensions (world-current life))))
    (dotimes (j (second (array-dimensions (world-current life))))
      (unless (zerop (aref (world-current life) i j))
        (incf (world-numdots life))
        (incf (world-numchanged life))
        (when (< i (world-xmin life)) (setf (world-xmin life) i))
        (when (> i (world-xmax life)) (setf (world-xmax life) i))
        (when (< j (world-ymin life)) (setf (world-ymin life) j))
        (when (> j (world-ymax life)) (setf (world-ymax life) j))))))


(defun life (source)
  (propagate (make-life-world source) 0))


;;;
;;; tests specific to object graphics

(defun run-life (&key (cycles *life-cycles*) (sleep *life-sleep*) (size 64)
                      (initialize-p t))
  (stroke-agent 1.0 1.0 1.0)
  (fill-agent 1.0 1.0 1.0)
  (clear-agent 0.0 0.0 0.0)
  (when (or (null *life-raster*) initialize-p)
    (initialize-life-raster size))
  (loop (raster*2 -0.9 -0.9 0.9 0.9 *life-raster*)
        (color*3 0 0 0)
        (rectangle*2 10 10 64 0 '((path-effect :clear) (path-constituents :surfaces)))
        (text (make-point 10 10) "          "
              :courier-plain-10)
        (text (make-point 10 10) (format nil "~6,'0d" cycles)
              :courier-plain-10)
        (flush-view)
        (when sleep (sleep sleep))
        (unless (and (next-generation *life-raster*)
                     (plusp (decf cycles)))
          (return)))
  t)
;;; (run-life)


#|
;;; Example:
(setq test
      (make-array '(8 8) :element-type '(unsigned-byte 8)
                  :initial-contents '((0 0 0 0 0 0 0 0)
                                      (0 0 0 1 1 0 1 0)
                                      (0 0 1 0 1 0 1 0)
                                      (0 0 1 1 1 0 0 0)
                                      (0 1 0 0 1 1 1 0)
                                      (0 1 1 1 0 0 0 0)
                                      (0 0 0 1 1 0 1 0)
                                      (0 0 0 0 0 0 0 0))))

(life test)

(let ((large-test (make-array '(256 256))))
  (dotimes (i 256) (dotimes (j 256) (setf (aref large-test i j) (random 3))))
  (defparameter *world* (make-life-world large-test)))

(time (next-cycle *world*))

|#

;;; *EOF*
