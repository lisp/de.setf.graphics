;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)

(document :file
 (description "This file defines drawing-mode tests for the 'de.setf.graphics' library.")
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
  (delta 20030928 "quickdraw")
  (delta 20040301 "updated the generic attribute global variables"))

 (long-description
  "draw a rectanlge with explicitly bound combinations of combined drawing mode and
  individual mode aspects."))


(defParameter *test-drawing-modes* nil)

;; generate mode for individual aspects, combined aspect, and combined aspect which override
(dolist (path-constituents *path-constituents*)
  (dolist (path-effect *path-effects*)
    (dolist (path-rule *path-rules*)
      (dolist (drawing-mode '(()
                              (:clear :surfaces :paint :lines)
                              (:erase :surfaces :invert :lines)))
        (push `(:drawing-mode ,drawing-mode :path-rule ,path-rule :path-effect ,path-effect
                              :path-constituents ,path-constituents)
              *test-drawing-modes*)))))
;(length *test-drawing-modes*)

(defun test-drawing-modes (&key (x-offset 20) (y-offset 20))
  ;(clear-agent 0.0 0.0 0.0) ; 0.9 0.9 0.9)
  ;(clear-view)
  (clear-agent 0.2 0.2 0.2) ; 0.1 0.1 0.1)
  (stroke-agent 1.0 0.0 0.0)
  (fill-agent 0.0 1.0 0.0)
  (let ((test-modes *test-drawing-modes*)
        (rows (ceiling (length *test-drawing-modes*) 2)))
    (dotimes (i rows)
      (dotimes (j 2) 
        (unless test-modes (return))
        (let ((modes (pop test-modes)))
          (destructuring-bind (&key ((:drawing-mode *context-drawing-mode*) nil)
                                    (path-constituents *context-path-constituents*)
                                    ((:path-effect *context-path-effect*) nil)
                                    ((:path-rule *context-path-rule*) nil)
                                    &aux
                                     *context-path-constituents*)
                              modes
            (path-constituents path-constituents)
            (let* ((x1 (+ x-offset (* j 432)))
                   (y1 (+ y-offset (* i 15)))
                   (x2 (+ x1 10))
                   (y2 (+ y1 10))
                   (polypoints (mapcar #'(lambda (p) (add-points p #@(20 0)))
                                       (list (make-point x1 y1) (make-point x2 y2) 
                                             (make-point x2 y1) (make-point x1 y2)
                                             (make-point x1 y1)))))
              (rectangle*2 x1 y1 x2 y2)
              (poly polypoints nil)
              (arc*2 (+ x1 45) (+ y1 5) 5 0.0 (* pi .5) (if (evenp i) :clockwise :counterclockwise))
              (line*2 (- x1 5) (+ y1 5) (+ x1 55) (+ y1 5))
              (text*2 (+ x1 60) (+ y1 8)
                      (format nil "~a + ~a/~a/~a"
                              *context-drawing-mode*
                              *context-path-effect* *context-path-constituents* *context-path-rule*)
                      t))))))
    t))


#||#
