;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)


(document :file
 (description "This file defines a simple ops-per-second test for the 'de.setf.graphics' library.")

 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `DSG:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


(defun ops-per-second (op count &key (repeat 1) (verbose t))
  (declare (optimize (speed 3) (safety 0))
           (ftype (function () t) op)
           (type fixnum count iterations))
  (let ((elapsed-times nil)
        (start 0)
        (end 0)
        (average 0))
    (declare (type list elapsed-times)
             (type integer start end))
    (dotimes (iteration repeat)
      (setf start (get-internal-run-time))
      (dotimes (i count) (funcall op))
      (setf end (get-internal-run-time))
      (push (- end start) elapsed-times)
      (when verbose (format *trace-output* "~%@~d x~d = ~d"
                            iteration count (first elapsed-times))))
    (list :average-elapsed (setf average (round (/ (apply #'+ elapsed-times) repeat)))
          :operations-per-second (round (* 1000 (/ count average))))))

