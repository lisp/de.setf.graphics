;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-


(in-package :de.setf.graphics.implementation)


(de.setf.utility:document :file
  (description "This file defines utilities for the 'de.setf.graphics' library.")
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
   (copyright 2003 )
   (delta 20030829 "porting from strict mcl")
   (delta 20040228 "mop operations removed to library")))


(unless (eq *read-default-float-format* 'double-float)
  (warn "Setting default float format to double.")
  (setq *read-default-float-format* 'double-float))

;;; macros

(document (%round %rounds sin! cos! sqrt! double-float!)
  "If the runtime provides destructive, inline-able, and/or domain-specific implemenentations for these
 operators inexcess of what declarations can accomplish, define the expansion here.")

(defMacro %round (datum)
  #+ccl `(ccl::%round-nearest-double-float->fixnum , datum)
  #-ccl `(round ,datum))

(defMacro %rounds (datum)
  #+ccl `(ccl::%round-nearest-short-float->fixnum , datum)
  #-ccl `(round ,datum))

(defmacro sin! (angle result)
  #+ccl `(ccl::%double-float-sin! ,angle ,result)
  #-ccl `(setf ,result (sin ,angle)))

(defmacro cos! (angle result)
  #+ccl `(ccl::%double-float-cos! ,angle ,result)
  #-ccl `(setf ,result (cos ,angle)))

(defMacro sqrt! (datum result)
  #+ccl `(ccl::%double-float-sqrt! ,datum ,result)
  #-ccl `(setf ,result (sqrt ,datum)))

(defMacro double-float! (datum result)
  #+ccl `(ccl::%int-to-dfloat ,datum ,result)
  #-ccl `(setf ,result (coerce ,datum 'double-float)))


;;; utilities

(defun path-mode-p (x) (member x *path-modes*))

(unless (fboundp 'fixnump)
  (defun fixnump (x) (typep x 'fixnum)))

(defmacro assert-type (m type)
  "assert the type of the given variable. often used in combination with `assert-types`"
  (when (and (consp type) (eq (first type) 'quote)) (setf type (second type)))
  (labels ((try-predicate (suffix)
             (let ((symbol (find-symbol (concatenate 'string (string type) (string suffix)))))
               (when (and symbol (fboundp symbol))
                 symbol)))
           (make-predicate ()
             (or (and (symbolp type)
                      (or (let ((*package* (symbol-package type))) (or (try-predicate :-p) (try-predicate :p)))
                          (try-predicate :-p) (try-predicate :p)))
                 `(lambda (x) (typep x ',type)))))
    `(unless (,(make-predicate) ,m)
       (error "datum not of type ~s: ~s." ',type ,m))))

(defmacro assert-types (variables type)
  "assert the type of the given variables. often used in combination with #+assert-types"
  `(progn ,@(mapcar #'(lambda (v) `(assert-type ,v ,type)) variables)))

(defmacro defgraphicelement ((name (&key (include 'node))) &rest slots)
  "the defGraphicElement translates the older structure-oriented descriptions to CLOS defClass forms"
  `(defClass ,name (,include)
     ,(mapcar #'(lambda (slot-declaration)
                  (destructuring-bind (name initform &key type) slot-declaration
                    `(,name :initform ,initform :initarg ,(intern (string name) :keyword)
                            ,@(when type `(:type ,type)))))
              slots)))

(defun nyi-error (operator)
  (error "NYI: ~s: ~a / ~a."
         operator (lisp-implementation-type) (lisp-implementation-version)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *projection-monitor-function*
    (ignore-errors (and (find-package :rt)
                        (symbol-function
                         (find-symbol
                          (string :call-with-time-and-memory-counted) :rt))))
    "specifies a function designator to be used by projection operators to monitor
     their time and space usage. should be a function of one argument - #'call-next-method
     which returns three values - a value list, time usage and memory usage."))

:de.setf.graphics

