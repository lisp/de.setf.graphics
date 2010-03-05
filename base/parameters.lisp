;;; -*- Mode: LISP; Syntax: Common-lisp; Package: de.setf.graphics.implementation; Base: 10; Lowercase: Yes -*-

(in-package :de.setf.graphics.implementation)

(document "This file defines global parameters for the 'de.setf.graphics' library."
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
   (delta 20030829  "porting from strict mcl")
   (delta 20040228  "mop operations removed to library")))


(defvar +2pi+ (* pi 2.0d0))

(defvar dsg::*version* "0.1.3")


(defparameter *context-view-transform* nil
  "special binding for the active context's view transform")
(defparameter *context-projection-transform* nil
  "special binding for the active context's projection transform")
(defparameter *context-transform* nil
  "special binding for the active context's composite transform")



(defvar *path-modes* '(:erase           ; fill with background or transparent
                       :eofill          ; fill with even/odd rule where supported, otherwise fill
                       :eofill-stroke   ; fill then stroke with e/o rule
                       :fill            ; fill with color/pattern
                       :fill-stroke     ; fill then stroke
                       :invert          ; invert
                       :point           ; render each vertex with the *path-vertex-aspects* function (where supported)
                       :stroke          ; stroke  with color/pattern
                       )
  "the abstract path mode designators. each concrete interface maps them to its own designators.")


(defvar *location-package* (find-package :de.setf.graphics)
  "Determines the package to intern location types in the location reader-macro.")

(defvar *rgba-registry* (make-hash-table :test 'equalp))

;;; ndc transform context parameters

(defparameter *arc-ndc* nil)
(defparameter *line-ndc* nil)
(defparameter *raster-ndc* nil)
(defparameter *rectangle-ndc* nil)
(defparameter *text-ndc* nil)

(defparameter *ndc-context* nil
  "bound within a call-with-projection-context for a ndc-transform-context for use the ndc-based operators.")

(defparameter *ndc-coordinate-type* 'short-float
  "if all coordinates satisfy this type, they are not transformed.")

;;; port context parameters

(defparameter *arc-port* nil)
(defparameter *line-port* nil)
(defparameter *raster-port* nil)
(defparameter *rectangle-port* nil)
(defparameter *text-port* nil)
(defparameter *port-coordinate-type* 'fixnum
  "if all coordinates satisfy this type, they are not transformed.
   if nound to NIL, all coordinates are transformed.")



:de.setf.graphics

