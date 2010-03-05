;;; -*- Mode: LISP; Syntax: Common-lisp; Package: common-lisp-user; Base: 10; Lowercase: Yes -*-

(document
  "A simple hook to load the correct OpenGL per runtime for the de.setf.graphics library"

  (copyright "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved" "'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

(in-package :common-lisp-user)


(progn
  (load "LIBRARY:com;agentsheets;OpenGL;Load OpenGL for MCL.lisp")
  (when (find-package :xqdm)
    ;; restore the qname reader
    (funcall (intern (string ':install-|{-reader|) :xqdm))))

