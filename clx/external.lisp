;;; -*- Mode: LISP; Syntax: Common-lisp; Package: common-lisp-user; Base: 10; Lowercase: Yes -*-

;;;  Define a simple hook to load the correct CLX per runtime for the de.setf.graphics library
;;;
;;; Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;; 'de.setf.graphics' is free software: you can redistribute it and/or modify it under the terms of version 3
;;; of the GNU Affero General Public License as published by the Free Software Foundation.
;;;
;;; 'de.setf.graphics' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;; without even the ;;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the Affero General Public License for more details.
;;;
;;; A copy of the GNU Affero General Public License should be included with 'de.setf.graphics' as `agpl.txt`.
;;; If not, see the GNU [site](http://www.gnu.org/licenses/).

(in-package :common-lisp-user)



#+digitool                              ; load last version konow to work
(let ((*features* (cons :clx-ansi-common-lisp *features*)))
  (require :opentransport)
  (require :io-buffer)

  (provide 'clx)

  (load "LIBRARY:edu;mit;clx;clx-030512;defsystem")
  (if (probe-file (compile-file-pathname "LIBRARY:edu;mit;clx;clx-030512;depdefs.lisp"))
    (load-clx "LIBRARY:edu;mit;clx;clx-030512;")
    (compile-clx "LIBRARY:edu;mit;clx;clx-030512;")))

#+(or allegro clozure)                     ; load latest version from ftp.clozure.com/pub/CLX
(let ((*features* (cons :clx-ansi-common-lisp *features*))
      (*read-default-float-format* 'single-float))
  (provide 'clx)
  (load "LIBRARY:edu;mit;clx;clx-0-7-3-openmcl-060101;clx.asd")
  (asdf:operate 'asdf:load-op :clx))

#+sbcl
(let ((*features* (cons :clx-ansi-common-lisp *features*))
      (*read-default-float-format* 'single-float))
  (provide 'clx)
  (load "LIBRARY:edu;mit;clx;clx-0-7-4;clx.asd")
  (asdf:operate 'asdf:load-op :clx))

;;#+allegro
;;(require :clx)

#(or digitool clozure allegro spcl)
(pushnew :clx *features*)

;;; (LOAD-CLX #P"slapl01:Home:janderson:Source:ravenpack-dev:bin:dev219-m:digi-5-1:LISP:LIBRARY:bin:digi-5-1:edu:mit:CLX:"))

