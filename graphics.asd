;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

;;; This file is the system definition for the 'de.setf.graphics' Common Lisp library.
;;; 'de.setf.graphics' is a Common Lisp scene model and rendering package.
;;;
;;; Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;; 'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
;;; of the GNU Affero General Public License as published by the Free Software Foundation.
;;;
;;; 'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;; without even the ;;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the Affero General Public License for more details.
;;;
;;; A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
;;; If not, see the GNU [site](http://www.gnu.org/licenses/).
;;;
;;; This file should reside in the root directory of the `de.setf.graphics` source files.
;;; That, in turn, should be a sibling directory to the `de.setf.utility` library, from which
;;; `pathnames.lisp` adds support for a system-specific logical host.
;;;
;;; In order to load the core system, obtain its required libraries (see below), and load it as
;;;
;;;    (asdf:operate 'asdf:load-op :de.setf.graphics)
;;;
;;; In order to use the library, one must load at least one concrete protocol device interface, for eaxmple,
;;;
;;;    (asdf:operate 'asdf:load-op :de.setf.graphics.svg)
;;;
;;; Each supported ouput device is present as an individual system and sub-directory.
;;;

(unless (find-package :de.setf.utility)
  (load (merge-pathnames (make-pathname :directory '(:relative :up "utility")
                                        :name "pathnames")
                         *load-pathname*)))


(de.setf.utility:set-relative-logical-pathname-translations "DSG")




(asdf:defsystem :de.setf.graphics
  :nicknames (:setf.graphics)
  :depends-on (:de.setf.utility
               :net.common-lisp.closer-mop
               :de.setf.utility.clos
               :de.setf.utility.graph
               :de.setf.utility.mime
               )
  :license :gal
  :components
  ((:module "libraries"
    :description "incorporates additional library modules"
    :components ((:module "utility"
                  :pathname #p"LIBRARY:de;setf;utility;"
                  :components ((:file "lock")
                               (:file "meta")))
                 (:file "skippy"
                  :pathname "LIBRARY:com;xach;skippy.lisp")))

   (:module "base"
    :depends-on ("libraries")
    :components ((:file "package")
                 (:file "parameters" :depends-on ("package"))
                 (:file "utilities" :depends-on ("parameters"))))

   (:module "geometry"
    :description "class and operation definitions for geometry location primitives"
    :depends-on ("base")
    :components ((:file "locations")
                 (:file "location-math" :depends-on ("locations"))
                 (:file "matrix" :depends-on ("location-math"))
                 (:file "transform-context" :depends-on ("matrix" "locations"))
                 (:file "location-transform" :depends-on ("transform-context"))))

  (:module "model"
   :description "class definitions for model components."
   :depends-on ("geometry")
   :components ((:file "abstract")
                (:file "geometric-constituents" :depends-on ("abstract"))
                (:file "raster" :depends-on ("abstract"))
                ;; (:file "triangle-strip")
                ;; (:file "text")
                ;; (:file "image"
                ;; (:file "view")
                ;; (:file "bitmap")
                ;; (:file "pixmap")
                ;; (:file "icon")
                ;; (:file "sphere")
                ;; (:file "compound")
                ;; (:file "wrapper")
                ;; (:file "stroke") 
                ))

  (:module "projection"
   :description "operation definitions for rendering.
 This includes both the abstract definitions and the concrete implementation for
 the respective graphics libraries. portable libraries are included by default -
 svg, clx, gd. others are included iff the lisp implementation includes them."
   :depends-on ("geometry" "model")
   :components ((:file "abstract-projection")
                (:file "port-transform-context" :depends-on ("abstract-projection"))
                (:file "ndc-transform-context" :depends-on ("abstract-projection"))
                #+(or )
                (:contingent-cl-source-file "capi"
                 :depends-on ("ndc-transform-context")
                 :contingent-on ((:feature  :lispworks)))))

  #+(or )  ;; resurrect once there is a concrete application
  (:documented-module "view"
    :description "operation definitions for window-system views"
    :depends-on ("model" "projection")
    :components (; "og:code;view;backed-view"
                 ; "og:code;view;graphic-view"
                 ; "og:code;view;view-transform-ops"
                 ; "og:code;view;graphic-item" 
                 ; "og:code;view;view-transform" 
                 ; "og:code;view;context-dialog" ; +:ccl
                 ; "og:code;view;presentation"
                 ; "og:code;view;graphic-cache"
                 ; "og:code;view;dialog" ; -:mcl 
                 ; "og:code;view;series-view"
                 ; "og:code;view;graphic-window"
                 ))

  #+(or )  ;; resurrect once there is a concrete application
  (:documented-module "interaction"
   :description "interactive editing."
   :components (;; "og:interaction;selection"
                ;; "og:interaction;context-figures" ; #+:ccl 
                ;; "og:interaction;edit-figures" ; #+:ccl 
                ;; "og:interaction;interactive" ;; needs to be rewritten...
                ;; "og:interaction;edit"
                ;; "og:interaction;graphic-interactor"
                ))

  #+(or ) ;; for now, display only
  (:documented-module "io"
   :description "persistence, serialization, and storage."
   :components (;; #+:mcl "og:resources"
                ;; "og:code;io;environment"
                ;; "og:code;io;graphic-persistor"
                ;; "og:code;io;crl"
                ))
  ))

(pushnew :de.setf.graphics *features*)

