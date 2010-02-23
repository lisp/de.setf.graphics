;;; -*- Mode: LISP; Syntax: Common-lisp; Package: common-lisp-user; Base: 10; Lowercase: Yes -*-

(in-package :common-lisp-user)

;;;  This file is the core package definition for the 'de.setf.graphics' Common Lisp library.
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

;;; Copyright 2003 [james anderson](mailto:james.anderson@setf.de)
;;; 20030829  rearranged, renamed, consolidated packages
;;; Copyright 2005 [james anderson](mailto:janderson@ravenpack.com)
;;; 20051007  end replaced end-location


(modPackage :de.setf.graphics
  (:use )
  (:nicknames :og :dsg :de.setf.object-graphics)
  #+digitool
  (:import-from :ccl
                :point-h
                :point-v
                :make-point
                :ignore-if-unused
                )
  (:import-from :setf.graph
                :children
                :parent
                :annotations
                )
  (:intern
   :version
   :*version*)
  (:export
   :*class.*polyline*
   :*class.*polyline-strip*
   :*class.line*
   :*class.projection-context*
   :*class.quadrangle*
   :*class.rectangle*
   :*class.triangle*
   :*clx-default-display*
   :*clx-display-host*
   :*clx-display-number*
   :*default.view-port-drawable-center*
   :*default.view-port-scale*
   :*default.view-projection-angle*
   :*default.view-projection-maximum*
   :*default.view-projection-minimum*
   :*default.view-projection-near-plane*
   :*default.view-projection-projection*
   :*default.view-station-center*
   :*default.view-station-normal*
   :*default.view-station-reference*
   :*default.view-station-station*
   :*default.view-station-up*
   :*interface*
   :+most-negative-location-world+
   :+most-positive-location-world+
   :|2|
   :|3|
   :|4|
   :apparent
   :apparent-node
   :cartesian->spherical
   :child
   :child-node
   :clear-agent
   :clear-view
   :clx-context
   :color*3
   :color*4
   :color-mode
   :component
   :component-node
   :constituent
   :constituent-node
   :context-click-event-handler
   :context-controller
   :context-display
   :context-expose-event-handler
   :context-get
   :context-key-event-handler
   :context-key-up-event-handler
   :context-make-view
   :context-mouse-up-event-handler
   :context-name
   :context-position
   :context-project
   :context-projection-task
   :context-size
   :context-view
   :context-view-p
   :context-close-view
   :copy-location
   :copy-location-2
   :copy-location-3
   :copy-location-4
   :copy-location-ndc
   :copy-location-object
   :copy-location-port
   :copy-location-world
   :copy-polar
   :copy-spherical
   :double-matrix
   :double-matrix-p
   :draw
   :dyad
   :d-value
   :edit
   :element
   :end-location
   :enqueue-projection-task
   :fill-agent
   :fill-view
   :find-projection-controller
   :find-projection-context
   :flush-view
   :aspect-p
   :indicate
   :line
   :line*2
   :line*3
   :line-p
   :located
   :located-node
   :location
   :location-*
   :location-+
   :location--
   :location-/
   :location-/=
   :location-2
   :location-2-p
   :location-3
   :location-3-p
   :location-4
   :location-4-p
   :location-<
   :location-<=
   :location-=
   :location->
   :location->=
   :location-abs
   :location-alpha
   :location-beta
   :location-cross
   :location-difference
   :location-distance
   :location-dot
   :location-h
   :location-inverse-scale
   :location-inverse-transform
   :location-magnitude
   :location-maximum
   :location-minimum
   :location-namestring
   :location-ndc
   :location-ndc-p
   :location-normalize
   :location-numberp
   :location-object
   :location-object-p
   :location-p
   :location-polar
   :location-polar-p
   :location-port
   :location-port-p
   :location-product
   :location-radius
   :location-ratio
   :location-scale
   :location-spherical
   :location-spherical-p
   :location-sum
   :location-time
   :location-transform
   :location-vector
   :location-vector-p
   :location-world
   :location-world-p
   :location-x
   :location-y
   :location-z
   :location-zerop
   :m-value
   :make-line
   :make-gif
   :make-location
   :make-location-2
   :make-location-3
   :make-location-4
   :make-location-ndc
   :make-location-object
   :make-location-port
   :make-location-world
   :make-material-properties
   :make-point
   :make-polar
   :make-polygon
   :make-rectangle
   :make-spherical
   :material-alpha
   :material-blue
   :material-green
   :material-kDiff
   :material-kPhong
   :material-kRefr
   :material-kSpec
   :material-kTrans
   :material-properties
   :material-properties-p
   :material-red
   :material-type
   :matrix
   :matrix-catenate
   :matrix-copy
   :matrix-identity
   :matrix-initialize
   :matrix-invert
   :matrix-rotate
   :matrix-scale
   :matrix-set
   :matrix-translate
   :medial
   :model
   :monad
   :ndc
   :node-children
   :node-connections
   :node-parent
   :object
   :opengl-context
   :p-values
   :path-mode
   :path-effect
   :point-h
   :point-v
   :polar
   :poly
   :polyad
   :polygon
   :polygon-p
   :port
   :project
   :projection-context
   :projection-context-p
   :projection-controller
   :projection-task
   :projection-view
   :properties
   :quickdraw-context
   :raster
   :rectangle
   :rectangle*2
   :rectangle*3
   :rectangle-location
   :rectangle-p
   :reference
   :reference-node
   :referent
   :region
   :rgb
   :rgba
   :rgba-r
   :rgba-g
   :rgba-b
   :rgba-a
   :spherical
   :spherical->cartesian
   :start-location
   :stroke-agent
   :t-value
   :text
   :text*2
   :text*3
   :task-function
   :task-timestamp
   :transform
   :transform-identity
   :transform-matrix
   :transform-matrix-p
   :terminate-relation
   :triad
   :view-angle
   :view-center
   :view-distance
   :view-drawable-center
   :view-drawable-halfsize
   :view-drawable-resolution
   :view-far-plane
   :view-near-plane
   :view-normal
   :view-projection
   :view-projection-context
   :view-reference
   :view-station
   :view-type
   :view-up
   :with-location-vectors
   :with-projection-context
   :with-short-location-coordinates
   :with-state
   :world
   ))

(modPackage :.og.
  (:nicknames :de.setf.object-graphics.indirection>)
  (:documentation
   "the :de.setf.object-graphics.indirection owns the symbols to which the implementations for the abstract graphics function are dynamically bound within a given context."))

(modPackage :de.setf.graphics.implementation
  (:nicknames :dsg.i :og.impl :de.setf.object-graphics.implementation)
  (:documentation
   "the package :de.setf.object-graphics.implementation hosts the implementation. it imports the :de.setf.object-graphics package.")
  (:shadowing-import-from :de.setf.object-graphics
                          :line)
  (:use :common-lisp
        :de.setf.utility
        :de.setf.graphics
        )
  #+digitool
  (:import-from :ccl
                :add-points
                :dispose-record
                :fsread
                :geteof
                :href
                :make-point
                :point-h
                :point-v
                :print-record
                :rset
                :subtract-points
                :view
                :view-size
                :view-position
                :view-window
                :window
                :with-focused-view
                :with-fsopen-file
                :wptr
                :with-cfstrs)
  #+digitool
  (:import-from :ccl
                :%get-ptr
                :%get-signed-long
                :%get-word
                :%new-ptr
                :%null-ptr
                :%null-ptr-p
                :%put-byte
                :%put-long
                :%put-ptr
                :%put-word
                :fixnump
                :generic-function-methods
                :ignore-if-unused
                :macptrp
                :make-record
                :method-specializers
                :record-length
                :rlet
                :rref
                :stream-position
                :terminate
                :terminate-when-unreachable
                :with-dereferenced-handles
                :with-macptrs
                :without-interrupts
                :*warn-if-redefine*
                )
  #+allegro
  (:import-from :mop
                :generic-function-methods
                )
 #+allegro
  (:import-from :excl
                :ignore-if-unused
                )
  )

:eof
