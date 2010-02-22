;;; -*- Mode: LISP; Syntax: Common-lisp; Package: common-lisp-user; Base: 10; Lowercase: Yes -*-

;;; system description file for tests for the `de.setf.graphics` library

(in-package :common-lisp-user)

;; nb. in order to get the contingencies to work,
;; this system description must load _after_ the graphics library has been built.

(asdf:defsystem :de.setf.graphics.tests
  :depends-on (:de.setf.graphics)
  :description "regression tests for de.setf.graphics."
  :components ((:module :geometry
                        :components ((:file "matrix")))
               (:module :projection
                        :components ((:file "drawing-mode")
                                     (:file "sampler")
                                     (:file "life-in-color")
                                     (:file "abstract-projection"
                                            :depends-on ("drawing-mode" "sampler" "life-in-color"))
                                     (:file "ops-per-second")
                                     #+de.setf.graphics.clx
                                     (:file "clx")
                                     #+de.setf.graphics.common-graphics
                                     (:file "common-graphics" :depends-on (:de.setf.graphics.common-graphics))
                                     #+de.etf.graphics.core-graphics
                                     (:file "core-graphics" :weakly-depends-on (:de.setf.graphics.core-graphics))
                                     #+de.setf.graphics.opengl
                                     (:file "opengl" :weakly-depends-on (:de.setf.graphics.opengl))
                                     #+de.setf.graphcis.quickdraw
                                     (:file "quickdraw" :weakly-depends-on (:de.setf.graphics.quickdraw))
                                     #+de.setf.graphics.svg
                                     (:file "svg" :weakly-depends-on (:de.setf.graphics.svg))
                                     ))))

:de.setf.graphics
