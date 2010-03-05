;;; -*- Mode: LISP; Syntax: Common-lisp; Package: common-lisp-user; Base: 10; Lowercase: Yes -*-

(in-package :common-lisp-user)

;;;  This file is extends the package definition for the 'de.setf.graphics' library for OpenGL.
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


(modpackage :de.setf.graphics.implementation
  ;; 20030823 the current opengl code does not export everything which it
  (:import-from :ccl
                :agl-error-check
                :aglcontext
                :aglCreateContext
                :aglsetcurrentcontext
                :aglChoosePixelFormat
                :aglDestroyPixelFormat
                :aglgetcurrentcontext
                :aglcontext
                :aglSetDrawable
                :aglSetInteger
                :AGLEnable
                :agl_rgba
                :agl_buffer_RECT
                :agl_doublebuffer
                :agl_depth_size
                :agl_fullscreen
                :agl_none
                :GLGenLists
                :aglUseFont
                :make-vector
                :aim-camera
                ;; :camera-class
                :current-font
                ;; :display
                :font-number-from-name
                :init
                :make-opengl-font
                :opengl-simple-view
                :opengl-window
                ;; :orthographic-camera
                ))

(let ((ccl (find-package :ccl))
      (gl (find-package :gl)))
  (with-package-iterator (next-symbol ccl :internal :external)
    (loop (multiple-value-bind (nextp sym) (next-symbol)
            (unless nextp (return))
            (let ((name (symbol-name sym)))
              (when (and (>= (length name) 3)
                         (string-equal name "agl" :end1 3))
                (import sym gl))))))
  (with-package-iterator (next-symbol gl :internal :external :inherited)
    (loop (multiple-value-bind (nextp sym access) (next-symbol)
            (unless nextp (return))
            (let ((name (symbol-name sym)))
              (when (or (and (>= (length name) 3)
                             (string-equal name "agl" :end1 3))
                        (and (>= (length name) 2)
                             (string-equal name "gl" :end1 2)))
                (case access
                  (:internal (export sym gl))
                  (:external )
                  (:inherited
                   (warn "can't export: ~s." sym)))))))))


:eof
