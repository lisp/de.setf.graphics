;;; -*- Mode: LISP; Syntax: Common-lisp; Package: common-lisp-user; Base: 10; Lowercase: Yes -*-

(in-package :common-lisp-user)

(asdf:defsystem :de.setf.graphics.opengl
  :depends-on (:de.setf.graphics)
  :long-description "The de.setf.graphics open gl projection relies on
 the an ffi interface to an open gl library and renders scene models direct from an `abstract-projection`."
  :components ((:file "external")
               (:file "package" :depends-on ("external"))
               (:file "opengl" :depends-on ("package"))))

(pushnew :de.setf.graphics.opengl *features*)

