;;; -*- Mode: LISP; Syntax: Common-lisp; Package: common-lisp-user; Base: 10; Lowercase: Yes -*-

(in-package :common-lisp-user)

(asdf:defsystem :de.setf.graphics.clx
  :depends-on (:de.setf.graphics)
  :long-description "The de.setf.graphics clx projection relies on
 the common lisp x library and uses the `port-transform-context` to render scene models
 in terms of that interface."
  :serial t
  :components ((:file "external")
               (:file "clx")))

(pushnew :de.setf.graphics.clx *features*)

