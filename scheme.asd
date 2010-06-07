;;;; -*- mode: lisp -*-
;;;;
;;;; This as an ASDF system for scheme
;;; -*- Lisp -*- mode

(defpackage #:scheme-system (:use #:cl #:asdf))
(in-package #:scheme-system)

(defclass acl-file (cl-source-file) ())
(defmethod source-file-type ((c acl-file) (s module)) "lisp")

(defsystem scheme
  :author "scheme"
  :licence "BSD"
  :default-component-class acl-file
  :components ((:file "scheme")
               )
  :version "0.0.1"
  :depends-on (fare-matcher cl-ppcre)
  :perform (load-op :after (op scheme)
                    (pushnew :scheme cl:*features*)))

