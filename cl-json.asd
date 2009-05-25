;;; -*- lisp -*-
;;; Copyright (c) 2006-2008 Henrik Hjelte
;;; All rights reserved.
;;; See the file LICENSE for terms of use and distribution.

(in-package #:cl-user)

(defpackage #:json-system
    (:use #:cl #:asdf))

(in-package #:json-system)

(pushnew :cl-json *features*)
#+(or mcl openmcl cmu sbcl clisp ecl scl lispworks allegro)
(pushnew :cl-json-clos *features*)

(defsystem :cl-json
  :name "cl-json"
  :description "JSON in Lisp. JSON (JavaScript Object Notation) is a lightweight data-interchange format."
  :version "0.4.0"
  :maintainer "Henrik Hjelte <henrik@evahjelte.com>"
  :licence "MIT"
  :components ((:static-file "cl-json.asd")
               (:module :src
                :components ((:file "package")
                             (:file "common" :depends-on ("package"))
                             #+cl-json-clos
                             (:file "objects" :depends-on ("package"))
                             (:file "camel-case" :depends-on ("package"))
                             (:file "decoder" :depends-on ("common" #+cl-json-clos "objects" "camel-case"))
                             (:file "encoder" :depends-on ("common" #+cl-json-clos "objects" "camel-case"))
                             (:file "utils" :depends-on ("decoder" "encoder"))
                             (:file "json-rpc" :depends-on ("package" "common" "utils" "encoder" "decoder"))))))

(defsystem :cl-json.test
  :depends-on (:cl-json :fiveam )
  :components ((:module :t
               :components ((:file "package")
                            (:file "testjson" :depends-on ("package" "testdecoder" "testencoder" "testmisc"))
                            (:file "testmisc" :depends-on ("package" "testdecoder" "testencoder"))
                            (:file "testdecoder" :depends-on ("package"))
                            (:file "testencoder" :depends-on ("package"))))))
