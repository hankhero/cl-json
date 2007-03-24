;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:json-system
    (:use #:cl #:asdf))

(in-package #:json-system)

(defsystem :cl-json
  :name "cl-json"
  :description "JSON in Lisp. JSON (JavaScript Object Notation) is a lightweight data-interchange format."
  :version "0.3.1"
  :author "Henrik Hjelte <henrik@evahjelte.com>"
  :licence "MIT"
  :components ((:static-file "cl-json.asd")
               (:module :src
                :components ((:file "package")
                             (:file "common" :depends-on ("package"))
                             (:file "decoder" :depends-on ("common"))
                             (:file "encoder" :depends-on ("common"))
                             (:file "utils" :depends-on ("decoder" "encoder"))
                             (:file "json-rpc" :depends-on ("package" "common" "utils" "encoder" "decoder")))))
  :depends-on (:parenscript))

(defsystem :cl-json.test
  :depends-on (:cl-json :fiveam )
  :components ((:module :t
               :components ((:file "package")
                            (:file "testjson" :depends-on ("package" "testdecoder" "testencoder" "testmisc"))
                            (:file "testmisc" :depends-on ("package" "testdecoder" "testencoder"))
                            (:file "testdecoder" :depends-on ("package"))
                            (:file "testencoder" :depends-on ("package"))))))

;;    Copyright (c) 2006 Henrik Hjelte
;;
;;    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;
;;    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;
;;    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 