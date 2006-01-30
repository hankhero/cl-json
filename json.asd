(in-package #:cl-user)

(defpackage #:json-system
    (:use #:cl #:asdf))

(in-package #:json-system)

(defsystem :json
  :name "cl-json"
  :description "JSON in Lisp. JSON (JavaScript Object Notation) is a lightweight data-interchange format."
  :version "0.1"
  :author "Henrik Hjelte <henrik@evahjelte.com>"
  :licence "LLGPL,Lisp Lesser GPL"
  :components ((:static-file "json.asd")
               (:module :src
                :components ((:file "package")
                             (:file "decoder" :depends-on ("package"))))))

(defsystem :json.test
  :depends-on (:json :fiveam)
  :components ((:module :t
               :components ((:file "package")
                            (:file "testdecoder" :depends-on ("package"))))))

