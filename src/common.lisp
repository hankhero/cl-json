;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json)

(defparameter +json-lisp-escaped-chars+
  `((#\" . #\")
    (#\\ . #\\)
    (#\/ . #\/)
    (#\b . #\Backspace)
    (#\f . ,(code-char 12))
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)))

(defparameter *use-strict-json-rules* t)

(defparameter *symbol-to-string-fn* #'symbol-to-js)

(defun json-escaped-char-to-lisp(json-escaped-char)
  (let ((ch (cdr (assoc json-escaped-char +json-lisp-escaped-chars+))))
    (if *use-strict-json-rules*
        (or ch (error 'json-parse-error))
        (or ch json-escaped-char))))

(defun lisp-special-char-to-json(lisp-char)
    (car (rassoc lisp-char +json-lisp-escaped-chars+)))
