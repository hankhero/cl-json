(in-package :json)

(defparameter *json-lisp-escaped-chars*
  `((#\" . #\")
    (#\\ . #\\)
    (#\/ . #\/)
    (#\b . #\Backspace)
    (#\f . ,(code-char 12))
    (#\n . #\Newline)
    (#\r . #\Return)
    (#\t . #\Tab)))

(defun json-escaped-char-to-lisp(json-escaped-char)
    (cdr (assoc json-escaped-char *json-lisp-escaped-chars*)))

(defun lisp-special-char-to-json(lisp-char)
    (car (rassoc lisp-char *json-lisp-escaped-chars*)))


