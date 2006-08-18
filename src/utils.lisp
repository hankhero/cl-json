(in-package :json)

(defmacro json-bind (vars json-string &body body)
  (let ((a-list (gensym)))
    `(let ((,a-list (decode-json-from-string ,json-string)))
      (let ,(loop for v in vars collect `(,v (cdr (assoc (intern (symbol-name ',v) (find-package :keyword)) ,a-list))))
        ,@body))))
