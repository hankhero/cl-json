(in-package :json)

;; helpers for json-bind
(defun cdas(item alist)
  "Alias for (cdr (assoc item alist))"
  (cdr (assoc item alist)))

(defun last1 (lst)
  (first (last lst)))

(defmacro assoc-lookup (&rest lookuplist)
  "(assoc-lookup :x :y alist) => (cdr (assoc :y (cdr (assoc :x alist))))"
  (let ((alist-form (last1 lookuplist))
        (lookups (reverse (butlast lookuplist))))
    (labels ((mk-assoc-lookup (lookuplist)
               (if lookuplist
                  `(cdas ,(first lookuplist) ,(mk-assoc-lookup (rest lookuplist)))
                  alist-form)))
      (mk-assoc-lookup lookups))))

(defmacro json-bind (vars json-string-or-alist &body body)
  (labels ((symbol-as-string (symbol)
           (string-downcase (symbol-name symbol)))
          (split-by-dots (string)
           (loop for ch across string
                 with x
                 with b
                 do (if (char= #\. ch)
                        (progn 
                          (push (concatenate 'string (nreverse b)) x)
                          (setf b nil))
                        (push ch b))
                 finally (progn
                           (push (concatenate 'string (nreverse b)) x)
                           (return (nreverse x)))))
         (lookup-deep (variable)
           (mapcar #'json-intern (split-by-dots (symbol-as-string variable)))))
     (let ((a-list (gensym)))
      `(let ((,a-list (if (stringp ,json-string-or-alist)
                          (decode-json-from-string ,json-string-or-alist)
                          ,json-string-or-alist)))
        (let ,(loop for v in vars collect `(,v (assoc-lookup ,@(lookup-deep v)
                                                ,a-list)))
          ,@body)))))



