;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json)

(defun range-keys (var-keys)
  (loop for var-key in var-keys
     for (primary-key . subkey) =
       (destructuring-bind (var . key) var-key
         (let ((dot (position #\. key :test #'char=)))
           (if dot
               (cons (subseq key 0 dot) (cons var (subseq key (1+ dot))))
               (cons key var))))
     for subkeys-of-primary =
       (assoc primary-key subkeys :test #'string=)
     if subkeys-of-primary
       do (push subkey (cdr subkeys-of-primary))
     else
       collect (cons primary-key (list subkey)) into subkeys
     finally (return subkeys)))

(defun json-bind-level-customizations (level-keys value-required
                                       defaults trivials)
  (if (endp level-keys)
      (if value-required defaults trivials)
      (loop for (key . subs) in (range-keys level-keys)
         with subkeys and vars-to-bind
         do (loop for sub in subs
               initially (setq subkeys nil vars-to-bind nil)
               if (consp sub) do (push sub subkeys)
               else do (push sub vars-to-bind))
         collect
           `((string= key ,key)
             (set-custom-vars
              :internal-decoder
                (custom-decoder
                 ,@(json-bind-level-customizations
                    subkeys vars-to-bind defaults trivials))
              :object-value
                (lambda (value)
                  ,@(loop for var in vars-to-bind
                       collect `(setq ,var value))
                  ,(if value-required
                       `(funcall ,(getf defaults :object-value) value)))))
           into match-clauses
         finally
           (return
             `(:object-key
               (lambda (key)
                 (let ((key (funcall *json-identifier-name-to-lisp* key)))
                   (cond
                     ,@match-clauses
                     (t (set-custom-vars
                         :internal-decoder
                           (custom-decoder
                            ,@(if value-required defaults trivials))
                         :object-value
                           ,(if value-required
                                (getf defaults :object-value)
                                (getf trivials :object-value))))))
                 ,(if value-required
                      `(funcall ,(getf defaults :object-key) key))))))))

(defmacro json-bind ((&rest vars) json-source &body body)
  (let ((boo (gensym)) (ok (gensym)) (ov (gensym)) (eoo (gensym))
        (boa (gensym)) (ae (gensym)) (eoa (gensym)) (bos (gensym))
        (sc (gensym)) (eos (gensym)) (id (gensym)) (pass (gensym))
        (jsrc (gensym)))
    `(let (,@vars
           (,boo *beginning-of-object-handler*) (,ok *object-key-handler*)
           (,ov *object-value-handler*) (,eoo *end-of-object-handler*)
           (,boa *beginning-of-array-handler*)
           (,ae *array-element-handler*) (,eoa *end-of-array-handler*)
           (,bos *beginning-of-string-handler*) (,sc *string-char-handler*)
           (,eos *end-of-string-handler*) (,id *internal-decoder*)
           (,pass (constantly t)) (,jsrc ,json-source))
       (bind-custom-vars
           (,@(json-bind-level-customizations
               (loop for var in vars collect (cons var (symbol-name var)))
               nil
               (list :beginning-of-object boo :object-key ok
                     :object-value ov :end-of-object eoo
                     :beginning-of-array boa :array-element ae
                     :end-of-array eoa :beginning-of-string bos
                     :string-char sc :end-of-string eos
                     :internal-decoder id)
               (list :beginning-of-object pass :object-key pass
                     :object-value pass :end-of-object pass
                     :beginning-of-array pass :array-element pass
                     :end-of-array pass :beginning-of-string pass
                     :string-char pass :end-of-string pass
                     :internal-decoder #'decode-json))
            :structure-scope-variables
              (union *structure-scope-variables*
                     '(*object-key-handler* *object-value-handler*
                       *internal-decoder*)))
         (etypecase ,jsrc
           (string (decode-json-from-string ,jsrc))
           (stream (decode-json ,jsrc))))
       ,@body)))

;;; Old code:

;;; helpers for json-bind
;(defun cdas(item alist)
;  "Alias for (cdr (assoc item alist))"
;  (cdr (assoc item alist)))
;
;(defun last1 (lst)
;  (first (last lst)))
;
;(defmacro assoc-lookup (&rest lookuplist)
;  "(assoc-lookup :x :y alist) => (cdr (assoc :y (cdr (assoc :x alist))))"
;  (let ((alist-form (last1 lookuplist))
;        (lookups (reverse (butlast lookuplist))))
;    (labels ((mk-assoc-lookup (lookuplist)
;               (if lookuplist
;                  `(cdas ,(first lookuplist) ,(mk-assoc-lookup (rest lookuplist)))
;                  alist-form)))
;      (mk-assoc-lookup lookups))))
;
;(defmacro json-bind (vars json-string-or-alist &body body)
;  (labels ((symbol-as-string (symbol)
;           (string-downcase (symbol-name symbol)))
;          (split-by-dots (string)
;           (loop for ch across string
;                 with x
;                 with b
;                 do (if (char= #\. ch)
;                        (progn 
;                          (push (concatenate 'string (nreverse b)) x)
;                          (setf b nil))
;                        (push ch b))
;                 finally (progn
;                           (push (concatenate 'string (nreverse b)) x)
;                           (return (nreverse x)))))
;         (lookup-deep (variable)
;           (mapcar (lambda (nm) `(json-intern ,nm))
;                   (split-by-dots (symbol-as-string variable)))))
;     (let ((a-list (gensym)))
;      `(let ((,a-list (if (stringp ,json-string-or-alist)
;                          (decode-json-from-string ,json-string-or-alist)
;                          ,json-string-or-alist)))
;        (let ,(loop for v in vars collect `(,v (assoc-lookup ,@(lookup-deep v)
;                                                ,a-list)))
;          ,@body)))))
