;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json)

(defvar *prototype-name* 'prototype)

(defun find-class* (class-designator)
  (if (typep class-designator
             '(or standard-class structure-class built-in-class))
      class-designator
      (find-class class-designator)))

(defun old-and-new-slots (bindings superclasses)
  (let ((superclasses-slots
         (mapcar #'slot-definition-name
                 (reduce #'union (mapcar #'class-slots superclasses)
                         :initial-value nil))))
    (loop for binding in bindings
      if (member (car binding) superclasses-slots) collect binding into old
      else collect binding into new
      finally (return (values old new)))))

(defun make-and-populate-instance (class bindings)
  (let ((object (make-instance class)))
    (loop for (slot . value) in bindings
      do (setf (slot-value object slot) value))
    object))

(defmethod make-object (bindings (class (eql nil)) &key (superclasses nil))
  (let ((superclasses (mapcar #'find-class* superclasses)))
    (multiple-value-bind (old-slot-bindings new-slot-bindings)
        (old-and-new-slots bindings superclasses)
      (if (and (null new-slot-bindings) (<= 0 (length superclasses) 1))
          (setq class (or (first superclasses) 'standard-object))
          (let ((anonymous-class
                 (make-instance 'standard-class
                   :direct-superclasses superclasses
                   :direct-slots (mapcar
                                  (lambda (binding) `(:name ,(car binding)))
                                  new-slot-bindings))))
            (dolist (superclass superclasses)
              ;; The anonymous class should be removed from the
              ;; superclasses' direct-subclasses lists right away because
              ;; it is purely interim.  If it is not removed, it remains
              ;; after its (single) instance is disused and so never GCs.
              (remove-direct-subclass superclass anonymous-class))
            (setq class anonymous-class)))
      (make-and-populate-instance
       class (nconc new-slot-bindings old-slot-bindings)))))

(defmethod make-object (bindings class &key &allow-other-keys)
  (let ((class (find-class* class)))
    (make-and-populate-instance
     class (old-and-new-slots bindings (list class)))))

(defmethod make-object (bindings (class (eql (find-class 'cons)))
                        &key &allow-other-keys)
  (copy-seq bindings))

(defmethod make-object (bindings (class (eql (find-class 'list)))
                        &key &allow-other-keys)
  (loop for (key . value) in bindings
     collect key collect value))

(defmethod make-object (bindings (class (eql (find-class 'hash-table)))
                        &key &allow-other-keys)
  (let ((table (make-hash-table)))
    (loop for (key . value) in bindings
      do (setf (gethash key table) value))
    table))

(defmethod make-object (bindings (class symbol) &key &allow-other-keys)
  (make-object bindings (find-class class)))

(defclass prototype ()
  ((lisp-class :initarg :lisp-class :reader lisp-class)
   (lisp-superclasses :initarg :lisp-superclasses :reader lisp-superclasses)
   (lisp-package :initarg :lisp-package :reader lisp-package))
  (:default-initargs :lisp-class nil :lisp-superclasses nil
                     :lisp-package nil))

(defmethod make-load-form ((prototype prototype) &optional environment)
  (declare (ignore environment))
  `(make-instance 'prototype
     ,@(if (slot-boundp prototype 'lisp-class)
           `(:lisp-class ,(lisp-class prototype)))
     ,@(if (slot-boundp prototype 'lisp-superclasses)
           `(:lisp-superclasses ,(lisp-superclasses prototype)))
     ,@(if (slot-boundp prototype 'lisp-package)
           `(:lisp-package ,(lisp-package prototype)))))

(defun max-package (symbols &key ((:initial-value package)
                                  (find-package '#:common-lisp)))
  (labels ((symbol-in-package-p (symbol)
             (eq (find-symbol (symbol-name symbol) package) symbol))
           (update-package-for-symbol (symbol)
             (if (not (symbol-in-package-p symbol))
                 (setq package (symbol-package symbol))))
           (check-symbol (symbol)
             (if (not (symbol-in-package-p symbol))
                 (warn "Symbol ~S cannot be found in apparent package ~S."
                       symbol package))))
    (mapc #'update-package-for-symbol symbols)
    (mapc #'check-symbol symbols)
    package))

(defun package-name* (package)
  (let ((name (package-name package)))
    (if (stringp name) (make-symbol name) name)))

(defmethod make-object-prototype (object &optional slot-names)
  (let* ((class (class-of object))
         (class-name (if class (class-name class)))
         (superclass-names
          (if (not class-name)
              (remove 'standard-object
                      (mapcar #'class-name
                              (class-direct-superclasses class)))))
         (package
          (max-package (append superclass-names slot-names)
            :initial-value (if class-name
                               (symbol-package class-name)
                               (find-package '#:common-lisp)))))
      (make-instance 'prototype
        :lisp-class class-name
        :lisp-superclasses superclass-names
        :lisp-package (package-name* package))))

(defmethod make-object-prototype ((class-name symbol) &optional slot-names)
  (declare (ignore slot-names))
  (make-instance 'prototype
    :lisp-class class-name
    :lisp-package (package-name* (symbol-package class-name))))

(defmethod make-object-prototype ((object prototype) &optional slot-names)
  (declare (ignore object slot-names))
  nil)

(defun maybe-add-prototype (object prototype)
  (if prototype
      (loop for slot in (class-slots (class-of object))
        for slot-name = (slot-definition-name slot)
        if (eql slot-name *prototype-name*)
          return (setf (slot-value object slot-name) prototype)))
  object)

(defun map-object-slots-and-prototype (function object)
  (loop for slot in (class-slots (class-of object))
     for slot-name = (slot-definition-name slot)
     with explicit-prototype = nil
     if (slot-boundp object slot-name)
       do (funcall function slot-name (slot-value object slot-name))
       if *prototype-name*
         if (eq slot-name *prototype-name*)
           do (setq explicit-prototype t)
         else unless explicit-prototype
           collect slot-name into slot-names
     finally
       (if (and *prototype-name* (not explicit-prototype))
           (let ((prototype
                  (make-object-prototype object slot-names)))
             (funcall function *prototype-name* prototype)))))
