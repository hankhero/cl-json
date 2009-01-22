;;;; Copyright (c) 2006-2008 Henrik Hjelte
;;;; All rights reserved.
;;;; See the file LICENSE for terms of use and distribution.

(in-package :json)

(defvar *class-registry* nil
  "A list of anonymous fluid classes, one member for every distinct
combination of direct superclasses.")

(defmacro with-local-class-registry ((&key inherit) &body body)
  "Run BODY in a dynamic environment where *CLASS-REGISTRY* is a
temporary local list.  If :INHERIT is non-null, the local registry
shall initially have the same content as the exterior *CLASS-REGISTRY*, 
otherwise it shall be NIL."
  `(let ((*class-registry*
          ,(if inherit
               `(copy-alist
                 ,(if (eql inherit t) '*class-registry* inherit)))))
     ,@body))

(defun clear-class-registry ()
  "Reset the *CLASS-REGISTRY* to NIL."
  (setq *class-registry* nil))

(defun find-class* (class-designator)
  "Like FIND-CLASS, but allow self-designating classes for the
argument, and assert that the resulting class is a STANDARD-CLASS."
  (let ((class
         (if (typep class-designator 'class)
             class-designator
             (find-class class-designator))))
    (check-type class standard-class)
    class))

(defclass fluid-class (standard-class) ()
  (:documentation "A class to whose instances arbitrary new slots may
be added on the fly."))

(defmethod add-direct-subclass ((superclass class)
                                (subclass fluid-class))
  "Fluid classes are thought to be anonymous, and so should not be
registered in the superclass."
  (declare (ignore superclass subclass))
  (values))

(defmethod remove-direct-subclass ((superclass class)
                                   (subclass fluid-class))
  "Fluid classes are thought to be anonymous, and so should not be
registered in the superclass."
  (declare (ignore superclass subclass))
  (values))

(defmethod validate-superclass ((class fluid-class)
                                (superclass standard-class))
  "Any fluid class is also a standard class."
  t)

(defclass fluid-object (standard-object) ()
  (:documentation "Any instance of a fluid class."))

(defmethod compute-class-precedence-list ((class fluid-class))
  "Objects of fluid classes are fluid objects."
  (loop for c in (call-next-method)
     with standard = (find-class 'standard-object)
       and fluid = (find-class 'fluid-object)
     if (eq c fluid) do (setq fluid nil)
     else if (and (eq c standard) fluid) collect fluid
     collect c))

(defmethod slot-missing ((class fluid-class) (object fluid-object) name
                         (op (eql 'slot-boundp)) &optional new-value)
  "A missing slot in a fluid class is considered unbound."
  (declare (ignore class object name op new-value))
  nil)

(defmethod slot-missing ((class fluid-class) (object fluid-object) name
                         (op (eql 'setf)) &optional new-value)
  "On attempting to set a missing slot, add the slot to the class,
then repeat SETF."
  (reinitialize-instance class
    :direct-superclasses
      (class-direct-superclasses class)
    :direct-slots
      (let ((extant-slots (class-direct-slots class)))
        (if (null extant-slots)
            `((:name ,name))
            (loop for slots on extant-slots
               for slot-name = (slot-definition-name (car slots))
               if (endp (cdr slots))
                 collect `(:name ,slot-name)
                 and collect `(:name ,name)
               else
                 collect `(:name ,slot-name)))))
  (make-instances-obsolete class)
  (setf (slot-value object name) new-value))

(defun ensure-fluid-class-with-slots (slots superclasses
                                      &optional extant-class)
  "Create or update a fluid class, ensuring that it has (at least) all
the given SLOTS and SUPERCLASSES."
  (flet ((extant-slot-p (name)
           (lambda (class)
             (loop for slot in (class-slots class)
                thereis (eq (slot-definition-name slot) name))))
         (slot-init (name) `(:name ,name)))
    (if extant-class
        (let* ((extant-superclasses
                (class-direct-superclasses extant-class))
               (new-superclasses
                (remove-if (lambda (class)
                             (loop for super in extant-superclasses
                                thereis (eql class super)))
                           superclasses))
               (extant-slots
                (mapcar #'slot-definition-name
                        (class-direct-slots extant-class)))
               (new-slots
                (remove-if (lambda (name)
                             (let ((containing (extant-slot-p name)))
                               (or (funcall containing extant-class)
                                   (some containing superclasses))))
                           slots)))
          (if (or new-superclasses new-slots)
              (make-instances-obsolete
               (reinitialize-instance extant-class
                 :direct-superclasses
                   (append extant-superclasses new-superclasses)
                 :direct-slots
                   (mapcar #'slot-init (nconc extant-slots new-slots)))))
          extant-class)
        (make-instance 'fluid-class
          :direct-superclasses superclasses
          :direct-slots
            (loop for slot in slots
               unless (some (extant-slot-p slot) superclasses)
               collect (slot-init slot))))))

(defun make-and-populate-instance (class bindings)
  "Make an instance of the given CLASS, and set its slots to given
values.  BINDINGS must be a list of pairs whose CARs are slot names
and CDRs are the respective values.  If no slot of a given name is
defined in the CLASS, the corresponding value is discarded."
  (let ((object (make-instance class)))
    (loop for (slot . value) in bindings
       if (slot-exists-p object slot)
         do (setf (slot-value object slot) value))
    object))

(defmethod make-object (bindings (class (eql nil)) &key (superclasses nil))
  "Create a FLUID-OBJECT with the slots given by BINDINGS and whose
class has all the given SUPERCLASSES.  If the current *CLASS-REGISTRY*
has a member with exactly the same direct superclasses, it is updated
to include all the given slots.  Otherwise, a new FLUID-CLASS is
allocated and added to the *CLASS-REGISTRY*."
  (let* ((superclasses
          (mapcar #'find-class* (or superclasses '(standard-object))))
         (extant-class-etc
          (member superclasses *class-registry*
                  :test #'equal :key #'class-direct-superclasses))
         (extant-class (car extant-class-etc))
         (updated-class
          (ensure-fluid-class-with-slots
           (mapcar #'car bindings) superclasses extant-class)))
    (if extant-class
        (if (not (eq extant-class updated-class))
            (setf (car extant-class-etc) updated-class))
        (push updated-class *class-registry*))
    (make-and-populate-instance updated-class bindings)))

(defmethod make-object (bindings class &key &allow-other-keys)
  "If the CLASS is explicitly specified, just create and populate an
instance."
  (let ((class (find-class* class)))
    (make-and-populate-instance class bindings)))

(defmethod make-object (bindings (class (eql (find-class 'cons)))
                        &key &allow-other-keys)
  "If the CLASS is given as 'CONS, return the BINDINGS as alist."
  (copy-seq bindings))

(defmethod make-object (bindings (class (eql (find-class 'list)))
                        &key &allow-other-keys)
  "If the CLASS is given as 'LIST, return the BINDINGS as plist."
  (loop for (key . value) in bindings
     collect key collect value))

(defmethod make-object (bindings (class (eql (find-class 'hash-table)))
                        &key &allow-other-keys)
  "If the CLASS is given as 'HASH-TABLE, return the BINDINGS as hash
table."
  (let ((table (make-hash-table)))
    (loop for (key . value) in bindings
      do (setf (gethash key table) value))
    table))

(defmethod make-object (bindings (class symbol) &key &allow-other-keys)
  "If the CLASS is given as a symbol, find it and resort to the usual
procedure."
  (make-object bindings (find-class class)))

(defun max-package (symbols &key ((:initial-value package)
                                  (find-package '#:common-lisp)))
  "Try to find a package P such that the names of the given SYMBOLS,
when interned in P, yield the same symbols.  If no such package
exists, return an unspecific value and issue a warning."
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
  "Same as PACKAGE-NAME, but ensure that the result is a symbol."
  (let ((name (package-name package)))
    (if (stringp name) (make-symbol name) name)))

(defvar *prototype-name* 'prototype
  "The name of the prototype field in a JSON object, and the name of a
slot in a Lisp object which accepts its prototype.")

(defclass prototype ()
  ((lisp-class :initarg :lisp-class :reader lisp-class)
   (lisp-superclasses :initarg :lisp-superclasses :reader lisp-superclasses)
   (lisp-package :initarg :lisp-package :reader lisp-package))
  (:default-initargs :lisp-class nil :lisp-superclasses nil
                     :lisp-package nil)
  (:documentation "A PROTOTYPE contains metadata for an object's class
in a format easily serializable to JSON: either the name of the class
as a string or (if it is anonymous) the names of the superclasses as a
list of strings; and the name of the Lisp package into which the names
of the class's slots and the name of the class / superclasses are to
be interned."))

(defmethod make-load-form ((prototype prototype) &optional environment)
  (declare (ignore environment))
  `(make-instance 'prototype
     ,@(if (slot-boundp prototype 'lisp-class)
           `(:lisp-class ,(lisp-class prototype)))
     ,@(if (slot-boundp prototype 'lisp-superclasses)
           `(:lisp-superclasses ,(lisp-superclasses prototype)))
     ,@(if (slot-boundp prototype 'lisp-package)
           `(:lisp-package ,(lisp-package prototype)))))

(defmethod make-object-prototype (object &optional slot-names)
  "Return a PROTOTYPE describing the OBJECT's class or superclasses,
and the package into which the names of the class / superclasses and
of the OBJECT's slots are to be interned."
  (let* ((class (class-of object))
         (class-name (if class (class-name class)))
         (superclass-names
          (if (not class-name)
              (set-difference
               (mapcar #'class-name (class-direct-superclasses class))
               '(standard-object fluid-object))))
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
  "Return a PROTOTYPE of an object of the class named by CLASS-NAME."
  (declare (ignore slot-names))
  (make-instance 'prototype
    :lisp-class class-name
    :lisp-package (package-name* (symbol-package class-name))))

(defmethod make-object-prototype ((object prototype) &optional slot-names)
  "Prototypes are not to be given their own prototypes, otherwise we
would proceed ad malinfinitum."
  (declare (ignore object slot-names))
  nil)

(defun maybe-add-prototype (object prototype)
  "If the OBJECT has a slot to accept the prototype, do set it.
Return OBJECT."
  (if (and prototype (slot-exists-p object *prototype-name*))
      (setf (slot-value object *prototype-name*) prototype))
  object)

(defun map-slots (function object)
  "Call FUNCTION on the name and value of every bound slot in OBJECT."
  (loop for slot in (class-slots (class-of object))
     for slot-name = (slot-definition-name slot)
     if (slot-boundp object slot-name)
       do (funcall function slot-name (slot-value object slot-name))))
