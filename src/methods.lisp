;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock; -*-

(in-package #:cl-mock)

;;; mocking of generic methods and objects

(defun find-methods (methods)
  (mapcar (lambda (method)
            (destructuring-bind (generic-function qualifiers specializers) method
              (cons
               generic-function
               (find-method generic-function qualifiers specializers NIL))))
          methods))

;; TODO: because we use ENSURE-METHOD, each value is a FORM rather than a
;; FUNCTION, so not quite the same as PROGF; judging by the implementation-
;; specific code in CLOSER-MOP, we also can just create method objects
;; ourselves reliably, so either we duplicate the cases or just use SBCL

(defun call-with-method-bindings* (methods values function
                                   &optional (previous (find-methods methods)))
  (mapc (lambda (previous)
          (destructuring-bind (generic-function . method) previous
            (when method
              (remove-method generic-function method))))
        previous)
  (let ((new-methods
          (mapcar (lambda (method previous value)
                    (destructuring-bind (generic-function qualifiers specializers) method
                      (declare (ignore generic-function))
                      (destructuring-bind (generic-function . method) previous
                        (cons
                         generic-function
                         (if method
                             (ensure-method generic-function value
                                            :method-class (class-of method)
                                            :qualifiers (method-qualifiers method)
                                            :lambda-list (method-lambda-list method)
                                            :specializers (method-specializers method))
                             (ensure-method generic-function value
                                            :qualifiers qualifiers
                                            :specializers specializers))))))
                  methods previous values)))
    (unwind-protect (funcall function)
      (mapc (lambda (new-method)
              (destructuring-bind (generic-function . method) new-method
                (remove-method generic-function method)))
            new-methods)
      (mapc (lambda (previous)
              (destructuring-bind (generic-function . method) previous
                (when method
                  (add-method generic-function method))))
            previous))))

(defmacro progm* (methods values &body body)
  `(call-with-method-bindings* ,methods ,values (lambda () ,@body)))

(defun classify (specializer)
  (if (classp specializer)
      specializer
      (find-class specializer)))

(defun call-with-method-bindings (methods values function
                                  &optional previous)
  (let ((methods
          (mapcar (lambda (method)
                    (destructuring-bind (generic-function qualifiers specializers) method
                      (list
                       (ensure-function generic-function)
                       qualifiers
                       (mapcar #'classify specializers))))
                  methods)))
    (call-with-method-bindings* methods values function (or previous (find-methods methods)))))

(defmacro progm (methods values &body body)
  `(call-with-method-bindings ,methods ,values (lambda () ,@body)))
