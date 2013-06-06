;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock; -*-

(in-package #:cl-mock)

;;; dynamic rebinding of functions

(defun maybe-fdefinition (name)
  "If NAME is FBOUNDP, return its FDEFINITION, else NIL."
  (and (fboundp name) (fdefinition name)))

(defun set-fdefinition (name value)
  "FUNCALLABLE expansion of (SETF (FDEFINITION NAME) VALUE)."
  (setf (fdefinition name) value))

(defun set-or-unbind-fdefinition (name value)
  "If VALUE is true, set the FDEFINITION of NAME to it, else FMAKUNBOUND
it completely."
  (if value (set-fdefinition name value) (fmakunbound name)))

(defun call-with-function-bindings (functions values function
                                    &optional (previous (mapcar #'maybe-fdefinition functions)))
  "Calls FUNCTION while temporarily binding all FUNCTIONS to VALUES.
See PROGF and PROGV."
  (unwind-protect
       (progn
         (mapc #'set-fdefinition functions values)
         (funcall function))
    (mapc #'set-or-unbind-fdefinition functions previous)))

(defmacro progf (functions values &body body)
  "Like PROGV, but for FUNCTIONS."
  `(call-with-function-bindings ,functions ,values (lambda () ,@body)))

(defmacro dflet ((&rest definitions) &body body)
  "Like FLET, but dynamically sets the FDEFINITIONS during the duration of
the BODY."
  `(progf
       ',(mapcar #'car definitions)
       (list
        ,.(mapcar (lambda (definition)
                    `(lambda ,(cadr definition)
                       ,@(cddr definition)))
                  definitions))
     ,@body))
