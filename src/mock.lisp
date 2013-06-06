;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock; -*-

(in-package #:cl-mock)

;;; mocking of regular functions

(defstruct mock-bindings
  mocks)

(defvar *previous*)
(defvar *arguments*)

(defun call-previous (&rest args)
  "Invokes the previous binding either with the current arguments or with
the given ones.  Use *PREVIOUS*/*ARGUMENTS* directly in edge cases."
  (apply *previous* (or args *arguments*)))

(defun find-and-invoke-mock (*previous* cases *arguments*)
  "Looks for a compatible mock (i.e. calls the TEST until one returns true)
and executes it.  If no mock was found, no values are returned instead."
  (dolist (case cases (values))
    (when (ignore-errors (apply (car case) *arguments*))
      (return (apply (cdr case) *arguments*)))))

(defun call-with-mocks (mock-bindings function &key (recordp T))
  "Calls FUNCTION with the given MOCK-BINDINGS established and returns
its first return value, if any.  If RECORDP is set, all invocations will
be recorded and returned as the second return value, else NIL."
  (let* ((mocks (mock-bindings-mocks mock-bindings))
         (functions (mapcar #'car mocks))
         (previous (mapcar #'maybe-fdefinition functions))
         invocations)
    (call-with-function-bindings
     functions
     (mapcar (lambda (binding previous &aux (name (car binding)) (cases (cdr binding)))
               (lambda (&rest args)
                 (when recordp
                   (push (cons name args) invocations))
                 (find-and-invoke-mock previous cases args)))
             mocks previous)
     (lambda ()
       (values
        (funcall function)
        (nreverse invocations)))
     previous)))

(defun register-mock (mock-bindings name)
  "Registers a mocked function under NAME.  The mocked function will
return no values.  See IF-CALLED to add some behaviour to it."
  (let ((found (member name (mock-bindings-mocks mock-bindings) :key #'car :test #'eq)))
    (or (car found)
        (let ((binding (list name)))
          (push binding (mock-bindings-mocks mock-bindings))
          binding))))

(defun if-called (mock-bindings name test function &key at-start)
  "Registers a new binding to be called when the TEST function returns
true.  If AT-START is set, the binding is put at the start of the bindings
list instead.  Calls REGISTER-MOCK automatically."
  (let ((binding (register-mock mock-bindings name))
        (case (cons test function)))
    (if at-start
        (push case (cdr binding))
        (setf (cdr binding) (append (cdr binding) (list case))))))
