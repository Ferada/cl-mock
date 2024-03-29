;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock; -*-

(in-package #:cl-mock)

;;; mocking of regular functions

(defvar *mock-bindings*)
(defvar *invocations*)
(defvar *recordp*)

(defvar *invocations-lock* (bt:make-lock))

(defvar *previous*)
(defvar *arguments*)

(defun invocations (&optional name)
  (let ((invocations (car *invocations*)))
    (if name
        (remove name invocations :key #'car :test-not #'eq)
        invocations)))

(defun call-previous (&rest args)
  "Invokes the previous binding either with the current arguments or with
the given ones.  Use *PREVIOUS*/*ARGUMENTS* directly in edge cases."
  (apply *previous* (or args *arguments*)))

(defun record-invocation (record &aux (record (list record)))
  (bt:with-lock-held (*invocations-lock*)
    (setf (cdr *invocations*)
          (if (null (car *invocations*))
              (setf (car *invocations*) record)
              (setf (cddr *invocations*) record)))))

(defun find-and-invoke-mock (binding *arguments*)
  "Looks for a compatible mock (i.e. calls the TEST until one returns true)
and executes it.  If no mock was found, no values are returned instead."
  (when *recordp*
    (record-invocation (cons (car binding) *arguments*)))
  (dolist (case (cdddr binding) (values))
    (let ((*previous* (cadr binding)))
      (catch 'unhandled
        (return (apply case *arguments*))))))

(defun unhandled ()
  (throw 'unhandled (values)))

(defun register-mock (name)
  "Registers a mocked function under NAME.  The mocked function will
return no values.  See IF-CALLED to add some behaviour to it."
  (let ((found (member name *mock-bindings* :key #'car :test #'eq)))
    (or (car found)
        (let* ((binding (list name (maybe-fdefinition name) NIL))
               (function (lambda (&rest arguments)
                           (find-and-invoke-mock binding arguments))))
          (setf (caddr binding) function)
          (push binding *mock-bindings*)
          (set-fdefinition name function)
          binding))))

(defun if-called (name function &key at-start)
  "Registers a new binding, which should return true if it handled the
invocation.  If AT-START is set, the binding is put at the start of the
bindings list instead.  Calls REGISTER-MOCK automatically."
  (let ((binding (register-mock name)))
    (if at-start
        (push function (cdddr binding))
        (setf (cdddr binding) (append (cdddr binding) (list function))))))

(defun call-with-mocks (function &key ((:recordp recordp) T))
  "Call FUNCTION with a new mocking context.  Invocations will be
recorded if RECORDP is set (default true)."
  (let (*mock-bindings*)
    (setf *invocations* (list NIL))
    (setf *recordp* recordp)
    (unwind-protect (funcall function)
      (mapc (lambda (binding)
              (set-or-unbind-fdefinition (car binding) (cadr binding)))
            *mock-bindings*))))

(defmacro with-mocks ((&key (recordp T)) &body body)
  "Execute BODY in a new mocking context.  Invocations will be recorded
if RECORDP is set (default true)."
  `(call-with-mocks
    (lambda () ,@body)
    :recordp ,recordp))
