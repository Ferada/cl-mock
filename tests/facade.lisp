;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock-tests; -*-

(in-package #:cl-mock-tests)

(import 'cl-mock::(call-with-mocks make-mock-bindings register-mock if-called when-called call-previous))

(def-test call-with-mocks.empty ()
  (is (eq T (call-with-mocks
             (make-mock-bindings)
             (constantly T)))))

(def-test call-with-mocks.discards-values ()
  (is (equal
       '(1 NIL)
       (multiple-value-list
        (call-with-mocks
         (make-mock-bindings)
         (lambda ()
           (values 1 2 3)))))))

(def-test call-with-mocks.simple ()
  (declare (notinline foo))
  (defun foo ()
    (fail "original function binding ~A was called" 'foo))
  (let ((mock-bindings (make-mock-bindings)))
    (register-mock mock-bindings 'foo)
    (call-with-mocks
     mock-bindings
     (lambda ()
       (foo)
       (pass)))))

(def-test call-with-mocks.default-values ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (register-mock mock-bindings 'foo)
    (call-with-mocks
     mock-bindings
     (lambda ()
       (is (null (multiple-value-list (foo))))))))

(def-test if-called.simple ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (if-called mock-bindings 'foo (constantly T) (constantly 42))
    (call-with-mocks
     mock-bindings
     (lambda ()
       (is (eql 42 (foo)))))))

(def-test call-with-mocks.invocations ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (register-mock mock-bindings 'foo)
    (is (equal
         '(NIL ((foo 1) (foo 2) (foo 3)))
         (multiple-value-list
          (call-with-mocks
           mock-bindings
           (lambda ()
             (foo 1)
             (foo 2)
             (foo 3))))))))

(def-test when-called.simple ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (when-called mock-bindings foo 42)
    (when-called mock-bindings foo 23)
    (call-with-mocks
     mock-bindings
     (lambda ()
       (is (eql 42 (foo)))))))

(def-test when-called.literal ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (when-called mock-bindings (foo 1) 2)
    (when-called mock-bindings (foo 2) 3)
    (when-called mock-bindings foo 42)
    (call-with-mocks
     mock-bindings
     (lambda ()
       (is (eql 2 (foo 1)))
       (is (eql 2 (foo 1)))
       (is (eql 3 (foo 2)))
       (is (eql 3 (foo 2)))
       (is (eql 42 (foo)))
       (is (eql 42 (foo 'foo)))))))

(def-test when-called.times ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (when-called mock-bindings foo 1 2 3)
    (call-with-mocks
     mock-bindings
     (lambda ()
       (is (eql 1 (foo)))
       (is (eql 2 (foo)))
       (is (eql 3 (foo)))
       (is (eql 3 (foo)))))))

(def-test when-called.call-previous ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (when-called mock-bindings foo 3 (call-previous))
    (call-with-mocks
     mock-bindings
     (lambda ()
       (is (eql 3 (foo)))
       (is (eq 'foo (foo)))))))
