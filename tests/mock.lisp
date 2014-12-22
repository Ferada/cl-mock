;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock-tests; -*-

(in-package #:cl-mock-tests)

(in-suite cl-mock)

(def-test call-with-mocks.empty ()
  (is (eq T (with-mocks () T))))

(def-test call-with-mocks.discards-values ()
  (is (equal
       '(1 2 3)
       (multiple-value-list
        (with-mocks ()
          (values 1 2 3))))))

(declaim (notinline foo/simple))
(defun foo/simple ()
  (fail "original function binding ~A was called" 'foo/simple))

(def-test call-with-mocks.simple ()
  (with-mocks ()
    (register-mock 'foo/simple)
    (foo/simple)
    (pass)))

(declaim (notinline foo bar))
(defun foo () 'foo)
(defun bar () 'bar)

(def-test call-with-mocks.default-values ()
  (with-mocks ()
    (register-mock 'foo)
    (is (null (multiple-value-list (foo))))))

(def-test if-called.simple ()
  (with-mocks ()
    (if-called 'foo (constantly 42))
    (is (eql 42 (foo)))))

(def-test invocations.length ()
  (with-mocks ()
    (register-mock 'foo)
    (dotimes (i 3) (foo))
    (is (eql 3 (length (invocations))))))

(def-test invocations.arguments ()
  (with-mocks ()
    (register-mock 'foo)
    (let ((sym (gensym)))
      (foo sym)
      (is (equal `((foo ,sym)) (invocations))))))

(def-test invocations.name ()
  (with-mocks ()
    (register-mock 'foo)
    (register-mock 'bar)
    (foo)
    (bar)
    (is (equal `((foo)) (invocations 'foo)))))
