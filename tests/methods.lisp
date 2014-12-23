;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock-tests; -*-

(in-package #:cl-mock-tests)

(in-suite cl-mock)

(defclass foo ()
  ())

(defgeneric baz (foo)
  (:method ((foo foo))
    42))

(def-test gf.simple ()
  (cl-mock::progm
      '((baz NIL (list)))
      '((lambda (list) list))
    (is (equal '(1 2 3) (baz '(1 2 3))))
    (signals error (eq T (baz T)))
    (is (eql 42 (baz (make-instance 'foo))))))

(def-test gf.overwrite ()
  (cl-mock::progm
      '((baz NIL (foo)))
      '((lambda (foo) 23))
    (is (eql 23 (baz (make-instance 'foo)))))
  (is (eql 42 (baz (make-instance 'foo)))))
