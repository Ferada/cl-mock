;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock-tests; -*-

(in-package #:cl-mock-tests)

(import 'cl-mock::(progm))

(defclass foo ()
  ())

(defgeneric baz (foo)
  (:method ((foo foo))
    42))

(def-test gf.simple ()
  (progm
      '((baz NIL (list)))
      '((lambda (list) list))
    (is (equal '(1 2 3) (baz '(1 2 3))))
    (signals error (equal T (baz T)))
    (is (equal 42 (baz (make-instance 'foo))))))
