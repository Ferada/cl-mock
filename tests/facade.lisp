;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock-tests; -*-

(in-package #:cl-mock-tests)

(in-suite cl-mock)

(def-test answer.simple ()
  (with-mocks ()
    (answer (foo 1) 42)
    (answer foo 23)
    (is (eql 42 (foo 1)))))

(def-test answer.literal ()
  (with-mocks ()
    (answer (foo 1) 2)
    (answer (foo 2) 3)
    (answer foo 42)
    (is (eql 2 (foo 1)))
    (is (eql 2 (foo 1)))
    (is (eql 3 (foo 2)))
    (is (eql 3 (foo 2)))
    (is (eql 42 (foo)))
    (is (eql 42 (foo 'foo)))))

(def-test answer.times ()
  (with-mocks ()
    (answer foo 1 2 3)
    (is (eql 1 (foo)))
    (is (eql 2 (foo)))
    (is (eql 3 (foo)))
    (is (eql 3 (foo)))))

(def-test answer.call-previous ()
  (with-mocks ()
    (answer foo 3 (call-previous))
    (is (eql 3 (foo)))
    (is (eq 'foo (foo)))))
