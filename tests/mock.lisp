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
    (is (equal '((foo)) (invocations 'foo)))))

;; utility function to check asynchronous results
(defun assert-cond (assert-fun max-time &optional (sleep-time 0.05))
  (do ((wait-time sleep-time (+ wait-time sleep-time))
       (fun-result nil (funcall assert-fun)))
      ((eq fun-result t) (return t))
    (if (> wait-time max-time) (return)
        (sleep sleep-time))))

(def-test invocations.threaded ()
  (with-mocks ()
    (register-mock 'foo)
    (register-mock 'bar)
    (bt:make-thread (lambda ()
                      (foo)
                      (bar)))
    (is (assert-cond (lambda ()
                       (and (equal '((foo)) (invocations 'foo))
                            (equal '((bar)) (invocations 'bar))))
                     .5))))
