;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock-tests; -*-

(in-package #:cl-mock-tests)

(in-suite cl-mock)

(def-test progf.calls-binding ()
  (progf '(foo) (list (lambda () 23))
    (is (eql 23 (foo)))))

(def-test dflet.calls-binding ()
  (dflet ((foo () 23))
    (is (eql 23 (foo)))))

(def-test dflet.notinline.works ()
  (declare (notinline foo bar))
  (defun foo () 23)
  (dflet ((foo () 42))
    (is (eql 42 (foo)))))

(def-test dflet.simple-mock ()
  (defun foo (&optional (string "Hello, World!"))
    (1+ (bar string)))
  (defun bar (string)
    (length string))
  (dflet ((bar (string)
            (cond
              ((equalp string "Hello, World!")
               42))))
    (is (eql 43 (foo)))
    (is (eql 43 (foo "HELLO, WORLD!")))))

(def-test dflet.package-locks ()
  "Either we can rebind LIST, or an error occurs and the binding is not
modified."
  (let ((list #'list))
    (handler-case (dflet ((list ()))
                    (is (eql 42 (list))))
      (error ()
        (is (eq #'list list))))))

(def-test dflet.package-locks.order.1 ()
  (defun foo ()
    23)
  (let ((list #'list)
        (foo #'foo))
    (handler-case (dflet
                      ((foo () 13)
                       (list () 42))
                    (is (eql 42 (list)))
                    (is (eql 13 (foo))))
      (error ()
        (is (eq #'list list))
        (is (eq #'foo foo))))))

(def-test dflet.package-locks.order.2 ()
  (defun foo ()
    23)
  (let ((list #'list)
        (foo #'foo))
    (handler-case (dflet
                      ((list () 42)
                       (foo () 13))
                    (is (eql 42 (list)))
                    (is (eql 13 (foo))))
      (error ()
        (is (eq #'list list))
        (is (eq #'foo foo))))))
