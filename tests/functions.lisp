;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock-tests; -*-

(in-package #:cl-mock-tests)

(in-suite cl-mock)

(def-test progf.calls-binding ()
  (progf '(foo) (list (lambda () 23))
    (is (eql 23 (foo)))))

(def-test dflet.calls-binding ()
  (dflet ((foo () 23))
    (is (eql 23 (foo)))))

(declaim (inline foo/inline))
(defun foo/inline ()
  23)

(def-test dflet.inline.works ()
  "If a function is declared INLINE (and that request is honored), DFLET
won't work.  Not a failure, since we can't force INLINE."
  (dflet ((foo/inline () 42))
    (if (eql 23 (foo/inline))
        (pass "INLINE declaration honored, so DFLET fails")
        (skip "INLINE declaration not honored, so DFLET works"))))

(def-test dflet.notinline.works ()
  "If a function is declared INLINE, but NOTINLINE is used locally,
DFLET will work."
  (declare (notinline foo/inline))
  (dflet ((foo/inline () 42))
    (is (eql 42 (foo/inline)))))

(defun foo/mock (&optional (string "Hello, World!"))
  (1+ (bar/mock string)))

(defun bar/mock (string)
  (length string))

(def-test dflet.simple-mock ()
  (dflet ((bar/mock (string)
            (cond
              ((equalp string "Hello, World!")
               42))))
    (is (eql 43 (foo/mock)))
    (is (eql 43 (foo/mock "HELLO, WORLD!")))))

(def-test dflet.package-locks ()
  "Either we can rebind LIST, or an error occurs and the binding is not
modified."
  (let ((list #'list))
    (handler-case (dflet ((list ()))
                    (is (eql 42 (list))))
      (error ()
        (is (eq #'list list))))))

(defun foo/lock ()
  23)

(def-test dflet.package-locks.order.1 ()
  "Either we can rebind LIST, or an error occurs and both binding are
restored."
  (let ((list #'list)
        (foo/lock #'foo/lock))
    (handler-case (dflet
                      ((foo/lock () 13)
                       (list () 42))
                    (is (eql 42 (list)))
                    (is (eql 13 (foo/lock))))
      (error ()
        (is (eq #'list list))
        (is (eq #'foo/lock foo/lock))))))
