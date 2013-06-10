;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-mock
  :description "Mocking library"
  :description "Mocking (generic) functions."
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :license "Simplified BSD License"
  :depends-on (#:closer-mop)
  :in-order-to ((asdf:test-op (asdf:load-op #:cl-mock-tests)))
  :perform (asdf:test-op :after (op c)
             (funcall (find-symbol (symbol-name '#:run!) '#:fiveam)
                      (find-symbol (symbol-name '#:cl-mock) '#:cl-mock-tests)))
  :serial T
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "functions")
                 (:file "mock")
                 (:file "methods")
                 (:file "facade")))))

(asdf:defsystem #:cl-mock-tests
  :depends-on (#:cl-mock #:fiveam)
  :serial T
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "suite")
                 (:file "functions")
                 (:file "facade")
                 (:file "methods")))))
