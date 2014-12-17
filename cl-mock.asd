;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-mock
  :description "Mocking library"
  :long-description "Mocking library to test plain and generic functions."
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :license "Simplified BSD License"
  :version "0.0.1"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (#:closer-mop #:alexandria #:arnesi)
  :in-order-to ((asdf:test-op (asdf:load-op #:cl-mock-tests)))
  :perform (asdf:test-op :after (op c)
             (funcall (find-symbol (symbol-name '#:run!) '#:fiveam)
                      (find-symbol (symbol-name '#:cl-mock) '#:cl-mock-tests)))
  :serial T
  :components ((:static-file "README.md")
               (:module "src"
                :components
                ((:file "package")
                 (:file "functions")
                 (:file "mock")
                 (:file "methods")
                 (:file "facade")))))
