;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-mock-basic
  :description "Mocking library"
  :long-description "Mocking library to test plain functions."
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :license "Simplified BSD License"
  :version "1.0.0"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (#:closer-mop #:alexandria)
  :in-order-to ((asdf:test-op (asdf:load-op #:cl-mock-tests-basic)))
  :perform (asdf:test-op :after (op c)
             (funcall (find-symbol (symbol-name '#:run!) '#:fiveam)
                      (find-symbol (symbol-name '#:cl-mock) '#:cl-mock-tests)))
  :serial T
  :components ((:static-file "README.md")
               (:module "src"
                :components
                ((:file "package")
                 (:file "functions")
                 (:file "mock")))))
