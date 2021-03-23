;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-mock
  :description "Mocking library"
  :long-description "Mocking library to test plain functions (extended version)."
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :license "AGPL-3+"
  :version "1.1.0"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (#:cl-mock-basic #:trivia)
  :in-order-to ((asdf:test-op (asdf:load-op #:cl-mock-tests)))
  :perform (asdf:test-op :after (op c)
             (funcall (find-symbol (symbol-name '#:run!) '#:fiveam)
                      (find-symbol (symbol-name '#:cl-mock) '#:cl-mock-tests)))
  :serial T
  :components ((:module "src"
                :components
                ((:file "facade")))))
