;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-mock-tests
  :description "Tests for CL-MOCK"
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :license "Simplified BSD License"
  :version "0.0.1"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (#:cl-mock #:fiveam)
  :serial T
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "suite")
                 (:file "functions")
                 (:file "facade")
                 (:file "methods")))))