;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-mock-tests-basic
  :description "Tests for CL-MOCK"
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :license "AGPL-3+"
  :version "1.0.0"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (#:cl-mock-basic #:fiveam)
  :serial T
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "suite")
                 (:file "functions")
                 (:file "mock")))))
