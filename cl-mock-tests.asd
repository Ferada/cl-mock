;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-mock-tests
  :description "Tests for CL-MOCK (extended version)"
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :license "Simplified BSD License"
  :version "1.0.0"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on (#:cl-mock #:cl-mock-tests-basic)
  :serial T
  :components ((:module "tests"
                :components
                ((:file "facade")))))
