;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-mock
  :description "Mocking library"
  :description "Mocking (generic) functions."
  :author "Olof-Joachim Frahm <olof@macrolet.net>"
  :license "Simplified BSD License"
  :depends-on (#:closer-mop)
  :serial T
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "functions")))))
