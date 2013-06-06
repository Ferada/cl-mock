;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem #:cl-mock
  :depends-on (#:closer-mop)
  :serial T
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "functions")))))
