;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(defpackage #:cl-mock-tests
  (:use #:cl #:cl-mock #:fiveam)
  (:import-from
   #:cl-mock
   #:call-with-mocks
   #:progm
   #:make-mock-bindings
   #:if-called
   #:when-called
   #:call-previous
   #:register-mock))
