;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(defpackage #:cl-mock
  (:use #:closer-common-lisp #:alexandria)
  (:export
   ;; regular functions
   #:maybe-fdefinition
   #:set-fdefinition
   #:set-or-unbind-fdefinition
   #:call-with-function-bindings

   #:progf
   #:dflet

   ;; mocking of regular functions
   #:*previous*
   #:*arguments*
   #:call-previous
   #:register-mock
   #:invocations
   #:if-called
   #:unhandled
   #:answer
   #:call-with-mocks
   #:with-mocks

   ;; mocking of generic functions
   ))
