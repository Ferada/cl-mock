;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-user; -*-

(in-package #:cl-user)

(defpackage #:cl-mock
  (:use #:closer-common-lisp #:alexandria)
  (:import-from #:arnesi #:with-collector)
  (:export ;; regular functions
           #:progf
           #:dflet

           ;; mocking of regular functions
           ;; mocking of generic functions
           ))
