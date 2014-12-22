;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock; -*-

(in-package #:cl-mock)

;;; syntactic sugar for defining the mock interactions

(defun true (&rest arguments)
  (declare (ignore arguments))
  T)

(defmacro answer (call &body forms)
  (let ((name (if (listp call) (car call) call))
        (sym (gensym)))
    `(if-called
      ',name
      (let ((,sym (fdefinition ',name)))
        (declare (ignorable ,sym))
        (let ((times 0))
          (lambda (&rest args)
            (declare (ignorable args))
            ,(let ((cases
                     `(case (prog1 times (incf times))
                        ,.(loop
                            for i from 0
                            for (form . rest) on forms
                            collect `(,(if rest i T) ,form)))))
               (if (listp call)
                   `(optima:match args
                     ((list . ,(cdr call)) ,cases)
                     (_ (unhandled)))
                   cases))))))))
