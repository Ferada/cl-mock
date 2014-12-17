;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-mock; -*-

(in-package #:cl-mock)

;;; syntactic sugar for defining the mock interactions

(defun make-lambda-pattern (literal-pattern)
  (let (lambda-pattern values)
    (loop
      for (car . cdr) = literal-pattern
      while car
      do (let ((sym (gensym)))
           (setf lambda-pattern (append lambda-pattern (list sym)))
           (push `(,sym . ,(if (or (symbolp car) (listp car)) `(quote ,car) car)) values)
           (pop literal-pattern)))
    (values lambda-pattern values)))

(defun make-test-pattern (values)
  `(and ,.(mapcar (lambda (value)
                    `(equal ,(car value) ,(cdr value)))
                  values)))

(defmacro when-called (mock-bindings call &body forms)
  (let ((name (if (listp call) (car call) call))
        (sym (gensym)))
    `(if-called
      ,mock-bindings
      ',name
      ,(if (listp call)
           (multiple-value-bind (lambda-pattern values)
               (make-lambda-pattern (cdr call))
             `(lambda (&rest args)
                (destructuring-bind ,lambda-pattern args
                  ,(make-test-pattern values))))
           '(constantly T))
      (let ((,sym (fdefinition ',name)))
        (declare (ignorable ,sym))
        ,(if (cdr forms)
             `(let ((times 0))
                (lambda (&rest args)
                  (declare (ignorable args))
                  (case (prog1 times (incf times))
                    ,.(loop
                        for i from 0
                        for (form . rest) on forms
                        collect `(,(if rest i T) ,form)))))
             `(lambda (&rest args)
                (declare (ignorable args))
                ,@forms))))))

(defun invocation-count (name invocations)
  (count name invocations :key #'car :test #'eq))

(defun was-called-p (name invocations)
  (member name invocations :key #'car :test #'eq))
