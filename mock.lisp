(in-package #:cl-user)

;;; dynamic rebinding of functions

(defun maybe-fdefinition (name)
  "If NAME is FBOUNDP, return its FDEFINITION, else NIL."
  (and (fboundp name) (fdefinition name)))

(defun set-fdefinition (name value)
  "FUNCALLABLE expansion of (SETF (FDEFINITION NAME) VALUE)."
  (setf (fdefinition name) value))

(defun set-or-unbind-fdefinition (name value)
  "If VALUE is true, set the FDEFINITION of NAME to it, else FMAKUNBOUND
it completely."
  (if value (set-fdefinition name value) (fmakunbound name)))

(defun call-with-function-bindings (functions values function
                                    &optional (previous (mapcar #'maybe-fdefinition functions)))
  "Calls FUNCTION while temporarily binding all FUNCTIONS to VALUES.
See PROGF and PROGV."
  (unwind-protect
       (progn
         (mapc #'set-fdefinition functions values)
         (funcall function))
    (mapc #'set-or-unbind-fdefinition functions previous)))

(defmacro progf (functions values &body body)
  "Like PROGV, but for FUNCTIONS."
  `(call-with-function-bindings ,functions ,values (lambda () ,@body)))

(defmacro dflet ((&rest definitions) &body body)
  "Like FLET, but dynamically sets the FDEFINITIONS during the duration of
the BODY."
  `(progf
       ',(mapcar #'car definitions)
       (list
        ,.(mapcar (lambda (definition)
                    `(lambda ,(cadr definition)
                       ,@(cddr definition)))
                  definitions))
     ,@body))

(def-test dflet.calls-binding ()
  (dflet ((foo () 23))
    (is (eql 23 (foo)))))

(def-test dflet.notinline.works ()
  (declare (notinline foo bar))
  (defun foo () 23)
  (dflet ((foo () 42))
    (is (eql 42 (foo)))))

(def-test dflet.simple-mock ()
  (defun foo (&optional (string "Hello, World!"))
    (1+ (bar string)))
  (defun bar (string)
    (length string))
  (dflet ((bar (string)
            (cond
              ((equalp string "Hello, World!")
               42))))
    (is (eql 43 (foo)))
    (is (eql 43 (foo "HELLO, WORLD!")))))

(def-test dflet.package-locks ()
  "Either we can rebind LIST, or an error occurs and the binding is not
modified."
  (let ((list #'list))
    (handler-case (dflet ((list ()))
                    (is (eql 42 (list))))
      (error ()
        (is (eq #'list list))))))

(def-test dflet.package-locks.order.1 ()
  (defun foo ()
    23)
  (let ((list #'list)
        (foo #'foo))
    (handler-case (dflet
                      ((foo () 13)
                       (list () 42))
                    (is (eql 42 (list)))
                    (is (eql 13 (foo))))
      (error ()
        (is (eq #'list list))
        (is (eq #'foo foo))))))

(def-test dflet.package-locks.order.2 ()
  (defun foo ()
    23)
  (let ((list #'list)
        (foo #'foo))
    (handler-case (dflet
                      ((list () 42)
                       (foo () 13))
                    (is (eql 42 (list)))
                    (is (eql 13 (foo))))
      (error ()
        (is (eq #'list list))
        (is (eq #'foo foo))))))

;;; mocking of regular functions

(defstruct mock-bindings
  mocks)

(defvar *previous*)
(defvar *arguments*)

(defun call-previous (&rest args)
  (apply *previous* (or args *arguments*)))

(defun find-and-invoke-mock (*previous* cases *arguments*)
  (dolist (case cases (values))
    (when (ignore-errors (apply (car case) *arguments*))
      (return (apply (cdr case) *arguments*)))))

(defun call-with-mocks (mock-bindings function &key (recordp T))
  "Calls FUNCTION with the given MOCK-BINDINGS established and returns
its first return value, if any.  If RECORDP is set, all invocations will
be recorded and returned as the second return value, else NIL."
  (let* ((mocks (mock-bindings-mocks mock-bindings))
         (functions (mapcar #'car mocks))
         (previous (mapcar #'maybe-fdefinition functions))
         invocations)
    (call-with-function-bindings
     functions
     (mapcar (lambda (binding previous &aux (name (car binding)) (cases (cdr binding)))
               (lambda (&rest args)
                 (when recordp
                   (push (cons name args) invocations))
                 (find-and-invoke-mock previous cases args)))
             mocks previous)
     (lambda ()
       (values
        (funcall function)
        (nreverse invocations)))
     previous)))

(defun register-mock (mock-bindings name)
  (let ((found (member name (mock-bindings-mocks mock-bindings) :key #'car :test #'eq)))
    (or (car found)
        (let ((binding (list name)))
          (push binding (mock-bindings-mocks mock-bindings))
          binding))))

(defun if-called (mock-bindings name test function &key at-start)
  (let ((binding (register-mock mock-bindings name))
        (case (cons test function)))
    (if at-start
        (push case (cdr binding))
        (setf (cdr binding) (append (cdr binding) (list case))))))

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

(def-test call-with-mocks.empty ()
  (is (eq T (call-with-mocks
             (make-mock-bindings)
             (constantly T)))))

(def-test call-with-mocks.discards-values ()
  (is (equal
       '(1 NIL)
       (multiple-value-list
        (call-with-mocks
         (make-mock-bindings)
         (lambda ()
           (values 1 2 3)))))))

(def-test call-with-mocks.simple ()
  (declare (notinline foo))
  (defun foo ()
    (fail "original function binding ~A was called" 'foo))
  (let ((mock-bindings (make-mock-bindings)))
    (register-mock mock-bindings 'foo)
    (call-with-mocks
     mock-bindings
     (lambda ()
       (foo)
       (pass)))))

(def-test call-with-mocks.default-values ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (register-mock mock-bindings 'foo)
    (call-with-mocks
     mock-bindings
     (lambda ()
       (is (null (multiple-value-list (foo))))))))

(def-test if-called.simple ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (if-called mock-bindings 'foo (constantly T) (constantly 42))
    (call-with-mocks
     mock-bindings
     (lambda ()
       (is (eql 42 (foo)))))))

(def-test call-with-mocks.invocations ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (register-mock mock-bindings 'foo)
    (is (equal
         '(NIL ((foo 1) (foo 2) (foo 3)))
         (multiple-value-list
          (call-with-mocks
           mock-bindings
           (lambda ()
             (foo 1)
             (foo 2)
             (foo 3))))))))

(def-test when-called.simple ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (when-called mock-bindings foo 42)
    (when-called mock-bindings foo 23)
    (call-with-mocks
     mock-bindings
     (lambda ()
       (is (eql 42 (foo)))))))

(def-test when-called.literal ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (when-called mock-bindings (foo 1) 2)
    (when-called mock-bindings (foo 2) 3)
    (when-called mock-bindings foo 42)
    (call-with-mocks
     mock-bindings
     (lambda ()
       (is (eql 2 (foo 1)))
       (is (eql 2 (foo 1)))
       (is (eql 3 (foo 2)))
       (is (eql 3 (foo 2)))
       (is (eql 42 (foo)))
       (is (eql 42 (foo 'foo)))))))

(def-test when-called.times ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (when-called mock-bindings foo 1 2 3)
    (call-with-mocks
     mock-bindings
     (lambda ()
       (is (eql 1 (foo)))
       (is (eql 2 (foo)))
       (is (eql 3 (foo)))
       (is (eql 3 (foo)))))))

(def-test when-called.call-previous ()
  (declare (notinline foo))
  (defun foo () 'foo)
  (let ((mock-bindings (make-mock-bindings)))
    (when-called mock-bindings foo 3 (call-previous))
    (call-with-mocks
     mock-bindings
     (lambda ()
       (is (eql 3 (foo)))
       (is (eq 'foo (foo)))))))

;;; mocking of generic methods and objects

(defun find-methods (methods)
  (mapcar (lambda (method)
            (destructuring-bind (generic-function qualifiers specializers) method
              (cons
               generic-function
               (find-method generic-function qualifiers specializers NIL))))
          methods))

;; TODO: because we use ENSURE-METHOD, each value is a FORM rather than a
;; FUNCTION, so not quite the same as PROGF; judging by the implementation-
;; specific code in CLOSER-MOP, we also can just create method objects
;; ourselves reliably, so either we duplicate the cases or just use SBCL

(defun call-with-method-bindings* (methods values function
                                   &optional (previous (find-methods methods)))
  (mapc (lambda (previous)
          (destructuring-bind (generic-function . method) previous
            (when method
              (remove-method generic-function method))))
        previous)
  (let ((new-methods
          (mapcar (lambda (method previous value)
                    (destructuring-bind (generic-function qualifiers specializers) method
                      (destructuring-bind (generic-function . method) previous
                        (cons
                         generic-function
                         (if method
                             (ensure-method generic-function value
                                            :method-class (class-of method)
                                            :qualifiers (method-qualifiers method)
                                            :lambda-list (method-lambda-list method)
                                            :specializers (method-specializers method))
                             (ensure-method generic-function value
                                            :qualifiers qualifiers
                                            :specializers specializers))))))
                  methods previous values)))
    (unwind-protect (funcall function)
      (mapc (lambda (new-method)
              (destructuring-bind (generic-function . method) new-method
                (remove-method generic-function method)))
            new-methods)
      (mapc (lambda (previous)
              (destructuring-bind (generic-function . method) previous
                (when method
                  (add-method generic-function method))))
            previous))))

(defmacro progm* (methods values &body body)
  `(call-with-method-bindings* ,methods ,values (lambda () ,@body)))

(defun call-with-method-bindings (methods values function
                                  &optional previous)
  (let ((methods
          (mapcar (lambda (method)
                    (destructuring-bind (generic-function qualifiers specializers) method
                      (list
                       (if (functionp generic-function)
                           generic-function
                           (fdefinition generic-function))
                       qualifiers
                       (mapcar (lambda (specializer)
                                 (if (classp specializer)
                                     specializer
                                     (find-class specializer)))
                               specializers))))
                  methods)))
    (call-with-method-bindings* methods values function (or previous (find-methods methods)))))

(defmacro progm (methods values &body body)
  `(call-with-method-bindings ,methods ,values (lambda () ,@body)))

(defclass foo ()
  ())

(defgeneric bar (foo)
  (:method ((foo foo))
    42))

(def-test gf.simple ()
  (progm
      '((bar NIL (list)))
      '((lambda (list) list))
    (is (equal '(1 2 3) (bar '(1 2 3))))
    (signals error (equal T (bar T)))
    (is (equal 42 (bar (make-instance 'foo))))))
