(defpackage #:mock
  (:use :cl)
  (:export
   #:mock-labels
   #:call-mocked-function
   #:mock-call-args
   #:mock-call-return-values
   #:mocked-function-called-p
   #:mocked-function-calls))

(in-package #:mock)

(defstruct mock-state
  (calls (make-hash-table :test 'eq)))

(defstruct mock-call
  args return-values)

(defvar *mock-state-chain* nil)

(defun binding-name (binding)
  (etypecase binding
    (list   (car binding))
    (symbol binding)))

(defun make-mock-lambda (binding orig-definition state)
  (let* ((args    (gensym "ARGS"))
         (results (gensym "RESULTS"))
         (name    (binding-name binding))
         (body    (etypecase binding
                    (list   `(apply (lambda ,(second binding) ,@(cddr binding))
                                    ,args))
                    (symbol '(call-mocked-function)))))
    `(lambda (&rest ,args)
       (macrolet ((call-mocked-function (&rest changed-args)
                    (if (null changed-args)
                      `(apply ,',orig-definition ,',args)
                      `(funcall ,',orig-definition ,@changed-args))))
         (let ((,results (multiple-value-list ,body)))
           (push (make-mock-call :args ,args :return-values ,results)
                 (gethash ',name (mock-state-calls ,state)))
           (values-list ,results))))))

(defmacro mock-labels (bindings &body body)
  (let ((temps (loop for b in bindings collect (gensym)))
        (state (gensym "MOCK-STATE")))
    `(let* ((,state             (make-mock-state))
            (*mock-state-chain* (cons ,state *mock-state-chain*))
           ,@temps)
       (unwind-protect
            (progn
              ,@(loop for binding in bindings
                      for temp in temps
                      for name = (binding-name binding)
                      collect `(setf ,temp (fdefinition ',name))
                      collect `(setf (fdefinition ',name)
                                     ,(make-mock-lambda binding temp state)))
              ,@body)
         ,@(loop for binding in bindings
                 for temp in temps
                 for name = (binding-name binding)
                 collect `(setf (fdefinition ',name) ,temp))))))

(defmacro call-mocked-function (&rest args)
  (declare (ignore args))
  (error "~A used outside of ~A definition"
         'call-mocked-function 'mock-labels))

(defun mocked-function-calls (name)
  (loop for s in *mock-state-chain*
        thereis (gethash name (mock-state-calls s))))

(defun mocked-function-called-p (name)
  (not (null (mocked-function-calls name))))


;; (defun foobar (x y)
;;   (+ x y))

;; (defun mock-test-1 ()
;;   (mock-labels ((foobar (x y) (call-mocked-function x (1+ y))))
;;     (values (foobar 2 3)
;;             (foobar 1 1)
;;             (mocked-function-calls 'foobar))))

;; (defun mock-test-2 ()
;;   (mock-labels (foobar)
;;     (values (foobar 2 3)
;;             (foobar 1 1)
;;             (mocked-function-calls 'foobar))))
