-*- mode: markdown; coding: utf-8-unix; -*-

CL-MOCK - Mocking functions.

Copyright (C) 2013-14 Olof-Joachim Frahm

Release under a Simplified BSD license.

Working, but unfinished.

Should be portable.


# INTRODUCTION

This small library provides a way to replace the actual implementation
of either regular or generic functions with mocks.  On the one hand how
to integrate this facility with a testing library is up to the user; the
tests for the library are written in [`FIVEAM`][2] though, so most
examples will take that into account.  On the other hand writing
interactions for mocks usually relies on a bit of pattern matching,
therefore the regular `CL-MOCK` package relies on [`OPTIMA`][3] to
provide that facility instead of deferring to the user.  Should this be
a concern a reduced system definition is available as `CL-MOCK-BASIC`,
which excludes the definition of `ANSWER` and the dependency on
[`OPTIMA`][3].

Since it is pretty easy to just roll something like this on your own,
the main purpose is to develop a nice (lispy, declarative) syntax to
keep your tests readable and maintainable.

Some parts may be used independently of the testing facilities,
e.g. dynamic `FLET` may be of general interest.


# MOCKING REGULAR FUNCTIONS

Let's say we have a function `FOO`, then we can replace it for testing
by establishing a new mocking context and then specifying how the new
function should behave (see below in **UTILITIES** for a more primitive
dynamic function rebinding):

    > (declaim (notinline foo bar))
    > (defun foo () 'foo)
    > (defun bar (&rest args)
    >   (declare (ignore args))
    >   'bar)
    > (with-mocks ()
    >   (answer (foo 1) 42)
    >   (answer foo 23)
    >   (values
    >    (eql 42 (foo 1))
    >    (eql 23 (foo 'bar))))
    > => T T

The `ANSWER` macro has pattern matching (see [`OPTIMA`][3]) integrated.
Therefore something like the following will now work as expected:

    > (with-mocks ()
    >   (answer (foo x) (format T "Hello, ~A!" x))
    >   (foo "world"))
    > => "Hello, world!"

If you don't like `ANSWER` as it is, you can still use `IF-CALLED`
directly.  Note however that unless `UNHANDLED` is called, the function
always matches and the return value is directly returned again:

    > (with-mocks ()
    >   (if-called 'foo (lambda (x)
    >                     (unhandled)
    >                     (error "Not executed!")))
    >   (if-called 'foo (lambda (x) (format T "Hello, ~A!" x)))
    >   (foo "world"))
    > => "Hello, world!"

Be especially careful to handle all given arguments, otherwise the
function call will fail and that error is propagated upwards.

`IF-CALLED` also has another option to push a binding to the front of
the list, which (as of now) isn't available via `ANSWER` (and should be
treated as subject to change anyway).

Should you wish to run the previously defined function, use the function
`CALL-PREVIOUS`.  If no arguments are passed it will use the current
arguments from `*ARGUMENTS*`, if any.  Otherwise it will be called with
the passed arguments instead.  For cases where explicitely calling it
with no arguments is necessary, using `(funcall *previous*)` is still
possible as well.

    > (with-mocks ()
    >   (answer foo `(was originally ,(funcall *previous*)))
    >   (answer bar `(was originally ,(call-previous)))
    >   (values
    >    (foo "hello")
    >    (bar "hello")))
    > => (WAS ORIGINALLY FOO) (WAS ORIGINALLY BAR)

The function `INVOCATIONS` may be used to retrieve all recorded
invocations of mocks (so far); the optional argument can be used to
filter for a particular name:

    > (with-mocks ()
    >   (answer foo)
    >   (foo "hello")
    >   (foo "world")
    >   (bar "test")
    >   (invocations 'foo))
    > => ((FOO "hello")
    >     (FOO "world"))

Currently there are no further predicates to check these values, this is
however an area of investigation, so presumably either a macro like
[`FIVEAM`][2]s `IS`, or regular predicates could appear in this place.


# EXAMPLES

The following examples may give a better impression.

Here we test a particular [`ECLASTIC`][4] method, `GET*`.  In order to
replace the HTTP call with a supplied value, we use `ANSWER` with
`HTTP-REQUEST` and return a pre-filled stream.  Afterwards both the
number of `INVOCATIONS` and the actual returned values are checked.

    (use-package '(#:cl-mock #:fiveam #:eclastic #:drakma #:puri))

    (def-test search.empty ()
      (let* ((events (make-instance '<type> :type "document" :index "index"
                                            :host "localhost" :port 9292))
             (text "{\"took\":3,\"timed_out\":false,\"_shards\":{\"total\":5,\
    \"successful\":5,\"failed\":0},\"hits\":{\"total\":123,\"max_score\":1.0,\
    \"hits\":[{\"_index\":\"index\",\"_type\":\"document\",\"_id\":\"12345\",\
    \"_score\":1.0,\"_source\":{\"test\": \"Hello, World!\"}}]}}")
             (stream (make-string-input-stream text)))
        (with-mocks ()
          (answer http-request
            (values stream 200 NIL
                    (parse-uri "http://localhost:9292/index/document/_search")
                    stream NIL "OK"))
          (let ((values (multiple-value-list
                         (get* events (new-search NIL)))))
            (is (eql 1 (length (invocations))))
            (is (eql 1 (length (car values))))
            (is-true (typep (caar values) '<document>))
            (is (equal (cdr values)
                       '(NIL (:hits 123
                              :shards (:total 5 :failed 0 :successful 5)
                              :timed-out NIL :took 3))))))))

Of course, running this should produce no errors:

    > (run! 'search.empty)
    >
    > Running test SEARCH.EMPTY ....
    > Did 4 checks.
    >    Pass: 4 (100%)
    >    Skip: 0 ( 0%)
    >    Fail: 0 ( 0%)
    >
    > => NIL


# UTILITIES

`DFLET` dynamically rebinds functions similar to `FLET`:

    > (defun foo () 42)
    > (defun bar () (foo))
    > (bar)
    > => 42
    > (dflet ((foo () 23))
    >   (bar))
    > => 23
    > (OR) => 42, if FOO was inlined

The caveat is that this might not work on certain optimisation settings,
including inlining.  That trade-off seems acceptable; it would be nice
if a warning could be issued depending on the current optimisation
settings, however that is particularly implementation dependent, so lack
of a warning won't indicate a working environment.

The underlying function `PROGF` may be used as well similarly to the
standard `PROG`:

    > (progf '(foo) (list (lambda () 23))
    >   (bar))
    > => 23
    > (OR) => 42, if FOO was inlined

[1]: http://common-lisp.net/project/closer/closer-mop.html
[2]: http://common-lisp.net/project/fiveam/
[3]: https://github.com/m2ym/optima
[4]: https://github.com/gschjetne/eclastic
