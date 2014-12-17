-*- mode: markdown; coding: utf-8-unix; -*-

CL-MOCK - Mocking (generic) functions.

Copyright (C) 2013-14 Olof-Joachim Frahm

Release under a Simplified BSD license.

Working, but unfinished.

Should be portable thanks to [`CLOSER-MOP`][1].


# INTRODUCTION

This small library provides a way to replace the actual implementation
of either regular or generic functions with mocks.  How to integrate
this facility with a testing library is up to the user; the tests for
the library are written in [`FIVEAM`][2] though, so most examples will
take that into account.

Since it is pretty easy to just roll something like this on your own,
the main purpose is to develop a nice (lispy, declarative) syntax to
keep your tests readable and maintainable.

Some parts may be used independently of the testing facilities,
e.g. dynamic `FLET` and method bindings with `PROGM` may be of general
interest.


# MOCKING CONTEXT

In addition to having macros and functions to install bindings into the
mocking context, the actual context object may be retrieved and passed
around as well.  This might be useful for further analysis or other
helpers.


# GENERIC FUNCTIONS

Since behaviour isn't bound to classes, but to generic functions,
creating new classes on the fly isn't particularly interesting.  If
necessary, additional shortcuts will be added, but until then I don't
see the need for this.  On the contrary, providing a way to temporarily
supersede generic function bindings sounds like a more viable approach,
especially with regards to (custom) method combinations.

Thus, the form `PROGM` is provided to bind a number of methods during
the execution of its body:

    > (progm
    >     '((baz NIL (list)))
    >     '((lambda (list) list))
    >   ...)

For example:

    > (defclass foo () ())
    > (defgeneric baz (foo)
        (:method ((foo foo))
          42))
    > (progm '((baz NIL (list)))
             '((lambda (list) list))
        (values (baz (make-instance 'foo)) (baz '(1 2 3))))
    > => 42
    > => (1 2 3)

This is implemented via [`CLOSER-MOP`][1], so compatiblity with that
library is required.


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
