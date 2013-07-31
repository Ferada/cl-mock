-*- mode: markdown; coding: utf-8-unix; -*-

CL-MOCK - Mocking (generic) functions.

Copyright (C) 2013 Olof-Joachim Frahm

Release under a Simplified BSD license.

Working, but unfinished.

Should be portable thanks to [`CLOSER-MOP`][1].

Since it is pretty easy to just roll something like this on your own,
the main purpose is to develop a nice syntax (lispy, declarative) to
keep tests readable and maintainable.

Some parts may be used independently of the testing facilities,
e.g. dynamic `FLET` and method bindings with `PROGM` may be of general
interest.


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

The caveat is that this might not work on certain optimization settings,
including inlining.

The underlying function `PROGF` may be used as well similarly to standard
`PROG`:

    > (progf '(foo) (list (lambda () 23))
    >   (bar))
    > => 23
    > (OR) => 42, if FOO was inlined


[1]: http://common-lisp.net/project/closer/closer-mop.html
