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

[1]: http://common-lisp.net/project/closer/closer-mop.html