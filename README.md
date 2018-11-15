termcolor
=========

Output ANSI terminal colors in Common Lisp. See termcolor.lisp for
available colors (`grep defcolor termcolor.lisp`).

Usage
-----

Load termcolor.lisp:

    (load (compile-file "termcolor"))

Change the foreground color:

    (termcolor:fg :red)

Change the background color:

    (termcolor:bg :cyan)

Change the style:

    (termcolor:style :bright)

Change multiple things in one go:

    (termcolor:color :fg :red :bg :black :style :dim)

Reset things:

    (termcolor:fg :reset)
    (termcolor:bg :reset)
    (termcolor:style :reset)

Reset everything:

    (termcolor:reset)

By default, the escape sequences are written to standard output. To
write to a different stream, use the STREAM keyword arg:

    (termcolor:fg :red :stream s)

To not write to any stream, and just return the escape sequence as a
string, use the PRINT arg:

    (termcolor:fg :red :print nil)
