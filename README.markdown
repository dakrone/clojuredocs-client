# cd-client

A tiny client for the http://clojuredocs.org API

## Usage

    (use 'cd-client.core)
    
Examples work with strings for the ns and name, as well as just a
symbol (no quoting needed).  The symbol can be of a special form
(e.g. let, try, catch), macros (e.g. for, areduce), or functions.  Use
'pr-examples' to pretty-print a list of all examples for a method.

    (examples "clojure.core" "map")
    (examples map)
    (pr-examples map) ; pretty-prints the examples
    (pr-examples clojure.string/join)

If you like even shorter names for interactive use:

    (defmacro ex [sym] `(pr-examples ~sym))
    (ex let)

Search for a method using just the name or a namespace and name.

    (search "pmap")
    (search "clojure.core" "map")

Comments works just like examples do, with strings, a symbol, and
pretty-printing.

    (comments "clojure.java.io" "reader")
    (comments read-json)
    (pr-comments read-json) ; pretty-prints the comments

See-also works with either strings for ns/name or a symbol.

    (see-also "clojure.test" "are")
    (see-also map)

Browse to the url for a method in your default browser:

    (browse-to "clojure.core" "map")
    (browse-to map)

## As a dependency:

For Leiningen:

    [org.thnetos/cd-client "0.3.3"]

## Installation

    lein deps
    lein jar

## License

Copyright (C) 2010 Lee Hinman

Distributed under the Eclipse Public License, the same as Clojure.
