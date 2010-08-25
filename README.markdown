# cd-client

A tiny client for the http://clojuredocs.org API

## Usage

    (use 'cd-client.core)
    
    (examples "clojure.core" "map")
    (search "pmap")
    (search "clojure.core" "map")
    (comments "clojure.contrib.json" "read-json")
    (see-also "clojure.test" "are")

## Installation

    lein jar

## License

Copyright (C) 2010 Lee Hinman

Distributed under the Eclipse Public License, the same as Clojure.
