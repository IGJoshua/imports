# org.baznex/imports

A collection of utilities around importing Java classes.

This is the unofficial new home of clojure.contrib.import-static,
used for "importing" static Java methods/fields into Clojure programs.

Please note that "importing" static fields is implemented by copying
them to private vars in the namespace -- this blocks inlining.

## Usage

Use the following dependency line in Leiningen:

    [org.baznex/imports "1.4.0"]

(Please ignore the "1.0.0" release, it was simply mis-versioned.)

## Building

Built with Leiningen v1.x.

## Changelog

### v1.3.0
* Original version from Clojure Contrib (hence the version string)
* Produces macros

### v1.4.0
* Added: org.baznex.imports/rename

## License

Copyright:

* [Stuart Sierra](http://stuartsierra.com/), 2008.
* [Tim McCormack](http://www.brainonfire.net/), 2012
* [Tim McIver](http://timmciver.com/), 2012

Licensed under the Eclipse Public License v1.0. See <epl-v10.html>.
