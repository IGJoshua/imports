(ns org.baznex.test.imports
  (:use clojure.test
        org.baznex.imports))

(import-static java.lang.Math PI sqrt)

(deftest test-import-static
  (is (not (nil? (ns-resolve 'org.baznex.test.imports 'PI))))
  (is (not (nil? (ns-resolve 'org.baznex.test.imports 'sqrt)))))