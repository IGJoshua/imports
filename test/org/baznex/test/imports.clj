(ns org.baznex.test.imports
  "Ideally, each test would run inside a temporary namespace, so as not
to pollute this ns. However, that turns out to be more complicated than
we'd like, so for now all the imports happen in the same ns."
  (:use clojure.test
        org.baznex.imports))

;; Due to some limitations of deftest, these imports apparently have to go
;; outside.
(import-static java.lang.Math PI sqrt)

(deftest test-basic-import
  (is (number? (sqrt PI)))
  (is (== 2 (first (map sqrt [4 5 6])))))

(deftest test-metadata
  (is (:private (meta #'sqrt)))
  (is (string? (:doc (meta #'sqrt))))
  (is (:private (meta #'PI)))
  (is (string? (:doc (meta #'PI)))))

(import '(java.awt Color))
(deftest test-resolve ;; shouldn't need full qualification of imported classes
  (import-static Color decode))
