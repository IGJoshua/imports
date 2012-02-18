(ns org.baznex.test.imports
  "Ideally, each test would run inside a temporary namespace, so as not
to pollute this ns. However, that turns out to be more complicated than
we'd like, so for now all the imports happen in the same ns."
  (:use clojure.test
        org.baznex.test.utils
        org.baznex.imports)
  (:import (org.baznex.test.imports Statics)))

;; Due to some limitations of deftest, these imports apparently have to go
;; outside.
(import-static java.lang.Math PI sqrt)

(deftest test-deprecated-import
  (is (number? (sqrt PI))))

(import-static org.baznex.test.imports.Statics over)

(defmacro over*
  "Call Statics/over with the given args, returning a message from the
invoked method about the signature that was used."
  [& args]
  `(talkback (over ~@args)))

(deftest test-overloads
  (are [call sig] (= call sig)
       (over*) "O"
       (over* (int 5)) "O/int"
       (over* (long 5)) "O/long"))

;;;; def-statics

(defn the-method
  [cls meth-name & types]
  (.getMethod cls (name meth-name) (into-array Class types)))

;; Note that we're testing against some instance methods (for convenience.)

(deftest extraction
  (are [meth sig] (= (extract-signature meth) sig)
       ;; normalization
       (the-method Math 'abs Float/TYPE)
       {:arity 1, :args [Float/TYPE]}
       (the-method System 'currentTimeMillis)
       {:arity 0, :args []}
       (the-method String 'valueOf Double/TYPE)
       {:arity 1, :args [Double/TYPE]}))

(deftest collapsing-signatures
  (are [sigs norm-sigs] (= (set (collapse-sigs sigs)) (set norm-sigs))
       ;; individual
       [{:arity 2, :args [String, Long/TYPE]}
        {:arity 3, :args [String, Double/TYPE, String]}
        ;; nullary
        {:arity 0, :args []}
        ;; colliding
        {:arity 1, :args [Long/TYPE]}
        {:arity 1, :args [Long]}]
       [{:arity 2, :args [String, Long/TYPE]}
        {:arity 3, :args [String, Double/TYPE, String]}
        {:arity 0, :args []}
        ;; only collapse
        {:arity 1, :args nil}]))

(def-statics Math E abs)

(deftest test-def-statics
  (is (= (map abs (range -2 3)) [2 1 0 1 2])))

(deftest test-metadata
  (is (:private (meta #'abs)))
  (is (string? (:doc (meta #'abs))))
  (is (:private (meta #'E)))
  (is (string? (:doc (meta #'E)))))

(import '(java.awt Color))
(deftest test-resolve ;; shouldn't need full qualification of imported classes
  (def-statics Color decode))
