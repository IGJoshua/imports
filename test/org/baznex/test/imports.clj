(ns org.baznex.test.imports
  "Ideally, each test would run inside a temporary namespace, so as not
to pollute this ns. However, that turns out to be more complicated than
we'd like, so for now all the imports happen in the same ns."
  (:use clojure.test
        org.baznex.imports
        org.baznex.test.utils
        [org.timmc.handy :only (with-temp-ns)])
  (:import (org.baznex.test.imports Statics)))

;;;; Test Helpers

(defmacro with-test-ns
  "Run tests in temporary namespace with org.baznex.imports already use'd."
  [[& ns-exprs] & exprs]
  `(with-temp-ns [(use 'org.baznex.imports) ~@ns-exprs]
     ~@exprs))

;;;; Deprecated stuff: import-static

(deftest test-deprecated-import
  (is (number? (with-test-ns [(import-static java.lang.Math PI sqrt)]
                 (sqrt PI)))))

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

;;;; def-proxied

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

(deftest test-basic-def-proxied
  (is (= (with-test-ns [(def-proxied Math E abs)]
           (map abs (range -2 3)))
         [2 1 0 1 2])))

(deftest test-metadata
  (let [[abs E] (with-test-ns [(def-proxied Math E abs)]
                  [#'abs #'E])]
    (is (:private (meta abs)))
    (is (string? (:doc (meta abs))))
    (is (:private (meta E)))
    (is (string? (:doc (meta E))))))

(deftest test-resolve ;; shouldn't need full qualification of imported classes
  (is (= (-> (with-test-ns [(import '(java.awt Color))
                            (def-proxied Color decode)]
               (decode "123456"))
             class
             .getName)
         "java.awt.Color")))

(deftest test-missing
  (is (thrown? Throwable
               (eval `(def-proxied Math flurb narble grok)))))

(deftest multi-arity
  (let [value-of (with-test-ns [(def-proxied String valueOf)] valueOf)]
    (is (= (value-of true) "true"))
    (is (= (first (map value-of [(char-array "hello")] [1] [3])) "ell"))))

(deftest multi-clause-statics
  (let [[r s] (with-test-ns [(def-proxied
                               (String valueOf)
                               (Math copySign cbrt))]
                [(cbrt 8), ((identity valueOf) (copySign 25.0 -3.0))])]
    (is (== r 2))
    (is (= s "-25.0"))))
