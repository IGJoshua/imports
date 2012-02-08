(ns org.baznex.test.imports
  "Ideally, each test would run inside a temporary namespace, so as not
to pollute this ns. However, that turns out to be more complicated than
we'd like, so for now all the imports happen in the same ns."
  (:use clojure.test
        org.baznex.imports))

;; Due to some limitations of deftest, these imports apparently have to go
;; outside.
(import-static java.lang.Math PI sqrt)

(deftest test-deprecated-import
  (is (number? (sqrt PI))))

;;;; def-statics

(deftest utils
  (let [base ^{:a :b, :c :d} 'hello
        with (assoc-meta base :c 5, :e :f)]
    (is (= base with))
    (is (= (assoc (meta base) :c 5, :e :f) (meta with)))))

(deftest normalization
  (are [in out] (= (normalize-param in) out)
       Object Object
       Long/TYPE Long/TYPE
       Double/TYPE Double/TYPE
       (class (long-array 0)) Object
       Long Object
       Double Object
       (class (float 5)) Object
       Float Object
       String Object))

(defn the-method
  [cls meth-name & types]
  (.getMethod cls (name meth-name) (into-array Class types)))

;; Note that we're testing against some instance methods (for convenience.)

(deftest extraction-general ;; nothing eligible for prim-invoke
  (are [meth sig] (= (extract-signature meth) sig)
       ;; normalization
       (the-method Math 'abs Float/TYPE)
       {:prim nil, :ret Float/TYPE,
        :param-hints [Object], :arg-hints [Float/TYPE]}))

;; TODO: This test totally doesn't work (broken for 1.2)
(if (= (:minor *clojure-version*) 3)
  (deftest extraction-1.3 ;; 1.3-specific: require .invokePrim
    (are [meth sig] (= (extract-signature meth) sig)
         ;; nullary priminvoke
         (the-method System 'currentTimeMillis)
         {:prim clojure.lang.IFn$L, :ret Long/TYPE,
          :param-hints [], :arg-hints []}))
  (deftest extraction-1.2 ;; 1.2-specific: ignore .invokePrim
    (are [meth sig] (= (extract-signature meth) sig)
         ;; nullary priminvoke
         (the-method System 'currentTimeMillis)
         {:prim nil, :ret Object,
          :param-hints [], :arg-hints []})))

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
