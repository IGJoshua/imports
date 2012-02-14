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

(deftest test-find-prim-invoke
  (are [ret params interface] (= (find-prim-invoke ret params) interface)
       Long/TYPE [] clojure.lang.IFn$L
       Long/TYPE [Object Long/TYPE] clojure.lang.IFn$OLL
       Double/TYPE [Object Object Object] clojure.lang.IFn$OOOD
       Object [Double/TYPE Object] clojure.lang.IFn$DOO
       Double/TYPE [Long/TYPE Object Double/TYPE] clojure.lang.IFn$LODD
       Object [Double/TYPE Long/TYPE Object Long/TYPE] clojure.lang.IFn$DLOLO))

;; Note that we're testing against some instance methods (for convenience.)

(deftest extraction-general ;; nothing eligible for prim-invoke
  (are [meth sig] (= (extract-signature meth) sig)
       ;; normalization
       (the-method Math 'abs Float/TYPE)
       {:prim nil, :ret Float/TYPE,
        :params [Object], :args [Float/TYPE]}))

(deftest test-normalize-signatures
  (are [sigs norm-sigs] (= (normalize-signatures sigs) norm-sigs)
       [{:prim clojure.lang.IFn$LL :ret Long/TYPE :params [Long/TYPE]
         :args [Long/TYPE]}
        {:prim clojure.lang.IFn$DD :ret Double/TYPE :params [Double/TYPE]
         :args [Double/TYPE]}
        {:prim nil :ret Float/TYPE :params [Object]
         :args [Float/TYPE]}
        {:prim nil :ret Integer/TYPE :params [Object]
         :args [Integer/TYPE]}]
       [{:prim clojure.lang.IFn$LL :ret Long/TYPE :params [Long/TYPE]
         :args [Long/TYPE]}
        {:prim clojure.lang.IFn$DD :ret Double/TYPE :params [Double/TYPE]
         :args [Double/TYPE]}
        {:prim nil :ret Object :params [Object] :args nil}]))

(if capable-prim-invoke?
  (deftest extraction-1.3 ;; 1.3-specific: require .invokePrim
    (are [meth sig] (= (extract-signature meth) sig)
         ;; nullary priminvoke
         (the-method System 'currentTimeMillis)
         {:prim clojure.lang.IFn$L, :ret Long/TYPE,
          :params [], :args []}))
  (deftest extraction-1.2 ;; 1.2-specific: ignore .invokePrim
    (are [meth sig] (= (extract-signature meth) sig)
         ;; nullary priminvoke
         (the-method System 'currentTimeMillis)
         {:prim nil, :ret Long/TYPE,
          :params [], :args []})))

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
