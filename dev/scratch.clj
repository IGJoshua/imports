
;; weird shit with macro expansion in the expressions?
(defmacro in-temp-ns
  "Run some code in a namespace sandbox. "
  [& exprs]
  `(let [old-ns# (.name *ns*)
         tmp-ns# (gensym 'sandbox)]
     (in-ns tmp-ns#)
     (clojure.core/refer 'clojure.core)
     ;; Pull in whatever namespaces are needed for standard tests
     (use 'clojure.test
          'org.baznex.imports)
     ;; Test-specific imports can go inside the macro callsite's body, of course
     (try
       ~@exprs
       (finally
        (in-ns old-ns#)
        (remove-ns tmp-ns#)))))


;; doesn't hide outside vars -- macro is namespace-resolving?
(defmacro in-temp-ns
  "Run some code in a namespace sandbox. "
  [& exprs]
  '(let [tmp-ns# (gensym 'sandbox)]
     (try
       (binding [*ns* (create-ns tmp-ns#)]
         ;; Pull in whatever namespaces are needed for standard tests
         (use 'clojure.test
              'org.baznex.imports)
         ;; Test-specific imports can go inside the macro callsite's body
         ~@exprs)
       (finally
        (remove-ns tmp-ns#)))))


