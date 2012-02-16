;; Copyright (c) Stuart Sierra, 2008. (See README file for full list.)
;; of contributors. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns org.baznex.imports
  "Import static Java methods/fields into Clojure"
  (:use clojure.set)
  (:import (java.lang.reflect Method Field Modifier)))

;;;; Old, deprecated stuff

(defmacro import-static
  "Imports the named static fields and/or static methods of the class
  as (private) symbols in the current namespace.

  Example: 
      user=> (import-static java.lang.Math PI sqrt)
      nil
      user=> PI
      3.141592653589793
      user=> (sqrt 16)
      4.0

  Note: The class name must be fully qualified, even if it has already
  been imported.  Static methods are defined as MACROS, not
  first-class fns."
  [class & fields-and-methods]
  (let [only (set (map str fields-and-methods))
        the-class (. Class forName (str class))
        static? (fn [x]
                  (. java.lang.reflect.Modifier
                     (isStatic (. x (getModifiers)))))
        statics (fn [array]
                  (set (map (memfn getName)
                            (filter static? array))))
        all-fields (statics (. the-class (getFields)))
        all-methods (statics (. the-class (getMethods)))
        fields-to-do (intersection all-fields only)
        methods-to-do (intersection all-methods only)
        make-sym (fn [string]
                   (with-meta (symbol string) {:private true}))
        import-field (fn [name]
                       (list 'def (make-sym name)
                             (list '. class (symbol name))))
        import-method (fn [name]
                        (list 'defmacro (make-sym name)
                              '[& args]
                              (list 'list ''. (list 'quote class)
                                    (list 'apply 'list
                                          (list 'quote (symbol name))
                                          'args))))]
    `(do ~@(map import-field fields-to-do)
         ~@(map import-method methods-to-do))))

;;;; helpers

(def capable-prim-invoke?
  (or (< 1 (:major *clojure-version*))
      (< 2 (:minor *clojure-version*))))

;;;; def-statics

(defn ^:internal priv-sym
  "Produce a private name (with minimal docs) for imported statics."
  [^Class cls, ^String name]
  (vary-meta (symbol name)
             assoc
             :private true
             :doc (str (.getCanonicalName cls) "/" name " via def-statics")))

;; Sample signature:
;; {:prim true,
;;  :ret Object,
;;  :params [Long/TYPE Double/TYPE Object]
;; ?:args [Long/TYPE Double/TYPE String] ; optional
;; }

(def ^:internal invocable-prims #{Long/TYPE Double/TYPE})

(defn ^:internal normalize-param
  "Normalize a parameter's class to Long/TYPE, Double/TYPE, or Object."
  [^Class cls]
  (get invocable-prims cls Object))

(defn ^:internal find-prim-invoke
  "Find a subinterface of IFn that provides a primitive invocation method,
e.g. IFn$LOLD. Return type and params are assumed to be normalized already."
  [ret params]
  (when capable-prim-invoke?
    (when (and (<= (count params) 4)
               (or (contains? invocable-prims ret)
                   (some invocable-prims params)))
      (let [subtype (apply str (map {Object \O, Long/TYPE \L, Double/TYPE \D}
                                    (concat params [ret])))]
        (resolve (symbol (str "clojure.lang.IFn$" subtype)))))))

(defn ^:internal extract-signature
  "Given a method, return the signature as a map of :prim (IFn subinterface
that this invocation can match, or nil), :ret (declared class of return type),
and :params (sequence of classes of parameters for invocation), and
:args (sequence of the declared parameter classes of the static method.)

:args is intended for use when the dispatch is unambiguous but the
actual parameters of the static method are narrower than what invoke or
invokePrim can provide."
  [^Method meth]
  (let [ret-type (.getReturnType meth)
        par-actual (vec (seq (.getParameterTypes meth)))
        par-norm (map normalize-param par-actual)]
    {:prim (find-prim-invoke (normalize-param ret-type) par-norm)
     :ret ret-type
     :params par-norm
     :args par-actual}))

(defn ^:internal collapse-sigs
  "Collapse signatures together that must use the same .invoke or .invokePrim."
  [sigs]
  (for [[invocation sgroup] (group-by #(select-keys % [:prim :params]) sigs)
        :let [sample (first sgroup)]]
    (if (= 1 (count sgroup))
      sample
      (-> invocation
          (assoc :ret (if (:prim invocation) (:ret sample) Object))
          (assoc :args nil)))))

(defn ^:internal invocation
  "Produce a single invocation from a signature."
  [^Class cls, ^String name, {:keys [prim ret params args]}]
  (let [proxargs (repeatedly (count params) (partial gensym 'p_))]
    `(~(with-meta (if prim 'invokePrim 'invoke) {:tag ret})
      [~@(map #(with-meta %1 {:tag %2}) proxargs params)]
        (. ~(symbol (.getName cls))
           ~(symbol name)
           ~@(if (seq args)
               (map #(with-meta %1 {:tag %2}) proxargs args)
               proxargs)))))

(defn ^:internal emit-methods
  "Produce a definition form for a set of static methods with the same name."
  [^Class cls, ^String name, meths]
  (let [sigs (collapse-sigs (map extract-signature meths))
        interfaces (-> (map :prim sigs) distinct set (disj nil))]
    `(def ~(priv-sym cls name)
       (proxy [clojure.lang.AFn ~@(map #(symbol (.getName ^Class %))
                                       interfaces)] []
         ~@(map (partial invocation cls name) sigs)))))

(defn ^:internal emit-field
  [^Class cls, ^Field fld]
  {:pre [(instance? Field fld)]}
  `(def ~(priv-sym cls (.getName fld))
     (. ~(symbol (.getName cls)) ~(symbol (.getName fld)))))

(defmacro ^{:since "1.4.0"} def-statics
  "\"Imports\" the named static fields and/or static methods of the class
  as (private) symbols in the current namespace.

  Example: 
      user=> (def-statics java.lang.Math PI sqrt)
      nil
      user=> PI
      3.141592653589793
      user=> (sqrt 16)
      4.0

  Note: Reflection and primitive boxing will be used with all methods.
  TODO: Only do that for methods whose signatures are not listed in
    clojure.lang.IFn."
  [class-sym & fields-and-methods]
  (if-let [cls (resolve class-sym)]
    (let [only (set (map str fields-and-methods))
          todo? (fn [mem]
                  (and (Modifier/isStatic (.getModifiers mem))
                       (contains? only (.getName mem))))
          fields (filter todo? (.getFields cls))
          methods-by-name (group-by #(.getName ^Method %)
                                    (filter todo? (.getMethods cls)))]
      `(do ~@(map (partial emit-field cls) fields)
           ~@(map #(emit-methods cls (key %) (val %)) methods-by-name)))
    (throw (ClassNotFoundException.
            (str "Could not resolve class " class-sym " for static import.")))))
