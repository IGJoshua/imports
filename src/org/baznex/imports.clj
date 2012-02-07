;; Copyright (c) Stuart Sierra, 2008. (See README file for full list.)
;; of contributors. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns 
  ^{:author "Stuart Sierra",
    :doc "Import static Java methods/fields into Clojure"}
  org.baznex.imports
  (:use clojure.set)
  (:import (java.lang.reflect Method Field Member Modifier)))

(defn- assoc-meta
  [metable & kvs]
  {:pre [(instance? clojure.lang.IMeta metable)]}
  (with-meta metable (apply assoc (meta metable) kvs)))

(defn- def-sym
  "Produce a private name (with minimal docs) for imported statics."
  [^Class cls, ^String name]
  (assoc-meta (symbol name)
              :private true
              :doc (str (.getCanonicalName cls) "/" name " via import-static")))

(defn emit-methods
  "Produce a definition form for a set of static methods with the same name."
  [^Class cls, ^String name, meths]
  (let [arities (distinct (map #(count (.getParameterTypes ^Method %)) meths))]
    `(def ~(def-sym cls name)
       (proxy [clojure.lang.AFn] []
         ~@(for [ary arities]
             (let [params (repeatedly ary (partial gensym 'p_))]
               `(~'invoke [~@params]
                          (. ~(symbol (.getName cls))
                             ~(symbol name)
                             ~@params))))))))

(defn- emit-field
  [^Class cls, ^Field fld]
  {:pre [(instance? Field fld)]}
  `(def ~(def-sym cls (.getName fld))
     (. ~(symbol (.getName cls)) ~(symbol (.getName fld)))))

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
