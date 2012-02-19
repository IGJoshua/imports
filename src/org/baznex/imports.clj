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
  (:require clojure.string)
  (:use [clojure.set :only (intersection difference)])
  (:import (java.lang.reflect Method Field Modifier)
           (clojure.lang AFn)))

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

;;;; def-statics

(defn tag
  "Provide a suitable value for :tag for use in macros."
  [^Class c]
  (cond
   (nil? c) nil
   (.isArray c) (.getName c)
   :else (symbol (.getName c))))

(defn emit-cast
  "Emit expression with casting or type hinting as appropriate."
  [^Class c, expr]
  (cond
   (nil? c) expr
   (= c Void/TYPE) (throw (IllegalArgumentException. "Cannot cast to void"))
   (.isPrimitive c) (list (tag c) expr)
   :else (with-meta expr {:tag (tag c)})))

(defn ^:internal priv-sym
  "Produce a private name (with minimal docs) for imported statics."
  [^Class cls, ^String name]
  (vary-meta (symbol name)
             assoc
             :private true
             :doc (str (.getCanonicalName cls) "/" name " via def-statics")))

;; Sample signature:
;; {:arity 3
;; ?:args [Long/TYPE Double/TYPE String] ; optional
;; }

(defn ^:internal extract-signature
  "Given a method, return the signature as a map of :arity (integer) and
:args (sequence of the declared parameter classes of the static method.)

:args is intended for use when the dispatch is unambiguous but the actual
parameters of the static method are narrower than what invoke can provide."
  [^Method meth]
  (let [par-actual (vec (seq (.getParameterTypes meth)))]
    {:arity (count par-actual)
     :args par-actual}))

(defn ^:internal collapse-sigs
  "Collapse signatures together that must use the same .invoke."
  [sigs]
  (for [[ary sgroup] (group-by :arity sigs)
        :let [sample (first sgroup)]]
    (if (= 1 (count sgroup))
      sample
      {:arity ary, :args nil})))

(defn ^:internal invocation
  "Produce a single invocation from a signature."
  [^Class cls, ^String name, {:keys [arity args] :as sig}]
  (when (> arity 20)
    (throw (RuntimeException.
            "def-statics does not yet support methods with > 20 params")))
  (let [proxargs (repeatedly arity (partial gensym 'p_))]
    `(~'invoke
      [~@proxargs]
      (. ~(symbol (.getName cls))
         ~(symbol name)
         ~@(if (seq args)
             (map #(emit-cast %2 %1) proxargs args)
             proxargs)))))

(defn ^:internal emit-methods
  "Produce a definition form for a set of static methods with the same name."
  [^Class cls, ^String name, meths]
  (let [sigs (collapse-sigs (map extract-signature meths))]
    `(def ~(priv-sym cls name)
       (proxy [AFn] []
         ~@(for [sig sigs]
             (invocation cls name sig))))))

(defn ^:internal emit-field
  [^Class cls, ^Field fld]
  {:pre [(instance? Field fld)]}
  `(def ~(priv-sym cls (.getName fld))
     (. ~(symbol (.getName cls)) ~(symbol (.getName fld)))))

(defmacro def-statics
  "\"Imports\" the named static fields and/or static methods of the class
  as (private) symbols in the current namespace.

  Example: 
      user=> (def-statics java.lang.Math PI sqrt)
      nil
      user=> PI
      3.141592653589793
      user=> (sqrt 16)
      4.0

  Note: Primitive boxing will be used with all methods. Reflection will only
be used where two overloads share an arity."
  [class-sym & fields-and-methods]
  (if-let [cls (resolve class-sym)]
    (let [only (set (map str fields-and-methods))
          todo? (fn [mem]
                  (and (Modifier/isStatic (.getModifiers mem))
                       (contains? only (.getName mem))))
          fields (filter todo? (.getFields cls))
          methods-by-name (group-by #(.getName ^Method %)
                                    (filter todo? (.getMethods cls)))]
      ;; a little friendly error checking first
      (when-let [missing (seq (difference
                               only
                               (set (keys methods-by-name))
                               (set (map #(.getName ^Field %) fields))))]
        (throw (IllegalArgumentException.
                (format (str "def-statics did not find these fields "
                             "or methods in class %s: %s")
                        (.getName cls)
                        (clojure.string/join ", " missing)))))
      ;; OK, we're good to go
      `(do ~@(map (partial emit-field cls) fields)
           ~@(map #(emit-methods cls (key %) (val %)) methods-by-name)))
    (throw (ClassNotFoundException.
            (str "Could not resolve class " class-sym " for static import.")))))
