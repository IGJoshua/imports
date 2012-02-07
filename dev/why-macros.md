Why does import-static produce macros? Simply put, there is not other way
to avoid boxing primitives in the general case. Macro expanding
`(let [x (float 4)] (abs x))` results in a call to the float variant of
`Math/abs`, equivalent to `(. Math abs ^float x)`. The huge downside, of
course, is that `abs` cannot be passed around between higher-order
functions: `(map abs (range -4 4))` is impossible with `abs` as a macro.

The Clojure compiler only provides unboxed primitive function invocations
in a [limited set of configurations](https://github.com/clojure/clojure/blob/1.3.x/src/jvm/clojure/lang/IFn.java)
-- every combination of double, long, and Object arguments and return types,
up to arity 4. You can implement `IFn` with fancy new overloads of `invoke`,
but the compiler only looks for the 22 invokes that are in the interface,
and any other special invokes that are indicated by implementation of one
of the subinterfaces (e.g. `IFn$OLDOL`). Your overload will be ignored.

The current state of affairs in 1.3.0, then, is that it is simply not
possible to pass around a function that accepts unboxed floats. (And in
1.2.x, there is no unboxed primitive support at all.)

This situation is further complicated by the presence of Java methods
overloads that differ only on primitive argument types. How would one write
an `abs` function that correctly dispatches to the int, long, float, or
double variant of `Math/abs`? Boxing the float not only damages the
performance the user was trying to achieve via use of floats over doubles,
but also runs afoul of Float vs. Number or Object distinctions. The
implementor might discover they are rewriting the JVM dispatch precedence
rules, an unpleasant and bug-prone business.

Some unsatisfying half-measures *could* be taken:

* Restrict the user to methods that can be dispatched unambiguously (via a
  custom reification of `IFn`). Including Clojure 1.2.x support would mean
  completely losing unboxed primitive support.
* Import each overload with a user-specified name, e.g.
  `(import-static (Math {abs-long (abs long)}))`.
* Dispatch on arity only (there's no `apply` for Java methods) and rely always
  on reflection on the arguments.

But in the general case, there is no way to provide high-performance functions
that proxy Java static methods.

(Thanks to arohner for clarifying these issues.)
