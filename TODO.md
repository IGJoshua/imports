# To do

* Allow unrolled varargs in #'proxied? Could be confusing.
* import+ -- an advanced import statement
** class renaming
** statics folded in?
** package unrolling
** (import+ [java.awt Color [geom :rename {Point2D$Double P2D}]])
   gives
   Color -> java.awt.Color, P2D -> java.awt.geom.Point2D$Double
** What would static import syntax look like? [Math :rename {PI pi}] inside
   a package clause is a no-go, since Java packages and classes can share
   names -- we could sniff it out, but there's no guarantee of unambiguity.
   Maybe we don't care about people who use libs that do shit like that?
