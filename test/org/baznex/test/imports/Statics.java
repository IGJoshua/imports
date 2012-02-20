package org.baznex.test.imports;

public class Statics extends org.baznex.test.TestBase {

    /* A whole bunch of overloads to test collapsing of invocations. */

    public static String over() { got("O"); return null; }
    public static String over(int a) { got("O/int"); return null; }
    // Void is Object
    public static void over(int a, char b) { got("O/int/char"); }

    /* Primitive-capable invocations */

    public static void over(long a) { got("O/long"); }
    // must rely on reflection for this pair
    public static Long over(long a, String b) {
        got("O/long/String");
        return 5L;
    }
    public static Long over(long a, Number b) {
        got("O/long/Number");
        return 5L;
    }
    // longest possible prim invocation
    public static long over(long a, double b, Math c, long d) {
        got("LLDOL");
        return 7L;
    }
    // too long
    public static long over(long a, double b, Math c, long d, long e) {
        got("OOOOOO");
        return 7L;
    }

    /* JVM allows fields and methods with same name, even though it is a
       terrible idea. */

    public static String horrible = "OH GOD WHY";

    public static String horrible() { return "WTF, seriously?"; }
}
