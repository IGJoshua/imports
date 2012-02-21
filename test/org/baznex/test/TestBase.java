package org.baznex.test;

/**
 * Assist testing by allowing Java methods a backchannel with which to
 * communicate to a caller (without using their return value.)
 *
 * Assumes single-threaded testing.
 */
public abstract class TestBase {

    private static String response;

    /** Retrieve the backchannel message. */
    public static String getResponse() {
        return response;
    }

    /** Set the backchannel message. */
    protected static void got(String which) {
        response = which;
    }
}
