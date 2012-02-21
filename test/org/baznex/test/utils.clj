(ns org.baznex.test.utils
  (:import (org.baznex.test TestBase)))

(defmacro talkback
  "Perform the body expressions and return the TestBase backchannel message."
  [& body]
  `(do ~@body (TestBase/getResponse)))
