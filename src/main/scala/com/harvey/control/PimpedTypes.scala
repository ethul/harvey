package com.harvey.control

/**
 * harvey (c) 2010
 */
import scalaz._, Scalaz._

/**
 * @author ethul
 * @version 1.1
 * @since 1.1
 */
object PimpedTypes {
  implicit def pimpEphemeralStream[A](s: EphemeralStream[A]) = new PimpedEphemeralStream(s)
}

class PimpedEphemeralStream[A](s: EphemeralStream[A]) {
  import PimpedTypes._

  def take(n: Int): EphemeralStream[A] =
    if (n <= 0 || s.isEmpty) EphemeralStream.empty
    else EphemeralStream.cons(s.head(), if (n == 1) EphemeralStream.empty else s.tail().take(n-1))

  def drop(n: Int): EphemeralStream[A] = {
    var these = s
    var count = n
    while (!these.isEmpty && count > 0) {
      these = these.tail()
      count -= 1
    }
    these
  }
}
