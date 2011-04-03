/**
 * harvey (c) 2011
 */
package com.harvey.service.random

import Formatter._

// TODO: make this a Stream[Double] ? then we can take random values from the stream
// algebraic data type, Variable = Uniform(seed1, seed2) | Normal | Poisson | etc.
sealed trait Variable {
  def variate[A: Formatter]: A
}

case class Uniform(seed: Long) extends Variable {
  private[this] val factor = 1.0 / 2147483563.0
  private[this] val seedSized = seed.toString drop (seed.toString.length-12)
  private[this] var s1 = (seedSized take 6) toLong
  private[this] var s2 = (seedSized drop 6) toLong
  
  def variate[A: Formatter] = implicitly[Formatter[A]].format(uniform)

  /**
   * this algorithm is from http://cg.scs.carleton.ca/~luc/lecuyer.c
   */
  private[this] def uniform = {
    var k = s1 / 53668L
    s1 = 40014 * (s1 % 53668) - k * 12211
    if (s1 < 0) { 
      s1 += 2147483563 
    }

    k = s2 / 52774
    s2 = 40692 * (s2 % 52774) - k * 3791;
    if (s2 < 0) {
      s2 += 2147483399
    }

    var z: Double = (s1 - 2147483563) + s2;
    if (z < 1) {
      z += 2147483562
    }

    z * factor;
  }
}
