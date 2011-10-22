/**
 * harvey (c) 2011
 */
package com.harvey
package service
package random

sealed trait Generator[A] {
  def generate: A
}

object Generator {
  /**
   * this algorithm is from http://cg.scs.carleton.ca/~luc/lecuyer.c
   */
  implicit object LecuyerUniform extends Generator[Uniform] {
    private[this] val factor = 1.0 / 2147483563.0
    private[this] val seed = implicitly[Seed[String]].seed
    private[this] var s1 = (seed take 6) toLong
    private[this] var s2 = (seed drop 6) toLong

    def generate = {
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

      Uniform(z * factor);
    }
  }
}

case class Uniform(x: Probability)
