/**
 * harvey (c) 2010
 */
package com.harvey.domain.sound

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

/**
 * @author ethul
 * @version
 * @since
 *
 */
object TestWave extends Properties("Wave") {
  private[this] val rate = 44100.0
  private[this] val minFrequency = 0.0
  private[this] val maxFrequency = rate
/*
  val sampler = new Sampler(rate).sample(1.0) _
  
  implicit def arbSine: Arbitrary[Sine] = Arbitrary(Gen.choose(minFrequency, maxFrequency) map (x => Sine(x)))
  
  property("+") = Prop.forAll {
    (s1: Sine, s2: Sine) => {
      val a = sampler(s1).toList
      val b = sampler(s2).toList
      val c = sampler(s1+s2).toList
      (a zip b).map { z => z match { case (x,y) => x+y }} == c
    }
  }
*/
}
