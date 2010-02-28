package com.harvey.platform.util

abstract class RandomNumberAccessor {
  def rand(): Double
  def rand(max: Int): Double = max * rand
  def randInt(): Int = randInt(1)
  def randInt(max: Int) = rand(max).floor.asInstanceOf[Int]
}

class UniformRandomNumberAccessor extends RandomNumberAccessor {
  private val nanoTime = System.nanoTime.toString
  private val nanoLength = nanoTime.length
  private val factor = 1.0 / 2147483563.0
  private var s1 = nanoTime.substring(nanoLength-6, nanoLength).toLong
  private var s2 = nanoTime.substring(nanoLength-12, nanoLength-6).toLong
  
  /**
   * this algorithm is from
   * http://cg.scs.carleton.ca/~luc/lecuyer.c
   */
  def rand(): Double = {
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
