package com.harvey.domain.util

import scala.math.{sqrt,log}

abstract class RandomNumberGenerator {
  def rand(): Double 
}

class UniformRandomNumberGenerator extends RandomNumberGenerator {
  private val lock = new Object
  private val nanoTime = System.nanoTime.toString
  private val nanoLength = nanoTime.length
  private val factor = 1.0 / 2147483563.0
  private var s1 = nanoTime.substring(nanoLength-6, nanoLength).toLong
  private var s2 = nanoTime.substring(nanoLength-12, nanoLength-6).toLong
  
  /**
   * this algorithm is from
   * http://cg.scs.carleton.ca/~luc/lecuyer.c
   */
  def rand(): Double = lock.synchronized {
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

class NormalRandomNumberGenerator(m: Double, s: Double) extends RandomNumberGenerator {
  private var random = new UniformRandomNumberGenerator
  private var normal_x2_valid = false
  private var normal_x2 = 0.0
  
  def rand(): Double = {
    var normal_x1 = 0.0
    var w = 0.0 
    
    if (normal_x2_valid) {
      normal_x2_valid = false
      normal_x2 * s + m
    }    
    else {
      do {
        normal_x1 = 2.0 * random.rand - 1.0
        normal_x2 = 2.0 * random.rand - 1.0
        w = normal_x1 * normal_x1 + normal_x2 * normal_x2
      }
      while (w >= 1.0 || w < 1e-30)
    
      w = sqrt(log(w)*(-2.0/w))
      normal_x1 *= w
      normal_x2 *= w
      normal_x2_valid = true
    
      normal_x1 * s + m
    }
  }
}