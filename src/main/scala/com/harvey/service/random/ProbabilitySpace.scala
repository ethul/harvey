/**
 * harvey (c) 2011
 */
package com.harvey
package service
package random

trait ProbabilitySpace[A] {
  def sampleSpace: List[A]
  def measure: A => Probability
}

object ProbabilitySpace {
  def apply[A](s: List[A], m: A => Probability) = {
    new ProbabilitySpace[A] {
      def sampleSpace = s
      def measure = m
    }
  }
}
