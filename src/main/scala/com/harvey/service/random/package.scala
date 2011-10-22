/**
 * harvey (c) 2011
 */
package com.harvey
package service

/**
 * this package object for the random service
 */
package object random {
  type Probability = Double
  type SampleSpace[A] = Set[A]
  type Pmf[A] = A => Probability
}
