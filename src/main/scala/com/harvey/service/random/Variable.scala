/**
 * harvey (c) 2011
 */
package com.harvey
package service
package random

import scalaz.EphemeralStream

// algebraic data type, Variable = Uniform(seed1, seed2) | Normal | Poisson | etc.
// TODO: ethul, the algebraic data type should be
//   Variable = Discrete | Continuous
// the variates of a random variable are realized given a type of distribution, like
// uniform, normal, poisson, etc.

/*
trait Variable {
  def variate
}

case class Discrete() extends Variable {
  val generator: Generator = implicitly[Generator[A]].generate
  val discreteSet: Set[B] = implicitly[Set[B]].get
}

perhaps we can make it so that the generator and the set of values to pick can
be instances of a type class. for example, we can have uniform, normal, poisson
generators. and we can have integers, set of values, lists as the discrete set
which can be picked by the result of the generator

the client code can then import the right generator and set of values into the
namespace to be picked up each time that the variate method is invoked on the
randome variable
*/

sealed trait Variable[A] {
  def variate: A
  def variates: EphemeralStream[A] = EphemeralStream.cons(variate, variates)
}

case class Discrete[A: VariateMethod](space: ProbabilitySpace[A]) extends Variable[A] {
  def variate = implicitly[VariateMethod[A]].apply
}
