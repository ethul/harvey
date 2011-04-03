/**
 * harvey (c) 2011
 */
package com.harvey.service.random

sealed trait Formatter[A] {
  def format(x: Double): A
}

object Formatter {
  implicit object DoubleFormatter extends Formatter[Double] {
    def format(x: Double) = x
  }
  implicit object StringFormatter extends Formatter[String] {
    def format(x: Double) = x+"s"
  }
  implicit object BooleanFormatter extends Formatter[Boolean] {
    def format(x: Double) = if (x >= 0.5) true else false
  }
  implicit object IntFormatter extends Formatter[Int] {
    def format(x: Double) = 0
  }
}
