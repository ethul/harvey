/**
 * harvey (c) 2011
 */
package com.harvey
package service
package random

trait Seed[A] {
  def seed: A
}

object Seed {
  implicit object StringTime extends Seed[String] {
    private[this] val time = System.nanoTime.toString
    def seed = time.drop(time.length - 12)
  }
}
