/**
 * harvey (c) 2011
 */
package com.harvey.domain.sound

/**
 * @author ethul
 * @version 1.1
 * @since 1.1
 */
case class Sampler(rate: Hertz) {
  def sample: Wave => Stream[Sample] = w => Stream.from(0) map (w sample time(_))
  private[this] def time: Int => Time = x => 1.0 / rate * x.toDouble
}
