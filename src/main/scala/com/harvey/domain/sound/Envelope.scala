/**
 * harvey (c) 2010
 */
package com.harvey.domain.sound

/**
 * algebraic data type Envelope = Identity | Gaussian | Entropy 
 *
 * @author ethul
 * @version 1.1
 * @since 1.1
 *
 */
sealed trait Envelope {
  def envelope: Time => Sample => Sample
}

final case object Identity extends Envelope {
  def envelope: Time => Sample => Sample = t => x => x
}

final case class Gaussian() extends Envelope {
  private[this] val a = 1.0 / math.sqrt(2.0 * math.Pi)
  private[this] val b = a * math.pow(math.E, 0.0)
  def envelope: Time => Sample => Sample = 
    t => x => a * math.pow(math.E, -1.0 * (x * x) / 2.0) / b
}

final case class Entropy() extends Envelope {
  private[this] val a = math.log10(2.0)
  def envelope: Time => Sample => Sample = 
    t => x => {
      val hx = -1.0 * x * (math.log10(x) / a) - (1.0 - x) * (math.log10(1.0-x) / a)
      if (hx.isNaN || hx.isInfinite) 0.0 else hx
    }
}
