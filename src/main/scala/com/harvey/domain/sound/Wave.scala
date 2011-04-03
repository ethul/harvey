/**
 * harvey (c) 2010
 */
package com.harvey.domain.sound

/**
 * algebraic data type 
 * Wave = Sine(n) | Square(n) | Triangle(n) | Sawtooth(n)
 *
 * @author ethul
 * @version 1.1
 * @since 1.0
 */
sealed trait Wave { a =>
  type Lifted = (Sample,Sample) => Sample

  /**
   * samples the wave at the given time point
   */
  def sample: Time => Sample

  /**
   * binary addition for waves
   */
  def + = lift(_+_)

  /**
   * lifts a binary function to act on waves. note that we name
   * the self type as "a" for this trait to retain that reference
   * when we set up the new wave instance's apply method
   */
  private[this] def lift: Lifted => Wave => Wave = {
    f => b => new Wave {
      def sample: Time => Sample = x => f(a sample x, b sample x)
    }
  }
}

object Wave {
  val zero = new Wave {
    def sample: Time => Sample = _ => 0.0
  }
}

/**
 * represents the equation for a sine wave
 * amplitude * math.sin(2.0 * math.Pi * harmonic * frequency / rate * time)
 * we assume that amplitude and harmonic are 1 and x is (1/rate) * time
 */
final case class Sine(n: Hertz) extends Wave {
  def sample: Time => Sample = x => math.sin(2.0 * math.Pi * n * x)
}
  
// TODO: ethul, define the following

/**
 * represents the equation for a square wave
 */
final case class Square(n: Hertz) extends Wave {
  def sample: Time => Sample = x => x
}

/**
 * represents the equation for a triangle wave
 */
final case class Triangle(n: Hertz) extends Wave {
  def sample: Time => Sample = x => x
}

/**
 * represents the equation for a sawtooth wave
 */
final case class Sawtooth(n: Hertz) extends Wave {
  def sample: Time => Sample = x => x
}
