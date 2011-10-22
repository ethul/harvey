/**
 * harvey (c) 2010
 */
package com.harvey

import domain.Harvey
import control.Iteration._
import service.system.Device
import service.system.Sample
import service.random.Uniform
import service.config.Configuration
import scalaz._, Scalaz._
import effects._

/**
 * this is the effect-side of harvey
 * @author ethul
 * @version 1.1
 * @since 1.0
 */
object Startup {
  private[this] val version = 1.1

  def main(args: Array[String]) {

    import service.random.Discrete
    import service.random.{Pmf,Probability,ProbabilitySpace}
    import service.random.VariateMethod
  
    def pmf: Pmf[Double] = {
      x => x match {
        case 261.63 => 0.3
        case 392.0 => 0.3
        case _ => 0.08
      }
    }

    implicit val space = ProbabilitySpace(List(261.63,293.66,311.13,349.23,392.0,415.30,466.16),pmf)
    implicit val audio = Device.Audio(22050.0f)
    implicit val wavefile = Device.Wavefile("harvey-1.1.wav",22050.0f)

    val r =
      for {
        _ <- putStrLn("harvey " + version)
        _ <- putOut("composing... ")
        a <- Device.open(audio)
        x <- Device.open(wavefile)
        b <- IO(w => (w, Configuration))
        c <- IO(w => (w, Discrete(space)))
        d <- IO(w => (w, Harvey.generate(b)(c)))
        e <- Enumerators.ephemeralNonrecursiveIO(d)(22050) {
               Iteratees.ephemeralIO { 
                (s: EphemeralStream[Sample]) => 
                  Device.writeStream(a)(s) >>=| Device.writeStream(x)(s)
               }
             }
        _ <- Device.close(a)
        _ <- Device.close(x)
        _ <- putStrLn("done")
      } yield e

    r.unsafePerformIO

    // create the effect free harvey and pass this function
    // the configuration and random variable. the output of
    // harvey is a stream of samples

    // map the stream of samples to something suitable for
    // the audio system. so we pipe the output of harvey
    // as the input to the audio system. and music results
  }
}
