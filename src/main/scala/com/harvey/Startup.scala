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
 * @version
 * @since 1.0
 */
object Startup {
  private[this] val version = 1.1

  def main(args: Array[String]) {
    implicit val audio = Device.Audio(22050.0f)
    implicit val wavefile = Device.Wavefile("harvey-1.1.wav",22050.0f)

    val r =
      for {
        _ <- putStrLn("harvey " + version)
        _ <- putOut("composing... ")
        a <- Device.open(audio)
        x <- Device.open(wavefile)
        b <- IO(w => (w, Configuration))
        c <- IO(w => (w, Uniform(System.nanoTime)))
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
