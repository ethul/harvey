/**
 * harvey (c) 2010
 */
package com.harvey

import domain.Harvey
import service.audio.Device, Device.SystemDevice
import service.random.Uniform
import service.config.Configuration
import scalaz._, Scalaz._
import scalaz.effects._

/**
 * this is the effect-side of harvey
 * @author ethul
 * @version
 * @since 1.0
 */
object Startup {
  private[this] val version = 1.1

  def main(args: Array[String]) {
    val r =
      for {
        _ <- putStrLn("harvey " + version)
        _ <- putOut("composing... ")
        a <- Device.open
        b <- IO(w => (w, Configuration))
        c <- IO(w => (w, Uniform(System.nanoTime)))
        _ <- IO(w => (w, { 
              for (_ <- 0 to 10) {
                val s = 
                  for {
                    _ <- Device.writeStream(a)(Harvey.generate(b)(c))
                  } yield () 
                s.unsafePerformIO
              }
             }))
        _ <- Device.close(a)
        _ <- putStrLn("done")
      } yield ()

    r.unsafePerformIO

    // create the effect free harvey and pass this function
    // the configuration and random variable. the output of
    // harvey is a stream of samples

    // map the stream of samples to something suitable for
    // the audio system. so we pipe the output of harvey
    // as the input to the audio system. and music results

  }
}
