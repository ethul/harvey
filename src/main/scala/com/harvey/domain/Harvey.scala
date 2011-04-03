/**
 * harvey (c) 2010
 */
package com.harvey.domain

import sound._, Granular._
import com.harvey.service.random
import com.harvey.service.random.Variable
import com.harvey.service.config.Configuration
import scalaz._, Scalaz._

/**
 * @author ethul
 * @version 1.1
 * @since 1.0
 */
object Harvey {
  private[this] val sampleRate = 22050.0
  private[this] val maxFrequency = sampleRate / 3.0
  private[this] val maxTime = 0.5
  private[this] val maxPosition = 5.0 * sampleRate
  private[this] val maxGrains = 25

  def generate: Configuration => Variable => Stream[Sample] = {
    config => random => {
      import Util._

      val r =
        for {
          a <- makeWave
          b <- Grains.create
          c <- Grains.forCloud
          d <- Grains.repeater
        } yield d(maxGrains)(a map b map c)

      (Clouds.form compose r)(random) take maxPosition.toInt
    }
  }

  private[this] object Util {
    def makeWave: Variable => () => Wave = {
      random => () => Sine(maxFrequency * random.variate[Double])
    }
  }
}
