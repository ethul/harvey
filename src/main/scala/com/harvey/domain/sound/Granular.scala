/**
 * harvey (c) 2011
 */
package com.harvey.domain.sound

import com.harvey.service.random.{Variable,Generator,Uniform}
import scala.annotation.tailrec

/**
 * @author ethul
 * @version 1.1
 * @since 1.1
 */
object Granular {
  type Samples = Stream[Sample]
  type CloudGrains = List[CloudGrain]
  type CloudPositionBounds = List[Int]
  type WindowedCloudGrains = (Int,Int,CloudGrains)
  case class Grain(duration: Time, wave: Wave)
  case class CloudGrain(position: Int, grain: Grain)

  private[this] val sampleRate = 22050.0
  private[this] val maxTime = 0.5
  private[this] val maxPosition = 5.0 * sampleRate

  object Grains {
    import Generator._

    def forCloud: Variable[Double] => Grain => CloudGrain = {
      random => grain => {
        //val position = (random.variate[Double] * maxPosition).toInt
        implicitly[Generator[Uniform]].generate match {
          case Uniform(x) => {
            val position = (x * maxPosition).toInt
            CloudGrain(position = position, grain = grain)
          }
        }
      }
    }

    def create: Variable[Double] => Wave => Grain = {
      random => wave => {
        //val time = random.variate[Double] * maxTime * sampleRate
        implicitly[Generator[Uniform]].generate match {
          case Uniform(x) => {
            val time = x * maxTime * sampleRate
            Grain(duration = time, wave = wave)
          }
        }
      }
    }

    def repeater: Variable[Double] => Int => (() => CloudGrain) => CloudGrains = {
      random => repeats => f => {
        @tailrec def loop(xs: CloudGrains, n: Int, f: () => CloudGrain): CloudGrains = {
          n match {
           case 0 => xs
           case _ => loop(f() :: xs,n-1,f)
          }
        }
        //loop(Nil,(repeats * random.variate[Double]).toInt,f)
        implicitly[Generator[Uniform]].generate match {
          case Uniform(x) => loop(Nil,(x * repeats).toInt,f)
        }
      }
    }
  }

  object Clouds {
    import Util._

    def form: CloudGrains => Samples = {
      grains => {
        boundsFromWindows(grains map positionBounds).
          map(windowSamples compose windowGrains(grains)).toStream.flatten
      }
    }

    private[this] object Util {
      def boundsFromWindows: List[List[Int]] => Iterator[List[Int]] = {
        bounds => bounds.flatten.sortWith(_ < _).sliding(2)
      }

      def positionBounds: CloudGrain => CloudPositionBounds = {
        grain => grain match {
          case CloudGrain(position, Grain(duration,_)) => 
            List(position, position + duration.toInt)
        }
      }

      def windowGrains: CloudGrains => Seq[Int] => WindowedCloudGrains = {
        grains => window => window match {
          case List(start,end) => {
            val filtered = grains.filter {
              case CloudGrain(position,_) => {
                position >= start && position < end
              }
            }
            (start,end,filtered)
          }
        }
      }

      def windowSamples: WindowedCloudGrains => Samples = {
        windowedGrains => windowedGrains match {
          case (start,end,Nil) => Stream.continually(0.0).take(end-start)
          case (start,end,windowGrains) => {
            val wave = windowGrains.foldLeft(Wave zero)((wave,grains) => wave + grains.grain.wave)
            val envelope = Identity.envelope((end-start).toDouble)
            Sampler(sampleRate).sample(wave).map(envelope).take(end-start)
          }
        }
      }
    }
  }
}
