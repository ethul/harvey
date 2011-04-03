/**
 * harvey (c) 2010
 */
package com.harvey.domain

import sound._
import com.harvey.service.random
import com.harvey.service.random.Variable
import com.harvey.service.config.Configuration
import scalaz._, Scalaz._
import scala.annotation.tailrec

/**
 * @author ethul
 * @version 1.1
 * @since 1.0
 */
object Harvey {
  private[this] val sampleRate = 22050.0
  private[this] val maxFrequency = 440.0 * 3.0
  private[this] val maxTime = 0.5
  private[this] val maxPosition = 5.0 * sampleRate
  private[this] val maxGrains = 10

  def generate: Configuration => Variable => Stream[Sample] = c => rv => {
    import Util._

    val r =
      for {
        a <- makeWave
        b <- makeGrain
        c <- grainPosition
        d <- repeater
      } yield d(maxGrains)(a map b(sound.Identity) map c)

    (combine compose r)(rv) take maxPosition.toInt
  }

  private[this] object Util {
    type Samples = Stream[Sample]
    type GrainPositions = List[GrainPosition]
    case class Grain(size: Int, samples: Samples)
    case class GrainPosition(start: Int, grain: Grain)

    def makeWave: Variable => () => Wave = {
      random => () => Sine(maxFrequency * random.variate[Double])
    }

    def grainPosition: Variable => Grain => GrainPosition = {
      random => grain => {
        GrainPosition(start = (random.variate[Double] * maxPosition).toInt, grain = grain)
      }
    }

    def makeGrain: Variable => Envelope => Wave => Grain = {
      random => envelope => wave => {
        val time = random.variate[Double] * maxTime * sampleRate
        val sampled = Sampler(sampleRate).sample(wave).map(envelope envelope time)
        Grain(size = time toInt, samples = sampled take time.toInt)
      }
    }

    def repeater: Variable => Int => (() => GrainPosition) => GrainPositions = {
      random => repeats => f => {
        @tailrec def loop(xs: GrainPositions, n: Int, f: () => GrainPosition): GrainPositions = {
          n match {
           case 0 => xs
           case _ => loop(f() :: xs,n-1,f)
          }
        }
        loop(Nil,repeats,f)
      }
    }

    // combine all of the grains which overlap based on the starting
    // position for the length of the grain. in this method we map
    // using the initial value and the index and check if that index
    // falls within the [start,start+length) of any grain. if so
    // we keep that grain and then do a fold over the grains we've
    // kept adding up the sample value
    def combine: GrainPositions => Samples = {
      grainPositions => {
        Stream.continually(0.0).zipWithIndex.map { 
          case (zero,index) => {
            grainPositions.filter { 
              case GrainPosition(start,Grain(size,_)) => {
                index >= start && index < start+size
              }
            }.foldLeft(zero) { (acc,grainPosition) => 
              acc + grainPosition.grain.samples(index-grainPosition.start)
            }
          }
        }
      }
    }
  }
}
