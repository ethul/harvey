package com.harvey.platform

import com.harvey.platform.util._
import com.harvey.sound.{MidiSource,MidiSink,MidiProgramSignal,SoundDevice}
import com.harvey.sound.{PatternMidiSource,MidiSignalOnWithDuration,SignalPattern}

import scala.collection.mutable.ListBuffer
import java.util.concurrent.CountDownLatch
import javax.sound.midi._

class Facade {
  val tasks = new ListBuffer[Task]
  var device: SoundDevice = null
  val oracleUniform: Oracle = new UniformRandomNumberGenerator with Oracle {
    type T = Double
    def ask(): T = rand()
  }
  var started = false
  
  def startup() {
    device = SoundDevice()
    val j = (8 * oracleUniform.ask.asInstanceOf[Double]).floor.toInt
    //val j = 3 
    val oracles = for (k <- 0 to j-1) yield {
//      val mean = (24 * oracleUniform.ask.asInstanceOf[Double]).floor.toInt
//      val stdev = (10 * oracleUniform.ask.asInstanceOf[Double]).floor.toInt
//      new NormalRandomNumberGenerator(mean, stdev) with Oracle {
//        type T = Double
//        def ask(): T = {
//          val r = rand()
//          if (r < 0) 0
//          else if (r > 24) 24
//          else r
//        }
//        override def toString(): String = {
//          "mean=" + mean + ",stdev=" + stdev
//        }
//      }
      oracleUniform
    }
    val pattern = SignalPattern(() => {
      val intervals = List(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12)
      val velocity = 120.0
      val maximum = 100
      val minimum = 30
      val durations = List(200,600,1000,200,200,600,200)
      val durationmax = 1000.0
      var tone = 60
      
      def changeTone(change: Int): Int = {
        tone += change
        tone
      }
      
      val a = for {
        i <- 0 to (10 * oracleUniform.ask.asInstanceOf[Double]).toInt
        j = intervals((intervals.length * oracleUniform.ask.asInstanceOf[Double]).floor.toInt)
        v = (velocity * oracleUniform.ask.asInstanceOf[Double]).toInt
        //d = (durationmax * oracleUniform.ask.asInstanceOf[Double]).toInt
        d = durations((durations.length * oracleUniform.ask.asInstanceOf[Double]).floor.toInt)
      } yield MidiSignalOnWithDuration(changeTone(j), v, d)
      a.toList
    })
    val sinks = device.sinks.slice(0,j)
    val sources = for (i <- 0 to sinks.length-1) yield PatternMidiSource(pattern, sinks(i), oracles(i))
    sources.foreach{ s => 
      val i = (127.0 * oracleUniform.ask.asInstanceOf[Double]).floor.toInt
      Logger.log("instrument:" + i + "\n")
      s transmit(MidiProgramSignal(i))
    }

    (new OneTimeRunner).execute(new Runnable() {
      def run() {
        val startLatch = new CountDownLatch(1)
        val finishLatch = new CountDownLatch(sources.length)
        //val expiration = System.currentTimeMillis + 1L * 60L * 1000L
        val expiration = System.currentTimeMillis + 20L * 1000L
    
        sources.foreach { s =>
          Logger.log(s + "\n")
          //val task = new ExpirableTask(() => s transmit, expiration)
          val task = new StoppableTask(() => s transmit)
          tasks += task
          new Thread(new Worker(startLatch, finishLatch, task)).start
        }
    
        startLatch.countDown
        finishLatch.await
      }
    })
    
    started = true
  }
  
  def shutdown() {
    tasks.foreach { t =>
      t.asInstanceOf[StoppableTask].stop
    }
    tasks.clear
    device.release
    started = false
  }
  
  def hasStarted() = started
}
