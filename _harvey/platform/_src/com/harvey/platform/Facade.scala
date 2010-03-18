package com.harvey.platform

import com.harvey.platform.util._
import com.harvey.sound.{MidiSource,MidiSink,MidiProgramSignal,SoundDevice}

import java.util.concurrent.CountDownLatch
import javax.sound.midi._

class Facade {
  def startup() {
    val device = SoundDevice()
    val oracleUniform: Oracle = new UniformRandomNumberGenerator with Oracle {
      type T = Double
      def ask(): T = rand()
    }
    
    val j = (9 * oracleUniform.ask.asInstanceOf[Double]).floor.toInt
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
    val sinks = device.sinks.slice(0,j)
    val sources = for (i <- 0 to sinks.length-1) yield MidiSource(sinks(i), oracles(i))
    sources.foreach{ s => 
      val i = (127.0 * oracleUniform.ask.asInstanceOf[Double]).floor.toInt
      println("instrument:" + i)
      s transmit(MidiProgramSignal(i))
    }

    val startLatch = new CountDownLatch(1)
    val finishLatch = new CountDownLatch(sources.length)
    val expiration = System.currentTimeMillis + 1L * 60L * 1000L
    
    sources.foreach { s =>
      println(s)
      new Thread(new Worker(startLatch, finishLatch, new ExpirableTask(() => s transmit, expiration))).start
    }
    
    startLatch.countDown
    finishLatch.await
    device.release 
  }
}
