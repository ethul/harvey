package com.harvey.platform

import com.harvey.platform.util._
import com.harvey.sound.{MidiSource,MidiSink,MidiProgramSignal,SoundDevice}
import com.harvey.sound.{PatternMidiSource,MidiSignalOnWithDuration,SignalPattern}
import com.harvey.sound.{SignalContext,MidiSignalContext}
import com.harvey.sound.{SignalMetaGeneratePattern}

import scala.collection.mutable.ListBuffer

import java.util.concurrent.CountDownLatch

import javax.sound.midi._

class Facade {
  var started = false
  var device: SoundDevice = null
  val context: SignalContext = MidiSignalContext
  val oracle: Oracle = new UniformRandomNumberGenerator with Oracle {
    type T = Double
    def ask(): T = rand()
  }
  
  def startup() {
    device = SoundDevice()
    
    //val j = (8 * oracle.ask.asInstanceOf[Double]).floor.toInt
    val j = 2
    val meta = SignalMetaGeneratePattern()
    val pattern = meta(oracle, context)
    val sinks = device.sinks.slice(0,j)
    val sources = for (i <- 0 to sinks.length-1) yield {
      PatternMidiSource(pattern, sinks(i), oracle)
    }
    
    val zero = context.zeroLengthSources((context.zeroLengthSources.length * oracle.ask.asInstanceOf[Double]).floor.toInt)
    Logger.log("instrument:" + zero + "\n")
    
    val low = context.lowFrequencySources((context.lowFrequencySources.length * oracle.ask.asInstanceOf[Double]).floor.toInt)
    Logger.log("instrument:" + low + "\n")
    
    var presignals = MidiProgramSignal(zero) :: MidiProgramSignal(low) :: Nil
    
    //val normal = context.normalFrequencySources((context.normalFrequencySources.length * oracle.ask.asInstanceOf[Double]).floor.toInt)
    //Logger.log("instrument:" + normal + "\n")
    //sources(1) transmit(MidiProgramSignal(normal))
    
    //val normal2 = context.normalFrequencySources((context.normalFrequencySources.length * oracle.ask.asInstanceOf[Double]).floor.toInt)
    //Logger.log("instrument:" + normal2 + "\n")
    //sources(3) transmit(MidiProgramSignal(normal2))
    
    
    //sources.foreach{ s => 
    //  val i = (127.0 * oracle.ask.asInstanceOf[Double]).floor.toInt
    //  Logger.log("instrument:" + i + "\n")
    //  s transmit(MidiProgramSignal(i))
    //}

    (new OneTimeRunner).execute(new Runnable() {
      def run() {
        val startLatch = new CountDownLatch(1)
        val finishLatch = new CountDownLatch(sources.length)
    
        sources.foreach { source =>
          Logger.log(source + "\n")
          val worker = new Worker(startLatch, finishLatch, source)
          Manager attach worker
          worker.start
          source.start
          source.sink.start
          source ! presignals.head
          worker ! "ready"
          presignals = presignals.tail
        }
    
        startLatch.countDown
        finishLatch.await
      }
    })
    
    started = true
  }
  
  def shutdown() {
    Manager broadcast "stop"
    Manager.detachAll
    device.release
    started = false
  }
  
  def hasStarted() = started
}
