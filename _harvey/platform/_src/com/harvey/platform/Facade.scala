package com.harvey.platform

import com.harvey.platform.util._
import com.harvey.sound.{MidiSink,MidiProgramSignal,SoundDevice,SoundException}
import com.harvey.sound.{MidiSource,SignalSource,MidiSignalOnWithDuration,SignalPattern}
import com.harvey.sound.{SignalContext,MidiSignalContext}
import com.harvey.sound.{SignalCompositeOperation,SignalFlattenAmplitude,SignalEvenlyDistributeDuration,SignalBinaryFission,SignalBinaryFusion,SignalMetaGeneratePattern}
import com.harvey.sound.{SignalIdentity,SignalReverse,SignalInverse,SignalFlipFlop,SignalHalfs,SignalAccentOneThree,SignalModulate,SignalDoubleDuration,SignalHalfDuration}

import scala.collection.mutable.ListBuffer

import java.util.concurrent.CountDownLatch

import javax.sound.midi._

/**
 * this class represents the facade for harvey exposing a simple startup/shutdown interface
 * 
 * @author ethul
 * @date 2010.02.22
 * @email eric.thul AT gmail.com
 */
class Facade {
  private val context: SignalContext = MidiSignalContext
  private var device: SoundDevice = _
  private var mstarted = false
  
  Oracle install {
    new UniformRandomNumberGenerator with Oracle {
      def ask(): Double = rand()
    }
  }
  
  private def started_= (started: Boolean) {
    mstarted = started
  }
  
  /**
   * accessor method to determine if the facade has started
   * 
   * @return true if the facade has already been started, false otherwise
   */
  def started = mstarted
  
  /**
   * handles the necessary steps to start up harvey
   */
  def startup() {
    try {
      device = SoundDevice()
      innerStartup()
      started = true
    }
    catch {
      case e: SoundException => Logger.log(e.getMessage + "\n")
    }
  }
    
  /**
   * handles the necessary steps to shutdown harvey
   */
  def shutdown() {
    if (started) {
      Manager broadcast StopMessage()
      Manager.detachAll
      HistogramAccessor.uninstall
      device.release
      started = false
    }
  }
    
  private def innerStartup()
  {
    val j = 3
    val melody = SignalMetaGeneratePattern()(context)
    val rhythm = SignalAccentOneThree()(melody)
    val sinks = device.sinks.slice(0,j)
    
    val rhythmOperations = List(
      SignalIdentity(), SignalBinaryFission(), SignalBinaryFusion(), SignalIdentity(),
      SignalBinaryFusion(), SignalIdentity(), SignalBinaryFusion(), SignalDoubleDuration(),
      SignalHalfDuration()
//      SignalCompositeOperation(SignalFlattenAmplitude() :: SignalEvenlyDistributeDuration() :: SignalBinaryFission() :: Nil),
//      SignalCompositeOperation(SignalFlattenAmplitude() :: SignalEvenlyDistributeDuration() :: SignalBinaryFusion() :: Nil),
//      SignalCompositeOperation(SignalFlattenAmplitude() :: SignalEvenlyDistributeDuration() :: Nil)
    )
    
    val melodyOperations = List(
      SignalIdentity(), SignalReverse(), SignalInverse(), SignalFlipFlop(), SignalBinaryFusion(), 
      SignalBinaryFission(), SignalModulate(), SignalDoubleDuration(), SignalHalfDuration()
    )
    
    val rhythmHistogram = new ListHistogram(rhythmOperations.length)
    val melodyHistogram1 = new ListHistogram(melodyOperations.length)
    val melodyHistogram2 = new ListHistogram(melodyOperations.length)
    
//    val sourcesA = 
//      for (i <- 0 to j-2) 
//        yield new MidiSource(pattern, sinks(i)) {
//          def operations() = opsA
//          def metas() = throw new UnsupportedOperationException
//        }
    
//    val presignalsA = 
//      for (i <- 0 to j-2)
//        yield MidiProgramSignal {
//          context.zeroLengthSources {
//            (context.zeroLengthSources.length * Oracle.ask).floor.toInt
//          }
//        }
    
    val rhythmSource = new MidiSource(rhythm, sinks(0)) {
      def operations() = rhythmOperations
      def metas() = throw new UnsupportedOperationException  
    }
    
    val melodySource1 = new MidiSource(melody, sinks(1)) {
      def operations() = melodyOperations
      def metas() = throw new UnsupportedOperationException  
    }
    
    val melodySource2 = new MidiSource(melody, sinks(2)) {
      def operations() = melodyOperations
      def metas() = throw new UnsupportedOperationException  
    }
  
    val rhythmPresignal = MidiProgramSignal {
      context.zeroLengthSources {
        (context.zeroLengthSources.length * Oracle.ask).floor.toInt
      }
    }
    
    val melodyPresignal1 = MidiProgramSignal {
      context.normalFrequencySources {
        (context.normalFrequencySources.length * Oracle.ask).floor.toInt
      }
    }
    
    val melodyPresignal2 = MidiProgramSignal {
      context.normalFrequencySources {
        (context.normalFrequencySources.length * Oracle.ask).floor.toInt
      }
    }
    
    HistogramAccessor.install(rhythmSource.hashCode, rhythmHistogram)
    HistogramAccessor.install(melodySource1.hashCode, melodyHistogram1)
    HistogramAccessor.install(melodySource2.hashCode, melodyHistogram2)
    
    val sources = rhythmSource :: melodySource1 :: melodySource2 :: Nil
    val presignalIterator = (rhythmPresignal :: melodyPresignal1 :: melodyPresignal2 :: Nil).iterator
    
    Logger.log("instruments:" + rhythmPresignal + " and " + melodyPresignal1 + " and " + melodyPresignal2 + "\n")
    
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
          source ! SignalMessage(presignalIterator next)
          worker ! ReadyMessage 
        }
    
        startLatch.countDown
        finishLatch.await
      }
    })
  }
}
