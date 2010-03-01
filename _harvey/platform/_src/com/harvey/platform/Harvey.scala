package com.harvey.platform

import com.harvey.platform.util._
import com.harvey.sound.{MidiSource,MidiSink,MidiSignal}
import com.harvey.sound.SoundDevice

import java.io.File
import java.util.concurrent._
import javax.sound.midi._

/**
 * this object is the entry point
 * 
 * @author ethul
 * @date 2010.02.22
 * @email eric.thul AT gmail.com
 */
object Harvey {
  def main(args: Array[String]) {
    val device = SoundDevice()
    val sinks = device.sinks
    val sourceA = MidiSource(sinks(0).asInstanceOf[MidiSink])
    val sourceB = MidiSource(sinks(1).asInstanceOf[MidiSink])
    val sourceC = MidiSource(sinks(2).asInstanceOf[MidiSink])
    val sourceD = MidiSource(sinks(3).asInstanceOf[MidiSink])

    sourceA.transmit(MidiSignal(ShortMessage.PROGRAM_CHANGE, 1))
    sourceD.transmit(MidiSignal(ShortMessage.PROGRAM_CHANGE, 1))
    sourceB.transmit(MidiSignal(ShortMessage.PROGRAM_CHANGE, 115))
    sourceC.transmit(MidiSignal(ShortMessage.PROGRAM_CHANGE, 35))
    
    val startMillis = System.currentTimeMillis
    val endMillis = startMillis + 1L * 60L * 1000L
    
    val e = new Function0[Unit] {
      def apply() {
        val r: RandomNumberAccessor = new UniformRandomNumberAccessor
        val s: RandomNumberAccessor = new UniformRandomNumberAccessor
        val tones = List(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12)
        //val durations = List(62,125,187,250,375,2000)
        val durations = List(100)
        var start = 80
        while (System.currentTimeMillis < endMillis) {
          if (start >= 100 || start.abs <= 20) {
            start = 80
          }
          
          val d = durations(s.randInt(durations.length))
          sourceD.transmit(new MidiSignal(start.abs), d)
          start = start + tones(r.randInt(tones.length))
          println("melody1["+start+","+d+"]")
        }
      }
    }
    
    val f = new Function0[Unit] {
      def apply() {
        val r: RandomNumberAccessor = new UniformRandomNumberAccessor
        val s: RandomNumberAccessor = new UniformRandomNumberAccessor
        val tones = List(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12)
        //val durations = List(62,125,187,250,375,2000)
        val durations = List(1000,2000)
        var start = 60
        while (System.currentTimeMillis < endMillis) {
          if (start >= 100 || start.abs <= 20) {
            start = 60
          }
          
          val d = durations(s.randInt(durations.length))
          sourceA.transmit(new MidiSignal(start.abs), d)
          start = start + tones(r.randInt(tones.length))
          println("melody2["+start+","+d+"]")
        }
      }
    }
    
    val g = new Function0[Unit] {
      def apply() {
        while (System.currentTimeMillis < endMillis) {
          sourceB.transmit(new MidiSignal(70), 4000)
          println("rhythm[70]")
        }
      }
    }
    
    val h = new Function0[Unit] {
      def apply() {
        val r: RandomNumberAccessor = new UniformRandomNumberAccessor
        val s: RandomNumberAccessor = new UniformRandomNumberAccessor
        val tones = List(-12,-2,0,2,12)
        //val tones = List(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12)
        val durations = List(500,2000)
        var start = 30
        while (System.currentTimeMillis < endMillis) {
          if (start >= 50 || start.abs < 25) {
            start = 30
          }
          
          sourceC.transmit(new MidiSignal(start.abs), durations(s.randInt(durations.length)))
          start = start + tones(r.randInt(tones.length))
          println("bass["+start+"]")
        }
      }
    }
    
    val startSignal = new CountDownLatch(1)
    val doneSignal = new CountDownLatch(4)
    
    new Thread(new Worker(startSignal, doneSignal, e)).start
    new Thread(new Worker(startSignal, doneSignal, f)).start
    new Thread(new Worker(startSignal, doneSignal, g)).start
    new Thread(new Worker(startSignal, doneSignal, h)).start
    
    startSignal.countDown
    doneSignal.await
    device.release
  }
}

class Worker(start: CountDownLatch, done: CountDownLatch, f: Function0[Unit]) extends Runnable {
  override def run {
    start.await
    f()
    done.countDown
  }
}
