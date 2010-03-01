package com.harvey.platform

import com.harvey.platform.util._
import com.harvey.sound.{MidiSource,MidiSink,MidiSignal,SoundDevice}

import java.util.concurrent.CountDownLatch
import javax.sound.midi._

class Facade {
  def startup() {
    val device = SoundDevice()
    val sinks = device.sinks
    val sourceA = MidiSource(sinks(0))
    val sourceB = MidiSource(sinks(1))
    val sourceC = MidiSource(sinks(2))
    val sourceD = MidiSource(sinks(3))

    sourceA.transmit(MidiSignal(ShortMessage.PROGRAM_CHANGE, 1))
    sourceD.transmit(MidiSignal(ShortMessage.PROGRAM_CHANGE, 1))
    sourceB.transmit(MidiSignal(ShortMessage.PROGRAM_CHANGE, 115))
    sourceC.transmit(MidiSignal(ShortMessage.PROGRAM_CHANGE, 35))
    
    val r: RandomNumberAccessor = new UniformRandomNumberAccessor
    
    val e = new Function0[Unit] {
      val tones = List(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12)
      //val durations = List(62,125,187,250,375,2000)
      val durations = List(100)
      var start = 80
      
      def apply() {
        if (start >= 100 || start.abs <= 20) {
          start = 80
        }
          
        val d = durations(r.randInt(durations.length))
        sourceD.transmit(new MidiSignal(start.abs), d)
        start = start + tones(r.randInt(tones.length))
        println("melody1["+start+","+d+"]")
      }
    }
    
    val f = new Function0[Unit] {
      val tones = List(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12)
      //val durations = List(62,125,187,250,375,2000)
      val durations = List(1000,2000)
      var start = 60
      
      def apply() {
        if (start >= 100 || start.abs <= 20) {
          start = 60
        }
          
        val d = durations(r.randInt(durations.length))
        sourceA.transmit(new MidiSignal(start.abs), d)
        start = start + tones(r.randInt(tones.length))
        println("melody2["+start+","+d+"]")
      }
    }
    
    val g = new Function0[Unit] {
      def apply() {
        sourceB.transmit(new MidiSignal(70), 4000)
        println("rhythm[70]")
      }
    }
    
    val h = new Function0[Unit] {
      val tones = List(-12,-2,0,2,12)
      //val tones = List(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12)
      val durations = List(500,2000)
      var start = 30
      
      def apply() {
        if (start >= 50 || start.abs < 25) {
          start = 30
        }
          
        sourceC.transmit(new MidiSignal(start.abs), durations(r.randInt(durations.length)))
        start = start + tones(r.randInt(tones.length))
        println("bass["+start+"]")
      }
    }
    
    val tasks = e :: f :: g :: h :: Nil
    val startLatch = new CountDownLatch(1)
    val finishLatch = new CountDownLatch(tasks.length)
    val endMillis = System.currentTimeMillis + 1L * 60L * 1000L
    
    tasks.foreach { t =>
      new Thread(new Worker(startLatch, finishLatch, new ExpirableTask(t, endMillis))).start
    }
    
    startLatch.countDown
    finishLatch.await
    device.release 
  }
}
