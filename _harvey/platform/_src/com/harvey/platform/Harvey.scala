package com.harvey.platform

import com.harvey.platform.util._
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
    val info = MidiSystem.getMidiDeviceInfo
    println(info.foreach(println(_)))
    
    val synthesizer = MidiSystem.getSynthesizer
    synthesizer.open
    synthesizer.getAvailableInstruments.foreach(println(_))
    
    val channels = synthesizer.getChannels
//    channels(10).programChange(115)
//    
//    //x..x..x...x.x...
//    for (val i <- 0 to 10) {
//      channels(10).noteOn(70, 60)
//      Thread.sleep(600)
//      channels(10).noteOff(70)
//    
//      channels(10).noteOn(70, 60)
//      Thread.sleep(600)
//      channels(10).noteOff(70)
//    
//      channels(10).noteOn(70, 60)
//      Thread.sleep(800)
//      channels(10).noteOff(70)
//    
//      channels(10).noteOn(70, 60)
//      Thread.sleep(400)
//      channels(10).noteOff(70)
//    
//      channels(10).noteOn(70, 60)
//      Thread.sleep(800)
//      channels(10).noteOff(70)
//    }
//    
//    println("ok")
//    return
      
    
    
    channels(1).programChange(1)
    channels(2).programChange(35)
    channels(10).programChange(115)
    
    val startMillis = System.currentTimeMillis
    val endMillis = startMillis + 1L * 60L * 1000L
    
    val f = new Function0[Unit] {
      def apply() {
        val r: RandomNumberAccessor = new UniformRandomNumberAccessor
        val s: RandomNumberAccessor = new UniformRandomNumberAccessor
        val tones = List(-12,-7,-5,-4,-1,0,1,4,7,12)
        val durations = List(62,125,187,250,375,2000)
        var start = 60
        while (System.currentTimeMillis < endMillis) {
          if (start >= 100 || start.abs <= 20) {
            channels(1).noteOn(100, 60)
            Thread.sleep(durations(s.randInt(durations.length)))
            channels(1).noteOff(100)
            start = 60
          }
          else {
            channels(1).noteOn(start.abs, 60)
            Thread.sleep(durations(s.randInt(durations.length)))
            channels(1).noteOff(start.abs)
          }
      
          start = start + tones(r.randInt(tones.length))
          println("melody["+start+"]")
        }
      }
    }
    
    val g = new Function0[Unit] {
      def apply() {
        while (System.currentTimeMillis < endMillis) {
          channels(10).noteOn(70, 60)
          Thread.sleep(4000)
          channels(10).noteOff(70)
        }
      }
    }
    
    val h = new Function0[Unit] {
      def apply() {
        val r: RandomNumberAccessor = new UniformRandomNumberAccessor
        val s: RandomNumberAccessor = new UniformRandomNumberAccessor
        val tones = List(-12,-2,0,2,12)
        val durations = List(500,2000)
        var start = 30
        while (System.currentTimeMillis < endMillis) {
          if (start >= 50 || start.abs < 10) {
            channels(2).noteOn(30, 60)
            Thread.sleep(durations(s.randInt(durations.length)))
            channels(2).noteOff(30)
            start = 30
          }
          else {
            channels(2).noteOn(start.abs, 60)
            Thread.sleep(durations(s.randInt(durations.length)))
            channels(2).noteOff(start.abs)
          }
      
          start = start + tones(r.randInt(tones.length))
          println("bass["+start+"]")
        }
      }
    }
    
    val startSignal = new CountDownLatch(1)
    val doneSignal = new CountDownLatch(3)
    
    new Thread(new Worker(startSignal, doneSignal, f)).start
    new Thread(new Worker(startSignal, doneSignal, g)).start
    new Thread(new Worker(startSignal, doneSignal, h)).start
    
    startSignal.countDown
    doneSignal.await
    
    synthesizer.close
    
    println("test")
  }
}

class Worker(start: CountDownLatch, done: CountDownLatch, f: Function0[Unit]) extends Runnable {
  override def run {
    start.await
    f()
    done.countDown
  }
}
