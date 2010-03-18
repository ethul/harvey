package com.harvey.sound

import com.harvey.platform.Oracle
import javax.sound.midi.ShortMessage

sealed abstract class SignalSource(sink: SignalSink, oracle: Oracle) {
  def transmit(): Unit
  def transmit(signal: Signal): Unit
}

case class MidiSource(sink: SignalSink, oracle: Oracle) extends SignalSource(sink, oracle) {
  val intervals = List(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12)
  val duration = 2000.0
  val velocity = 120.0
  val maximum = 100
  val minimum = 30
  var tone = 60 
  
  def transmit() {
    //val d = (duration * oracle.ask.asInstanceOf[Double]).floor.toLong
    val d = (duration * oracle.ask.asInstanceOf[Double]).toLong
    val i = intervals((24 * oracle.ask.asInstanceOf[Double]).floor.toInt) 
    val v = (velocity * oracle.ask.asInstanceOf[Double]).toInt
    
    tone = tone + i
    
    if (tone > maximum) {
      tone = minimum
    }
    else if (tone < minimum) {
      tone = maximum
    }
    
    println("sounding["+tone+","+d+","+v+"]")
    sink.receive(MidiSignalOn(tone, v))
    Thread.sleep(d)
    sink.receive(MidiSignalOff(tone))
  }
  
  def transmit(signal: Signal) {
    sink.receive(signal)
  }
  
  override def toString(): String = {
    "MidiSource {\n" +
    "  sink={" + sink.toString + "}\n" +
    "  oracle={" + oracle.toString + "}\n" +
    "}"
  }
}
