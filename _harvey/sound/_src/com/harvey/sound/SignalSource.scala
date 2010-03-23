package com.harvey.sound

import com.harvey.platform.Oracle
import com.harvey.platform.util.Logger
import javax.sound.midi.ShortMessage

sealed abstract class SignalSource(sink: SignalSink, oracle: Oracle) {
  def transmit(): Unit
  def transmit(signal: Signal): Unit
}

case class MidiSource(sink: SignalSink, oracle: Oracle) extends SignalSource(sink, oracle) {
  val intervals = List(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12)
  // x..x..x...x.x...
  val durations = List(400,600,600,800,800)
  val duration = 2000.0
  val velocity = 120.0
  val maximum = 100
  val minimum = 30
  var tone = 60 
  
  def transmit() {
    //val d = (duration * oracle.ask.asInstanceOf[Double]).toLong
    val d = durations((durations.length * oracle.ask.asInstanceOf[Double]).floor.toInt) 
    val i = intervals((intervals.length * oracle.ask.asInstanceOf[Double]).floor.toInt) 
    val v = (velocity * oracle.ask.asInstanceOf[Double]).toInt
    
    tone = tone + i
    
    if (tone > maximum) {
      tone = minimum
    }
    else if (tone < minimum) {
      tone = maximum
    }
    
    Logger.log("sounding["+tone+","+d+","+v+"]\n")
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

case class PatternMidiSource(pattern: SignalPattern, sink: SignalSink, oracle: Oracle) 
extends SignalSource(sink, oracle) {
  val operations = List(SignalIdentity(), SignalReverse(), SignalNullify(), SignalInverse(), SignalFlipFlop(), SignalHalfs())
  val durations = List(400,600,600,800,800)
  val durationmax = 1000.0
  
  def transmit() {
    val op = operations((operations.length * oracle.ask.asInstanceOf[Double]).floor.toInt) 
    val variation = op(pattern)
    
    Logger.log("variation["+op+"] {\n")
    variation.asList.foreach { signal =>
      //val duration = durations((durations.length * oracle.ask.asInstanceOf[Double]).floor.toInt) 
      //val duration = (durationmax * oracle.ask.asInstanceOf[Double]).toInt
      val s = signal.asInstanceOf[MidiSignalOnWithDuration]
      Logger.log("  sounding["+s+"]\n")
      sink.receive(MidiSignalOn(s.value, s.velocity))
      Thread.sleep(s.duration)
      sink.receive(MidiSignalOff(s.value))
    }
    Logger.log("}\n")
  }
  
  def transmit(signal: Signal) {
    sink.receive(signal)
  }
  
  override def toString(): String = {
    "PatternMidiSource {\n" +
    "  pattern={" + pattern.toString + "}\n" +
    "  sink={" + sink.toString + "}\n" +
    "  oracle={" + oracle.toString + "}\n" +
    "}"
  }
}
