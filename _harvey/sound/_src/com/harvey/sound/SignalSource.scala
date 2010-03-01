package com.harvey.sound

import javax.sound.midi.ShortMessage

sealed abstract class SignalSource(sink: SignalSink) {
  def transmit(signal: Signal): Unit
  def transmit(signal: Signal, duration: Long): Unit
}

case class MidiSource(sink: SignalSink) extends SignalSource(sink) {
  def transmit(signal: Signal) {
    sink.receive(signal)
  }
  
  def transmit(signal: Signal, duration: Long) {
    sink.receive(signal)
    Thread.sleep(duration)
    sink.receive(MidiSignal(ShortMessage.NOTE_OFF, signal.value))
  }
}
