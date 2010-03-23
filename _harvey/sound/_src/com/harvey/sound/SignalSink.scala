package com.harvey.sound

import javax.sound.midi.MidiChannel
import javax.sound.midi.ShortMessage

sealed abstract class SignalSink {
  def receive(signal: Signal) 
}

case class MidiSink(channel: MidiChannel) extends SignalSink {
  def receive(signal: Signal) {
    signal match {
      case MidiSignalOn(_ @ s, _ @ t) => channel.noteOn(s, t)
      case MidiSignalOff(_ @ s) => channel.noteOff(s)
      case MidiProgramSignal(_ @ s) => channel.programChange(s)
      case _ => println("unmatched")
    }
  }
}
