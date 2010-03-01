package com.harvey.sound

import javax.sound.midi.MidiChannel
import javax.sound.midi.ShortMessage

sealed abstract class SignalSink {
  def receive(signal: Signal) 
}

case class MidiSink(channel: MidiChannel) extends SignalSink {
  def receive(signal: Signal) {
    signal match {
      case MidiSignal(ShortMessage.NOTE_ON, _ @ s) => channel.noteOn(s, 60)
      case MidiSignal(ShortMessage.NOTE_OFF, _ @ s) => channel.noteOff(s)
      case MidiSignal(ShortMessage.PROGRAM_CHANGE, _ @ s) => channel.programChange(s)
    }
  }
}
