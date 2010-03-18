package com.harvey.sound

import javax.sound.midi.ShortMessage

sealed abstract class Signal(value: Int) {
  def value(): Int
}

case class MidiSignalOn(value: Int, velocity: Int) extends Signal(value)
case class MidiSignalOff(value: Int) extends Signal(value)
case class MidiProgramSignal(value: Int) extends Signal(value)
