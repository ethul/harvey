package com.harvey.sound

import javax.sound.midi.ShortMessage

sealed abstract class Signal(value: Int) {
  def value(): Int
}

case class MidiSignal(command: Int, value: Int) extends Signal(value) {
  def this(value: Int) = this(ShortMessage.NOTE_ON, value)
}
