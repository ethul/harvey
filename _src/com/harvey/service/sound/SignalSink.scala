package com.harvey.service.sound

import com.harvey.domain.{StopMessage,SignedMessage,SignalMessage,TransmitMessage}

import scala.actors.Actor
import scala.actors.Actor._

import javax.sound.midi.MidiChannel
import javax.sound.midi.ShortMessage

sealed abstract class SignalSink extends Actor {
  def receive(signal: Signal) 
  def act() {
    loop {
      react {
        case StopMessage => {
          exit
        }
        case SignedMessage(sender, SignalMessage(MidiSignalOnWithDuration(value, amplitude, length))) => {
          receive(MidiSignalOn(value, amplitude))
          retransmit(sender, value, length)
        }
        case SignalMessage(signal) => {
          receive(signal)
        }
      }
    }
  }
  def retransmit(sender: Actor, value: Int, length: Long) {
    val sink = self
    actor {
      Thread sleep length
      receive(MidiSignalOff(value))
      sender ! TransmitMessage
    }
  }
}

case class MidiSink(channel: MidiChannel) extends SignalSink {
  def receive(signal: Signal) {
    signal match {
      case MidiSignalOn(value, amplitude) => channel.noteOn(value, amplitude)
      case MidiSignalOff(value) => channel.noteOff(value)
      case MidiProgramSignal(value) => channel.programChange(value)
      case _ => println("unmatched")
    }
  }
}
