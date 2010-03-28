package com.harvey.sound

import scala.actors.Actor
import scala.actors.Actor._

import javax.sound.midi.MidiChannel
import javax.sound.midi.ShortMessage

sealed abstract class SignalSink extends Actor {
  def receive(signal: Signal) 
  def act() {
    loop {
      react {
        case "stop" => {
          exit
        }
        case (MidiSignalOnWithDuration(value, amplitude, length), sender: Actor) => {
          println("got signal from source")
          receive(MidiSignalOn(value, amplitude))
          retransmit(sender, value, length)
        }
        case _ @ message: Signal => {
          receive(message)
        }
      }
    }
  }
  def retransmit(sender: Actor, value: Int, length: Long) {
    println("will send back to source from sink after: " + length)
    val sink = self
    actor {
      Thread sleep length
      receive(MidiSignalOff(value))
      sender ! "transmit"
      println("done, going back to source from sink helper")
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
