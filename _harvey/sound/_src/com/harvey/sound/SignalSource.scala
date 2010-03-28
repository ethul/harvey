package com.harvey.sound

import com.harvey.platform.{Manager,Oracle}
import com.harvey.platform.util.Logger

import scala.actors.Actor
import scala.actors.Actor._

import javax.sound.midi.ShortMessage

sealed abstract class SignalSource(sink: SignalSink, oracle: Oracle) extends Actor {
  def act() {
    loop {
      react {
        case "stop" => {
          sink ! "stop"
          exit
        }
        case ("broadcasted", "stop", _) => {
          sink ! "stop"
          exit
        }
        case ("broadcasted", _ @ message, sender: Actor) => {
          println("got broadcasted: " + message)
        }
        case "transmit" => {
          if (regenerate) {
            println("done with variation")
            generate()
            transmit()
          }
          else {
            transmit()
          }
        }
        case "generate" => {
          generate()
          transmit()
        }
        case _ @ signal: Signal => {
          sink ! signal
        }
      }
    }
  }
  def generate(): Unit
  def regenerate(): Boolean
  def transmit(): Unit
}

case class PatternMidiSource(pattern: SignalPattern, sink: SignalSink, oracle: Oracle) 
extends SignalSource(sink, oracle) {
  val id = hashCode
  val operations = List(SignalIdentity(), SignalReverse(), SignalNullify(), SignalInverse(), SignalFlipFlop(), SignalHalfs())
  val metas = List(SignalMetaModifyPattern(pattern), SignalMetaRedistributeDurations(pattern))
  var current = pattern
  var variation = List[Signal]()
  
  def generate() {
    if (oracle.ask.asInstanceOf[Double] > 0.99) {
      val context: SignalContext = MidiSignalContext
      val meta = metas((metas.length * oracle.ask.asInstanceOf[Double]).floor.toInt)
      Logger.log("meta["+id+"]["+meta+"] {\n")
      current = meta(oracle, context)
      Logger.log("current["+id+"]["+current.asList.mkString("{\n  ","\n  ","}\n")+"]")
      Logger.log("}\n")
      Manager broadcast "pattern change to you"
      println("broadcast sent")
    }
    
    val op = operations((operations.length * oracle.ask.asInstanceOf[Double]).floor.toInt) 
    //val v = op(current)
    val v = current
    variation = v.asList
    
    Logger.log("variation["+id+"]["+op+"]\n")
  }
  
  def regenerate() = variation == Nil
  
  def transmit() {
    println("sending to sink")
    sink ! (variation.head.asInstanceOf[MidiSignalOnWithDuration], self)
    variation = variation.tail
  }
  
  override def toString(): String = {
    "PatternMidiSource {\n" +
    "  pattern={" + pattern.toString + "}\n" +
    "  sink={" + sink.toString + "}\n" +
    "  oracle={" + oracle.toString + "}\n" +
    "}"
  }
}
