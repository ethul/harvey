package com.harvey.sound

import com.harvey.platform.{Manager,Oracle,HistogramAccessor}
import com.harvey.platform.{BroadcastMessage,GenerateMessage,SignedMessage,SignalMessage,StopMessage,TransmitMessage}
import com.harvey.platform.util.Logger

import scala.actors.Actor
import scala.actors.Actor._

import javax.sound.midi.ShortMessage

/**
 * this class represents a source of signals
 * 
 * <p>
 * the main interface this class exports is the message-passing interface
 * where handlers are invoked upon the reaction to different messages
 * received. this encourages the use of an actor-based framework for 
 * communicating with a signal source
 * 
 * @author ethul
 * @date 2010.02.28
 * @email eric.thul AT gmail.com
 */
sealed abstract class SignalSource extends Actor {
  /**
   * {@inheritDoc}
   * 
   * <p>
   * this method handle the reactive cases for message sent to this source. messages
   * which are top-level include stop and broadcast. those specific to a source are
   * transmit and generate.
   */
  def act() {
    loop {
      react {
        case StopMessage => {
          sink ! StopMessage
          exit
        }
        case BroadcastMessage(StopMessage()) => {
          sink ! StopMessage
          exit
        }
        case BroadcastMessage(message) => {
          println("got broadcasted: " + message)
        }
        case TransmitMessage => {
          if (regenerate) {
            generate()
          }
          transmit()
        }
        case GenerateMessage => {
          generate()
          transmit()
        }
        case SignalMessage(signal) => {
          sink ! SignalMessage(signal)
        }
      }
    }
  }
  
  /**
   * this method is an accessor for the source's sink
   * 
   * @return the signal sink
   */
  def sink: SignalSink
  
  /**
   * this method is a handler for the generate message
   * 
   * <p>
   * the contract of the generate method is to create a signal pattern
   * in order to be transmitted
   */
  protected[this] def generate(): Unit
  
  /**
   * this method determines whether or not the source should generate pattern
   * 
   * @return true when the pattern should be generate, false otherwise
   */
  protected[this] def regenerate(): Boolean
  
  /**
   * this method is a handler for the transmit message
   * 
   * <p>
   * the intention of this method is to transmit the signal of the
   * pattern which have been created by the generate method
   */
  protected[this] def transmit(): Unit
}

/**
 * this class is a case of a signal source which is implemented in terms of midi signals
 * 
 * @author ethul
 * @date 2010.03.22
 * @email eric.thul AT gmail.com
 */
abstract class MidiSource(pattern: SignalPattern, sink: SignalSink) extends SignalSource {
  private val id = hashCode
  private var variation = pattern.asList
  private var iterator = variation.iterator
  
  def operations(): List[SignalOperation]
  def metas(): List[SignalMetaOperation]
  
  /**
   * {@inheritDoc}
   */
  def sink() = sink
  
  /**
   * {@inheritDoc}
   * 
   * <p>
   * for a midi source the generate method creates a signal pattern of midi signals
   * which includes the value of the signal, the amplitude (velocity), and the 
   * length (duration) of the signal.
   */
  protected[this] def generate() {
    val index = (operations.length * Oracle.ask).floor.toInt
    val op = operations()(index)
    // TODO: ethul, make an operation list class which is observable to do this
    HistogramAccessor(id).increment(index)
    
    try {
      val v = op(SignalPattern(variation))
      variation = v.asList
    }
    catch {
      case e: IllegalArgumentException => {
        Logger.log("could not perform: ["+id+"]["+op+"]["+e.getMessage+"]\n")
      }
    }
    
    iterator = variation.iterator
    
    Logger.log("variation["+id+"]["+op+"]\n")
    Logger.log("variation["+id+"][\n"+variation.mkString("\n")+"]\n")
    Logger.log("histogram["+id+"]["+HistogramAccessor(id)+"]\n")
  }
  
  /**
   * {@inheritDoc}
   */
  protected[this] def regenerate() = {
    iterator == null || !iterator.hasNext
  }
  
  /**
   * {@inheritDoc}
   */
  protected[this] def transmit() {
    // TODO: ethul, why do we get an error "head of empty list"?
    sink ! SignedMessage(self, SignalMessage(iterator.next))
  }
  
  /**
   * {@inheritDoc}
   */
  override def toString(): String = {
    "MidiSource {\n" +
    "  pattern={" + pattern.toString + "}\n" +
    "  sink={" + sink.toString + "}\n" +
    "}"
  }
}
