package com.harvey.sound

import scala.collection.mutable.{Buffer,ListBuffer}

sealed abstract class SignalOperation extends Function1[SignalPattern,SignalPattern]

case class SignalIdentity() extends SignalOperation {
  def apply(pattern: SignalPattern): SignalPattern = pattern 
  override def toString(): String = "identity"
}
  
case class SignalReverse() extends SignalOperation {
  def apply(pattern: SignalPattern): SignalPattern = SignalPattern(pattern.asList.reverse)
  override def toString(): String = "reverse"
}

case class SignalNullify() extends SignalOperation {
  def apply(pattern: SignalPattern): SignalPattern = {
    val head = pattern.asList.head
    val nullified = for (i <- pattern.asList.elements) yield MidiSignalOnWithDuration(head.value,i.asInstanceOf[MidiSignalOnWithDuration].velocity,i.asInstanceOf[MidiSignalOnWithDuration].duration)
    SignalPattern(nullified.toList)
  }
  
  override def toString(): String = "nullify"
}

case class SignalInverse() extends SignalOperation {
  def apply(pattern: SignalPattern): SignalPattern = {
    // inverse the interval, so if you go up 2, make
    // the interval go down 2
    val list = pattern.asList
    val intervals = 
      for (i <- 0 to list.length-2) yield -(list(i+1).value - list(i).value)
    
    var inverses = new ListBuffer[Signal]
    var start = list.head.value
    // TODO: ethul, make this not bound to the type MidiSignalOn?
    for (i <- 0 to list.length-1) {
      val midisignal: MidiSignalOnWithDuration = list(i).asInstanceOf[MidiSignalOnWithDuration]
      inverses += MidiSignalOnWithDuration(start, midisignal.velocity, midisignal.duration)
      if (i < intervals.length) {
        start += intervals(i)
      }
    } 
    
    SignalPattern(inverses.toList)
  }
  
  override def toString(): String = "inverse"
}

/**
 * pick the first note, then last note, then second note
 * then the second to last note, etc.
 */
case class SignalFlipFlop() extends SignalOperation {
  def apply(pattern: SignalPattern): SignalPattern = {
    val list = pattern.asList
    var flipflops = new ListBuffer[Signal]
    val half = 
      if (list.length % 2 == 0) {
        list.length / 2 - 1
      }
      else {
        list.length / 2
      }
    
    for (i <- 0 to half) {
      flipflops += list(i)
      if (2*i != list.length-1) {
        flipflops += list(list.length-1-i)
      }
    }
    
    SignalPattern(flipflops.toList)
  }
  
  override def toString(): String = "flipflop"
}

/**
 * weave through the pattern
 *  0 1 2 3 4 5 6 
 *  0 3 6 5 1 2 4
 */
case class SignalWeave() extends SignalOperation {
  def apply(pattern: SignalPattern): SignalPattern = {
    SignalPattern(Nil)
  }
  
  override def toString(): String = "weave"
}

/**
 * 0 1 2 3 4 5 6
 * 0 3 6 1 4 2 5
 * 7/2 = 3
 * 
 * 0 1 2 3 4 5
 * 0 2 4 1 3 5
 * 6/2 = 3-1 = 2
 * 
 * 64 63 74
 * 0  1  2
 * 1
 */
case class SignalHalfs() extends SignalOperation {
  def apply(pattern: SignalPattern): SignalPattern = {
    val list = pattern.asList
    val halfs = new ListBuffer[Signal]
    val half = 
      if (list.length % 2 == 0) {
        list.length / 2 - 1
      }
      else {
        list.length / 2
      }
    
    for (i <- 0 to half-1) {
      for (j <- 0 to half) {
        if (i+j*half < list.length) {
          halfs += list(i+j*half)
        }
      }
    }
    
    SignalPattern(halfs.toList)
  }
  
  override def toString(): String = "halfs"
}