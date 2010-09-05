package com.harvey.service.sound

import scala.collection.mutable.ListBuffer

/**
 * this class is represents a pattern of signals
 * 
 * <p>
 * basically it can be thought it like a musical
 * theme
 */
trait SignalPattern {
  def apply(index: Int): Signal
  def insert(signal: Signal): Unit
  def asList(): List[Signal]
}

object SignalPattern {
  def apply(): SignalPattern = new SignalPatternImpl()
  
  def apply(pattern: List[Signal]): SignalPattern = {
    val signalPattern = new SignalPatternImpl()
    pattern foreach(signalPattern insert _)
    signalPattern
  }
  
  def apply(f: Function0[List[Signal]]): SignalPattern = {
    val signalPattern = new SignalPatternImpl()
    f() foreach(signalPattern insert _)
    signalPattern
  }
  
  private class SignalPatternImpl extends SignalPattern {
    var pattern = new ListBuffer[Signal]
    
    def apply(index: Int): Signal = pattern(index)
    
    def insert(signal: Signal): Unit = {
      pattern += signal
    }
    
    def asList(): List[Signal] = pattern.toList
    
    override def toString(): String = {
      "signal pattern {" +
      pattern.toString +
      "}"
    }
  }
}
