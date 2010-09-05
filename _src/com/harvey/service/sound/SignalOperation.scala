package com.harvey.service.sound

import com.harvey.domain.Oracle
import com.harvey.domain.util.Logger

import scala.collection.mutable.{Buffer,ListBuffer}

sealed abstract class SignalOperation extends Function1[SignalPattern,SignalPattern]

case class SignalIdentity() extends SignalOperation {
  def apply(pattern: SignalPattern) = pattern 
  override def toString() = "identity"
}
  
case class SignalReverse() extends SignalOperation {
  def apply(pattern: SignalPattern) = SignalPattern(pattern.asList.reverse)
  override def toString() = "reverse"
}

case class SignalNullify() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    val head = pattern.asList.head
    val nullified = for (i <- pattern.asList.iterator) yield MidiSignalOnWithDuration(head.value,i.asInstanceOf[MidiSignalOnWithDuration].velocity,i.asInstanceOf[MidiSignalOnWithDuration].duration)
    SignalPattern(nullified.toList)
  }
  
  override def toString() = "nullify"
}

case class SignalInverse() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    // inverse the interval, so if you go up 2, make
    // the interval go down 2
    val list = pattern.asList
    val intervals = 
      for (i <- 0 to list.length-2) yield -(list(i+1).value - list(i).value)
    
    var inverses = new ListBuffer[Signal]
    var start = list.head.value
    
    for (i <- 0 to list.length-1) {
      val midisignal = list(i).asInstanceOf[MidiSignalOnWithDuration]
      inverses += MidiSignalOnWithDuration(start, midisignal.velocity, midisignal.duration)
      if (i < intervals.length) {
        start += intervals(i)
        if (start > MidiSignalContext.maximumFrequency ||
          start < MidiSignalContext.minimumFrequency) {
          throw new IllegalArgumentException("cannot inverse from invalid frequency: " + start)
        }
      }
    } 
    
    SignalPattern(inverses.toList)
  }
  
  override def toString() = "inverse"
}

/**
 * pick the first note, then last note, then second note
 * then the second to last note, etc.
 */
case class SignalFlipFlop() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
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
  
  override def toString() = "flipflop"
}

/**
 * weave through the pattern
 *  0 1 2 3 4 5 6 
 *  0 3 6 5 1 2 4
 */
case class SignalWeave() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    SignalPattern(Nil)
  }
  
  override def toString() = "weave"
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
// TODO: ethul, fix this it is buggy
case class SignalHalfs() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    val list = pattern.asList
    
    if (list.length < 3) {
      throw new IllegalArgumentException {
        "cannot perform halfs on a pattern of length: " + list.length
      }
    }
    
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
  
  override def toString() = "halfs"
}

// takes a signal and flattens it to
//   .
//   . . . . .
//   . . . . .
//
case class SignalFlattenAmplitude() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    // we cannot flatten an empty pattern
    if (pattern == Nil || pattern.asList.isEmpty) {
      throw new IllegalArgumentException
    }
    
    // find the max amplitude
    val maxAmplitude = pattern.asList.foldLeft(0) { (b, a) =>
      val s = a.asInstanceOf[MidiSignalOnWithDuration]
      if (s.velocity > b) {
        s.velocity
      }
      else {
        b
      }
    }
    
    val meanAmplitude = pattern.asList.foldLeft(0) { (b, a) =>
      val s = a.asInstanceOf[MidiSignalOnWithDuration]
      s.velocity + b
    } / pattern.asList.length
    
    val p = new ListBuffer[Signal]
    
    val first = pattern.asList.head.asInstanceOf[MidiSignalOnWithDuration]
    //p += MidiSignalOnWithDuration(first.value, maxAmplitude, first.duration)
    
    //pattern.asList.tail.foreach { s =>
    pattern.asList.foreach { s =>
      val signal = s.asInstanceOf[MidiSignalOnWithDuration]
      p += MidiSignalOnWithDuration(first.value, meanAmplitude.toInt, signal.duration)
    }
    
    SignalPattern(p.toList)
  }
  
  override def toString() = "flatten amplitude"
}

// make an even rhythm; all durations as close to each other as possible
case class SignalEvenlyDistributeDuration() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    val max = pattern.asList.foldLeft(0) { (b, a) =>
      val s = a.asInstanceOf[MidiSignalOnWithDuration]
      s.duration + b
    }
    
    val p = new ListBuffer[Signal]
    
    if (max % pattern.asList.length == 0) {
      // we can evenly divide the total duration
      val duration = max / pattern.asList.length
      pattern.asList.foreach { s =>
        val signal = s.asInstanceOf[MidiSignalOnWithDuration]
        p += MidiSignalOnWithDuration(signal.value, signal.velocity, duration)
      }
    }
    else {
      // we cannot evenly divide so do nothing
      throw new IllegalArgumentException(
        "cannot evenly divide " + max + " by " + pattern.asList.length
      )
    }
    
    SignalPattern(p.toList)
  }
  
  override def toString() = "evenly distribute duration"
}

// binary fission where a note can be split into 2
case class SignalBinaryFission() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    // this will be the note to do fission on
    val fission = (pattern.asList.length * Oracle.ask).floor.toInt
    val list = pattern.asList
    val p = new ListBuffer[Signal]
    
    for (i <- 0 to pattern.asList.length-1) {
      val signal = list(i).asInstanceOf[MidiSignalOnWithDuration]
      if (i == fission) {
        val a = (Oracle.ask * signal.duration).floor.toInt
        val b = signal.duration - a
        if (a != 0 && b != 0) {
          p += MidiSignalOnWithDuration(signal.value, signal.velocity, a)
          p += MidiSignalOnWithDuration(signal.value, signal.velocity, b)
        }
        else {
          throw new IllegalArgumentException("cannot add a zero duration signal")
        }
      }
      else {
          p += MidiSignalOnWithDuration(signal.value, signal.velocity, signal.duration)
      }
    }
    
    SignalPattern(p.toList)
  }
  
  override def toString() = "binary fission"
}

// binary fusion where a note can be merged with the next to form 1
case class SignalBinaryFusion() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    // this will be the note to do fusion on
    // cannot be last note, because we fuse with the fusion+1
    val fusion = ((pattern.asList.length-2) * Oracle.ask).floor.toInt
    val list = pattern.asList
    val p = new ListBuffer[Signal]
    var fused = false
    
    // we can only fuse with at least two items in the pattern
    if (fusion < 0) {
      throw new IllegalArgumentException("cannot fuse one item")
    }
    
    for (i <- 0 to pattern.asList.length-1) {
      val signal = list(i).asInstanceOf[MidiSignalOnWithDuration]
      if (!fused) {
        if (i == fusion) {
          // do the fusion
          val fusee = list(i+1).asInstanceOf[MidiSignalOnWithDuration]
          p += MidiSignalOnWithDuration(
            (signal.value+fusee.value)/2,
            (signal.velocity+fusee.velocity)/2,
            signal.duration+fusee.duration
          )
          fused = true
        }
        else {
          p += MidiSignalOnWithDuration(signal.value, signal.velocity, signal.duration)
        }
      }
      else {
        fused = false
      }
    }
    
    SignalPattern(p.toList)
  }
  
  override def toString() = "binary fusion"
}

// makes one note over the entire signal pattern
case class SignalAccentOne() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    val totalDuration = pattern.asList.foldLeft(0) { (b, a) =>
      val s = a.asInstanceOf[MidiSignalOnWithDuration]
      b + s.duration
    }
    
    val meanAmplitude = pattern.asList.foldLeft(0) { (b, a) =>
      val s = a.asInstanceOf[MidiSignalOnWithDuration]
      b + s.velocity
    } / pattern.asList.length
    
    SignalPattern(MidiSignalOnWithDuration(60, meanAmplitude, totalDuration) :: Nil)
  }
  
  override def toString() = "accent one"
}

// makes a note on the one and three
case class SignalAccentOneThree() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    val totalDuration = pattern.asList.foldLeft(0) { (b, a) =>
      val s = a.asInstanceOf[MidiSignalOnWithDuration]
      b + s.duration
    }
    
    val meanAmplitude = pattern.asList.foldLeft(0) { (b, a) =>
      val s = a.asInstanceOf[MidiSignalOnWithDuration]
      b + s.velocity
    } / pattern.asList.length
    
    if (totalDuration % 4 == 0) {
      SignalPattern {
        MidiSignalOnWithDuration(60, meanAmplitude, totalDuration/2) :: 
        MidiSignalOnWithDuration(60, meanAmplitude/2, totalDuration/2) :: 
        Nil
      }
    }
    else {
      SignalPattern(MidiSignalOnWithDuration(60, meanAmplitude, totalDuration) :: Nil)
    }
  }
  
  override def toString() = "accent one three"
}

// modulate the frequency for some signal in the pattern
case class SignalModulate() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    val modulatee = ((pattern.asList.length-1) * Oracle.ask).floor.toInt
    // TODO: ethul, can we abstract which context is being used?
    val modulation = ((MidiSignalContext.intervals.length-1) * Oracle.ask).floor.toInt
    val list = pattern.asList
    val p = new ListBuffer[Signal]
    for (i <- 0 to list.length-1) {
      if (i == modulatee) {
        val s = list(i).asInstanceOf[MidiSignalOnWithDuration]
        val modvalue = s.value + MidiSignalContext.intervals(modulation)
        if (modvalue > MidiSignalContext.maximumFrequency || 
              modvalue < MidiSignalContext.minimumFrequency) {
          throw new IllegalArgumentException("modulated value is invalid: " + modvalue)
        }
        p += MidiSignalOnWithDuration(modvalue, s.velocity, s.duration)
      }
      else {
        p += list(i) 
      }
    }
    SignalPattern(p.toList)
  }
  
  override def toString() = "modulate"
}

// doubles the duration of all signals
case class SignalDoubleDuration() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    val p = new ListBuffer[Signal]
    val list = pattern.asList
    for (i <- 0 to list.length-1) {
      val s = list(i).asInstanceOf[MidiSignalOnWithDuration]
      p += MidiSignalOnWithDuration(s.value, s.velocity, s.duration*2)
    }
    SignalPattern(p.toList)
  }
  
  override def toString() = "double duration"
}

// halfs the duration of all signals
case class SignalHalfDuration() extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    val p = new ListBuffer[Signal]
    val list = pattern.asList
    for (i <- 0 to list.length-1) {
      val s = list(i).asInstanceOf[MidiSignalOnWithDuration]
      if (s.duration == 0 || s.duration % 2 != 0) {
        throw new IllegalArgumentException("cannot half the duration: " + s.duration)
      }
      p += MidiSignalOnWithDuration(s.value, s.velocity, s.duration/2)
    }
    SignalPattern(p.toList)
  }
  
  override def toString() = "half duration"
}

// composite to allow for the composition of signal operations
case class SignalCompositeOperation(composites: List[SignalOperation]) extends SignalOperation {
  def apply(pattern: SignalPattern) = {
    composites.foldLeft(pattern) { 
      (b, a) => {
        try {
          a(b)
        }
        catch {
          // do nothing and return the input
          case e: IllegalArgumentException => {
            Logger.log("could not perform: ["+a+"]["+e.getMessage+"]\n")
            b
          } 
        }
      }
    }
  }
  
  override def toString() = {
    composites.foldLeft("composite:") { (b ,a) => b + a + "," }
  }
}
