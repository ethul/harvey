package com.harvey.sound

import com.harvey.platform.Oracle
import scala.collection.mutable.ListBuffer

sealed abstract class SignalMetaOperation extends Function2[Oracle,SignalContext,SignalPattern]

case class SignalMetaGeneratePattern() extends SignalMetaOperation {
   def apply(oracle: Oracle, context: SignalContext): SignalPattern = {
     SignalPattern(() => {
       val intervals = context intervals
       val durations = context lengths
       val amplitude = context maximumAmplitude
       var tone = 60
      
       def changeTone(change: Int): Int = {
         tone += change
         tone
       }
      
       val signals = for {
         i <- 0 to (context.maximumPatternLength * oracle.ask.asInstanceOf[Double]).toInt
         j = intervals((intervals.length * oracle.ask.asInstanceOf[Double]).floor.toInt)
         a = (amplitude.toDouble * oracle.ask.asInstanceOf[Double]).toInt
         d = durations((durations.length * oracle.ask.asInstanceOf[Double]).floor.toInt)
       } yield MidiSignalOnWithDuration(changeTone(j), a, d)
      
       signals toList
     })
   }
   
   override def toString(): String = "meta generate pattern"
}

case class SignalMetaModifyPattern(pattern: SignalPattern) extends SignalMetaOperation {
   def apply(oracle: Oracle, context: SignalContext): SignalPattern = {
     SignalPattern(() => {
       val intervals = context intervals
         
       def alter(value: Int, delta: Int): Int = 
         if (value + delta > context.maximumFrequency) context minimumFrequency
         else if (value + delta < context.minimumFrequency) context maximumFrequency
         else (value + delta)
      
       val signals = new ListBuffer[MidiSignalOnWithDuration]
       for (j <- 0 to pattern.asList.length-1) {
         val s = (pattern.asList())(j).asInstanceOf[MidiSignalOnWithDuration]
         val i = intervals((intervals.length * oracle.ask.asInstanceOf[Double]).floor.toInt)
         signals += MidiSignalOnWithDuration(alter(s.value, i), s.velocity, s.duration)
       }
       
       /*val signals = for {
         p <- pattern.asList
         s = p.asInstanceOf[MidiSignalOnWithDuration]
         i = intervals((intervals.length * oracle.ask.asInstanceOf[Double]).floor.toInt)
       } yield MidiSignalOnWithDuration(alter(s.value, i), s.velocity, s.duration)
       */
      
       signals toList
     })
   }
   
   override def toString(): String = "meta modify pattern"
}

case class SignalMetaRedistributeDurations(pattern: SignalPattern) extends SignalMetaOperation {
   def apply(oracle: Oracle, context: SignalContext): SignalPattern = {
     SignalPattern(() => {
       val maximumDelta = 100
       val end = pattern.asList.length-1
       var cache = 0
       
       def alter(value: Int, delta: Int, position: Int): Int = {
         if (position == end) {
           // we are at the end and have cache to make up
           value + cache
         }
         else {
           // do we add or subtract
           if (oracle.ask.asInstanceOf[Double] > 0.5) {
             // don't subtract from the cache when the last duration is less than the cache value
             if (cache - delta >= pattern.asList()(end-1).asInstanceOf[MidiSignalOnWithDuration].duration) {
               cache = cache - delta
               value + delta
             }
             else {
               value
             }
           }
           else if (value - delta > 0) {
             cache = cache + delta
             value - delta
           }
           else {
             value
           }
         } 
       }
      
       val signals = new ListBuffer[MidiSignalOnWithDuration]
       for (i <- 0 to pattern.asList.length-1) {
         val s = (pattern.asList())(i).asInstanceOf[MidiSignalOnWithDuration]
         val d = (maximumDelta.toDouble * oracle.ask.asInstanceOf[Double]).floor.toInt
         signals += MidiSignalOnWithDuration(s.value, s.velocity, alter(s.duration, d, i))
       }
       
       // TODO: ethul, why is this not the same as above
       /*val signals = for {
         i <- 0 to pattern.asList.length-1
         s = (pattern.asList())(i).asInstanceOf[MidiSignalOnWithDuration]
         d = (maximumDelta.toDouble * oracle.ask.asInstanceOf[Double]).floor.toInt
       } yield MidiSignalOnWithDuration(s.value, s.velocity, alter(s.duration, d, i))
      */
      
       signals toList
     })
   }
   
   override def toString(): String = "meta redistribute durations"
}
