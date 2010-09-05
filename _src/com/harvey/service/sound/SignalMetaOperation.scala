package com.harvey.service.sound

import com.harvey.domain.Oracle
import scala.collection.mutable.ListBuffer

sealed abstract class SignalMetaOperation extends Function1[SignalContext,SignalPattern]

case class SignalMetaGeneratePattern() extends SignalMetaOperation {
   def apply(context: SignalContext): SignalPattern = {
     SignalPattern(() => {
       val intervals = context intervals
       val durations = context lengths
       val amplitudes = List(-2,-1,0,1,2)
       var amplitude = 60
       var tone = 60
       
       //                    C  D  E  F  G  A  Bb C
       val myxolidian = List(60,62,64,65,67,69,70,72)
      
       def changeTone(change: Int): Int = {
         tone += change
         tone
       }
       
       def changeAmplitude(change: Int): Int = {
         amplitude += change
         amplitude
       }
      
       val signals = for {
         i <- 0 to (context.maximumPatternLength * Oracle.ask).toInt
         //j = intervals((intervals.length * Oracle.ask).floor.toInt)
         j = myxolidian((myxolidian.length * Oracle.ask).floor.toInt)
         a = amplitudes((amplitudes.length * Oracle.ask).floor.toInt)
         d = durations((durations.length * Oracle.ask).floor.toInt)
       } yield MidiSignalOnWithDuration(j, changeAmplitude(a), d)
       //} yield MidiSignalOnWithDuration(changeTone(j), changeAmplitude(a), d)
      
       signals toList
     })
   }
   
   override def toString(): String = "meta generate pattern"
}

case class SignalMetaModifyPattern(pattern: SignalPattern) extends SignalMetaOperation {
   def apply(context: SignalContext): SignalPattern = {
     SignalPattern(() => {
       val intervals = context intervals
         
       def alter(value: Int, delta: Int): Int = 
         if (value + delta > context.maximumFrequency) context minimumFrequency
         else if (value + delta < context.minimumFrequency) context maximumFrequency
         else (value + delta)
      
       val signals = new ListBuffer[MidiSignalOnWithDuration]
       for (j <- 0 to pattern.asList.length-1) {
         val s = (pattern.asList())(j).asInstanceOf[MidiSignalOnWithDuration]
         val i = intervals((intervals.length * Oracle.ask).floor.toInt)
         signals += MidiSignalOnWithDuration(alter(s.value, i), s.velocity, s.duration)
       }
       
       /*val signals = for {
         p <- pattern.asList
         s = p.asInstanceOf[MidiSignalOnWithDuration]
         i = intervals((intervals.length * Oracle.ask).floor.toInt)
       } yield MidiSignalOnWithDuration(alter(s.value, i), s.velocity, s.duration)
       */
      
       signals toList
     })
   }
   
   override def toString(): String = "meta modify pattern"
}

case class SignalMetaRedistributeDurations(pattern: SignalPattern) extends SignalMetaOperation {
   def apply(context: SignalContext): SignalPattern = {
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
           if (Oracle.ask > 0.5) {
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
         val d = (maximumDelta.toDouble * Oracle.ask).floor.toInt
         signals += MidiSignalOnWithDuration(s.value, s.velocity, alter(s.duration, d, i))
       }
       
       // TODO: ethul, why is this not the same as above
       /*val signals = for {
         i <- 0 to pattern.asList.length-1
         s = (pattern.asList())(i).asInstanceOf[MidiSignalOnWithDuration]
         d = (maximumDelta.toDouble * Oracle.ask).floor.toInt
       } yield MidiSignalOnWithDuration(s.value, s.velocity, alter(s.duration, d, i))
      */
      
       signals toList
     })
   }
   
   override def toString(): String = "meta redistribute durations"
}
