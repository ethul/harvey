package com.harvey.service.system

import scala.annotation.tailrec
import scala.collection.SeqView
import scalaz.EphemeralStream
import java.nio.ByteBuffer
import javax.sound.sampled._

case class AudioHandle protected[system] (rate: Float) {
  private[this] val short = 32767.0
  private[this] val bits = 2 * 8
  private[this] val channels = 1
  private[this] val signed = true
  private[this] val bigendian = true
  private[this] val format = new AudioFormat(rate, bits, channels, signed, bigendian)
  private[this] val line = 
    AudioSystem.getMixer(AudioSystem.getMixerInfo.apply(0)).
      getLine(new DataLine.Info(classOf[SourceDataLine], format)).
        asInstanceOf[SourceDataLine]

  line.open(format)
  line.start

  protected[system] def close: Unit = {
    line.drain
    line.stop
    line.close
  }

  private[this] val chunk = rate * 1.0
  private[this] val buffer = ByteBuffer.allocate(chunk.toInt * 2)
  protected[system] def writeStream: EphemeralStream[Sample] => Unit = {
    case EphemeralStream.empty => ()
    case xs => {
      buffer.clear
      var ys = xs
      while (!ys.isEmpty) {
        buffer.putShort((ys.head() * short).toShort)
        ys = ys.tail()
      }
      line.write(buffer.array, 0, chunk.toInt * 2)
    }
  }
}
