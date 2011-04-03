package com.harvey.service.audio

import scala.annotation.tailrec
import scala.collection.SeqView
import java.nio.ByteBuffer
import javax.sound.sampled._

case class SystemHandle protected[audio] (rate: Float) {
  private[this] val short = 32767.0
  private[this] val bits = 2 * 8
  private[this] val channels = 1
  private[this] val signed = true
  private[this] val bigendian = true
  private[this] val format = new AudioFormat(rate, bits, channels, signed, bigendian)
  private[this] val mixer = AudioSystem.getMixer(AudioSystem.getMixerInfo.apply(2))
  private[this] val source = mixer.getLine(new DataLine.Info(classOf[SourceDataLine], format))
  private[this] val speaker = source.asInstanceOf[SourceDataLine]
  speaker.open(format)
  speaker.start

  protected[audio] def close: Unit = {
    speaker.drain
    speaker.stop
    speaker.close
  }

  private[this] val chunk = rate * 1.0
  private[this] val buffer = ByteBuffer.allocate(chunk.toInt * 2)
  protected[audio] def writeStream: Stream[Sample] => Unit = {
    xs => {
      @tailrec def loop(n: Int, v: SeqView[Sample,Stream[Sample]]): Unit = {
        if (!v.isEmpty) {
          val start = System.currentTimeMillis
          val samples = v.toList
          println("writeStream.v.tolist took: " + (System.currentTimeMillis-start) + " millis")

          buffer.clear
          samples.foreach(x => buffer.putShort((x * short).asInstanceOf[Short]))
          speaker.write(buffer.array, 0, chunk.toInt * 2)
          loop(n+1, xs.view(chunk.toInt * n, chunk.toInt * (n+1)))
        }
      }
      loop(1, xs.view(0, chunk.toInt))
    }
  }
}
