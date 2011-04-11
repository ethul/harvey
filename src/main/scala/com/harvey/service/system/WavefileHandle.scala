package com.harvey.service.system

import scala.annotation.tailrec
import scala.collection.SeqView
import scalaz.EphemeralStream
import java.io.{ByteArrayInputStream,BufferedOutputStream,FileOutputStream,File,RandomAccessFile}
import java.nio.{ByteOrder,ByteBuffer}

case class WavefileHandle protected[system] (fileName: String, rate: Float) {
  private[this] val short = 32767.0
  private[this] val bits = 2 * 8
  private[this] val channels = 1
  private[this] val f = new File(fileName)
  private[this] val file = new BufferedOutputStream(new FileOutputStream(f))
  private[this] val randomFile = new RandomAccessFile(f,"rws")

  protected[system] def close: Unit = {
    if ((chunksWritten * rate.toInt * 2) % 2 == 1) {
      file.write(0.toByte)
    }
    file.flush
    file.close
    randomFile.close
  }

  private[this] val identifier = ByteBuffer.allocate(12)
  private[this] val format = ByteBuffer.allocate(24)
  private[this] val data = ByteBuffer.allocate(8)

  writeIdentifier
  writeFormat
  writeData

  private[this] def writeIdentifier: Unit = {
    identifier.clear

    // RIFF   4-byte
    //identifier.order(ByteOrder.BIG_ENDIAN)
    identifier.order(ByteOrder.LITTLE_ENDIAN)
    identifier.put('R'.toByte)
    identifier.put('I'.toByte)
    identifier.put('F'.toByte)
    identifier.put('F'.toByte)

    // chunk size   4-byte
    identifier.order(ByteOrder.LITTLE_ENDIAN)
    identifier.putInt(4 + 24 + 8 + rate.toInt * 2 + (if ((rate.toInt * 2) % 2 == 0) 0 else 1))

    // WAVE   4-byte
    //identifier.order(ByteOrder.BIG_ENDIAN)
    identifier.order(ByteOrder.LITTLE_ENDIAN)
    identifier.put('W'.toByte)
    identifier.put('A'.toByte)
    identifier.put('V'.toByte)
    identifier.put('E'.toByte)

    file.write(identifier.array)
  }

  private[this] def writeFormat: Unit = {
    format.clear

    // "fmt "  4-byte
    //format.order(ByteOrder.BIG_ENDIAN)
    format.order(ByteOrder.LITTLE_ENDIAN)
    format.put('f'.toByte)
    format.put('m'.toByte)
    format.put('t'.toByte)
    format.put(' '.toByte)

    format.order(ByteOrder.LITTLE_ENDIAN)
    // chunk size
    format.putInt(16)
    // WAVE_FORMAT_PCM
    format.putShort(1)
    // channels
    format.putShort(channels.toShort)
    // sampling rate
    format.putInt(rate.toInt)
    // average bytes per second
    format.putInt(rate.toInt * 2)
    // block align
    format.putShort(2.toShort)
    // bits per sample
    format.putShort((8 * 2).toShort)

    file.write(format.array)
  }

  private[this] def writeData: Unit = {
    data.clear

    // "data"  4-byte
    //data.order(ByteOrder.BIG_ENDIAN)
    data.order(ByteOrder.LITTLE_ENDIAN)
    data.put('d'.toByte)
    data.put('a'.toByte)
    data.put('t'.toByte)
    data.put('a'.toByte)

    data.order(ByteOrder.LITTLE_ENDIAN)
    // chunk size
    data.putInt(rate.toInt * 2)

    // samples
    //data.put(ds)

    // padding
/*
    if ((rate.toInt * 2) % 2 == 1) {
      data.put(0.toByte)
    }
*/

    file.write(data.array)
  }

  var chunksWritten = 0
  def appendData(ds: Array[Byte]): Unit = {
    randomFile.seek(4)
    randomFile.writeInt(4 + 24 + 8 + chunksWritten * rate.toInt * 2 + (if ((chunksWritten * rate.toInt * 2) % 2 == 0) 0 else 1))
    randomFile.seek(40)
    randomFile.writeInt(chunksWritten * rate.toInt * 2)
    
    file.write(ds.array)
  }

  private[this] val chunk = rate * 1.0
  private[this] val buffer = ByteBuffer.allocate(chunk.toInt * 2)
  buffer.order(ByteOrder.LITTLE_ENDIAN)
  protected[system] def writeStream: EphemeralStream[Sample] => Unit = {
    case EphemeralStream.empty => ()
    case xs => {
      buffer.clear
      var ys = xs
      while (!ys.isEmpty) {
        buffer.putShort((ys.head() * short).toShort)
        ys = ys.tail()
      }
      chunksWritten = chunksWritten + 1
      appendData(buffer.array)
    }
  }
}
