/**
 * harvey (c) 2010
 */
package com.harvey.service.system

import scalaz.EphemeralStream
import scalaz.effects.IO

/**
 * @author ethul
 * @version 1.1
 * @since 1.0
 */
trait Device[M[_],H] {
  def open: M[H]
  def close(h: H): M[Unit]
  def writeStream(h: H)(xs: EphemeralStream[Sample]): M[Unit]
}

object Device {
  def open[M[_],H](implicit e: Device[M,H]): M[H] = e.open
  def close[M[_],H](h: H)(implicit e: Device[M,H]): M[Unit] = e.close(h)
  def writeStream[M[_],H](h: H)(xs: EphemeralStream[Sample])(implicit e: Device[M,H]): M[Unit] = e.writeStream(h)(xs)

  case class Audio(sampleRate: Float) extends Device[IO,AudioHandle] {
    def open: IO[AudioHandle] = IO(w => (w,AudioHandle(sampleRate)))
    def close(h: AudioHandle): IO[Unit] = IO(w => (w,h.close))
    def writeStream(h: AudioHandle)(xs: EphemeralStream[Sample]): IO[Unit] = IO(w => (w,h writeStream xs))
  }

  case class Wavefile(fileName: String, sampleRate: Float) extends Device[IO,WavefileHandle] {
    def open: IO[WavefileHandle] = IO(w => (w,WavefileHandle(fileName,sampleRate)))
    def close(h: WavefileHandle): IO[Unit] = IO(w => (w,h.close))
    def writeStream(h: WavefileHandle)(xs: EphemeralStream[Sample]): IO[Unit] = IO(w => (w,h writeStream xs))
  }
}
