/**
 * harvey (c) 2010
 */
package com.harvey.service.audio

import scalaz.effects.IO

/**
 * @author ethul
 * @version 1.1
 * @since 1.0
 */
trait Device[M[_],H] {
  def open: M[H]
  def close(h: H): M[Unit]
  def writeStream(h: H)(xs: Stream[Sample]): M[Unit]
}

object Device {
  def open[M[_],H](implicit e: Device[M,H]): M[H] = e.open
  def close[M[_],H](h: H)(implicit e: Device[M,H]): M[Unit] = e.close(h)
  def writeStream[M[_],H](h: H)(xs: Stream[Sample])(implicit e: Device[M,H]): M[Unit] = e.writeStream(h)(xs)

  implicit object SystemDevice extends Device[IO,SystemHandle] {
    def open: IO[SystemHandle] = IO(w => (w,SystemHandle(22050.0f)))
    def close(h: SystemHandle): IO[Unit] = IO(w => (w,h.close))
    def writeStream(h: SystemHandle)(xs: Stream[Sample]): IO[Unit] = IO(w => (w,h writeStream xs))
  }
}
