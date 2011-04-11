/**
 * harvey (c) 2011
 */
package com.harvey.control

import PimpedTypes._
import scalaz._, Scalaz._
import effects._
import IterV._

/**
 * this is the iteration control object
 * <a href="http://www.google.com/url?sa=D&q=http://groups.google.com/group/scalaz/browse_thread/thread/7d9c0b1ac32b9218">scalaz</a>
 *
 * @author ethul
 * @version
 * @since 1.0
 */
object Iteration {

  object Enumerators {
    def streamIO[A]: Stream[A] => Int => Iteratee[IO,Stream[A],Unit] => Iteratee[IO,Stream[A],Unit] = {
      as => chunk => i => {
        Iteratee(for { 
          curr <- i.value 
          next <- curr match { 
            case x@DoneM(_, _) => x.pure[IO] 
            case ContM(k) => as match { 
              case x@Stream.Empty => (streamIO(x)(chunk)(k(EOF[Stream[A]]))).value
              case xs => {
                val (left, right) = xs.splitAt(chunk)
                (streamIO(right)(chunk)(k(El(left)))).value
              }
            }
          }
        } yield next) 
      }
    }

    def ephemeralIO[A]: EphemeralStream[A] => Int => Iteratee[IO,EphemeralStream[A],Unit] => Iteratee[IO,EphemeralStream[A],Unit] = {
      as => chunk => i => {
        Iteratee(for { 
          curr <- i.value 
          next <- curr match { 
            case x@DoneM(_, _) => x.pure[IO] 
            case ContM(k) => as match { 
              case x@EphemeralStream.empty => (ephemeralIO(x)(chunk)(k(EOF[EphemeralStream[A]]))).value
              case xs => {
                val left = xs.take(chunk)
                val right = xs.drop(chunk)
                (ephemeralIO(right)(chunk)(k(El(left)))).value
              }
            }
          }
        } yield next) 
      }
    }

    // TODO: ethul, clean up isDone+ephemeralIO2. these seem to keep a steady mem usage without the recursion above
    //              but they are a bit hackish
    private[this] def isDone[A](ivalue: IterVM[IO,EphemeralStream[A],Unit]): Boolean = ivalue match {
      case x@DoneM(_, _) => true
      case _ => false
    }
    def ephemeralIO2[A]: EphemeralStream[A] => Int => Iteratee[IO,EphemeralStream[A],Unit] => Iteratee[IO,EphemeralStream[A],Unit] =  {
      as => chunk => i => {
        val r =
          for {
            curr <- i.value
          } yield {
            var cc = curr
            var cs = as
            while(! isDone(cc)) {
              cc = cs match {
                case EphemeralStream.empty => cc match { case ContM(k) => k(EOF[EphemeralStream[A]]).value.unsafePerformIO }
                case xs => {
                  val left = xs.take(chunk)
                  val right = xs.drop(chunk)
                  cs = right
                  cc match { case ContM(k) => k(El(left)).value.unsafePerformIO }
                }
              }
            }
            cc
          }
        Iteratee(r)
      }
    }
  }

  object Iteratees {
    def streamIO[A]: (Stream[A] => IO[Unit]) => Iteratee[IO,Stream[A],Unit] = { 
      f => {
        def go: IO[IterVM[IO,Stream[A],Unit]] = IO(w => (w,ContM(step)))
        def step(s: Input[Stream[A]]): Iteratee[IO,Stream[A],Unit] = 
          s(el = e => Iteratee(f(e) >>=| go), 
            empty = Iteratee(go), 
            eof = Iteratee(IO(w => (w,DoneM((),EOF[Stream[A]]))))) 
        Iteratee(go) 
      }
    } 

    def ephemeralIO[A]: (EphemeralStream[A] => IO[Unit]) => Iteratee[IO,EphemeralStream[A],Unit] = { 
      f => {
        def go: IO[IterVM[IO,EphemeralStream[A],Unit]] = IO(w => (w,ContM(step)))
        def step(s: Input[EphemeralStream[A]]): Iteratee[IO,EphemeralStream[A],Unit] = 
          s(el = e => Iteratee(f(e) >>=| go), 
            empty = Iteratee(go), 
            eof = Iteratee(IO(w => (w,DoneM((),EOF[EphemeralStream[A]]))))) 
        Iteratee(go) 
      }
    } 
  }
}
