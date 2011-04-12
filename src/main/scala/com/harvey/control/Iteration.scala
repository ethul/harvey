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
 *
 * <p>
 * note that the ephemeralIO enumerator and iteratee are adapted from
 * <a href="http://www.google.com/url?sa=D&q=http://groups.google.com/group/scalaz/browse_thread/thread/7d9c0b1ac32b9218">here</a>
 *
 * @author ethul
 * @version
 * @since 1.0
 */
object Iteration {

  object Enumerators {
    /**
     * original recursive version, not used currently
     */
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

    /**
     * nonrecursive version of the enumerator above. this was necessary because
     * the recursion caused the program to keep consuming the heap since we are
     * dealing with an infinite ephemeral stream.
     */
    def ephemeralNonrecursiveIO[A]: EphemeralStream[A] => Int => Iteratee[IO,EphemeralStream[A],Unit] => Iteratee[IO,EphemeralStream[A],Unit] = {
      as => chunk => i => {
        val r =
          for {
            curr <- i.value
            next <- {
              var next = curr
              var done = false
              var bs = as;
              while (! done) {
                next match {
                  case x@IterV.DoneM(_,_) => {
                    next = x
                    done = true
                  }
                  case IterV.ContM(k) => {
                    bs match {
                      case EphemeralStream.empty => {
                        next = k(IterV.EOF[EphemeralStream[A]]).value.unsafePerformIO
                      }
                      case cs => {
                        val left = cs.take(chunk)
                        bs = cs.drop(chunk)
                        next = k(IterV.El(left)).value.unsafePerformIO
                      }
                    }
                  }
                }
              }
              IO { (w => (w,next)) }
            }
          } yield next
        Iteratee(r)
      }
    }
  }

  object Iteratees {
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
