package com.harvey.domain

import scala.actors.Actor
import scala.actors.Actor._

import java.util.concurrent.CountDownLatch

/**
 * this class represents a proxy worker which delegates top-level messages 
 * 
 * <p>
 * the purpose of this proxy is to provide a control at a high level for a
 * delegate actor. for example, this class waits on a count-down latch
 * before allowing the delegate to proceed. additionally, this class propogates
 * messags coming from other parts of the system to its delegate
 * 
 * @author ethul
 * @date 2010.03.27
 * @email eric.thul AT gmail.com
 */
class Worker(start: CountDownLatch, finish: CountDownLatch, delegate: Actor) extends Actor {
  /**
   * {@inheritDoc}
   * 
   * <p>
   * the act method uses a looping react message processing handler to allow for
   * the actor library to manage the thread usage instead of explicityly declaring
   * the receive method.
   * 
   * <p>
   * only top-level messages are handled here and thus passed along to the delegate
   * actor. additionally, a count-down latch is used to create a barrier such that
   * instances of this class may all begin processing at the ``same time''
   * 
   * <p>
   * as a note, the broadcast top-level message is propogated here to the delegate.
   * this is a special case which allows for delegates to communicate with other
   * delegates in the system
   */
  def act() {
    loop {
      react {
        case StopMessage => {
          delegate ! StopMessage
          exit
        }
        case ReadyMessage => {
          start.await
          delegate ! GenerateMessage
          finish.countDown
        }
        case BroadcastMessage(message) => {
          delegate ! BroadcastMessage(message)
        }
      }
    }
  }
}
