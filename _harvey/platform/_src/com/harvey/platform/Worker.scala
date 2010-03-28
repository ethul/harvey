package com.harvey.platform

import scala.actors.Actor
import scala.actors.Actor._

import java.util.concurrent.CountDownLatch

class Worker(start: CountDownLatch, finish: CountDownLatch, delegate: Actor) extends Actor {
  def act() {
    loop {
      react {
        case "stop" => {
          delegate ! "stop"
          exit
        }
        case ("broadcasted", _ @ message) => {
          delegate ! ("broadcasted", message, self)
        }
        case _ => {
          println("in awaiting case")
          start.await
          delegate ! "generate"
          finish.countDown
        }
      }
    }
  }
}
