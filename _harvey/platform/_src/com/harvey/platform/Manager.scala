package com.harvey.platform

import scala.collection.mutable.ListBuffer

object Manager {
  val workers = new ListBuffer[Worker]

  def attach(worker: Worker) {
    workers += worker
  }
  
  def detachAll() {
    workers clear
  }
  
  def broadcast(message: String) {
    workers foreach (_ ! ("broadcasted", message))
  }
}
