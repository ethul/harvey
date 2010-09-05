package com.harvey.domain

import scala.collection.mutable.ListBuffer

/**
 * this singleton represents a manager for the worker instances
 * 
 * <p>
 * the functionality provided by this object is to allow for
 * inter-worker communication via the notion of broadcasting.
 * essentially this provides a way to send messages to all
 * workers which have been attached to this object
 * 
 * @author ethul
 * @date 2010.03.27
 * @email eric.thul AT gmail.com
 */
object Manager {
  private val workers = new ListBuffer[Worker]

  /**
   * attach a worker instance to the list of managed workers
   * 
   * @param worker is the worker instance to manage
   */
  def attach(worker: Worker) {
    workers += worker
  }
  
  /**
   * detach all worker instances from the list of manged workers
   */
  def detachAll() {
    workers clear
  }
  
  /**
   * broadcast a message to all managed worker instances
   * 
   * @param message is the message to broadcast
   */
  def broadcast(message: Message) {
    workers foreach (_ ! BroadcastMessage(message))
  }
}
