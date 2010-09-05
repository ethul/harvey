package com.harvey.domain.util

import scala.collection.mutable.Set

object Logger {
  val listeners = Set[Function1[String, Unit]]()
  
  def attach(f: Function1[String, Unit]) {
    listeners += f
  }
  
  def log(message: String) {
    listeners.foreach { f => 
      f(message)
    }
  }
}
