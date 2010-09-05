package com.harvey.domain

trait Oracle {
  def ask(): Double
}

object Oracle extends Oracle {
  var oracle: Oracle = _
  
  def install(o: Oracle) {
    oracle = o
  }
  
  def ask() = oracle.ask
}
