package com.harvey.platform

trait Oracle {
  type T
  def ask(): T
}
