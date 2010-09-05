package com.harvey.domain

import java.util.concurrent.Executor

abstract class Runner extends Executor {
  def execute(r: Runnable)
}

class OneTimeRunner extends Runner {
  private var thread: Thread = null
  private var ran = false
  def execute(r: Runnable) {
    if (!ran) {
      ran = true
      thread = new Thread(r)
      thread.start
    }
  }
}