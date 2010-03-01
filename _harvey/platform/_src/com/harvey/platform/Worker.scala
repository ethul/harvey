package com.harvey.platform

import java.util.concurrent.CountDownLatch

class Worker(start: CountDownLatch, finish: CountDownLatch, task: Task) extends Runnable {
  override def run {
    start.await
    task()
    finish.countDown
  }
}
