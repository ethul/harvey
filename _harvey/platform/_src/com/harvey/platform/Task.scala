package com.harvey.platform

sealed abstract class Task(task: Function0[Unit]) extends Function0[Unit]

case class ExpirableTask(task: Function0[Unit], expiration: Long) extends Task(task) {
  def apply() {
    while (System.currentTimeMillis < expiration) {
      task()
    }
  }
}

case class StoppableTask(task: Function0[Unit]) extends Task(task) {
  private var stopTask = false
  def stop {
    stopTask = true
  }
  def apply() {
    while (!stopTask) {
      task()
    }
  }
}