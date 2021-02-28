package org.gorbv.taskmanager.process

object Process {
  type PID = Long
  type Priority = Int

  trait Job { def run(): Unit }

  sealed trait CreateProcessError
  object Busy extends CreateProcessError

  val cpuCount = 100
}
