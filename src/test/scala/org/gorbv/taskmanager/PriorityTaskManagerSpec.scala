package org.gorbv.taskmanager

import org.gorbv.taskmanager.process.Process.{Busy, CreateProcessError}
import org.specs2.mutable.Specification

import java.util.concurrent.Semaphore

final class PriorityTaskManagerSpec extends Specification {

  sequential

  "PriorityTaskManager" should {

    "try to execute new task with lower priority in a single cpu" in {

      val taskManager = TaskManager(1)

      val secondJobAddedSemaphore = new Semaphore(0, true)
      val secondJobFinishedSemaphore = new Semaphore(0, true)

      var job0Finished = false

      val result0 = taskManager.addJob(() => {
        secondJobAddedSemaphore.acquire()
        job0Finished = true
        secondJobFinishedSemaphore.release()
      }, PriorityTaskStrategy(1))

      val result1 = taskManager.addJob(() => {
        sys.error("should not be started")
      }, PriorityTaskStrategy(0))

      secondJobAddedSemaphore.release()

      result0 must beRight(0)
      result1 must beLeft(Busy: CreateProcessError)

      secondJobFinishedSemaphore.acquire()

      job0Finished must beTrue
    }

    "try to execute new task with higher priority in a single cpu" in {

      val taskManager = TaskManager(1)

      val secondJobFinishedSemaphore = new Semaphore(0, true)

      var job0Finished = false
      var job1Finished = false

      val result0 = taskManager.addJob(() => {
        Thread.sleep(Long.MaxValue)
        job0Finished = true
      }, PriorityTaskStrategy(0))

      val result1 = taskManager.addJob(() => {
        job1Finished = true
        secondJobFinishedSemaphore.release()
      }, PriorityTaskStrategy(1))

      result0 must beRight(0)
      result1 must beRight(1)

      secondJobFinishedSemaphore.acquire()

      job0Finished must beFalse
      job1Finished must beTrue
    }

    "queue two tasks on one cpu" in {

      val taskManager = TaskManager(1)

      var job0Finished = false
      var job1Finished = false

      val firstJobCompletionSemaphore = new Semaphore(0, true)
      val secondJobCompletionSemaphore = new Semaphore(0, true)

      val result0 = taskManager.addJob(() => {
        job0Finished = true
        firstJobCompletionSemaphore.release()
      }, PriorityTaskStrategy(0))

      firstJobCompletionSemaphore.acquire()
      taskManager.killAllJobs()

      val result1 = taskManager.addJob(() => {
        job1Finished = true
        secondJobCompletionSemaphore.release()
      }, PriorityTaskStrategy(0))

      secondJobCompletionSemaphore.acquire()

      result0 must beRight(0)
      result1 must beRight(1)

      job0Finished must beTrue
      job1Finished must beTrue
    }

    "kill all" in {
      val taskManager = TaskManager(2)

      var job0Started = false
      var job1Started = false

      var job0Finished = false
      var job1Finished = false

      val firstJobCompletionSemaphore = new Semaphore(0, true)
      val secondJobCompletionSemaphore = new Semaphore(0, true)

      val result0 = taskManager.addJob(() => {
        job0Started = true
        firstJobCompletionSemaphore.release()
        Thread.sleep(Long.MaxValue)
        job0Finished = true
      }, PriorityTaskStrategy(0))

      val result1 = taskManager.addJob(() => {
        job1Started = true
        secondJobCompletionSemaphore.release()
        Thread.sleep(Long.MaxValue)
        job1Finished = true
      }, PriorityTaskStrategy(1))

      taskManager.killAllJobs()

      result0 must beRight(0)
      result1 must beRight(1)

      job0Started must beTrue
      job1Started must beTrue

      job0Finished must beFalse
      job1Finished must beFalse
    }

    "kill by pid" in {
      val taskManager = TaskManager(2)

      var job1Started = false

      var job0Finished = false
      var job1Finished = false

      val firstJobCompletionSemaphore = new Semaphore(0, true)
      val secondJobCompletionSemaphore = new Semaphore(0, true)

      val result0 = taskManager.addJob(() => {
        job0Finished = true
        firstJobCompletionSemaphore.release()
      }, PriorityTaskStrategy(0))

      val result1 = taskManager.addJob(() => {
        job1Started = true
        secondJobCompletionSemaphore.release()
        Thread.sleep(Long.MaxValue)
        job1Finished = true
      }, PriorityTaskStrategy(0))

      firstJobCompletionSemaphore.acquire()
      secondJobCompletionSemaphore.acquire()
      taskManager.killPid(1)

      result0 must beRight(0)
      result1 must beRight(1)

      job1Started must beTrue

      job0Finished must beTrue
      job1Finished must beFalse
    }

    "kill by priority" in {
      val taskManager = TaskManager(2)

      var job1Started = false

      var job0Finished = false
      var job1Finished = false

      val firstJobCompletionSemaphore = new Semaphore(0, true)
      val secondJobCompletionSemaphore = new Semaphore(0, true)

      val result0 = taskManager.addJob(() => {
        job0Finished = true
        firstJobCompletionSemaphore.release()
      }, PriorityTaskStrategy(0))

      val result1 = taskManager.addJob(() => {
        job1Started = true
        secondJobCompletionSemaphore.release()
        Thread.sleep(Long.MaxValue)
        job1Finished = true
      }, PriorityTaskStrategy(1))

      firstJobCompletionSemaphore.acquire()
      secondJobCompletionSemaphore.acquire()
      taskManager.killAllForPriority(1)

      result0 must beRight(0)
      result1 must beRight(1)

      job1Started must beTrue

      job0Finished must beTrue
      job1Finished must beFalse
    }

  }

}
