package org.gorbv.taskmanager

import org.specs2.mutable.Specification

import java.util.concurrent.Semaphore

final class FifoTaskManagerSpec extends Specification {

  sequential

  "FifoContainer" should {

    "try to execute two tasks in a single cpu" in {

      val taskManager = TaskManager(1)

      val secondJobCompletionSemaphore = new Semaphore(0, true)

      var job0Finished = false
      var job1Finished = false

      val result0 = taskManager.addJob(() => {
        Thread.sleep(Long.MaxValue)
        job0Finished = true
      }, FifoTaskStrategy)

      val result1 = taskManager.addJob(() => {
        job1Finished = true
        secondJobCompletionSemaphore.release()
      }, FifoTaskStrategy)

      result0 must beRight(0)
      result1 must beRight(1)

      secondJobCompletionSemaphore.acquire()

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
      }, FifoTaskStrategy)

      firstJobCompletionSemaphore.acquire()
      taskManager.killAllJobs()

      val result1 = taskManager.addJob(() => {
        job1Finished = true
        secondJobCompletionSemaphore.release()
      }, FifoTaskStrategy)

      result0 must beRight(0)
      result1 must beRight(1)

      secondJobCompletionSemaphore.acquire()

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
      }, FifoTaskStrategy)

      val result1 = taskManager.addJob(() => {
        job1Started = true
        secondJobCompletionSemaphore.release()
        Thread.sleep(Long.MaxValue)
        job1Finished = true
      }, FifoTaskStrategy)

      firstJobCompletionSemaphore.acquire()
      secondJobCompletionSemaphore.acquire()
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
      }, FifoTaskStrategy)

      val result1 = taskManager.addJob(() => {
        job1Started = true
        secondJobCompletionSemaphore.release()
        Thread.sleep(Long.MaxValue)
        job1Finished = true
      }, FifoTaskStrategy)

      firstJobCompletionSemaphore.acquire()
      secondJobCompletionSemaphore.acquire()
      taskManager.killPid(1)

      result0 must beRight(0)
      result1 must beRight(1)

      job1Started must beTrue

      job0Finished must beTrue
      job1Finished must beFalse
    }

  }

}
