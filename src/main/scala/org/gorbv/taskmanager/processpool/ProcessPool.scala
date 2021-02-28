package org.gorbv.taskmanager.processpool

import org.gorbv.taskmanager.process.Process
import org.gorbv.taskmanager.process.Process.{Busy, CreateProcessError, Priority}

sealed trait SortOrder
object ByCreationTime extends SortOrder
object ByPriority extends SortOrder
object ByPID extends SortOrder

final class ProcessPool[ProcessT, PriorityT, Executor](maxSize: Int) {

  private var lastPid: Process.PID = -1

  def generateUniquePid(): Process.PID = {
    lastPid += 1
    lastPid
  }

  private lazy val container = FixedCapacityContainer[Process.PID, Executor](maxSize)

  def addJob(
    strategy: ContainerStrategy[Process.PID, Executor],
    valueFactory: Process.PID => Executor
  ): Either[CreateProcessError, (Process.PID, Executor)] =
    container.put(strategy, () => generateUniquePid(), valueFactory) match {
      case Some(result) => Right(result)
      case None => Left(Busy)
    }

  def jobForId(pid: Process.PID): Option[Executor] =
    container.valueForKey(pid)

  def jobsForPriority(priority: Priority): Iterable[Executor] =
    container.valuesForPriority(priority)

  def removeJobForPriority(priority: Priority): Unit =
    container.removeValuesForPriority(priority)

  def removeJob(pid: Process.PID): Unit =
    container.remove(pid)

  def allJobs: Iterable[Executor] =
    container.allValues

  def removeAll(): Unit =
    container.clear()

  def sortedJobs(sortOrder: SortOrder): Iterable[Executor] = {
    sortOrder match {
      case ByCreationTime | ByPID =>
        //PIDs number are incremental, so it's enough just to enumerate our fifo list
        container.allValues
      case ByPriority => container.prioritySortedJobs
    }
  }

}
