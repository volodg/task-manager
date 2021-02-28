package org.gorbv.taskmanager

import akka.actor.typed
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.util.Timeout
import org.gorbv.taskmanager.actors.{CPUActor, TaskManagerActor}
import org.gorbv.taskmanager.process.Process
import org.gorbv.taskmanager.process.Process.{CreateProcessError, Job, PID, Priority, cpuCount}
import org.gorbv.taskmanager.processpool.ProcessPool

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

sealed trait TaskStrategy
object DefaultTaskStrategy extends TaskStrategy
object FifoTaskStrategy extends TaskStrategy
case class PriorityTaskStrategy(priority: Priority) extends TaskStrategy

final case class TaskManager private[taskmanager](cpuCount: Int = cpuCount) {

  type CPUActorRef = ActorRef[CPUActor.Command]

  private implicit val processStrategy: ProcessPool[Job, Priority, CPUActorRef] = new ProcessPool(cpuCount)

  private val actorSystem = ActorSystem(TaskManagerActor(), "TaskManagerActorSystem")

  private implicit val timeout: Timeout = Timeout(100.seconds)
  private implicit val schedule: typed.Scheduler = actorSystem.scheduler

  def addJob(job: Job, strategy: TaskStrategy): Either[CreateProcessError, Process.PID] = {
    val future = actorSystem ? (replyTo => TaskManagerActor.AddJob(job, strategy, replyTo))
    Await.result(future, timeout.duration)
  }

  def killPid(pid: PID): Unit =
    actorSystem ! TaskManagerActor.KillJob(pid)

  def killAllForPriority(priority: Priority): Unit =
    actorSystem ! TaskManagerActor.KillJobsByPriority(priority)

  def killAllJobs(): Unit =
    actorSystem ! TaskManagerActor.KillAll

}
