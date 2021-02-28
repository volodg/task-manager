package org.gorbv.taskmanager.actors

import akka.actor.{Props, typed}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.scaladsl.adapter._
import akka.actor.typed.{ActorRef, Behavior}
import org.gorbv.taskmanager.{DefaultTaskStrategy, FifoTaskStrategy, PriorityTaskStrategy, TaskStrategy}
import org.gorbv.taskmanager.actors.TaskManagerActor.{AddJob, Command, KillAll, KillJob, KillJobsByPriority, OnCompleteJob}
import org.gorbv.taskmanager.process.Process.{Priority, _}
import org.gorbv.taskmanager.processpool.{ContainerStrategy, DefaultStrategy, FifoStrategy, PriorityStrategy, ProcessPool}

object TaskManagerActor {

  def apply()(implicit processPool: ProcessPool[Job, Priority, ActorRef[CPUActor.Command]]): Behavior[Command] =
    Behaviors.setup(context => new TaskManagerActor(context))

  sealed trait Command
  case class AddJob(job: Job, strategy: TaskStrategy, replyTo: typed.ActorRef[Either[CreateProcessError, PID]]) extends Command
  case class KillJob(pid: PID) extends Command
  case class KillJobsByPriority(priority: Int) extends Command
  object KillAll extends Command

  //CPU commands
  case class OnCompleteJob(pid: PID) extends Command
}

final class TaskManagerActor
(context: ActorContext[Command])
(implicit val processPool: ProcessPool[Job, Priority, ActorRef[CPUActor.Command]])

  extends AbstractBehavior[Command](context) {

  type CPUActorType = ActorRef[CPUActor.Command]

  override def onMessage(msg: Command): Behavior[Command] =
    msg match {
      case AddJob(job, strategy, replyTo) =>
        val onCreate: PID => CPUActorType = _ => {
          context.actorOf(Props(classOf[CPUActor]))
        }
        val onDelete: CPUActorType => Unit = _ ! CPUActor.KillJob

        val jobStrategy: ContainerStrategy[PID, CPUActorType] = strategy match {
          case DefaultTaskStrategy => DefaultStrategy()
          case FifoTaskStrategy => FifoStrategy((_, ex) => onDelete(ex))
          case PriorityTaskStrategy(priority: Priority) => PriorityStrategy((_, ex) => onDelete(ex), priority)
        }

        processPool.addJob(jobStrategy, onCreate) match {
          case Right((newPid, cpuRef)) =>
            cpuRef ! CPUActor.RunJob(job, newPid, context.self)
            replyTo ! Right(newPid)
          case Left(_) =>
            replyTo ! Left(Busy)
        }
        this
      case KillJob(pid) =>
        processPool.jobForId(pid) match {
          case Some(cpuRef) =>
            context.log.debug(s"killing pid: $pid")
            processPool.removeJob(pid)
            cpuRef ! CPUActor.KillJob
          case None =>
            context.log.info(s"no job with pid: $pid")
        }
        this
      case KillJobsByPriority(priority) =>
        processPool.jobsForPriority(priority).foreach(_ ! CPUActor.KillJob)
        processPool.removeJobForPriority(priority)
        this
      case KillAll  =>
        processPool.allJobs.foreach(_ ! CPUActor.KillJob)
        processPool.removeAll()
        this
      case OnCompleteJob(pid) =>
        processPool.removeJob(pid)
        this
    }
}
