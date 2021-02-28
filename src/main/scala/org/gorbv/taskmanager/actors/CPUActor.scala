package org.gorbv.taskmanager.actors

import akka.actor.{FSM, typed}
import org.gorbv.taskmanager.actors.CPUActor.{Active, CompleteJob, Data, Idle, KillJob, RunJob, RunningJob, State, Uninitialized}
import org.gorbv.taskmanager.actors.TaskManagerActor.OnCompleteJob
import org.gorbv.taskmanager.process.Process.{Job, PID}

import java.util.concurrent.{ExecutorService, Executors, Future => JavaFuture}

object CPUActor {

  sealed trait Command
  case class RunJob(job: Job, pid: PID, onComplete: typed.ActorRef[TaskManagerActor.Command]) extends Command
  object KillJob extends Command
  case class CompleteJob(pid: PID) extends Command

  sealed trait State
  object Idle extends State
  object Active extends State

  sealed trait Data
  case object Uninitialized extends Data
  final case class RunningJob(pid: PID, jobFuture: JavaFuture[Unit], onComplete: typed.ActorRef[TaskManagerActor.Command]) extends Data
}

final class CPUActor extends FSM[State, Data] {

  private val executor: ExecutorService = Executors.newFixedThreadPool(1)

  startWith(Idle, Uninitialized)

  when(Idle) {
    case Event(RunJob(job, pid, onComplete), Uninitialized) =>
      val future = executor.submit(() => {
        job.run()
        context.self ! CompleteJob(pid)
      }, ())
      goto(Active).using(RunningJob(pid, future, onComplete))
  }

  when(Active) {
    case Event(CompleteJob(completePid), RunningJob(activePid, _, onComplete)) =>
      if (completePid == activePid) {
        onComplete ! OnCompleteJob(activePid)
        goto(Idle).using(Uninitialized)
      } else {
        log.warning(s"invalid complete job pid: $completePid, active pid is: $activePid")
        stay()
      }
    case Event(KillJob, RunningJob(pid, jobFuture, _)) =>
      log.debug(s"cpu stop job in actor: ${context.self.path} pid: $pid")
      jobFuture.cancel(true)
      goto(Idle).using(Uninitialized)
  }

  whenUnhandled {
    case Event(e, s) =>
      log.warning(s"received unhandled request $e in state $stateName/$s")
      stay()
  }

  initialize()

}
