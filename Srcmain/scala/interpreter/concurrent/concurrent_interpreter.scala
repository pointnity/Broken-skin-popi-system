package interpreter.concurrent

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import akka.util.Timeout
import akka.actor._
import akka.pattern.{Patterns, ask}
import syntax._
import interpreter._

/**
 * This module gives an interpreted concurrent implementation of the nodes
 * language using the akka library. There are 4 major classes used in this
 * implementation. These are:
 *    - Launcher: responsible for initiating the interpreter and interfacing
 *      with the outside. One instance per invocation.
 *    - ProcManager: a one-per-invocation actor that manages and keeps track of
 *      all other actors in the system. Creates new ProcRunners and Channels
 *      when required, when requested to do so by a ProcRunner, either because
 *      a new channel has been created, or because a fork (parallel composition
 *      under an input or output) has been executed.
 *    - ProcRunner: One per source-level process. Responsible for 'executing'
 *      the process it contains, communicating with other ProcRunners via
 *      Channels to exchange messages. A ProcRunner keeps a mapping between
 *      ChanLiteral Names and the ActorRefs that represent those channels at
 *      runtime. Every time a ProcRunner sends a message, it must also send any
 *      mappings required for any ChanLiterals present in the sent message.
 *    - Channel: Represents a pi calculus channel. At the implementation level,
 *      it is a sort of latch - it waits for a sender and a receiver to tell it
 *      that they each want to send and receive a message respectively, on this
 *      channel. When this occurs, the messgage (and required channel mappings)
 *      are taken from the sender and given to the receiver.
 */

class Launcher(
    p: Proc,
    printName: Option[Name],
    nextName: Name,
    names: Map[Name, String],
    onCompletion: Function1[Proc, Unit],
    procRunnerClass: Class[_ <: ProcRunner] = classOf[ProcRunner]) {

  var result: Option[Proc] = None

  val (system: ActorSystem, procManager: ActorRef) = {

    val sys: ActorSystem = ActorSystem("Launcher")

    sys.registerOnTermination(new Runnable {
      def run: Unit = onCompletion(result.get)
    })

    val procManager: ActorRef = sys.actorOf(
      Props(classOf[ProcManager], this, nextName, procRunnerClass),
      "ProcManager")

    val initChanMap: Map[Name, ActorRef] = (p.chanLiterals map {

      case n if (Some(n) == printName) => (n, sys.actorOf(
        Props(classOf[PrintingChannel], names, procManager), s"LIT$$print"))

      case n if (Some(n) != printName) => (n, sys.actorOf(
        Props(classOf[Channel], procManager), s"LIT${names(n)}"))

    }).toMap

    procManager ! SetLiveActors(initChanMap.values.toSet)

    procManager ! MakeRunner(None, initChanMap, p)

    sys.scheduler.schedule(3.seconds, 3.seconds, procManager,
      CheckFinished)(sys.dispatcher)

    (sys, procManager)
  }
}
