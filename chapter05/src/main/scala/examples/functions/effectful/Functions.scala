package examples.functions.effectful

import scala.io.StdIn.readInt

// added for chapter03
import types.And
//

val readBigIntArgumentFunction: Function[Unit, BigInt] = u =>
  println()
  println("please type a BigInt argument")
  readInt()

def writeResultFunction[Z](programName: String): Function[Z, Unit] = i =>
  println(s"the result of applying $programName is $i")

// added for chapter03
def writeArgumentAndResultFunction[Z, Y](programName: String): And[Z, Y] => Unit =
  (z, y) => println(s"the result of binding argument $z to $programName is $y")
//

// added for chapter06
def logArgumentAndResultFunction[Z, Y](programName: String): Tuple2[Z, Y] => Unit =
  (z, y) =>

    import ch.qos.logback.classic.{Logger, LoggerContext, Level}

    import Level.{INFO, ERROR}

    import org.slf4j.LoggerFactory.getILoggerFactory

    import akka.actor.typed.scaladsl.{ActorContext}

    import akka.actor.typed.{ActorSystem, Behavior}

    import akka.actor.typed.scaladsl.{Behaviors}

    import Behaviors.{receive, stopped}

    def log[X](actorContext: ActorContext[X])(message: String): Unit =
      val packageName = "examples.functions.effectful"
      val logger: Logger =
        getILoggerFactory().asInstanceOf[LoggerContext].getLogger(packageName)
      logger.setLevel(INFO)
      actorContext.log.info(message)
      logger.setLevel(ERROR)

    object LogActor:

      case class Log(argumentAndResult: And[Z, Y])

      def apply(): Behavior[Log] =
        receive { (context, message) =>
          val (z, y): Tuple2[Z, Y] = message.argumentAndResult
          log(context)(s"the result of binding argument $z to $programName is $y")
          stopped
        }

    import LogActor.Log

    lazy val logActor = ActorSystem(LogActor(), "logActor")

    logActor ! Log((z, y))
//
