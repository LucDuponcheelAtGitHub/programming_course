package api.implementations.reactive.parallel.logging

import types.{And, Or}

import utilities.bind

import api.specification.parallel

import api.implementations.reactive

given programImpl: parallel.ProgramSpec[reactive.Function] with

  def FUNCTION_TO_PROGRAM[Z, Y]: Function[Z, Y] => reactive.Function[Z, Y] =
    reactive.programImpl.FUNCTION_TO_PROGRAM

  extension [Z, Y, X](f_z2cbhy: reactive.Function[Z, Y])
    infix def AND_THEN(f_y2cbhx: => reactive.Function[Y, X]): reactive.Function[Z, X] =
      reactive.programImpl.AND_THEN(f_z2cbhy)(f_y2cbhx)

  extension [Z, Y, X](f_z2cbhy: reactive.Function[Z, Y])
    infix def SEQ_AND(
        f_z2cbhx: => reactive.Function[Z, X]
    ): reactive.Function[Z, And[Y, X]] =
      reactive.programImpl.SEQ_AND(f_z2cbhy)(f_z2cbhx)

  extension [Z, Y, X](f_x2cbhz: => reactive.Function[X, Z])
    infix def OR(f_y2cbhz: => reactive.Function[Y, Z]): reactive.Function[Either[X, Y], Z] =
      reactive.programImpl.OR(f_x2cbhz)(f_y2cbhz)

  extension [Z, Y, X, W](f_z2cbhx: reactive.Function[Z, X])
    infix def PAR_WITH(
        f_y2cbhw: reactive.Function[Y, W]
    ): reactive.Function[And[Z, Y], And[X, W]] =
      (z, y) =>
        cbxaw =>

          import ch.qos.logback.classic.{Logger, LoggerContext, Level}

          import Level.{INFO, ERROR}

          import org.slf4j.LoggerFactory.getILoggerFactory

          import akka.actor.typed.scaladsl.{ActorContext}

          import akka.actor.typed.{ActorSystem, ActorRef, Behavior}

          import akka.actor.typed.scaladsl.{Behaviors}

          import Behaviors.{receive, stopped}

          def log[V](actorContext: ActorContext[V])(message: String): Unit =
            val packageName = "api.implementations.reactive.parallel.logging"
            val logger: Logger =
              getILoggerFactory().asInstanceOf[LoggerContext].getLogger(packageName)
            logger.setLevel(INFO)
            actorContext.log.info(message)
            logger.setLevel(ERROR)

          lazy val reactor = ActorSystem(Reactor(), s"reactor")
          lazy val leftActor = ActorSystem(LeftActor(reactor), s"leftActor")
          lazy val rightActor = ActorSystem(RightActor(reactor), s"rightActor")

          import Reactor.React
          import React.{LeftReact, RightReact}

          import LeftActor.LeftAct
          import RightActor.RightAct

          object LeftActor:

            case object LeftAct

            lazy val leftAct: Behavior[LeftAct.type] =
              receive { (context, _) =>
                log(context)(s"leftActor received LeftAct (upon $z)")
                { (x: X) => reactor ! LeftReact(x) } bind (z bind f_z2cbhx)
                stopped
              }

            def apply(reactor: ActorRef[React[X, W]]) = leftAct

          object RightActor:

            case object RightAct

            lazy val rightAct: Behavior[RightAct.type] =
              receive { (context, _) =>
                log(context)(s"rightActor received RightAct (upon $y)")
                { (w: W) => reactor ! RightReact(w) } bind (y bind f_y2cbhw)
                stopped
              }

            def apply(reactor: ActorRef[React[X, W]]) = rightAct

          object Reactor:

            enum React[X, W]:
              case LeftReact(x: X) extends React[X, W]
              case RightReact(w: W) extends React[X, W]

            def react(
                `option[x]`: Option[X],
                `option[w]`: Option[W]
            ): Behavior[React[X, W]] =
              receive { (context, message) =>
                message match {
                  case LeftReact(x) =>
                    `option[w]` match {
                      case Some(w) =>
                        log(context)(
                          s"reactor received both LeftReact($x) and RightReact($w)"
                        )
                        (x, w) bind cbxaw
                        stopped
                      case None =>
                        log(context)(s"reactor only received LeftReact($x)")
                        (Some(x), None) bind react
                    }
                  case RightReact(w) =>
                    `option[x]` match {
                      case Some(x) =>
                        log(context)(
                          s"reactor received both RightReact($x) and LeftReact($w)"
                        )
                        (x, w) bind cbxaw
                        stopped
                      case None =>
                        log(context)(s"reactor only received LeftReact($w)")
                        (None, Some(w)) bind react
                    }
                }
              }

            def apply() = (None, None) bind react

          leftActor ! LeftAct
          rightActor ! RightAct
