## Chapter05: Parallel Composition And Parallel Construction

This chapter concentrates on *parallelism related combinator*, in particular on the program combinator `PAR_WITH`,
modeling *parallel composition* and the program combinator `PAR_AND`, modeling *parallel construction*.

### Parallel Composition

### `parallel.ProgramSpec`

- *Parallel programs can compose programs in parallel*.

- *Parallel programs can construct information in parallel*.

Let

```scala
package api.specification.parallel

import types.And

trait Composition[Program[-_, +_]]:

  extension [Z, Y, X, W](p_z2x: Program[Z, X])
    infix def PAR_WITH(p_y2w: Program[Y, W]): Program[And[Z, Y], And[X, W]]
```

and

```scala
package api.specification.parallel

import types.And

trait Construction[Program[-_, +_]]:

  extension [Z, Y, X](p_z2y: Program[Z, Y])
    infix def PAR_AND(p_z2x: Program[Z, X]): Program[Z, And[Y, X]]
```

in

```scala
package api.specification.parallel

import types.And

import api.specification.{ProgramSpec => _, *}

trait ProgramSpec[Program[-_, +_]]
    extends api.specification.ProgramSpec[Program],
      parallel.Composition[Program],
      parallel.Construction[Program]:

  extension [Z, Y, X](p_z2y: Program[Z, Y])
    infix def PAR_AND(p_z2x: Program[Z, X]): Program[Z, And[Y, X]] =
      DUPLICATE AND_THEN (p_z2y PAR_WITH p_z2x)
```

`PAR_WITH` resp. `PAR_AND` are a *pointfree program combinators*, only programs are involved. They model a
*generic abstraction* of *composing functions in parallel* resp. *functions producing tuples in parallel*.

### `reactive.Function` `parallel.programImpl`

```scala
package api.implementations.reactive.parallel

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
    infix def SEQ_AND(f_z2cbhx: reactive.Function[Z, X]): reactive.Function[Z, And[Y, X]] =
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

          import akka.actor.typed.{ActorSystem, ActorRef, Behavior}

          import akka.actor.typed.scaladsl.{Behaviors}

          import Behaviors.{receive, stopped}

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
              receive { (_, _) =>
                { (x: X) => reactor ! LeftReact(x) } bind (z bind f_z2cbhx)
                stopped
              }

            def apply(reactor: ActorRef[React[X, W]]) = leftAct

          object RightActor:

            case object RightAct

            lazy val rightAct: Behavior[RightAct.type] =
              receive { (_, _) =>
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
              receive { (_, message) =>
                message match {
                  case LeftReact(x) =>
                    `option[w]` match {
                      case Some(w) =>
                        (x, w) bind cbxaw
                        stopped
                      case None =>
                        (Some(x), None) bind react
                    }
                  case RightReact(w) =>
                    `option[x]` match {
                      case Some(x) =>
                        (x, w) bind cbxaw
                        stopped
                      case None =>
                        (None, Some(w)) bind react
                    }
                }
              }

            def apply() = (None, None) bind react

          leftActor ! LeftAct
          rightActor ! RightAct
```

The definitions of `reactive.parallel.programImpl` of the decared `ProgramSpec` members delegate to
`reactive.programImpl`. The definition of `reactive.parallel.programImpl` of the decared `ParallelComposition` member
uses *actors* from `Scala`'s `Akka` library. `reactor` delegates callback handling to `leftActor` and `rightActor`, and,
when both finished acting, `reactor` reacts with its callback handling.

### `parallel.Programs`

```scala
package examples.programs.parallel.composite

import utilities.bind

import api.specification.parallel

import examples.programs.primitive

trait Programs[Program[-_, +_]: parallel.ProgramSpec]
    extends primitive.Programs[Program]:

  val summonedParallelProgramSpec: parallel.ProgramSpec[Program] =
    summon[parallel.ProgramSpec[Program]]
  import summonedParallelProgramSpec.IF

  val fibonacci: Program[BigInt, BigInt] =
    IF(isZero) {
      one
    } ELSE {
      IF(isOne) {
        one
      } ELSE {
        (subtractOne AND_THEN fibonacci) PAR_AND (subtractTwo AND_THEN fibonacci) AND_THEN add
      }
    }
```

### `parallel.Mains`

```scala
package examples.mains.parallel

import utilities.bind

import api.specification.{MainProgram, MainProgramToMainFunction}

import api.specification.parallel

import examples.programs.parallel.composite

import examples.programs.effectful

trait Mains[
    Program[-_, +_]
      : parallel.ProgramSpec
      : [Program[-_, +_]] =>> MainProgramToMainFunction[Program, A, B],
    A,
    B
] extends composite.Programs[Program],
      effectful.primitive.Programs[Program],
      effectful.composite.Programs[Program]:

  private val summonedMainProgramToMainFunction =
    summon[MainProgramToMainFunction[Program, A, B]]
  import summonedMainProgramToMainFunction.{MainFunction, MAIN_PROGRAM_TO_MAIN_FUNCTION}

  val fibonacciMain: MainFunction =
    readBigIntArgument AND_THEN (fibonacci IN writeArgumentAndResult(
      "fibonacci"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION
```

### `reactive.parallel.main`

```scala
package examples.applications.reactive.parallel

import utilities.bind

import api.implementations.reactive

import reactive.parallel.programImpl

import reactive.mainProgramToMainFunctionImpl

import examples.mains.parallel

object mains extends parallel.Mains[reactive.Function, Unit, Unit]

import mains.fibonacciMain

@main def main(): Unit =
  () bind fibonacciMain
```

### `logback.xml`

Below is the logging configuration file. For the moment there is only `ERROR` logging because
the code of `reactive.Function` `parallel.programImpl` does not do any `INFO` logging.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<configuration>

    <appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
        <encoder>
            <pattern>[%date{ISO8601}] - %msg %n</pattern>
        </encoder>
    </appender>

    <appender name="ASYNC" class="ch.qos.logback.classic.AsyncAppender">
        <queueSize>8192</queueSize>
        <neverBlock>true</neverBlock>
        <appender-ref ref="STDOUT" />
    </appender>

    <root level="ERROR">
        <appender-ref ref="ASYNC"/>
    </root>

</configuration>
```

### Running the applications

```scala
sbt:programming_course> run

Multiple main classes detected. Select one to run:
 [1] examples.applications.active.main
 [2] examples.applications.reactive.main
 [3] examples.applications.reactive.parallel.main

Enter number: 3
[info] running examples.applications.reactive.parallel.main 

please type a BigInt argument
10
[success] ...
the result of binding argument 10 to fibonacci is 89
```

The `[success] ...` message of `sbt` comes before the output because parallelism is involved.

### `reactive.Function` `parallel.logging.programImpl`

```scala
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
    infix def SEQ_AND(f_z2cbhx: reactive.Function[Z, X]): reactive.Function[Z, And[Y, X]] =
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
```

The code of `reactive.Function` `parallel.logging.programImpl` does `INFO` logging.

### Input/Output, IO For Short

Let

```scala
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
      val packageName = "functions.effectful"
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
```

in

```scala
package examples.programs.effectful.primitive

// added for chapter03
import types.And
//

import utilities.bind

import api.specification.ProgramSpec

import examples.functions.effectful.{
  readBigIntArgumentFunction,
  writeResultFunction,
  // added for chapter03
  writeArgumentAndResultFunction,
  //
  // added for chapter05
  logArgumentAndResultFunction
  //
}

trait Programs[Program[-_, +_]: ProgramSpec]:

  private val summonedProgramSpec = summon[ProgramSpec[Program]]
  import summonedProgramSpec.FUNCTION_TO_PROGRAM

  val readBigIntArgument: Program[Unit, BigInt] =
    readBigIntArgumentFunction bind FUNCTION_TO_PROGRAM

  def writeResult[Z](programName: String): Program[Z, Unit] =
    programName bind writeResultFunction bind FUNCTION_TO_PROGRAM

  // added for chapter03
  def writeArgumentAndResult[Z, Y](programName: String): Program[And[Z, Y], Unit] =
    programName bind writeArgumentAndResultFunction bind FUNCTION_TO_PROGRAM
  //

  // added for chapter05
  def logArgumentAndResult[Z, Y](programName: String): Program[And[Z, Y], Unit] =
    programName bind logArgumentAndResultFunction bind FUNCTION_TO_PROGRAM
  //
```

`logArgumentAndResultFunction` and `logArgumentAndResult` have been added. The code of `logArgumentAndResult` does
`INFO` logging.

This is cheating! 

The idea behind lifting up is that only effectfree primitive functions are lifted up to programs. The code above lifts
up *effectful IO functions* to programs. A later chapter will treat *IO* in a more disciplined way.

### `parallel.logging.Mains`

```scala
package examples.mains.parallel.logging

import utilities.bind

import api.specification.{MainProgram, MainProgramToMainFunction}

import api.specification.parallel

import examples.programs.parallel.composite

import examples.programs.effectful

trait Mains[
    Program[-_, +_]
      : parallel.ProgramSpec
      : [Program[-_, +_]] =>> MainProgramToMainFunction[Program, A, B],
    A,
    B
] extends composite.Programs[Program],
      effectful.primitive.Programs[Program],
      effectful.composite.Programs[Program]:

  private val summonedMainProgramToMainFunction =
    summon[MainProgramToMainFunction[Program, A, B]]
  import summonedMainProgramToMainFunction.{MainFunction, MAIN_PROGRAM_TO_MAIN_FUNCTION}

  val fibonacciMain: MainFunction =
    readBigIntArgument AND_THEN (fibonacci IN logArgumentAndResult(
      "fibonacci"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION
```

### `reactive.parallel.logging.main`

```scala
package examples.applications.reactive.parallel.logging

import utilities.bind

import api.implementations.reactive

import reactive.parallel.logging.programImpl

import reactive.mainProgramToMainFunctionImpl

import examples.mains.parallel

object mains extends parallel.logging.Mains[reactive.Function, Unit, Unit]

import mains.fibonacciMain

@main def main(): Unit =
  () bind fibonacciMain
```

### Running the applications

```scala
sbt:programming_course> run

Multiple main classes detected. Select one to run:
 [1] examples.applications.active.main
 [2] examples.applications.reactive.main
 [3] examples.applications.reactive.parallel.logging.main
 [4] examples.applications.reactive.parallel.main

Enter number: 3
[info] running examples.applications.reactive.parallel.logging.main 

please type a BigInt argument
5
[... 10:58:27,304] - leftActor received LeftAct (upon 5) 
[... 10:58:27,318] - rightActor received RightAct (upon 5) 
[success] Total time: 5 s, completed Sep 11, 2024, 10:58:27 AM
[... 10:58:27,347] - leftActor received LeftAct (upon 4) 
[... 10:58:27,366] - leftActor received LeftAct (upon 3) 
[... 10:58:27,373] - rightActor received RightAct (upon 4) 
[... 10:58:27,426] - rightActor received RightAct (upon 3) 
[... 10:58:27,430] - reactor only received LeftReact(1) 
[... 10:58:27,446] - leftActor received LeftAct (upon 3) 
[... 10:58:27,452] - leftActor received LeftAct (upon 2) 
[... 10:58:27,452] - leftActor received LeftAct (upon 2) 
[... 10:58:27,453] - reactor only received LeftReact(1) 
[... 10:58:27,456] - reactor only received LeftReact(1) 
[... 10:58:27,499] - rightActor received RightAct (upon 2) 
[... 10:58:27,499] - rightActor received RightAct (upon 3) 
[... 10:58:27,499] - reactor only received LeftReact(1) 
[... 10:58:27,503] - rightActor received RightAct (upon 2) 
[... 10:58:27,506] - reactor received both RightReact(1) and LeftReact(1) 
[... 10:58:27,508] - reactor received both RightReact(1) and LeftReact(1) 
[... 10:58:27,511] - reactor only received LeftReact(2) 
[... 10:58:27,514] - reactor received both LeftReact(2) and RightReact(1) 
[... 10:58:27,518] - reactor only received LeftReact(3) 
[... 10:58:27,550] - leftActor received LeftAct (upon 2) 
[... 10:58:27,551] - reactor only received LeftReact(1) 
[... 10:58:27,568] - rightActor received RightAct (upon 2) 
[... 10:58:27,568] - reactor received both RightReact(1) and LeftReact(1) 
[... 10:58:27,570] - reactor received both LeftReact(2) and RightReact(1) 
[... 10:58:27,573] - reactor received both LeftReact(3) and RightReact(2) 
[... 10:58:27,573] - reactor received both LeftReact(5) and RightReact(3) 
[... 10:58:27,601] - the result of binding argument 5 to fibonacci is 
```

The `[success] ...` message of `sbt` still comes in the middle of the output because parallelism is involved, but the
logging of the result of binding an argument is that last one.

### Conclusion

We can now execute the computations of programs in parallel. But, as promised in the introduction, maybe we want more
program features. Maybe we want the computations of programs to perform side-effects.

Please keep on reading ... .
