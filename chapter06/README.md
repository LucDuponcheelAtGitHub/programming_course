## Chapter06: Stateful Functionality

This chapter concentrates on *stateful functionality related combinators*, in particular on the program combinator
`READ_STATE`, modeling the *reading internal state side-effect* and the program combinator `WRITE_STATE`, modeling the
*writing internal state side-effect*.

 ### `StatefulProgram`

- *Stateful programs can manipulate internal state*.

Programs that manipulate internal state are called *(internal) state manipulating programs*,
*state manipulating programs* for short.

 Let

 ```scala
package api.specification.stateful

trait Manipulation[S, Program[-_, +_]]:

  val READ_STATE: Program[Unit, S]

  val WRITE_STATE: Program[S, Unit]
 ```

 in

 ```scala
package api.specification.stateful

import utilities.bind

import api.specification.{ProgramSpec => _, *}

trait ProgramSpec[S, Program[-_, +_]]
    extends api.specification.ProgramSpec[Program],
      Manipulation[S, Program]:

  def MODIFY_STATE_WITH[Z]: Function[S, S] => Program[Z, Unit] = f_s2s =>
    { (z: Z) => () } bind FUNCTION_TO_PROGRAM AND_THEN
      READ_STATE AND_THEN
      (f_s2s bind FUNCTION_TO_PROGRAM) AND_THEN
      WRITE_STATE

  def READ_STATE_MODIFIED_WITH[Z]: Function[S, S] => Program[Z, S] = f_s2s =>
    f_s2s bind MODIFY_STATE_WITH AND_THEN READ_STATE
 ```

`READ_STATE` resp. `WRITE_STATE` are *pointfree programs*, only programs are involved. They model a
*generic abstraction* of *reading variables* resp *writing variables*.

`MODIFY_STATE_WITH` resp `READ_STATE_MODIFIED_WITH` are *pointfree function and program combinators*, only functions and
programs are involved.

Recall that `READ_STATE`, `WRITE_STATE`, `MODIFY_STATE_WITH` and `READ_STATE_MODIFIED_WITH` are
*side-effect specifications*, called *effects*.

### `stateful.seed.Programs`

Let

```scala
package examples.types

type Seed = Long
```

and

```scala
package examples.functions.primitive

// added for chapter03
import types.And
//

def oneFunction[Z]: Function[Z, BigInt] = z => 1

val subtractOneFunction: Function[BigInt, BigInt] = z => z - 1

val subtractTwoFunction: Function[BigInt, BigInt] = z => z - 2

val isZeroFunction: Function[BigInt, Boolean] = z => z == 0

val isOneFunction: Function[BigInt, Boolean] = z => z == 1

// added for chapter03
val addFunction: Function[And[BigInt, BigInt], BigInt] = (z, y) => z + y

val multiplyFunction: Function[And[BigInt, BigInt], BigInt] = (z, y) => z * y
//

// added for chapter06
val isNotNegativeFunction: BigInt => Boolean = i => i >= 0

val negateFunction: BigInt => BigInt = i => -i

val moduloMillionFunction: BigInt => BigInt = i => i % 1000000

import examples.types.Seed

val seed2randomBigIntFunction: Seed => BigInt = seed => BigInt((seed >>> 16).toInt)

val randomSeedModifierFunction: Seed => Seed = seed =>
  (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
//
```
and

```scala
package examples.programs.primitive

// added for chapter03
import types.And
//

import utilities.bind

import api.specification.ProgramSpec

import examples.functions.primitive.{
  oneFunction,
  subtractOneFunction,
  subtractTwoFunction,
  isZeroFunction,
  isOneFunction,
  // added for chapter03
  addFunction,
  multiplyFunction,
  //
  // added for chapter06
  isNotNegativeFunction,
  negateFunction,
  moduloMillionFunction,
  seed2randomBigIntFunction
  //
}

trait Programs[Program[-_, +_]: ProgramSpec]:

  private val summonedProgramSpec = summon[ProgramSpec[Program]]
  import summonedProgramSpec.FUNCTION_TO_PROGRAM

  def one[Z]: Program[Z, BigInt] = oneFunction bind FUNCTION_TO_PROGRAM

  val subtractOne: Program[BigInt, BigInt] = subtractOneFunction bind FUNCTION_TO_PROGRAM

  val subtractTwo: Program[BigInt, BigInt] = subtractTwoFunction bind FUNCTION_TO_PROGRAM

  val isZero: Program[BigInt, Boolean] = isZeroFunction bind FUNCTION_TO_PROGRAM

  val isOne: Program[BigInt, Boolean] = isOneFunction bind FUNCTION_TO_PROGRAM

  // added for chapter03
  val add: Program[And[BigInt, BigInt], BigInt] =
    addFunction bind FUNCTION_TO_PROGRAM

  val multiply: Program[And[BigInt, BigInt], BigInt] =
    multiplyFunction bind FUNCTION_TO_PROGRAM
  //
  // added for chapter06

  import examples.types.Seed

  def isNotNegative: Program[BigInt, Boolean] =
    isNotNegativeFunction bind FUNCTION_TO_PROGRAM

  def negate: Program[BigInt, BigInt] = negateFunction bind FUNCTION_TO_PROGRAM

  def moduloMillion: Program[BigInt, BigInt] = moduloMillionFunction bind FUNCTION_TO_PROGRAM

  val seed2randomBigInt: Program[Seed, BigInt] =
    seed2randomBigIntFunction bind FUNCTION_TO_PROGRAM
  //
```
in

```scala
package examples.programs.stateful.primitive

import utilities.bind

import api.specification.stateful

import examples.types.Seed

import examples.functions.primitive.randomSeedModifierFunction

import examples.programs.primitive

trait Programs[Program[-_, +_]: [Program[-_, +_]] =>> stateful.ProgramSpec[Seed, Program]]
    extends primitive.Programs[Program]:

  private val summonedStatefulProgramSpec: stateful.ProgramSpec[Seed, Program] =
    summon[stateful.ProgramSpec[Seed, Program]]
  import summonedStatefulProgramSpec.READ_STATE_MODIFIED_WITH

  def readModifiedSeed[Z]: Program[Z, Seed] =
    randomSeedModifierFunction bind READ_STATE_MODIFIED_WITH
```

`readModifiedSeed` is a *primitive stateful program*.

and

```scala
package examples.programs.stateful.composite

import types.And

import utilities.bind

import api.specification.stateful

import examples.types.Seed

import examples.functions.primitive.randomSeedModifierFunction

import examples.programs.primitive

trait Programs[Program[-_, +_]: [Program[-_, +_]] =>> stateful.ProgramSpec[Seed, Program]]
    extends primitive.Programs[Program],
      examples.programs.stateful.primitive.Programs[Program]:

  private val summonedStatefulProgramSpec: stateful.ProgramSpec[Seed, Program] =
    summon[stateful.ProgramSpec[Seed, Program]]
  import summonedStatefulProgramSpec.{IDENTITY, IF}

  def randomBigInt[Z]: Program[Z, BigInt] = readModifiedSeed AND_THEN seed2randomBigInt

  val negateIfNegative: Program[BigInt, BigInt] =
    IF(isNotNegative) {
      IDENTITY
    } ELSE {
      negate
    }

  def randomNatural[Z]: Program[Z, BigInt] =
    randomBigInt AND_THEN negateIfNegative AND_THEN moduloMillion

  def twoRandomNaturals[Z]: Program[Z, And[BigInt, BigInt]] =
    randomNatural SEQ_AND randomNatural
```

`randomBigInt`, `randomNatural` and `twoRandomNaturals` are *composite stateful programs*.

### Input/Output, IO For Short

Let

```scala
package examples.functions.effectful

import scala.io.StdIn.readInt

// added for chapter03
import types.And
//

// added for chapter06
val readUnitArgumentFunction: Function[Unit, Unit] = u => u
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
  logArgumentAndResultFunction,
  //
  // added for chapter06
  readUnitArgumentFunction
  //
}

trait Programs[Program[-_, +_]: ProgramSpec]:

  private val summonedProgramSpec = summon[ProgramSpec[Program]]
  import summonedProgramSpec.FUNCTION_TO_PROGRAM

  // added for chapter06
  val readUnitArgument: Program[Unit, Unit] =
    readUnitArgumentFunction bind FUNCTION_TO_PROGRAM
  //
  
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

`readUnitArgumentFunction` and `readUnitArgument` have been added.

### `stateful.Mains`

```scala
package examples.mains.stateful

import utilities.bind

import api.specification.{MainProgram, MainProgramToMainFunction}

import api.specification.stateful

import examples.types.Seed

import examples.programs.stateful.composite

import examples.programs.effectful

trait Mains[
    Program[-_, +_]
      : [Program[-_, +_]] =>> stateful.ProgramSpec[Seed, Program]
      : [Program[-_, +_]] =>> MainProgramToMainFunction[Program, A, B],
    A,
    B
] extends composite.Programs[Program],
      effectful.primitive.Programs[Program],
      effectful.composite.Programs[Program]:

  private val summonedMainProgramToMainFunction =
    summon[MainProgramToMainFunction[Program, A, B]]
  import summonedMainProgramToMainFunction.{MainFunction, MAIN_PROGRAM_TO_MAIN_FUNCTION}

  val twoRandomNaturalsMain: MainFunction =
    readUnitArgument AND_THEN (twoRandomNaturals IN writeArgumentAndResult(
      "twoRandomNaturals"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION
```

### `active.stateful.Function[S]` `stateful.programImpl`

Let

```scala
package types

type Expression = [Z] =>> Z

type Callback = [C[+_]] =>> [Z] =>> Function[C[Z], Unit]

type CallbackHandler = [C[+_]] =>> [Z] =>> Function[Callback[C][Z], Unit]

// added for chapter03
type And = [Z, Y] =>> Tuple2[Z, Y]
//

// added for chapter04
type Or = [Z, Y] =>> Either[Z, Y]
//

// added for chapter06
type StateHandler = [S] =>> [Z] =>> Function[S, And[Z, S]]
//
```

and

```scala
package api.implementations.active.stateful

import types.StateHandler

import api.implementations.active

type Expression = [S] =>> [Z] =>> StateHandler[S][active.Expression[Z]]
```

and

```scala
package api.implementations.active.stateful

import api.implementations.active.stateful

type Function = [S] =>> [Z, Y] =>> scala.Predef.Function[Z, stateful.Expression[S][Y]]
```
in

 ```scala
package api.implementations.active.stateful

import types.{And, Or}

import utilities.bind

import api.specification.stateful

import api.implementations.active

given programImpl[S]: stateful.ProgramSpec[S, active.stateful.Function[S]] with

  def FUNCTION_TO_PROGRAM[Z, Y]
      : scala.Predef.Function[Z, Y] => active.stateful.Function[S][Z, Y] = f_z2y =>
    z => s => (z bind f_z2y, s)

  extension [Z, Y, X](f_z2shy: active.stateful.Function[S][Z, Y])
    infix def AND_THEN(
        f_y2shx: => active.stateful.Function[S][Y, X]
    ): active.stateful.Function[S][Z, X] = z =>
      s =>
        val s0: S = s
        val (y, s1) = s0 bind (z bind f_z2shy)
        val (x, s2) = s1 bind (y bind f_y2shx)
        (x, s2)

  extension [Z, Y, X](f_z2shy: active.stateful.Function[S][Z, Y])
    infix def SEQ_AND(
        f_z2shx: => active.stateful.Function[S][Z, X]
    ): active.stateful.Function[S][Z, And[Y, X]] = z =>
      s =>
        val s0: S = s
        val (y, s1) = s0 bind (z bind f_z2shy)
        val (x, s2) = s1 bind (z bind f_z2shx)
        ((y, x), s2)

  extension [Z, Y, X](f_x2shz: => active.stateful.Function[S][X, Z])
    infix def OR(
        f_y2shz: => active.stateful.Function[S][Y, Z]
    ): active.stateful.Function[S][Or[X, Y], Z] = xoy =>
      s =>
        xoy match {
          case Left(x)  => s bind (x bind f_x2shz)
          case Right(y) => s bind (y bind f_y2shz)
        }

  val READ_STATE: active.stateful.Function[S][Unit, S] = _ => s => (s, s)

  val WRITE_STATE: active.stateful.Function[S][S, Unit] = s => _ => ((), s)
 ```

The definitions of `active.stateful.programImpl` of the decared `ProgramSpec` members are handling state when bound to
their argument. The definition of `active.stateful.programImpl` of the decared `Manipulation` members handle state
by reading as an extra argument and writing an extra result.

### `active.stateful.Function` `mainProgramToMainFunctionImpl`

```scala
package api.implementations.active.stateful

import types.And

import utilities.bind

import api.specification.{MainProgram, MainProgramToMainFunction}

import api.implementations.active.stateful

given mainProgramToMainFunctionImpl[S]
    : MainProgramToMainFunction[stateful.Function[S], S, And[Unit, S]] with

  val MAIN_PROGRAM_TO_MAIN_FUNCTION: MainProgram[stateful.Function[S]] => MainFunction =
    f_u2shu => s => s bind (() bind f_u2shu)
```

### `active.stateful.main`

```scala
package examples.applications.active.stateful

import types.And

import utilities.bind

import api.implementations.active.stateful

import stateful.{programImpl, mainProgramToMainFunctionImpl}

import examples.types.Seed

object mains extends examples.mains.stateful.Mains[stateful.Function[Seed], Seed, And[Unit, Seed]]

import mains.twoRandomNaturalsMain

@main def main(): Unit =

  val initialSeed: Seed = 1234567890L

  initialSeed bind twoRandomNaturalsMain
```

### Running the applications

```scala
sbt:programming_course> run

Multiple main classes detected. Select one to run:
 [1] examples.applications.active.main
 [2] examples.applications.active.stateful.main
 [3] examples.applications.reactive.main
 [4] examples.applications.reactive.parallel.logging.main
 [5] examples.applications.reactive.parallel.main

Enter number: 2
[info] running examples.applications.active.stateful.main 
the result of binding argument () to twoRandomNaturals is (643327,143151)
[success]
```

The result of using `randomNatural SEQ_AND randomNatural` is `643327,143151)`, clearly showing that the computatios of
programs, like `randomNatural` perform internal side effects, more precisely internal state manipulation. Changing
`active.stateful.main` to

```scala
package examples.applications.active.stateful

import types.And

import utilities.bind

import api.implementations.active.stateful

import stateful.{programImpl, mainProgramToMainFunctionImpl}

import examples.types.Seed

object mains extends examples.mains.stateful.Mains[stateful.Function[Seed], Seed, And[Unit, Seed]]

import mains.twoRandomNaturalsMain


@main def main(): Unit =

  val initialSeed: Seed = 1234567890L

  println(s"initial seed = $initialSeed")

  val (_, finalSeed) = initialSeed bind twoRandomNaturalsMain

  println(s"final seed = $finalSeed")
```

shows this state manipulation.

```scala
sbt:programming_course> run

Multiple main classes detected. Select one to run:
 [1] examples.applications.active.main
 [2] examples.applications.active.stateful.main
 [3] examples.applications.reactive.main
 [4] examples.applications.reactive.parallel.logging.main
 [5] examples.applications.reactive.parallel.main

Enter number: 2
[info] running examples.applications.active.stateful.main 
initial seed = 1234567890
the result of binding argument () to twoRandomNaturals is (643327,143151)
final seed = 235000571183836
[success] ...
```

### Conclusion

This chapter and the previous one introduce extra combinators in a somewhat undisciplined way. The
`reactive.parallel.programImpl` and `active.stateful.programImpl` implementations were specific ones, while the
`active.programImpl` and `reactive.programImpl` were inferred ones using `computationSpecToProgramImpl`. Moreover,
if we want an `active.parallel.programImpl` and a `reactive.stateful.programImpl` implementation, the we have to
duplicate code, and that's exactly what we want to avoid. 

The types 

- `type Expression = [S] =>> [Z] =>> StateHandler[S][active.Expression[Z]]`

- `type Expression = [Z] =>> CallbackHandler[active.Expression][Z]`

suggests that `active.Expression][Z]` can be *generically transformed* to other `Expression` types 


Please keep on reading ... .