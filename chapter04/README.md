## Chapter04: Conditiononal Functionality

This chapter concentrates on *conditional functionality related combinators*, in particular on the program
combinator `OR`, modeling *condition*.

### Condition

### `Condition`

- *Programs can consume alternatives*.

Below is the code declaring *condition*.

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
```

in

```scala
package api.specification

import types.Or

trait Condition[Program[-_, +_]]:

  extension [Z, Y, X](p_x2z: => Program[X, Z])
    infix def OR(p_y2z: => Program[Y, Z]): Program[Or[X, Y], Z]
```

`OR` is a *pointfree program combinator*, only programs are involved. It models a *generic abstraction* of
*functions matching on alternatives*.

Just like `Either`, binary type constructor `Or`, is *covariant* in both its left parameter and its right parameter type
meaning that an *alternative* may have members that have more specific types than `Z` and `Y`.

### `ProgramSpec`

```scala
package api.specification

// added for chapter03
import types.And
//

// added for chapter04
import utilities.{bind, booleanChoiceFunction}
//

trait ProgramSpec[Program[-_, +_]]
    extends FunctionToProgram[Program],
      SequentialComposition[Program],
      // added for chapter03
      SequentialConstruction[Program],
      // added for chapter04
      Condition[Program]:
  //

  // added for chapter03
  def DUPLICATE[Z]: Program[Z, And[Z, Z]] = IDENTITY SEQ_AND IDENTITY

  extension [Z, Y, X](p_z2y: Program[Z, Y])
    infix def IN(p_zay2x: Program[And[Z, Y], X]): Program[Z, X] =
      IDENTITY SEQ_AND p_z2y AND_THEN p_zay2x
  //

  // added for chapter04
  private[specification] trait Else[Y, Z]:
    def ELSE(ff_y2z: Program[Y, Z]): Program[Y, Z]

  private[specification] trait Apply[Y, Z]:
    def apply(tf_y2z: Program[Y, Z]): Else[Y, Z]

  def IF[Y, Z](f_y2b: Program[Y, Boolean])(tf_y2z: Program[Y, Z]): Else[Y, Z] =
    new Else[Y, Z]:
      def ELSE(ff_y2z: Program[Y, Z]): Program[Y, Z] =
        f_y2b IN {
          booleanChoiceFunction bind FUNCTION_TO_PROGRAM
        } AND_THEN {
          tf_y2z OR ff_y2z
        }
  //
```

Composite programs `IF(f_y2b) { tf_y2z } ELSE { ff_y2z }` *generically abstract*
*if_then_else boolean conditional logic*.

### `Programs`

```scala
package examples.programs.composite

// added for chapter03
import types.And
//

import utilities.bind

import api.specification.ProgramSpec

import examples.programs.primitive

trait Programs[Program[-_, +_]: ProgramSpec] extends primitive.Programs[Program]:

  private val summonedProgramSpec: ProgramSpec[Program] = summon[ProgramSpec[Program]]
  // added for chapter04
  import summonedProgramSpec.{DUPLICATE, IF}
  //

  def shouldBeTrue[Z]: Program[Z, Boolean] = one AND_THEN subtractOne AND_THEN isZero

  def shouldBeFalse[Z]: Program[Z, Boolean] = one AND_THEN subtractOne AND_THEN isOne

  // added for chapter03
  def shouldBeTrueAndFalse[Z]: Program[Z, And[Boolean, Boolean]] =
    shouldBeTrue SEQ_AND shouldBeFalse

  val square: Program[BigInt, BigInt] = DUPLICATE AND_THEN multiply
  //

  // added for chapter04
  val fibonacci: Program[BigInt, BigInt] =
    IF(isZero) {
      one
    } ELSE {
      IF(isOne) {
        one
      } ELSE {
        (subtractOne AND_THEN fibonacci) SEQ_AND
          (subtractTwo AND_THEN fibonacci) AND_THEN
          add
      }
    }

  val factorial: Program[BigInt, BigInt] =
    IF(isZero) {
      one
    } ELSE {
      (subtractOne AND_THEN factorial) IN
        multiply
    }
  //
```

`fibonacci` and `factorial` are added.

### `Mains`

```scala
package examples.mains

// added for chapter03
import types.And
//

import utilities.bind

import api.specification.{ProgramSpec, MainProgramToMainFunction}

import examples.programs.{composite, effectful}

trait Mains[
    Program[-_, +_]
      : ProgramSpec
      : [Program[-_, +_]] =>> MainProgramToMainFunction[Program, A, B],
    A,
    B
] extends composite.Programs[Program],
      effectful.primitive.Programs[Program],
      // added for chapter03
      effectful.composite.Programs[Program]:
  //

  private val summonedMainProgramToMainFunction =
    summon[MainProgramToMainFunction[Program, A, B]]
  import summonedMainProgramToMainFunction.{MainFunction, MAIN_PROGRAM_TO_MAIN_FUNCTION}

  val shouldBeTrueMain: MainFunction =
    readBigIntArgument AND_THEN shouldBeTrue AND_THEN writeResult(
      "shouldBeTrue"
    ) bind MAIN_PROGRAM_TO_MAIN_FUNCTION

  val shouldBeFalseMain: MainFunction =
    readBigIntArgument AND_THEN shouldBeFalse AND_THEN writeResult(
      "shouldBeFalse"
    ) bind MAIN_PROGRAM_TO_MAIN_FUNCTION

  // added for chapter03

  val addMain: MainFunction =
    readTwoBigIntArguments AND_THEN (add IN writeArgumentAndResult(
      "add"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION

  val multiplyMain: MainFunction =
    readTwoBigIntArguments AND_THEN (multiply IN writeArgumentAndResult(
      "multiply"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION

  def shouldBeTrueAndFalseMain[Z]: MainFunction =
    readBigIntArgument AND_THEN (shouldBeTrueAndFalse IN writeArgumentAndResult(
      "shouldBeTrueAndFalse"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION

  def squareMain[Z]: MainFunction =
    readBigIntArgument AND_THEN (square IN writeArgumentAndResult(
      "square"
    )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION
  //

  // added for chapter04
  def fibonacciMain[Z]: MainFunction =
  readBigIntArgument AND_THEN (fibonacci IN writeArgumentAndResult(
    "fibonacci"
  )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION

  def factorialMain[Z]: MainFunction =
  readBigIntArgument AND_THEN (factorial IN writeArgumentAndResult(
    "factorial"
  )) bind MAIN_PROGRAM_TO_MAIN_FUNCTION
  // 
```

`fibonacciMain` and `factorialMain` are added.

### `computationSpecToProgramImpl`

```scala
package lpi.implementations.generic

// added for chapter03
import types.And
//

// added for chapter04
import types.Or
//

import utilities.bind

import api.specification.ProgramSpec

import lpi.specification.{FunctionProducing, ComputationSpec}

given computationSpecToProgramImpl[Computation[+_]: ComputationSpec]
    : ProgramSpec[FunctionProducing[Computation]] with

  private val summonedComputationSpec = summon[ComputationSpec[Computation]]
  import summonedComputationSpec.EXPRESSION_TO_COMPUTATION

  def FUNCTION_TO_PROGRAM[Z, Y]: Function[Z, Y] => FunctionProducing[Computation][Z, Y] =
    f_z2y => z => z bind f_z2y bind EXPRESSION_TO_COMPUTATION

  extension [Z, Y, X](f_z2cy: FunctionProducing[Computation][Z, Y])
    infix def AND_THEN(
        f_y2cx: => FunctionProducing[Computation][Y, X]
    ): FunctionProducing[Computation][Z, X] = z =>
      z bind f_z2cy BIND { y =>
        y bind f_y2cx BIND { x => x bind EXPRESSION_TO_COMPUTATION }
      }

  // added for chapter03
  extension [Z, Y, X](f_z2cy: FunctionProducing[Computation][Z, Y])
    infix def SEQ_AND(
        f_z2cx: => FunctionProducing[Computation][Z, X]
    ): FunctionProducing[Computation][Z, And[Y, X]] = z =>
      z bind f_z2cy BIND { y =>
        z bind f_z2cx BIND { x =>
          (y, x) bind EXPRESSION_TO_COMPUTATION
        }
      }
  //

  // added for chapter04
  extension [Z, Y, X](f_x2cz: => FunctionProducing[Computation][X, Z])
    infix def OR(
        f_y2cz: => FunctionProducing[Computation][Y, Z]
    ): FunctionProducing[Computation][Or[X, Y], Z] = {
      case Left(x)  => x bind f_x2cz
      case Right(y) => y bind f_y2cz
    }
  //
```

### `active.main`

```scala
package examples.applications.active

import utilities.bind

import api.implementations.active

import active.{programImpl, mainProgramToMainFunctionImpl}

import examples.mains.Mains

object mains extends Mains[active.Function, Unit, Unit]

import mains.{
  // shouldBeTrueMain,
  // shouldBeFalseMain,
  // added for chapter03
  addMain,
  multiplyMain,
  shouldBeTrueAndFalseMain,
  squareMain,
  //
  // added for chapter04
  fibonacciMain,
  factorialMain
  //
}

@main def main(): Unit =

  // () bind shouldBeTrueMain

  // () bind shouldBeFalseMain

  // added for chapter03
  // () bind addMain

  // () bind multiplyMain

  // () bind shouldBeTrueAndFalseMain

  // () bind squareMain
  //

  // added for chapter04
  () bind fibonacciMain

  () bind factorialMain
  //
```

### `reactive.main`

```scala
package examples.applications.reactive

import utilities.bind

import api.implementations.reactive

import reactive.{programImpl, mainProgramToMainFunctionImpl}

import examples.mains.Mains

object mains extends Mains[reactive.Function, Unit, Unit]

import mains.{
  // shouldBeTrueMain,
  // shouldBeFalseMain,
  // added for chapter03
  addMain,
  multiplyMain,
  shouldBeTrueAndFalseMain,
  squareMain,
  //
  // added for chapter04
  fibonacciMain,
  factorialMain
  //
}

@main def main(): Unit =

  // () bind shouldBeTrueMain

  // () bind shouldBeFalseMain

  // added for chapter03
  // () bind addMain

  // () bind multiplyMain

  // () bind shouldBeTrueAndFalseMain

  // () bind squareMain
  //

  // added for chapter04
  () bind fibonacciMain

  () bind factorialMain
  //
```

### Running the applications

```scala
sbt:programming_course> run

Multiple main classes detected. Select one to run:
 [1] examples.applications.active.main
 [2] examples.applications.reactive.main

Enter number: 1
[info] running examples.applications.active.main 

please type a BigInt argument
10
the result of binding argument 10 to fibonacci is 89

please type a BigInt argument
10
the result of binding argument 10 to factorial is 3628800
[success] ...
sbt:programming_course> run

Multiple main classes detected. Select one to run:
 [1] examples.applications.active.main
 [2] examples.applications.reactive.main

Enter number: 2
[info] running examples.applications.reactive.main 

please type a BigInt argument
10
the result of binding argument 10 to fibonacci is 89

please type a BigInt argument
10
the result of binding argument 10 to factorial is 3628800
[success] ...
```

### Conclusion

We can now write many pointfree effectfree programs. But, as promised in the introduction, we may want more program
features. We may want the execution of the computation of `fibonacci` to execute the computation of
`subtractOne AND_THEN fibonacci` and the computation of `subtractTwo AND_THEN fibonacci` in parallel.

Please keep on reading ... .