## Chapter03: Information Basics

The previous chapter concentrated on basic *functionality related program combinators*, in particular on program
combinator `AND_THEN` modeling *sequential composition*. This chapter concentrates on basic
*information related program combinators*, in particular on program combinator `SEQ_AND` modeling
*sequential construction*, *construction* for short.

### Functional Programming Revisited

Recall that *information* is modeled using *values* and that values have a *type*. The examples of the previous chapter
all involved `BigInt`, `Boolean`, ... values. `BigInt`, `Boolean`, ... are examples of a *primitive types* modeling
*integer information*, *boolean information*, ... .

Programming is also about modeling *non-primitive information*, also called *composite information*. The basic way in
which functional programming deals with composite information is by using *tuples*. 

Recall that functions of type `Function[Z, Y]` transform an argument value of type `Z` to a result value of type `Y`.

The types involved can be *`n`-tuple types*, modeling *functions with `n` arguments/results*.

- The *`0`-tuple type* `Unit` is a corner tuple case. The only effectfree value of type `Unit` is `()`.

- A *`1`-tuple type* `Z` is a corner tuple case. It is common practice not to explicitly mention 1-tuple,
as in *argument/result* instead of *`1`-tuple argument / `1`-tuple result*. It is also common practice not to use
parentheses as in `z` instead of `(z)`.

- The *`2`-tuple type* `Tuple2[Z, Y]` is the normal tuple case. *Tuple members* are separated by a comma as in `(z, y)`.

- *`n`-tuple types*, for `n >= 3`, are encoded as *nested `2`-tuple types* as in `Tuple2[Z, Tuple2[Y, X]]` and so on.
*Nested tuple members* are separated by commas and associated to the right as in `(z, (y, x))` and so on.

`z => e(z)` is a lambda expression with a (`1`-tuple) parameter. The parameter can also be a `0`-tuple, as in `u => e`,
`u` referring to type `Unit`, or an `n`-tuple, with `n >= 2`, as in `(z, (y, x)) => e(z, y, x)`, where `z`, `y`, and `x`
can occur at zero or more *positions* in the defining expression `e(z, y, x)`.

### `SequentialConstruction`

- *Programs can sequentially produce tuples*.

Below is the code declaring the program DSL information basics.

Let

```scala
package types

type Expression = [Z] =>> Z

type Callback = [C[+_]] =>> [Z] =>> Function[C[Z], Unit]

type CallbackHandler = [C[+_]] =>> [Z] =>> Function[Callback[C][Z], Unit]

// added for chapter03
type And = [Z, Y] =>> Tuple2[Z, Y]
//
```

in

```scala
package api.specification

import types.And

trait SequentialConstruction[Program[-_, +_]]:

  extension [Z, Y, X](p_z2y: Program[Z, Y])
    infix def SEQ_AND(p_z2x: Program[Z, X]): Program[Z, And[Y, X]]
```

`SEQ_AND` is a *pointfree program combinator*, only programs are involved. It models a *generic abstraction* of
*functions sequentially producing tuples*.

Just like `Tuple2`, binary type constructor `And`, is *covariant* in both its left parameter and its right parameter
type meaning that a *tuple* may have members that have more specific types than `Z` and `Y`.

### `ProgramSpec`

```scala
package api.specification

// added for chapter03
import types.And
//

trait ProgramSpec[Program[-_, +_]]
    extends FunctionToProgram[Program],
      SequentialComposition[Program],
      // added for chapter03
      SequentialConstruction[Program]:
  //

  // added for chapter03
  def DUPLICATE[Z]: Program[Z, And[Z, Z]] = IDENTITY SEQ_AND IDENTITY
  //
```

Also `DUPLICATE` has been added.

### `ComputationSpec`

```scala
package lpi.specification

// added for chapter03
import utilities.bind
//

private[lpi] trait ComputationSpec[Computation[+_]]
    extends ExpressionToComputation[Computation],
      Bind[Computation]:

  // added for chapter03
  def IDENTITY[Z]: Function[Z, Computation[Z]] = z =>
    z bind identity[Z] bind EXPRESSION_TO_COMPUTATION
  //
```

Also `IDENTITY` has been added.

### `Programs`

Let

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
```

in

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
  multiplyFunction
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
```

and

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
  import summonedProgramSpec.DUPLICATE

  def shouldBeTrue[Z]: Program[Z, Boolean] = one AND_THEN subtractOne AND_THEN isZero

  def shouldBeFalse[Z]: Program[Z, Boolean] = one AND_THEN subtractOne AND_THEN isOne

  // added for chapter03
  def shouldBeTrueAndFalse[Z]: Program[Z, And[Boolean, Boolean]] =
    shouldBeTrue SEQ_AND shouldBeFalse

  val square: Program[BigInt, BigInt] = DUPLICATE AND_THEN multiply
  //
```

`addFunction`, and `multiplyFunction`, *primitive `2`-tuple consuming functions*, are added.

`add`, and `multiply`, *primitive tuple consuming programs* are added, also
`shouldBeTrueAndFalse` a *composite tuple producing program* is added, and `square`, a
*composite first tuple producing and second tuple consuming program*, is added.

### `FunctionsProducing[Computation[+_]: ComputationSpec]` (for library developers)

```scala
package lpi.functionsProducing

// added for chapter03
import types.And
//

import utilities.bind

import lpi.specification.{FunctionProducing, ComputationSpec}

import examples.functions.primitive.{
  oneFunction,
  subtractOneFunction,
  subtractTwoFunction,
  isZeroFunction,
  isOneFunction,
  // added for chapter03
  addFunction,
  multiplyFunction
  //
}

private[lpi] class FunctionsProducing[Computation[+_]: ComputationSpec]:

  private[lpi] val summonedComputationSpec = summon[ComputationSpec[Computation]]
  // added for chapter03
  import summonedComputationSpec.{EXPRESSION_TO_COMPUTATION, IDENTITY}
  //

  def one[Z]: FunctionProducing[Computation][Z, BigInt] = z =>
    z bind oneFunction bind EXPRESSION_TO_COMPUTATION

  val subtractOne: FunctionProducing[Computation][BigInt, BigInt] = z =>
    z bind subtractOneFunction bind EXPRESSION_TO_COMPUTATION

  val subtractTwo: FunctionProducing[Computation][BigInt, BigInt] = z =>
    z bind subtractTwoFunction bind EXPRESSION_TO_COMPUTATION

  val isZero: FunctionProducing[Computation][BigInt, Boolean] = z =>
    z bind isZeroFunction bind EXPRESSION_TO_COMPUTATION

  val isOne: FunctionProducing[Computation][BigInt, Boolean] = z =>
    z bind isOneFunction bind EXPRESSION_TO_COMPUTATION

  def shouldBeTrue[Z]: FunctionProducing[Computation][Z, Boolean] = z =>
    z bind one BIND { y => y bind subtractOne BIND { x => x bind isZero } }

  def shouldBeFalse[Z]: FunctionProducing[Computation][Z, Boolean] = z =>
    z bind one BIND { y => y bind subtractOne BIND { x => x bind isOne } }

  // added for chapter03
  val add: FunctionProducing[Computation][And[BigInt, BigInt], BigInt] = zay =>
    zay bind addFunction bind EXPRESSION_TO_COMPUTATION

  val multiply: FunctionProducing[Computation][And[BigInt, BigInt], BigInt] = zay =>
    zay bind multiplyFunction bind EXPRESSION_TO_COMPUTATION

  def shouldBeTrueAndFalse[Z]: FunctionProducing[Computation][Z, And[Boolean, Boolean]] =
    z =>
      z bind shouldBeTrue BIND { y =>
        z bind shouldBeFalse BIND { x =>
          (y, x) bind EXPRESSION_TO_COMPUTATION
        }
      }

  val square: FunctionProducing[Computation][BigInt, BigInt] = z =>
    z bind IDENTITY BIND { y =>
      z bind IDENTITY BIND { x =>
        (y, x) bind EXPRESSION_TO_COMPUTATION
      } BIND multiply
    }
  //
```

`add`, `multiply`, `shouldBeTrueAndFalse` and `square` are added. Just like the useful pattern used by
`shouldBeTrue` and `shouldBeFalse`, the useful pattern used by `shouldBeTrueAndFalse` and `square` should, once and for
all, be *generically abstracted*.

### Input/Output, IO For Short

The output of the examples so far was suboptimal. It did not show the argument value, only the result value. Sequential
composition forgets the consumed argument value of a program and only passes the result value as an argument to a next
program. Using the program DSL so far it is possible to do better.

### Local Value Definitions

```scala
package api.specification

// added for chapter03
import types.And
//

trait ProgramSpec[Program[-_, +_]]
    extends FunctionToProgram[Program],
      SequentialComposition[Program],
      // added for chapter03
      SequentialConstruction[Program]:
  //

  // added for chapter03
  def DUPLICATE[Z]: Program[Z, And[Z, Z]] = IDENTITY SEQ_AND IDENTITY

  extension [Z, Y, X](p_z2y: Program[Z, Y])
    infix def IN(p_zay2x: Program[And[Z, Y], X]): Program[Z, X] =
      IDENTITY SEQ_AND p_z2y AND_THEN p_zay2x
  //
```

Composite programs `p_z2y IN p_zay2x` *generically abstract* lambda expression defined, anonymous
functions `z => { val y = z bind f_z2y ; (z, y) bind f_zay2x }` that use a *local value definition*
`val y = z bind f_z2y`.

#### Input/Output, IO For Short

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
  writeArgumentAndResultFunction
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
```

and

```scala
package examples.programs.effectful.composite

import types.And

import api.specification.ProgramSpec

import examples.programs.effectful

trait Programs[Program[-_, +_]: ProgramSpec] extends effectful.primitive.Programs[Program]:

  val readTwoBigIntArguments: Program[Unit, And[BigInt, BigInt]] =
    readBigIntArgument SEQ_AND readBigIntArgument
```

`writeArgumentAndResultFunction` and `writeArgumentAndResult` are added.

`readTwoBigIntArguments` is a composite effect. The definition of `readTwoBigIntArguments` simply uses `SEQ_AND`. When a
later chapter treats IO in a more disciplined way, the definition does not need to be changed.

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
```

`addMain`, `multiplyMain`, `shouldBeTrueAndFalseMain` and `squareMain` are added.

### `computationSpecToProgramImpl`

Below is the generic `ProgramSpec` implementation in terms of `ComputationSpec`.

```scala
package lpi.implementations.generic

// added for chapter03
import types.And
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
        f_z2cx: FunctionProducing[Computation][Z, X]
    ): FunctionProducing[Computation][Z, And[Y, X]] = z =>
      z bind f_z2cy BIND { y =>
        z bind f_z2cx BIND { x =>
          (y, x) bind EXPRESSION_TO_COMPUTATION
        }
      }
  //
```

The definition of `SEQ_AND` is a *generic abstraction* of the pattern that is used by the code of `shouldBeTrueAndFalse`
and `square`.

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
  squareMain
  //
}

@main def main(): Unit =

  // () bind shouldBeTrueMain

  // () bind shouldBeFalseMain

  // added for chapter03
  () bind addMain

  () bind multiplyMain

  () bind shouldBeTrueAndFalseMain

  () bind squareMain
  //
```

### `reactive.main`

Let

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
  squareMain
  //
}

@main def main(): Unit =

  // () bind shouldBeTrueMain

  // () bind shouldBeFalseMain

  // added for chapter03
  () bind addMain

  () bind multiplyMain

  () bind shouldBeTrueAndFalseMain

  () bind squareMain
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
12

please type a BigInt argument
12
the result of binding argument (12,12) to add is 24

please type a BigInt argument
12

please type a BigInt argument
12
the result of binding argument (12,12) to multiply is 144

please type a BigInt argument
12
the result of binding argument 12 to shouldBeTrueAndFalse is (true,false)

please type a BigInt argument
12
the result of binding argument 12 to square is 144
[success] ...
sbt:programming_course> run

Multiple main classes detected. Select one to run:
 [1] examples.applications.active.main
 [2] examples.applications.reactive.main

Enter number: 2
[info] running examples.applications.reactive.main 

please type a BigInt argument
12

please type a BigInt argument
12
the result of binding argument (12,12) to add is 24

please type a BigInt argument
12

please type a BigInt argument
12
the result of binding argument (12,12) to multiply is 144

please type a BigInt argument
12
the result of binding argument 12 to shouldBeTrueAndFalse is (true,false)

please type a BigInt argument
12
the result of binding argument 12 to square is 144
[success] ...
```

### Conclusion

We are not ready yet for programs like `fibonacci` and `factorial` that are shown in the introduction. We need one extra
funtionality feature.

Please keep on reading ... .