## Chapter02: Functionality Basics

### Functions

Recall that functions are values of *function type*, `Function[Z, Y]`. Their only purpose is to, *transform* an
*argument of type `Z`*, *`Z` argument* for short, to a *result of type `Y`*, *`Y` result* for short. Functions
transforming a *`Z` argument* to a result, are called *`Z` consuming functions*. Functions transforming an argument to
an *`Y` result* are called *`Y` producing functions*.

In this chapter argument types and result types are *primitive types*, like `Boolean`, `BigInt` ... .

Recall that functions are defined using *lambda expressions*. Lambda expressions are of the form `z => e_z` or, when
the `Scala` type system requires it, `(z: Z) => e_z`, with *parameter*, `z`, and a *defining expression* `e_z`.

Lambda expressions are *function literals*, *function values* that are called *anonymous functions*. The expression of a
lambda expression can itself be a lambda expression in which case the lambda expression is a, *higher-order*,
*function producing anonymous function*.

### The `bind` Binary Operation

*Binary operations* can be declared as `infix extension`s and defined accordingly. Binary operations can e used with
*infix notation*. The course uses the *binding an argument to a function* binary operation instead of the
*applying a function to an argument* binary operation. Below is the `bind` binary operation definition in terms of
`apply`.

```scala
package utilities

extension [Z, Y](z: Z) infix def bind(f_z2y: => Function[Z, Y]): Y = f_z2y apply z
```

`f_z2y apply z` is just another way of writing `f_z2y(z)`.

Expressions in which `bind` is used with more than one function fluently read all the way from left to right, and do
not need association parentheses, as in `z bind f_z2y bind f_y2x`. Expressions in which `apply` is used with more than one
function do not fluently read all the way from left to right, and need association parentheses, as in
`f_y2x apply (f_z2y apply z)`.

Functions like `f_y2x` in expressions like `z bind f_z2y bind f_y2x` are also called *continuation functions*,
*continuations* for short. Once an argument `a` has been bound to function `f_z2y`, transforming it to a result `b`,
transforming *continues* by using `b` as an argument and binding it to `f_y2x`.

The first parameter, `z`, of `BIND` is a *call by value*, *cbv* for short, one, the second parameter, `f_z2y`, of `BIND`
is a *call by name*, *cbn* for short, one. Ideally cbn parameters should be *lazy* parameters, but, as far as I know,
`Scala` does not fully support lazy parameters yet.

Recall that you can also think about `bind` as an operation binding an expression argument `ez` to an expression
producing function `bindExpression`. Below is the `bindExpression` binary operation definition in terms of `bind`.

Let

```scala
package types

type Expression = [Z] =>> Z
```

in

```scala
package utilities

// ...

import types.Expression

extension [Z, Y](ez: Expression[Z])
  infix def bindExpression(f_z2ey: => Function[Z, Expression[Y]]): Expression[Y] =
    ez bind f_z2ey
```

Recall that *expression producing functions* can, operationally, be seen as *expression evaluation continuations*.

### Specifications And Implementations

Recall that the program DSL and the computation DSL are specification DSLs, also called a specification APIs, that are
type classes that are encoded as a `trait`s. A `trait` *declares* members, and *defines* members using, declared or
defined, members.

### `ProgramSpec`

- *Primitive functions can be lifted up to primitive programs*.

- *Programs can be composed sequentially*.

Let

```scala
package api.specification

import utilities.bind

trait FunctionToProgram[Program[-_, +_]]:

  def FUNCTION_TO_PROGRAM[Z, Y]: Function[Z, Y] => Program[Z, Y]

  def IDENTITY[Z]: Program[Z, Z] = identity[Z] bind FUNCTION_TO_PROGRAM
```

and

```scala
package api.specification

trait SequentialComposition[Program[-_, +_]]:

  extension [Z, Y, X](p_z2y: Program[Z, Y])
    infix def AND_THEN(p_y2x: => Program[Y, X]): Program[Z, X]
```

in 

```scala
package api.specification

trait ProgramSpec[Program[-_, +_]]
    extends FunctionToProgram[Program],
      SequentialComposition[Program]
```

`FUNCTION_TO_PROGRAM` is a *pointfree function and program combinator*, only functions and programs are involved.

`IDENTITY` is a *program*, only a program is involved.

`AND_THEN` is a *pointfree program combinator*, only programs are involved. It models a *generic abstraction* of
*sequentially composing functions*.

`FUNCTION_TO_PROGRAM` and `AND_THEN` are declared. `IDENTITY` is defined.

The course *generically abstracts* functions of type `Function[Z, Y]` by *specifying* them as programs of type
`Program[Z, Y]`.

Binary type constructor parameter `Program`, is *contravariant* in its first type argument and *covariant* in its second
type argument, meaning that a program, say `p_z2y`, of type `Program[Z, Y]`, may also transform an argument of a less
specific type than `Z` to a result of a more specific type than `Y`.

Programs transforming a *`Z` argument* to a result, are called *`Z` consuming programs*. Programs transforming an
argument to an *`Y` result* are called *`Y` producing programs*.

### `ComputationSpec`

- *Primitive expressions can be lifted up to primitive computations* using member `EXPRESSION_TO_COMPUTATION` of the
`ComputationSpec` specification.

- *Computations can be bound to `ComputationSpec` valued functions*, *computation valued functions*, also called
*computation producing functions* or *computation execution continuations*, using member `BIND` of the `ComputationSpec`
specification.

Below is the code declaring the computation DSL functionality basics.

Let

```scala
package lpi.specification

private[lpi] type FunctionProducing =
  [Computation[+_]] =>> [Z, Y] =>> Function[Z, Computation[Y]]
```

and

```scala
package lpi.specification

private[lpi] trait ExpressionToComputation[Computation[+_]]:

  private[lpi] def EXPRESSION_TO_COMPUTATION[Z]: FunctionProducing[Computation][Z, Z]
```

and

```scala
package lpi.specification

private[lpi] trait Bind[Computation[+_]]:

  extension [Z, Y](cz: Computation[Z])
    private[lpi] infix def BIND(f_z2cy: => FunctionProducing[Computation][Z, Y]): Computation[Y]
```

in

```scala
package lpi.specification

private[lpi] trait ComputationSpec[Computation[+_]]
    extends ExpressionToComputation[Computation],
      Bind[Computation]
```

`BIND` is a *pointful computation combinator*, not only computations are involved. It models a *generic abstraction* of 
*binding an inner expression evaluation result to an outer expression evaluation continuation* or
*binding an outer expression evaluation result to an inner expression evaluation continuation*.

`BIND` can also be seen as a generic abstraction of *binding an argument to a function*. This is a
*pointful program combinator* that, intentionally, the course does not generically abstracts.

Unary type constructor parameter `Computation`, is *covariant* in its parameter type meaning that a computation, say
`cy`, of type `Computation[Y]`, may, when executed at runtime, yield a result having a more specific type than `Y`

### Primitve Functions

*Primitive functions* are functions that are lifted up to programs, called *primitive programs*. Primitive programs do
not benefit from the *various zyx-ilities*, like *flexibility* and *extensibility*, that *non-primitive programs*, more
commonly called *composite programs*, benefit from. 

Below are some primitive functions, they all have function types involving *primitive types*, like `BigInt` and
`Boolean`, or *generic types*, like `Z`.

```scala
package examples.functions.primitive

def oneFunction[Z]: Function[Z, BigInt] = z => 1

val subtractOneFunction: Function[BigInt, BigInt] = z => z - 1

val subtractTwoFunction: Function[BigInt, BigInt] = z => z - 2

val isZeroFunction: Function[BigInt, Boolean] = z => z == 0

val isOneFunction: Function[BigInt, Boolean] = z => z == 1
```

- parameter `z` of `oneFunction` occurs at zero positions in expression `1`,
- parameter `z` of `subtractOneFunction` occurs at one position in expression `z - 1`,

but, for example,

- parameter `z` of anonymous functio `z => z * z` occurs at two position in expression
  `z * z`.

### `Programs`

First some *primitive programs*.

```scala
package examples.programs.primitive

import utilities.bind

import api.specification.ProgramSpec

import examples.functions.primitive.{
  oneFunction,
  subtractOneFunction,
  subtractTwoFunction,
  isZeroFunction,
  isOneFunction
}

trait Programs[Program[-_, +_]: ProgramSpec]:

  private val summonedProgramSpec = summon[ProgramSpec[Program]]
  import summonedProgramSpec.FUNCTION_TO_PROGRAM

  def one[Z]: Program[Z, BigInt] = oneFunction bind FUNCTION_TO_PROGRAM

  val subtractOne: Program[BigInt, BigInt] = subtractOneFunction bind FUNCTION_TO_PROGRAM

  val subtractTwo: Program[BigInt, BigInt] = subtractTwoFunction bind FUNCTION_TO_PROGRAM

  val isZero: Program[BigInt, Boolean] = isZeroFunction bind FUNCTION_TO_PROGRAM

  val isOne: Program[BigInt, Boolean] = isOneFunction bind FUNCTION_TO_PROGRAM
```

and next some *composite programs*.

```scala
package examples.programs.composite

import utilities.bind

import api.specification.ProgramSpec

import examples.programs.primitive

trait Programs[Program[-_, +_]: ProgramSpec] extends primitive.Programs[Program]:

  def shouldBeTrue[Z]: Program[Z, Boolean] = one AND_THEN subtractOne AND_THEN isZero

  def shouldBeFalse[Z]: Program[Z, Boolean] = one AND_THEN subtractOne AND_THEN isOne
```

Function members, like `FUNCTION_TO_PROGRAM` need to be `import`ed. Binary operation members, declared as
`infix` `extension`s, like `AND_THEN`, do not need to be `import`ed.

`shouldBeTrue` and `shouldBeFalse` are a *composite programs*. Their definitions do not *open* program `one`. The
program DSL is a *pointfree* DSL, treating programs as *closed components*.

### `FunctionsProducing[Computation[+_]: ComputationSpec]` (for library developers)

```scala
package lpi.functionsProducing

import utilities.bind

import lpi.specification.{FunctionProducing, ComputationSpec}

import examples.functions.primitive.{
  oneFunction,
  subtractOneFunction,
  subtractTwoFunction,
  isZeroFunction,
  isOneFunction
}

private[lpi] class FunctionsProducing[Computation[+_]: ComputationSpec]:

  private[lpi] val summonedComputationSpec = summon[ComputationSpec[Computation]]
  import summonedComputationSpec.EXPRESSION_TO_COMPUTATION

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
```

Function members, like `EXPRESSION_TO_COMPUTATION` need to be `import`ed. Binary operation members, declared as
`infix` `extension`s, like `BIND`, do not need to be `import`ed.

The definitions of `shouldBeTrue` and `shouldBeFalse` *open* computation `z bind one`
and bind the result of executing `z bind one` as an argument to parameter `y` of a lambda expression. The computation
DSL is a *pointful* DSL, treating computations as *open components*.

`shouldBeTrue` and `shouldBeFalse` use the same complex *pattern* encoding
*nested bindings of computation arguments to computation producing functions*. 

This useful pattern should, once and for all, be *generically abstracted* to avoid repeating complex code over and over
again. 

The definition

```scala
  def shouldBeTrue[Z]: Z >--> Boolean = one AND_THEN subtractOne AND_THEN isZero
```

is simpler than the definition

```scala
  def shouldBeTrue[Z]: Z => C[Boolean] = z =>
    z bind one BIND { y => y bind subtractOne BIND { x => x bind isZero } }
```

By the way, `shouldBeTrue` could, using *underscore notation*, also have been defined as


```scala
  def shouldBeTrue[Z]: Z => C[Boolean] =
    _ bind one BIND { _ bind subtractOne BIND { _ bind isZero } }
```

being simpler than the first computation producing definition, but also somewhat more cumbersome. 

So far, the course only provided a *syntactic simplifications* at specification level. The *semantic correctness* of
syntactic simplifications requires justification at implementation level.

### Input/Output, IO For Short

Let

```scala
package examples.functions.effectful

import scala.io.StdIn.readInt

val readBigIntArgumentFunction: Function[Unit, BigInt] = u =>
  println()
  println("please type a BigInt argument")
  readInt()

def writeResultFunction[Z](programName: String): Function[Z, Unit] = i =>
  println(s"the result of applying $programName is $i")
```

in

```scala
package examples.programs.effectful.primitive

import utilities.bind

import api.specification.ProgramSpec

import examples.functions.effectful.{readBigIntArgumentFunction, writeResultFunction}

trait Programs[Program[-_, +_]: ProgramSpec]:

  private val summonedProgramSpec = summon[ProgramSpec[Program]]
  import summonedProgramSpec.FUNCTION_TO_PROGRAM

  val readBigIntArgument: Program[Unit, BigInt] =
    readBigIntArgumentFunction bind FUNCTION_TO_PROGRAM

  def writeResult[Z](programName: String): Program[Z, Unit] =
    programName bind writeResultFunction bind FUNCTION_TO_PROGRAM
```

This is cheating! 

The idea behind lifting up is that only effectfree primitive functions are lifted up to programs. The code above lifts
up *effectful IO functions* to programs. A later chapter will treat *IO* in a more disciplined way.

### `MainProgramToMainFunction`

Recall that programs, written using the program DSL, are program specifications, syntactic artifacts. The same holds for
corresponding main programs. 

- *Main programs can be lifted down to main functions* using member `MAIN_PROGRAM_TO_MAIN_FUNCTION` of the 
`MainProgramToMainFunction` specification.

Below is the code defining `MainProgramToMainFunction`

Let

```scala
package api.specification

type MainProgram = [Program[-_, +_]] =>> Program[Unit, Unit]
```

in

```scala
package api.specification

import api.specification.MainProgram

trait MainProgramToMainFunction[Program[-_, +_], A, B]:

  type MainFunction = Function[A, B]

  val MAIN_PROGRAM_TO_MAIN_FUNCTION: MainProgram[Program] => MainFunction
```

### `Mains`

```scala
package examples.mains

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
      effectful.primitive.Programs[Program]:

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
```

`shouldBeTrueMain` and `shouldBeFalseMain` implicitly become functions of type `Function[A, B]` when implementations of
`ProgramSpec` and `MainProgramToMainFunction[Program, A, B]`, using `Program`, and `A` and `B` type arguments, are
`import`ed.

### Implementing Program Features In Terms Of Computation Features

Just like functions can be defined in terms of expressions, program features can be implemente in terms of computation
features.

`FUNCTION_TO_PROGRAM` can *generically* be *implemented* by using `EXPRESSION_TO_COMPUTATION`. The pointfree program
combinator `AND_THEN` can *generically* be *implemented* by using the pointful computation combinator `BIND`. `AD_THEN`
is a *generic abstraction* of defining `f_z2y andThen f_y2x` as `f_y2x(f_z2y(z))`, or `f_y2x apply (f_z2y apply z)` or,
more natural, `z => (z bind f_z2y) bind f_y2x` because it naturally reads from left to right and can be simplified to
`z => z bind f_z2y bind f_y2x` which does not use association parenthesis.

### `computationSpecToProgramImpl`

Below is the generic `ProgramSpec` implementation in terms of `ComputationSpec`.

```scala
package lpi.implementations.generic

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
```

The definition of `AND_THEN` is a *generic abstraction* of the
*nesting of two bindings of computation arguments to computation producing functions* pattern that is used by the code
of `shouldBeTrue` and `shouldBeFalse`.

 - A *functional programming design pattern* is simply a *generic abstraction* that can be used to replace the usage,
   over and over again, of a useful complex pattern, once and for all, by a simpler pattern. In this case to replace the
   complex pattern using `BIND` by the simpler one using `AND_THEN`.

### `active.Expression` `computationImpl` 

Below is the *active `ComputationSpec` implementation*.

Let

```scala
package api.implementations.active

type Expression = [Z] =>> types.Expression[Z]
```

in

```scala
package lpi.implementations.active

import utilities.bind

import api.implementations.active

import lpi.specification.{FunctionProducing, ComputationSpec}

given computationImpl: ComputationSpec[active.Expression] with

  private[lpi] def EXPRESSION_TO_COMPUTATION[Z]: FunctionProducing[active.Expression][Z, Z] =
    identity

  extension [Z, Y](az: active.Expression[Z])
    private[lpi] infix def BIND(
        f_z2ay: => FunctionProducing[active.Expression][Z, Y]
    ): active.Expression[Y] = az bind f_z2ay
```

The definitions of `EXPRESSION_TO_COMPUTATION` and `BIND` are completely trivial since
*`active.Expression` producing functions* are simply *functions*.

### `active.Function` `programImpl` 

Below is the *active `Program` implementation*.

Let

```scala
package api.implementations.active

import api.implementations.active

type Function = [Z, Y] =>> scala.Predef.Function[Z, active.Expression[Y]]
```
in

```scala
package api.implementations.active

import api.specification.ProgramSpec

import api.implementations.active

import lpi.implementations.generic.computationSpecToProgramImpl

import lpi.implementations.active.computationImpl

given programImpl: ProgramSpec[active.Function] = computationSpecToProgramImpl
```

A `given` implementation of a `trait` specification is a *statement*, more precisely, an *existential statement*. Such
statements can be *generic* and *specific*. Such statements can then be used in *proofs*, more precisely,
*existential proofs*. 

For example, the existence of a specific, in this case active, program instance follows from the existence of a generic
program instance following from the existence of a generic computation instance and the existence of a specific, in this
case active, computation instance.

The `Scala` type system can derive such existential proofs. That is wonderful!

### `active.Function` `mainProgramToMainFunctionImpl`

Below is the *active `MainProgramToMainFunction` implementation*.

```scala
package api.implementations.active

import utilities.bind

import api.specification.{MainProgram, MainProgramToMainFunction}

import api.implementations.active

given mainProgramToMainFunctionImpl: MainProgramToMainFunction[active.Function, Unit, Unit]
with

  val MAIN_PROGRAM_TO_MAIN_FUNCTION: MainProgram[active.Function] => MainFunction =
    f_u2u => u => u bind f_u2u
```

### `active.main`

```scala
package examples.applications.active

import utilities.bind

import api.implementations.active

import active.{programImpl, mainProgramToMainFunctionImpl}

import examples.mains.Mains

object mains extends Mains[active.Function, Unit, Unit]

import mains.{shouldBeTrueMain, shouldBeFalseMain}

@main def main(): Unit =

  () bind shouldBeTrueMain

  () bind shouldBeFalseMain
```

### Conclusion

Using the program DSL we lifted primitive functions up to programs using `FUNCTION_TO_PROGRAM`, composed programs using
`AND_THEN`, lifted, abusively, effectful IO functions up to effects using `FUNCTION_TO_PROGRAM`, composed programs with
effects using `AND_THEN` to obtain main programs, and lifted main programs back down to main functions using
`MAIN_PROGRAM_TO_MAIN_FUNCTION`. We might as well not have lifted and have used `andThen` instead of `AND_THEN`.

What did we gain? So far the answer is: nothing. 

### `reactive.Expression` `computationImpl`

Below is the *reactive `ComputationSpec` implementation*.

Let

```scala
package types

type Expression = [Z] =>> Z

type Callback = [C[+_]] =>> [Z] =>> Function[C[Z], Unit]

type CallbackHandler = [C[+_]] =>> [Z] =>> Function[Callback[C][Z], Unit]
```

and 

```scala
package api.implementations.reactive

import types.CallbackHandler

import api.implementations.active

type Expression = [Z] =>> CallbackHandler[active.Expression][Z]
```

in

```scala
package lpi.implementations.reactive

import utilities.bind

import api.implementations.reactive

import lpi.specification.{FunctionProducing, ComputationSpec}

given computationImpl: ComputationSpec[reactive.Expression] with

  private[lpi] def EXPRESSION_TO_COMPUTATION[Z]
      : FunctionProducing[reactive.Expression][Z, Z] = z => cbz => z bind cbz

  extension [Z, Y](cbhz: reactive.Expression[Z])
    private[lpi] infix def BIND(
        f_z2cbhy: => FunctionProducing[reactive.Expression][Z, Y]
    ): reactive.Expression[Y] = cby => { (z: Z) => cby bind (z bind f_z2cbhy) } bind cbhz
```

### `reactive.Function` `programImpl`

Let

```scala
package api.implementations.reactive

import types.{Expression, CallbackHandler}

import api.implementations.reactive

type Function = [Z, Y] =>> scala.Predef.Function[Z, reactive.Expression[Y]]
```

in

```scala
package api.implementations.reactive

import api.specification.ProgramSpec

import api.implementations.reactive

import lpi.implementations.generic.computationSpecToProgramImpl

import lpi.implementations.reactive.computationImpl

given programImpl: ProgramSpec[reactive.Function] = computationSpecToProgramImpl
```

Compared to `active.programImpl`, `reactive.programImpl` simply uses `reactive.Function` instead of `active.Function`
and `reactive.computationImpl` instead of `active.computationImpl`.

### `reactive.Function` `mainProgramToMainFunctionImpl`

```scala
package api.implementations.reactive

import utilities.bind

import api.specification.{MainProgram, MainProgramToMainFunction}

import api.implementations.reactive

given mainProgramToMainFunctionImpl: MainProgramToMainFunction[reactive.Function, Unit, Unit]
with

  val MAIN_PROGRAM_TO_MAIN_FUNCTION: MainProgram[reactive.Function] => MainFunction =
    f_u2cbhu => u => identity[Unit] bind (u bind f_u2cbhu)
```

Compare the reactive definition with the active definition. The only *harmless* thing one can do with `u bind f_u2u` is
*binding it to `identity`*, resulting in `(u bind f_u2u) bind identity`. The only *meaningful* thing one can do with
`u bind f_u2cbhu` is *binding `identity` to it*, resulting in `identity[Unit] bind (u bind f_u2cbhu)`. There is some
kind of *duality*, or *bi-duality* if you wish, involved here (values of type `Function[Z, Unit]` could be called
*dual values* and values of type `Function[Function[Z, Unit], Unit]` could be called *bi-dual values*).

### `reactive.main`

```scala
package examples.applications.reactive

import utilities.bind

import api.implementations.reactive

import reactive.{programImpl, mainProgramToMainFunctionImpl}

import examples.mains.Mains

object mains extends Mains[reactive.Function, Unit, Unit]

import mains.{shouldBeTrueMain, shouldBeFalseMain}

@main def main(): Unit =

  () bind shouldBeTrueMain

  () bind shouldBeFalseMain
```

Compared to `mains`, used by `active.main`, `mains`, used by `reactive.main`, simply uses `reactive.Function`,
`reactive.programImpl` and `reactive.mainProgramToMainFunctionImpl` instead of `active.Function`, `active.programImpl`
and `active.mainProgramToMainFunctionImpl`.

### Running the applications

```scala
sbt:programming_course> run

Multiple main classes detected. Select one to run:
 [1] examples.applications.active.main
 [2] examples.applications.reactive.main

Enter number: 1
[info] running examples.applications.active.main 

please type a BigInt argument
9
the result of applying shouldBeTrue is true

please type a BigInt argument
9
the result of applying shouldBeFalse is false
[success] ...
sbt:programming_course> run

Multiple main classes detected. Select one to run:
 [1] examples.applications.active.main
 [2] examples.applications.reactive.main

Enter number: 2
[info] running examples.applications.reactive.main 

please type a BigInt argument
9
the result of applying shouldBeTrue is true

please type a BigInt argument
9
the result of applying shouldBeFalse is false
[success] ...
```

### Conclusion

So we now have both an active and a reactive program DSL implementation. 

What did we gain? So far the answer is: nothing.

Please keep on reading ... .