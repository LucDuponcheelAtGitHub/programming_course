## Chapter01: Introduction
 
Many sentences of the introduction chapter of the course that make a statement about functional programming begin with
the adverbial phrase *in my opinion* without fully motivating why the adverbial phrase applies to the statement. Do not
let those opinionated statements, even if you do not fully agree with them, make you decide to give up on studying the
course. Hopefully the content chapters of the course will, eventually, motivate the statements sufficiently. 

Programming, like many other activities, should be done in a disciplined way. This course reflects my vision on how
programming can be done in a disciplined way. In my opinion, mathematical concepts almost inevitably come into play. I
don't want to overwhelm you with mathematics. I want to concentrate on functional programming.

Writing the course therefore poses some challenges.

1. The choice of natural language constructs that describe functional programming concepts.

    - When such natural language constructs are first used, they *are emphasized*. When later used, they
      *may be emphasized* as well.

2. The order in which functional programming concepts are introduced. 

    - A course must not to use concepts that have not been defined earlier in the course. For a math course this
      requirement is mandatory. For a proramming course this requirement is highly desirable.

Writing the introduction chapter of the course then also comes some challenge.

1. The level of detail at which functional programming concepts are explained.

    - On the one hand, an introductory chapter should not go into too much detail. On the other hand, an introductory
      chapter should be detailed enough to be understandable.

**Warning**

1. Do not expect the introduction chapter to even try to be *sound* or *complete*.

    - The content chapters of the course try to be sound and complete.

2. Do not expect the introduction chapter to be *easy*.

    - The introduction chapter is, almost inevitably, *complex* because it introduces a lot of functional programming
      concepts and corresponding natural language constructs. Therefore it is inherently somewhat *difficult* since
      humans can only deal with a limited amount of complexity.

### Functional Programming

Programming is about modeling *information* and *functionality*. Functional programming is a programming paradigm that
uses *expressions* and *functions* to help you model information and functionality in a disciplined way. Programming is
also about modeling *side-effects*. One of the purposes of the course is to *abstract* expressions, functions and
side-effects in a *generic* way, by *specifying* them as *computations*, *programs*, and *effects*, to help you also
model side-effects in a disciplined way.

1. *Information* is modeled using *immutable values*, *values* for short. By the way, many programming courses use the
noun phrase *immutable variables* which sounds like a *contradictio in terminis* to me. Values have *type*. A
*value of type `Z`* is also called a *`Z` value*. Besides *types*, there are *type constructors* having
*type parameters*. Type constructors can be *unary type constructors*, *binary type constructors* and so on. Types resp.
type constructors are *classified* using *type classes* resp. *type constructor classes*. By abuse of notation the
course often simply uses *type* instead of *type constructor*, like in *type class* instead of *type constructor class*.

2. *Functionality* is modeled using *functions*. Functions are values having type, `Function[Z, Y]`, called a
*`Function` type*, or *function type*, and functions can be called *`Function` values*, or *function values*. The course
does not call them *function values*, but it keeps on calling them *functions* instead. Functions *consume* an
*argument value*, *argument* for short, of *argument type `Z`* and *produce* a *result value*, *result* for short, of
*result type `Y`*. Functions can be *higher-order functions*, argument type `Z` and/or result type `Y` can be a function
type. Functions are *denotational* artifacts, they *denote* something at *compile time* and can be given a meaningful
*function name*, *name* for short. Functions are defined using *lambda expressions* consisting of a
*function parameter*, *parameter* for short, of type `Z`, and a *function defining expression*, *defining expression*
for short, of type `Y`. The defining expression is an *expression* wherein the parameter occurs at zero or more
positions. In my opinion it is instructive to think of expressions as *operational* artifacts, they are *evaluated* at
*runtime*. In my opinion, *binding an argument to a function* is more natural than *applying a function to an argument*.
Binding an argument to a function yields an expression that is evaluated by first replacing it by the defining
expression of the function with the argument substituted for the parameter at all its occurrences, and
*continuing evaluation*. Another way to formulate this is as follows.
*Binding an expression argument to an expression producing function* yields an *expression result* that can, be bound as
an expression argument to a subsequent expression producing function. Expression producing functions are also called
*expression evaluation continuations*. Binding an  *expression argument* to an expression evaluation continuation yields
an *outer expression* that is evaluated by first replacing it by the defining expression of the function with the
expression argument substituted for the parameter at all its occurrences, first evaluating those *inner expressions* and
next, continuing evaluation of the outer expression. The evaluation strategy explained above is *eager evaluation*.
Another strategy is *lazy evaluation*, first evaluating the outer expression and next evaluating the inner expressions.
For effectfree functions it does not matter Which strategy is used (Church-Rosser Theorem). 

3. *Side-effects* are corner case function values. In my opinion it is instructive to think of side-effects as
*operational artifacts*, they are *performed* at *runtime*. Performing side-effects can consist of
*performing internal side-effects*, say, *changing internal state*, or *performing external side-effects*, say,
*changing external state*, by *performing input/output*, *performing IO* for short.

### About The Course

The course consists of the documentation of a *`Scala` domain specific language library* for the domain of *programs*,
called *`Scala` program DSL* or *program DSL* for short. `Scala` is a hybrid *object-oriented*, *functional* programming
language. The course mainly uses `Scala` as a functional programming language.

The program DSL is a *program specification DSL*. The program DSL is encoded as a `trait` type class. The program DSL
can be *generically implemented* as a `given` *type class instance* in terms of a *computation DSL* which is also a
*computation specification DSL* that, itself, can be *specifically implemented* in various ways as a `given` type class
instance. Type class instances have *generic* or *specific*, *type arguments* substituted for the type parameters of the
type class. 

The vocabulary of the DSLs consists of *UNDERSCORE_SEPARATED_UPPERCASE* words so that they can easily be distinguished
from *CamelCase* words of the `Scala` language and `Scala` standard library.

In what follows `Scala` is not explicitly mentioned any more. 

The program DSL is a *pointfree generic program specification DSL for application developers*. The program DSL is also
called *program API*. Programs are treated as *closed components*. Programs are values. The course does not call them
*program values*, but it keeps on calling them *programs* instead. In my opinion it is instructive to think of programs
as *denotational artifacts*, they denote  something at compile time and can be given a meaningful *program name*, *name*
for short. In my opinion it is instructive to think of programs as *generic abstractions of functions*. Functions can
then be seen as *concretizations of effectfree programs* and side-effects can then be seen as
*concretizations of effectful programs*.

The program DSL is implemented using a computation DSL.

The computation DSL is a *pointful generic computation specification DSL for library developers*. The computation DSL is
also called *computation LPI*. Computations are treated as *open components*. Computations are values. The course does
not call them *computation values*, but it keeps on calling them *computations* instead. In my opinion it is instructive
to think of computations as *operational artifacts*, they are *executed* at runtime. In my opinion it is instructive to
think of *computation execution* as a *generic abstraction of expression evaluation*. Expression evaluation can then be
seen as a *concretization of effectfree computation execution* and side-effect performing can then be seen as a
*concretization of effectful computation execution*. Another way to formulate this is as follows.
*Binding a computation argument to a computation producing function* yields a *computation result* that can, be bound as
a computation argument to a subsequent computation producing function. Computation producing functions are also called
*computation execution continuations*.

Using the program DSL, code can be written that *specifies a program*. Such code is called a
*program specification*. Program specifications are *syntactic artifacts*. When a *specific implementation* of the
program DSL is in scope, program specifications, implicitly, become *program implementations*. Program implementations
are *semantic artifacts*. The course often simply uses *program* when there is no danger of confusion.

Using the computation DSL, code can be written that *specifies a computation*. Such code is called a
*computation specification*. Computation specifications are *syntactic artifacts*. When a *specific implementation* of
the computation DSL is in scope, computation specifications, implicitly, become *computation implementations*.
Computation implementations are *semantic artifacts*. The course often simply uses *computation* when there is no danger
of confusion.

Using the program DSL, code can be written that *specifies side-effects*. Such code is called a
*side-effect specification*, or *effect* for short. Effects are, harmless, *syntactic artifacts*, evaluating them does
not change any internal or external state. When a *specific implementation* of the program DSL is in scope, effects,
implicitly, become, potentially harmful, *side-effect implementations*, or *side-effects* for short. Side-effects are
*semantic artifacts*, performing them changes internal or external state. The course often simply uses *effect* when there
is no danger of confusion.

Programs can be *combined* using *program combinators*, *combinators* for short, encoding *program features*, also
called *program capabilities*. Program combinators are also called *program composers*, *composers* for short.

- Functionality related combinators for

   - *sequential composition*,
   - *condition*.

- Information related combinators for

   - *sequential construction*,

So far the program combinators are similar to the *funtionals* of the
[FP Programming Language](https://en.wikipedia.org/wiki/FP_(programming_language)) of
[John_Backus](https://en.wikipedia.org/wiki/John_Backus). But there are also differences.

Since the program DSL is *library based* instead of *programming language based*, it can be *extended* with more
combinators. For example with combinators encoding program capabilities such as

- Functionality related combinators for

   - *parallel composition*,
   - ... .

- Information related combinators for

   - *parallel construction*,
   - *(recursive) aggregation*,
   - ... .

and    

- Side-effect related combinators for
 
   - *internal state manipulation*,
   - *external state manipulation*.

### Some Motivating Program Examples

```scala
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
```

The *UNDERSCORE_SEPARATED_UPPERCASE* words are part of of the program DSL. The *lowercase* words are *program names*. It
is instructive to thin of the underscore separated uppercase words as *keywords*, perhaps *keyphrases* would be a more
appropriate name for them.

The `fibonacci` and `factorial` code fragments above are written using the program DSL. The program capabilities that
are used are *sequential composition*, `p AND_THEN q`, *sequential construction*, `p SEQ_AND q` and `lp IN q` (a local
definition being a special case of a sequential construction), and *condition*, `IF(bp) { tq } ELSE { fq }`.

Recall that `fibonacci` and `factorial` are *program specifications*. They can, for example, be implemented using an
*active computation DSL implementation* or a *reactive computation DSL implementation*. Note that `fibonacci` and
`factorial` are *recursive* program specifications.

The `fibonacci` code fragment below uses *parallel construction*, `p PAR_AND q`, instead of sequential construction.

```scala
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

`fibonacci` can, for example, be implemented using a *reactive computation DSL implementation* using *actors* of
`Scala`'s `Akka` library.

### Conclusion

 - `FP` is a *function level programming language*,
 - the program DSL that the course documents is a *program level programming library*,
   as such naturally offering *implementation flexibility* and *feature specification extensibility*.

Please keep on reading ... .

## Chapter02: Functionality Basics

### Functions

Recall that functions are values of *function type*, `Function[Z, Y]`. Their only purpose is to, *transform* an
*argument of type `Z`*, *`Z` argument* for short, to a *result of type `Y`*, *`Y` result* for short. Functions
transforming a *`Z` argument* to a result, are called *`Z` consuming functions*. Functions transforming an argument to
an *`Y` result* are called *`Y` producing functions*.

In this chapter argument types and result types are *primitive types*, like `Boolean`, `BigInt` ... .

Recall that functions are defined using *lambda expressions*. Lambda expressions are of the form `z => e(z)` or, when
the `Scala` type system requires it, `(z: Z) => e(z)`, with *parameter*, `z`, and a *defining expression* `e(z)`.

Lambda expressions are *function literals*, *function values* that are called *anonymous functions*. The expression of a
lambda expression can itself be a lambda expression in which case the lambda expression is a, *higher-order*,
*function producing anonymous function*.

### The `bind` Binary Operation

*Binary operations* can be declared as `infix extension`s and defined accordingly. Binary operations then can be used
with *infix notation*. The course uses the *binding an argument to a function* binary operation instead of the
*applying a function to an argument* binary operation. Below is the `bind` binary operation definition in terms of
`apply`.

```scala
package utilities

extension [Z, Y](z: Z) infix def bind(f_z2y: Function[Z, Y]): Y = f_z2y apply z
```

`f_z2y apply z` is just another way of writing `f_z2y(z)`.

Expressions in which `bind` is used with more than one function fluently read all the way from left to right, and do
not need association parentheses, as in `z bind f_z2y bind f_y2x`. Expressions in which `apply` is used with more than one
function do not fluently read all the way from left to right, and need association parentheses, as in
`f_y2x apply (f_z2y apply z)`.

Functions like `f_y2x` in expressions like `z bind f_z2y bind f_y2x` are also called *continuation functions*,
*continuations* for short. Once an argument `a` has been bound to function `f_z2y`, transforming it to a result `b`,
transforming *continues* by using `b` as an argument and binding it to `f_y2x`.

In my opinion it is instructive to think about `bind` as an operation binding an expression argument `ez` to an
expression producing function `bindExpression`. Below is the `bindExpression` binary operation definition in terms of
`bind`.

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
  infix def bindExpression(f_z2ey: Function[Z, Expression[Y]]): Expression[Y] =
    ez bind f_z2ey
```

Recall that *expression producing functions* can, operationally, be seen as *expression evaluation continuations* as
explained in the introduction.

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
    private[lpi] infix def BIND(f_z2cy: FunctionProducing[Computation][Z, Y]): Computation[Y]
```

in

```scala
package lpi.specification

private[lpi] trait ComputationSpec[Computation[+_]]
    extends ExpressionToComputation[Computation],
      Bind[Computation]
```

`BIND` is a *pointful computation combinator*, not only computations are involved. It models a *generic abstraction* of 
*binding an inner expression evaluation result to an expression evaluation continuation*.

`BIND` can also be seen as a generic abstraction of *binding an argument to a function*. This is a
*pointful program combinator* that the course does not generically abstracts.

Unary type constructor parameter `Computation`, is *covariant* in its parameter type meaning that a computation, say
`cy`, of type `Computation[Y]`, may, when executed at runtime, yield a result having a more specific type than `Y`.

### Pointfree and Pointful

The pointfree code of `zero` is *simpler* (*less complex*) than the pointful code of `zero`. Agreed, it may not be
*easier to understand* (*less difficult to understand*) because it has a higher level of abstraction, but, once the
meaning of `FUNCTION_TO_PROGRAM` and `AND_THEN` is, once and for all, fully understood, it is, becomes easy to
understand.

Fully understanding something difficult, once and for all, is fun!

The pointful code of `zero` is more complex than the pointfree code of `zero`. Therefore it is also, somehow, more
difficult to understand because humans can only deal with a limited amount of complexity.

Being confronted, over and over again, with difficulty caused by complexity is not fun!

One of the main goals of programming in a disciplined way is to manage complexity.

The code of `zero` is only one line of code, with only one
*binding of a computation result as an argument to a computation producing function* involved. The more lines of code,
with more such bindings of, the more important the above statements are.

### Primitve Functions

*Primitive functions* are functions that are lifted up to programs, called *primitive programs*. Primitive programs do
not benefit from the *various zyx-ilities*, like *flexibility* and *extensibility*, that *non-primitive programs*, more
commonly called *composite programs*, benefit from. 

Below are some primitive functions, they all have function types involving *primitive types*, like `BigInt` and
`Boolean` or *generic types*, laike `Z`.

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

being simpler than the first computation producing definition, but, in my opinion, also somewhat more cumbersome. 

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
combinator `AND_THEN` can *generically* be *implemented* by using the pointful computation combinator `BIND`. In my
opinion it is instructive to think of this as a *generic abstraction* of defining `f_z2y andThen f_y2x` as
`f_y2x(f_z2y(z))`, or `f_y2x apply (f_z2y apply z)` or, in my opinion, more natural, `z => (z bind f_z2y) bind f_y2x`
because it naturally reads from left to right and can be simplified to `z => z bind f_z2y bind f_y2x` which does not use
association parenthesis.

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

In my opinion it is instructive to think of all this as folows:

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
        f_z2ay: FunctionProducing[active.Expression][Z, Y]
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

In my opinion, it is instructive think of a `given` implementation of a `trait` specification as a *statement*, more
precisely, an *existential statement*. Such statements can be *generic* and *specific*. Such statements can then be used
in *proofs*, more precisely, *existential proofs*. 

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
        f_z2cbhy: FunctionProducing[reactive.Expression][Z, Y]
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

In my opinion it is instructive to compare the reactive definition with the active definition. The only *harmless* thing
one can do with `u bind f_u2u` is *binding it to `identity`*, resulting in `(u bind f_u2u) bind identity`. The only
*meaningful* thing one can do with `u bind f_u2cbhu` is *binding `identity` to it*, resulting in
`identity[Unit] bind (u bind f_u2cbhu)`. There is some kind of *duality*, or *bi-duality* if you wish, involved here
(values of type `Function[Z, Unit]` could be called *dual values* and values of type `Function[Function[Z, Unit], Unit]`
could be called *bi-dual values*).

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
        f_z2cx: FunctionProducing[Computation][Z, X]
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

## Chapter06: Stateful Functionality

This chapter concentrates on *stateful functionality related combinators*, in particular on the program combinator
`READ_STATE`, modeling the *reading internal state side-effect* and the program combinator `WRITE_STATE`, modeling the
*writing internal state side-effect*.

 ### `StatefulProgram`

- *Programs can manipulate internal state*.

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

  // added for chapter06
  val readUnitArgument: Program[Unit, Unit] =
    readUnitArgumentFunction bind FUNCTION_TO_PROGRAM
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
        f_z2shx: active.stateful.Function[S][Z, X]
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