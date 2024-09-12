## Chapter01: Introduction
 
Many sentences of the introduction chapter of the course make an opinionated statement about functional programming. Do
not let those opinionated statements, even if you do not fully agree with them, make you decide to give up on studying
the course. Hopefully the content chapters of the course will, eventually, motivate the statements sufficiently. 

Programming, like many other activities, should be done in a disciplined way. This course reflects my vision on how
programming can be done in a disciplined way. Mathematical concepts almost inevitably come into play. I don't want to
overwhelm you with mathematics. I want to concentrate on functional programming.

Writing the course therefore poses some challenges.

1. The choice of natural language constructs that describe functional programming concepts.

    - When first used, such natural language constructs *are emphasized*. When later used, they *may be emphasized*.

2. The order in which functional programming concepts are introduced. 

    - A course must not to use concepts that have not been defined earlier in the course. For a math course this
      requirement is mandatory. For a proramming course this requirement is highly desirable.

Writing the introduction chapter of the course then also comes some challenge.

1. The level of detail at which functional programming concepts are explained.

    - On the one hand, an introductory chapter should not go into too much detail. On the other hand, an introductory
      chapter should be detailed enough to stay understandable.

**Warning**

1. Do not expect the introduction chapter to even try to be *sound* or *complete*.

    - The content chapters of the course try to be sound and complete.

2. Do not expect the introduction chapter to be *easy*.

    - The introduction chapter is, almost inevitably, *complex* because it introduces a lot of functional programming
      concepts and corresponding natural language constructs. Therefore it is inherently somewhat *difficult* since
      humans can only deal with a limited amount of complexity.

### About The Course

The course consists of the documentation of a *`Scala` domain specific language library* for the domain of *programs*,
called *`Scala` program DSL* or *program DSL* for short. `Scala` is a hybrid *object-oriented*, *functional* programming
language. The course uses `Scala` as a functional programming language.

There is a great risk that the words *program* and *programming* are used in an ambiguous way. Therefore I use the words
*code* and *encoding* when dealing with anything written in the `Scala` programming language, resp. *program* and
*programming* when dealing with anything related to the program DSL.

### Functional Programming

Programming is about modeling *information* and *functionality*. Functional programming is a programming paradigm that
uses *expressions* and *functions* to help you model information and functionality in a disciplined way. 

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
*function name*, *name* for short. Functions are defined using *lambda expressions* `z => e_z` consisting of a
*function parameter*, `z`, *parameter* for short, of type `Z`, and a *function defining expression*, `e_z`,
*defining expression* for short, of type `Y`. The defining expression `e_z` is an *expression* wherein parameter `z`
occurs at zero or more *positions*. Expressions are *operational* artifacts, they are *evaluated* at *runtime* to a
*value*. Evaluating an expression consists of *rewriting* many times until it yields a value. Functions can also
*consume* an *expression argument* to *produce* a *result value*. Values are expessions, so it makes sense to reason as
much as possible in terms of functions and expressions and let values play an auxiliary role.
*Binding an expression argument* to a function yields an expression that is evaluated by first rewriting it to the
defining expression of the function with the expression substituted for the parameter at all its positions, obtaining
an *outer expression* having *inner expressions* at all parameter positions. There is a famous theorem, called the
*Church-Rosser Theorem*, that states that, the way in which to *continue* rewriting does not matter. *Eager evaluation*
continues by first rewriting the inner expressions to yield values. *Lazy evaluation* continues by first rewriting the
outer expression as much as possible leaving the inner expressions intact. One way to reason about all this is as
follows. Let's, for the sake of explainig this reasoning, define a trivial unary type constructor for expressions:
`type Expression = [Z] =>> Z`.
*Binding an (expression) argument of type `E[Z]` to an expression producing function of type `Function[Z, E[Y]]`*
yields an *expression result* of type `E[Y]` that can be bound as an (expression) argument to a subsequent expression
producing function. Expression producing functions are also called *expression evaluation continuations*.

Programming is also about modeling *side-effects*.

3. *Side-effects* are corner case expressions. Side-effects as *operational artifacts*, they are *performed* at
*runtime*. Performing side-effects can consist of *performing internal side-effects*, say,
*manipulating internal state*, or *performing external side-effects*, say, *performing input/output*, *performing IO*
for short.

*The course generically abstracts expressions, functions and side-effects by specifying them as computations, programs,
and effects, in order to extend computations and programs with extra programming capabilities, and to also model
side-effects in a disciplined way.*

### Pointful Versus Pointfree

Functions can either be used in a *pointful* way, treating tham as *open components*, or be used in a *pointfree* way,
treating tham as *open components*. 

Below is a first illustration

```scala
scala> extension [Z, Y](z: Z) infix def bind(f_z2y: => Function[Z, Y]): Y = f_z2y apply z
def bind[Z, Y](z: Z)(f_z2y: Z => Y): Y
                                                                                                                                                    
scala> val z = 1
val z: Int = 1
                                                                                                                                                    
scala> val z2y = (z: Int) => z + 1
val z2y: Int => Int = Lambda$8326/0x00006ffc68c90a08@4394fcdf
                                                                                                                                                    
scala> val y2x = (y: Int) => y * y
val y2x: Int => Int = Lambda$8327/0x00006ffc69319208@3ffa7899
                                                                                                                                                    
scala> val z2x = (z: Int) => z bind z2y bind y2x
val z2x: Int => Int = Lambda$8328/0x00006ffc69319a08@22d67840
                                                                                                                                                    
scala> z bind z2x
val res0: Int = 4
                                                                                                                                                    
scala> val z2x = z2y andThen y2x
val z2x: Int => Int = scala.Function1$$Lambda$8137/0x00006ffc69223218@7b480c90
                                                                                                                                                    
scala> z bind z2x
val res1: Int = 4
```

`bind` is a *binary operation* that can be used with *infix notation*.

The first definition of `z2x` is a *pointful* one. The second definition of `z2x` is a *pointfree* one.

Below is a second illustration

```scala
scala> extension [Z, Y, X](z2y: Function[Z, Y]) 
         infix def seqAnd(z2x: Function[Z, X]): => Function[Z, (Y, X)] = z => (z bind z2y, z bind z2x)
def seqAnd[Z, Y, X](z2y: Z => Y)(z2x: Z => X): Z => (Y, X)
                                                                                                                                                    
scala> val z2x = (z: Int) => z * z 
val z2x: Int => Int = Lambda$8329/0x00006ffc69327040@7ff7c056
                                                                                                                                                    
scala> val z2yax = (z: Int) => z bind z2y bind { y => z bind z2x bind { x => (y, x) } }
val z2yax: Int => (Int, Int) = Lambda$8330/0x00006ffc69327840@4cc391d4
                                                                                                                                                    
scala> z bind z2yax
val res2: (Int, Int) = (2,1)
                                                                                                                                                    
scala> val z2yax = z2y seqAnd z2x
val z2yax: Int => (Int, Int) = Lambda$8333/0x00006ffc68ca4fc0@74620113
                                                                                                                                                    
scala> z bind z2yax
val res3: (Int, Int) = (2,1)
```

`seqAnd` is a *binary operation* that can be used with *infix notation*.

The first definition of `z2yax` is a *pointful* one. The second definition of `z2yax` is a *pointfree* one. 

The first definition of `z2yax`, kind of, opens expression `z bind z2y` to yield the value `y` of evaluating it and
subsequently, kind of, opens expression `z bind z2x` to yield the value `x` of evaluating it. `y` and `x` can then be
used to obtain `(y, x)`. The first definition of `z2x` of the first illustration could also have been
`val z2x = (z: Int) => z bind z2y bind { y => y bind y2x }`, opening expression `z bind z2y`to yield the value `y` of
evaluating it.

Pointfree code is *simpler* (*less complex*) than pointful code. Agreed, it may not be *easier to understand*
(*less difficult to understand*) because it has a higher level of abstraction, but, once its meaning is, once and for
all, fully understood, it is, becomes easy to understand.

Fully understanding something difficult, once and for all, is fun!

Pointful code is more complex than pointfree code. Therefore it is also, somehow, more difficult to understand because
humans can only deal with a limited amount of complexity.

Being confronted, over and over again, with difficulty caused by complexity is not fun!

One of the main goals of programming in a disciplined way is to manage complexity.

The examples above are only one line of code, with only one
*binding of an expression result as an argument to an expression producing function* involved. The more lines of code,
with more such bindings of, the more important the above statements are.

*The course treats programs as open components and computations as closed components, as a generic abstraction of 
treating functions as open components and expressions as open components.*

### About The Program DSL

The program DSL is a *program specification DSL*. The program DSL is encoded as a `trait` type class. The program DSL
can be *generically implemented* as a `given` *type class instance* in terms of a *computation DSL* which is also a
*computation specification DSL* that, itself, can be *specifically implemented* in various ways as a `given` type class
instance. Type class instances have *generic* or *specific*, *type arguments* substituted for the type parameters of the
type class. 

*The course defines a program specification DSL in order to offer library developers the flexibility to provide different
implementations without impacting application developers that use of the specification DSL. Moreover the course defines
a program specification DSL resp. computation specification DSL in order to offer application developers resp. library
developers a standard way to write applications resp. library implementations*.

The vocabulary of the DSLs consists of *UNDERSCORE_SEPARATED_UPPERCASE* words so that they can easily be distinguished
from *CamelCase* words of the `Scala` language and `Scala` standard library.

In what follows `Scala` is not explicitly mentioned any more. 

The program DSL is a *pointfree generic program specification DSL for application developers*. The program DSL is also
called *program API*, it's code belongs to `package api`. Programs are treated as *closed components*. Programs are
values. The course does not call them *program values*, but it keeps on calling them *programs* instead. Programs are
*denotational artifacts*, they denote something at compile time and can be given a meaningful *program name*, *name*
for short. Programs are *generic abstractions of functions*. Functions are *concretizations of effectfree programs* and
side-effects are *concretizations of effectful programs*.

The program DSL is implemented using a computation DSL.

The computation DSL is a *pointful generic computation specification DSL for library developers*. The computation DSL is
also called *computation LPI*, it's code belongs to `package lpi`. Computations are treated as *open components*.
Computations are values. The course does not call them *computation values*, but it keeps on calling them *computations*
instead. Computations as *operational artifacts*, they are *executed* at runtime. *Computation execution* is a
*generic abstraction of expression evaluation*. Expression evaluation is a
*concretization of effectfree computation execution* and side-effect performing is a
*concretization of effectful computation execution*. Another way to formulate this is as follows.

*Binding an computation argument of type `C[Z]` to a computation producing function of type `Function[Z, C[Y]]`*
yields a *computation result* of type `C[Y]` that can be bound as a computation argument to a subsequent computation
producing function. Computation producing functions are also called *computation execution continuations*.

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
*semantic artifacts*, performing them changes internal or external state. The course often simply uses *effect* when
there is no danger of confusion.

Programs can be *combined* using *program combinators*, *combinators* for short, encoding *program features*, also
called *program capabilities*. Program combinators are also called *program composers*, *composers* for short. For
example

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

### Some Motivating Effectfree Program Examples

```scala
  val fibonacci: Program[BigInt, BigI nt] =
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

The *UNDERSCORE_SEPARATED_UPPERCASE* words are part of the program DSL. The *lowercase* words are *program names*, in
this case *primitive program names*. The underscore separated uppercase words are *library level keywords*, perhaps
*library level keyphrases* would be more appropriate.

The `fibonacci` and `factorial` code fragments above are written using the program DSL. The program capabilities that
are used are *sequential composition*, `p AND_THEN q`, *sequential construction*, `p SEQ_AND q` and `lp IN q` (a local
definition, being a special case of a sequential construction), and *condition*, `IF(bp) { tq } ELSE { fq }`.
`fibonacci` and `factorial` are *recursive* program specifications. Similar to `FP`, a program specification may be
defined *recursively* by an equation of the form `val p = P(p)`, where `P(p)` is a program specification built from
program names, and the program name `p` itself, using combinators.

Recall that `fibonacci` and `factorial` are *program specifications*. They can, for example, be implemented using an
*active computation DSL implementation* or a *reactive computation DSL implementation*. 

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

`fibonacci` above can, for example, be implemented using a *reactive computation DSL implementation* using *actors* of
`Scala`'s `Akka` library.

### Conclusion

Compared with `FP`. a *function level programming language*, the program DSL that the course documents is a
*program level programming library*, as such naturally offering *implementation flexibility* and 
*both effectfree and effectful feature specification extensibility*.

Please keep on reading ... .



