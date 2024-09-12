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



