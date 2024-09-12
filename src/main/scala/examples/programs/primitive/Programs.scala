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
