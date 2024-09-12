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
