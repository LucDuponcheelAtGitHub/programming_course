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
