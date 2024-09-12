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
