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
