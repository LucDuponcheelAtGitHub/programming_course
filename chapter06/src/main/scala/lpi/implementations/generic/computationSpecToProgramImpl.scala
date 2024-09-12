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
