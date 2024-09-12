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
